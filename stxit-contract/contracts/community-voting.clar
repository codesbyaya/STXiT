;; Community Voting Smart Contract for Account Closure Requests
;; Implements STXit governance token staking and voting mechanism

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant VOTING-PERIOD u144) ;; ~7 days in blocks (assuming 10 min blocks)
(define-constant MIN-STAKE u1000000) ;; Minimum tokens required to vote (1 token with 6 decimals)

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-REQUEST (err u101))
(define-constant ERR-INSUFFICIENT-STAKE (err u102))
(define-constant ERR-ALREADY-VOTED (err u103))
(define-constant ERR-VOTING-ENDED (err u104))
(define-constant ERR-VOTING-ACTIVE (err u105))
(define-constant ERR-REQUEST-NOT-FOUND (err u106))
(define-constant ERR-INSUFFICIENT-BALANCE (err u107))

;; Data Variables
(define-data-var next-request-id uint u1)
(define-data-var governance-token-contract principal 'SP000000000000000000002Q6VF78.pox)

;; Data Maps
;; Closure requests
(define-map closure-requests
  { request-id: uint }
  {
    target-account: principal,
    requester: principal,
    reason: (string-ascii 500),
    created-at: uint,
    voting-end: uint,
    total-yes-votes: uint,
    total-no-votes: uint,
    total-staked: uint,
    is-executed: bool,
    is-approved: bool
  }
)

;; User stakes for each request
(define-map user-stakes
  { request-id: uint, voter: principal }
  {
    amount: uint,
    vote: bool, ;; true for yes, false for no
    timestamp: uint
  }
)

;; User's total staked tokens across all active votes
(define-map user-total-stakes
  { user: principal }
  { total-staked: uint }
)

;; Traits
(define-trait sip010-trait
  (
    (transfer (uint principal principal (optional (buff 34))) (response bool uint))
    (get-name () (response (string-ascii 32) uint))
    (get-symbol () (response (string-ascii 32) uint))
    (get-decimals () (response uint uint))
    (get-balance (principal) (response uint uint))
    (get-total-supply () (response uint uint))
    (get-token-uri () (response (optional (string-utf8 256)) uint))
  )
)

;; Public Functions

;; Initialize governance token contract (only owner)
(define-public (set-governance-token (token-contract principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (var-set governance-token-contract token-contract)
    (ok true)
  )
)

;; Create a new account closure request
(define-public (create-closure-request (target-account principal) (reason (string-ascii 500)))
  (let
    (
      (request-id (var-get next-request-id))
      (current-block stacks-block-height)
    )
    (begin
      ;; Store the closure request
      (map-set closure-requests
        { request-id: request-id }
        {
          target-account: target-account,
          requester: tx-sender,
          reason: reason,
          created-at: current-block,
          voting-end: (+ current-block VOTING-PERIOD),
          total-yes-votes: u0,
          total-no-votes: u0,
          total-staked: u0,
          is-executed: false,
          is-approved: false
        }
      )
      ;; Increment request ID for next request
      (var-set next-request-id (+ request-id u1))
      (ok request-id)
    )
  )
)

;; Stake tokens and vote on a closure request
(define-public (stake-and-vote (request-id uint) (stake-amount uint) (vote-yes bool))
  (let
    (
      (request (unwrap! (map-get? closure-requests { request-id: request-id }) ERR-REQUEST-NOT-FOUND))
      (current-block stacks-block-height)
      (token-contract (var-get governance-token-contract))
      (user-current-stake (default-to u0 (get total-staked (map-get? user-total-stakes { user: tx-sender }))))
    )
    (begin
      ;; Validate voting period
      (asserts! (< current-block (get voting-end request)) ERR-VOTING-ENDED)
      ;; Validate minimum stake
      (asserts! (>= stake-amount MIN-STAKE) ERR-INSUFFICIENT-STAKE)
      ;; Check if user already voted on this request
      (asserts! (is-none (map-get? user-stakes { request-id: request-id, voter: tx-sender })) ERR-ALREADY-VOTED)
      
      ;; Transfer tokens to contract for staking
      
      ;; Record user's vote and stake
      (map-set user-stakes
        { request-id: request-id, voter: tx-sender }
        {
          amount: stake-amount,
          vote: vote-yes,
          timestamp: current-block
        }
      )
      
      ;; Update user's total staked amount
      (map-set user-total-stakes
        { user: tx-sender }
        { total-staked: (+ user-current-stake stake-amount) }
      )
      
      ;; Update request vote totals
      (map-set closure-requests
        { request-id: request-id }
        (merge request {
          total-yes-votes: (if vote-yes (+ (get total-yes-votes request) stake-amount) (get total-yes-votes request)),
          total-no-votes: (if vote-yes (get total-no-votes request) (+ (get total-no-votes request) stake-amount)),
          total-staked: (+ (get total-staked request) stake-amount)
        })
      )
      
      (ok true)
    )
  )
)

;; Execute closure request after voting period ends
(define-public (execute-closure-request (request-id uint))
  (let
    (
      (request (unwrap! (map-get? closure-requests { request-id: request-id }) ERR-REQUEST-NOT-FOUND))
      (current-block stacks-block-height)
    )
    (begin
      ;; Validate voting has ended
      (asserts! (>= current-block (get voting-end request)) ERR-VOTING-ACTIVE)
      ;; Validate not already executed
      (asserts! (not (get is-executed request)) ERR-INVALID-REQUEST)
      
      ;; Determine if request is approved (simple majority)
      (let
        (
          (is-approved (> (get total-yes-votes request) (get total-no-votes request)))
        )
        ;; Update request status
        (map-set closure-requests
          { request-id: request-id }
          (merge request {
            is-executed: true,
            is-approved: is-approved
          })
        )
        (ok is-approved)
      )
    )
  )
)

;; Withdraw staked tokens after voting period ends
(define-public (withdraw-stake (request-id uint))
  (let
    (
      (request (unwrap! (map-get? closure-requests { request-id: request-id }) ERR-REQUEST-NOT-FOUND))
      (user-stake (unwrap! (map-get? user-stakes { request-id: request-id, voter: tx-sender }) ERR-INVALID-REQUEST))
      (current-block stacks-block-height)
      (token-contract (var-get governance-token-contract))
      (stake-amount (get amount user-stake))
      (user-current-total (default-to u0 (get total-staked (map-get? user-total-stakes { user: tx-sender }))))
    )
    (begin
      ;; Validate voting has ended
      (asserts! (>= current-block (get voting-end request)) ERR-VOTING-ACTIVE)
      
      ;; Transfer staked tokens back to user
      
      ;; Remove user stake record
      (map-delete user-stakes { request-id: request-id, voter: tx-sender })
      
      ;; Update user's total staked amount
      (map-set user-total-stakes
        { user: tx-sender }
        { total-staked: (- user-current-total stake-amount) }
      )
      
      (ok stake-amount)
    )
  )
)

;; Read-only functions

;; Get closure request details
(define-read-only (get-closure-request (request-id uint))
  (map-get? closure-requests { request-id: request-id })
)

;; Get user's vote for a specific request
(define-read-only (get-user-vote (request-id uint) (voter principal))
  (map-get? user-stakes { request-id: request-id, voter: voter })
)

;; Get user's total staked tokens
(define-read-only (get-user-total-stake (user principal))
  (default-to u0 (get total-staked (map-get? user-total-stakes { user: user })))
)

;; Get current request ID
(define-read-only (get-next-request-id)
  (var-get next-request-id)
)

;; Get governance token contract
(define-read-only (get-governance-token-contract)
  (var-get governance-token-contract)
)

;; Check if voting is active for a request
(define-read-only (is-voting-active (request-id uint))
  (match (map-get? closure-requests { request-id: request-id })
    request (< stacks-block-height (get voting-end request))
    false
  )
)
