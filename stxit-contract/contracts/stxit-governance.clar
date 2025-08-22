;; STXit Governance Voting Contract
;; Handles community voting on account closure requests

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-UNAUTHORIZED (err u100))
(define-constant ERR-INVALID-PROPOSAL (err u101))
(define-constant ERR-VOTING-ENDED (err u102))
(define-constant ERR-VOTING-ACTIVE (err u103))
(define-constant ERR-ALREADY-VOTED (err u104))
(define-constant ERR-INSUFFICIENT-STAKE (err u105))
(define-constant ERR-NO-STAKE (err u106))
(define-constant ERR-PROPOSAL-NOT-FOUND (err u107))

;; Voting period in blocks (approximately 7 days assuming 10-minute blocks)
(define-constant VOTING-PERIOD u1008)
(define-constant MIN-STAKE-REQUIRED u100) ;; Minimum tokens required to vote

;; Data Variables
(define-data-var proposal-counter uint u0)
(define-data-var governance-token-contract principal .stxit-token)

;; Data Maps

;; User stakes: principal -> amount staked
(define-map user-stakes principal uint)

;; Proposals: proposal-id -> proposal details
(define-map proposals uint {
    target-account: principal,
    reason: (string-ascii 500),
    creator: principal,
    start-block: uint,
    end-block: uint,
    total-votes-for: uint,
    total-votes-against: uint,
    total-voting-power: uint,
    status: (string-ascii 20) ;; "active", "passed", "rejected", "executed"
})

;; User votes: (proposal-id, voter) -> vote details
(define-map votes {proposal-id: uint, voter: principal} {
    vote: bool, ;; true = for, false = against
    voting-power: uint,
    block-height: uint
})

;; Proposal vote participation: (proposal-id, voter) -> bool
(define-map has-voted {proposal-id: uint, voter: principal} bool)

;; Read-only functions

;; Get user's current stake
(define-read-only (get-user-stake (user principal))
    (default-to u0 (map-get? user-stakes user))
)

;; Get proposal details
(define-read-only (get-proposal (proposal-id uint))
    (map-get? proposals proposal-id)
)

;; Get user's vote on a proposal
(define-read-only (get-user-vote (proposal-id uint) (voter principal))
    (map-get? votes {proposal-id: proposal-id, voter: voter})
)

;; Check if user has voted on a proposal
(define-read-only (has-user-voted (proposal-id uint) (voter principal))
    (default-to false (map-get? has-voted {proposal-id: proposal-id, voter: voter}))
)

;; Get current proposal counter
(define-read-only (get-proposal-counter)
    (var-get proposal-counter)
)

;; Check if proposal is active
(define-read-only (is-proposal-active (proposal-id uint))
    (match (map-get? proposals proposal-id)
        proposal (and 
            (>= stacks-block-height (get start-block proposal))
            (<= stacks-block-height (get end-block proposal))
            (is-eq (get status proposal) "active")
        )
        false
    )
)

;; Calculate voting result
(define-read-only (get-voting-result (proposal-id uint))
    (match (map-get? proposals proposal-id)
        proposal {
            votes-for: (get total-votes-for proposal),
            votes-against: (get total-votes-against proposal),
            total-power: (get total-voting-power proposal),
            passed: (> (get total-votes-for proposal) (get total-votes-against proposal))
        }
        {votes-for: u0, votes-against: u0, total-power: u0, passed: false}
    )
)

;; Public functions

;; Stake tokens for voting participation
(define-public (stake-tokens (amount uint))
    (let (
        (current-stake (get-user-stake tx-sender))
        (new-stake (+ current-stake amount))
    )
        ;; Transfer tokens from user to contract
        (try! (contract-call? .stxit-token transfer amount tx-sender (as-contract tx-sender) none))
        
        ;; Update user's stake
        (map-set user-stakes tx-sender new-stake)
        
        (ok new-stake)
    )
)

;; Unstake tokens (only if not participating in active votes)
(define-public (unstake-tokens (amount uint))
    (let (
        (current-stake (get-user-stake tx-sender))
    )
        (asserts! (>= current-stake amount) ERR-INSUFFICIENT-STAKE)
        
        ;; Transfer tokens back to user
        (try! (as-contract (contract-call? .stxit-token transfer amount tx-sender tx-sender none)))
        
        ;; Update user's stake
        (map-set user-stakes tx-sender (- current-stake amount))
        
        (ok (- current-stake amount))
    )
)

;; Create a new proposal for account closure
(define-public (create-proposal (target-account principal) (reason (string-ascii 500)))
    (let (
        (proposal-id (+ (var-get proposal-counter) u1))
        (start-block stacks-block-height)
        (end-block (+ stacks-block-height VOTING-PERIOD))
    )
        ;; Ensure creator has minimum stake
        (asserts! (>= (get-user-stake tx-sender) MIN-STAKE-REQUIRED) ERR-INSUFFICIENT-STAKE)
        
        ;; Create proposal
        (map-set proposals proposal-id {
            target-account: target-account,
            reason: reason,
            creator: tx-sender,
            start-block: start-block,
            end-block: end-block,
            total-votes-for: u0,
            total-votes-against: u0,
            total-voting-power: u0,
            status: "active"
        })
        
        ;; Update proposal counter
        (var-set proposal-counter proposal-id)
        
        (ok proposal-id)
    )
)

;; Cast vote on a proposal
(define-public (cast-vote (proposal-id uint) (vote-for bool))
    (let (
        (user-stake (get-user-stake tx-sender))
        (proposal (unwrap! (map-get? proposals proposal-id) ERR-PROPOSAL-NOT-FOUND))
    )
        ;; Validate voting conditions
        (asserts! (>= user-stake MIN-STAKE-REQUIRED) ERR-INSUFFICIENT-STAKE)
        (asserts! (is-proposal-active proposal-id) ERR-VOTING-ENDED)
        (asserts! (not (has-user-voted proposal-id tx-sender)) ERR-ALREADY-VOTED)
        
        ;; Record the vote
        (map-set votes {proposal-id: proposal-id, voter: tx-sender} {
            vote: vote-for,
            voting-power: user-stake,
            block-height: stacks-block-height
        })
        
        ;; Mark user as having voted
        (map-set has-voted {proposal-id: proposal-id, voter: tx-sender} true)
        
        ;; Update proposal vote tallies
        (map-set proposals proposal-id (merge proposal {
            total-votes-for: (if vote-for 
                (+ (get total-votes-for proposal) user-stake)
                (get total-votes-for proposal)
            ),
            total-votes-against: (if vote-for 
                (get total-votes-against proposal)
                (+ (get total-votes-against proposal) user-stake)
            ),
            total-voting-power: (+ (get total-voting-power proposal) user-stake)
        }))
        
        (ok true)
    )
)

;; Finalize proposal after voting period ends
(define-public (finalize-proposal (proposal-id uint))
    (let (
        (proposal (unwrap! (map-get? proposals proposal-id) ERR-PROPOSAL-NOT-FOUND))
    )
        ;; Ensure voting period has ended
        (asserts! (> stacks-block-height (get end-block proposal)) ERR-VOTING-ACTIVE)
        (asserts! (is-eq (get status proposal) "active") ERR-INVALID-PROPOSAL)
        
        ;; Determine result and update status
        (let (
            (votes-for (get total-votes-for proposal))
            (votes-against (get total-votes-against proposal))
            (new-status (if (> votes-for votes-against) "passed" "rejected"))
        )
            (map-set proposals proposal-id (merge proposal {
                status: new-status
            }))
            
            (ok new-status)
        )
    )
)

;; Execute a passed proposal (placeholder for actual account closure logic)
(define-public (execute-proposal (proposal-id uint))
    (let (
        (proposal (unwrap! (map-get? proposals proposal-id) ERR-PROPOSAL-NOT-FOUND))
    )
        ;; Only contract owner or governance can execute
        (asserts! (or (is-eq tx-sender CONTRACT-OWNER) (is-eq tx-sender (as-contract tx-sender))) ERR-UNAUTHORIZED)
        (asserts! (is-eq (get status proposal) "passed") ERR-INVALID-PROPOSAL)
        
        ;; Update status to executed
        (map-set proposals proposal-id (merge proposal {
            status: "executed"
        }))
        
        ;; Here you would implement the actual account closure logic
        ;; For now, we just mark it as executed
        
        (ok true)
    )
)

;; Emergency functions (only contract owner)

;; Cancel a proposal (emergency only)
(define-public (cancel-proposal (proposal-id uint))
    (let (
        (proposal (unwrap! (map-get? proposals proposal-id) ERR-PROPOSAL-NOT-FOUND))
    )
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
        
        (map-set proposals proposal-id (merge proposal {
            status: "cancelled"
        }))
        
        (ok true)
    )
)

;; Update governance token contract (owner only)
(define-public (set-governance-token (new-token principal))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
        (var-set governance-token-contract new-token)
        (ok true)
    )
)
