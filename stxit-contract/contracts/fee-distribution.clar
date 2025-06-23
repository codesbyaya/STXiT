;; Fee Distribution Smart Contract
;; Distributes fees to voters and platform after voting periods

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-UNAUTHORIZED (err u100))
(define-constant ERR-INVALID-PERCENTAGE (err u101))
(define-constant ERR-VOTING-ACTIVE (err u102))
(define-constant ERR-VOTING-ENDED (err u103))
(define-constant ERR-INSUFFICIENT-BALANCE (err u104))
(define-constant ERR-NO-VOTES (err u105))
(define-constant ERR-ALREADY-DISTRIBUTED (err u106))
(define-constant ERR-INVALID-VOTING-ID (err u107))

;; Data Variables
(define-data-var voter-reward-percentage uint u70) ;; 70% to voters
(define-data-var platform-fee-percentage uint u30) ;; 30% to platform
(define-data-var platform-wallet principal CONTRACT-OWNER)
(define-data-var voting-counter uint u0)

;; Data Maps
(define-map voting-sessions
  { voting-id: uint }
  {
    total-fees: uint,
    voter-count: uint,
    end-block: uint,
    distributed: bool,
    creator: principal
  }
)

(define-map voter-participation
  { voting-id: uint, voter: principal }
  { participated: bool, reward-claimed: bool }
)

(define-map voting-fees
  { voting-id: uint }
  { total-collected: uint }
)

;; Read-only functions
(define-read-only (get-voter-reward-percentage)
  (var-get voter-reward-percentage)
)

(define-read-only (get-platform-fee-percentage)
  (var-get platform-fee-percentage)
)

(define-read-only (get-voting-session (voting-id uint))
  (map-get? voting-sessions { voting-id: voting-id })
)

(define-read-only (get-voter-participation (voting-id uint) (voter principal))
  (map-get? voter-participation { voting-id: voting-id, voter: voter })
)

(define-read-only (calculate-voter-reward (voting-id uint))
  (let (
    (session (unwrap! (get-voting-session voting-id) (err ERR-INVALID-VOTING-ID)))
    (total-fees (get total-fees session))
    (voter-count (get voter-count session))
    (voter-pool (/ (* total-fees (var-get voter-reward-percentage)) u100))
  )
    (if (> voter-count u0)
      (ok (/ voter-pool voter-count))
      (ok u0)
    )
  )
)

(define-read-only (calculate-platform-fee (voting-id uint))
  (let (
    (session (unwrap! (get-voting-session voting-id) (err ERR-INVALID-VOTING-ID)))
    (total-fees (get total-fees session))
  )
    (ok (/ (* total-fees (var-get platform-fee-percentage)) u100))
  )
)

(define-read-only (is-voting-ended (voting-id uint))
  (let (
    (session (unwrap! (get-voting-session voting-id) (err ERR-INVALID-VOTING-ID)))
  )
    (ok (>= stacks-block-height (get end-block session)))
  )
)

;; Admin functions
(define-public (set-reward-percentages (voter-percentage uint) (platform-percentage uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (asserts! (is-eq (+ voter-percentage platform-percentage) u100) ERR-INVALID-PERCENTAGE)
    (var-set voter-reward-percentage voter-percentage)
    (var-set platform-fee-percentage platform-percentage)
    (ok true)
  )
)

(define-public (set-platform-wallet (new-wallet principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (var-set platform-wallet new-wallet)
    (ok true)
  )
)

;; Core functions
(define-public (create-voting-session (duration-blocks uint))
  (let (
    (voting-id (+ (var-get voting-counter) u1))
    (end-block (+ stacks-block-height duration-blocks))
  )
    (map-set voting-sessions
      { voting-id: voting-id }
      {
        total-fees: u0,
        voter-count: u0,
        end-block: end-block,
        distributed: false,
        creator: tx-sender
      }
    )
    (var-set voting-counter voting-id)
    (ok voting-id)
  )
)

(define-public (pay-voting-fee (voting-id uint) (fee-amount uint))
  (let (
    (session (unwrap! (get-voting-session voting-id) ERR-INVALID-VOTING-ID))
  )
    (asserts! (not (unwrap! (is-voting-ended voting-id) ERR-INVALID-VOTING-ID)) ERR-VOTING-ENDED)
    
    ;; Transfer fee to contract
    (try! (stx-transfer? fee-amount tx-sender (as-contract tx-sender)))
    
    ;; Update total fees
    (map-set voting-sessions
      { voting-id: voting-id }
      (merge session { total-fees: (+ (get total-fees session) fee-amount) })
    )
    
    (ok true)
  )
)

(define-public (register-vote (voting-id uint))
  (let (
    (session (unwrap! (get-voting-session voting-id) ERR-INVALID-VOTING-ID))
    (existing-participation (map-get? voter-participation { voting-id: voting-id, voter: tx-sender }))
  )
    (asserts! (not (unwrap! (is-voting-ended voting-id) ERR-INVALID-VOTING-ID)) ERR-VOTING-ENDED)
    
    ;; Only count new voters
    (if (is-none existing-participation)
      (begin
        (map-set voter-participation
          { voting-id: voting-id, voter: tx-sender }
          { participated: true, reward-claimed: false }
        )
        (map-set voting-sessions
          { voting-id: voting-id }
          (merge session { voter-count: (+ (get voter-count session) u1) })
        )
      )
      ;; Update existing participation
      (map-set voter-participation
        { voting-id: voting-id, voter: tx-sender }
        { participated: true, reward-claimed: false }
      )
    )
    
    (ok true)
  )
)


(define-public (distribute-fees (voting-id uint))
  (let (
    (session (unwrap! (get-voting-session voting-id) ERR-INVALID-VOTING-ID))
    (platform-fee (unwrap! (calculate-platform-fee voting-id) ERR-INVALID-VOTING-ID))
  )
    (asserts! (unwrap! (is-voting-ended voting-id) ERR-INVALID-VOTING-ID) ERR-VOTING-ACTIVE)
    (asserts! (not (get distributed session)) ERR-ALREADY-DISTRIBUTED)
    (asserts! (> (get total-fees session) u0) ERR-INSUFFICIENT-BALANCE)
    
    ;; Transfer platform fee
    (try! (as-contract (stx-transfer? platform-fee tx-sender (var-get platform-wallet))))
    
    ;; Mark as distributed
    (map-set voting-sessions
      { voting-id: voting-id }
      (merge session { distributed: true })
    )
    
    (ok true)
  )
)

(define-public (claim-voter-reward (voting-id uint))
  (let (
    (session (unwrap! (get-voting-session voting-id) ERR-INVALID-VOTING-ID))
    (participation (unwrap! (get-voter-participation voting-id tx-sender) ERR-NO-VOTES))
    (reward-amount (unwrap! (calculate-voter-reward voting-id) ERR-INVALID-VOTING-ID))
  )
    (asserts! (unwrap! (is-voting-ended voting-id) ERR-INVALID-VOTING-ID) ERR-VOTING-ACTIVE)
    (asserts! (get distributed session) ERR-VOTING-ACTIVE)
    (asserts! (get participated participation) ERR-NO-VOTES)
    (asserts! (not (get reward-claimed participation)) ERR-ALREADY-DISTRIBUTED)
    (asserts! (> reward-amount u0) ERR-INSUFFICIENT-BALANCE)
    
    ;; Transfer reward to voter
    (try! (as-contract (stx-transfer? reward-amount tx-sender tx-sender)))
    
    ;; Mark reward as claimed
    (map-set voter-participation
      { voting-id: voting-id, voter: tx-sender }
      (merge participation { reward-claimed: true })
    )
    
    (ok reward-amount)
  )
)

(define-public (batch-distribute-voter-rewards (voting-id uint) (voters (list 50 principal)))
  (let (
    (session (unwrap! (get-voting-session voting-id) ERR-INVALID-VOTING-ID))
    (reward-amount (unwrap! (calculate-voter-reward voting-id) ERR-INVALID-VOTING-ID))
  )
    (asserts! (unwrap! (is-voting-ended voting-id) ERR-INVALID-VOTING-ID) ERR-VOTING-ACTIVE)
    (asserts! (get distributed session) ERR-VOTING-ACTIVE)
    (asserts! (> reward-amount u0) ERR-INSUFFICIENT-BALANCE)
    
    (ok (map distribute-to-voter voters))
  )
)

(define-private (distribute-to-voter (voter principal))
  (let (
    (voting-id (var-get voting-counter)) ;; This would need to be passed properly in a real implementation
    (participation (map-get? voter-participation { voting-id: voting-id, voter: voter }))
    (reward-amount (unwrap-panic (calculate-voter-reward voting-id)))
  )
    (match participation
      some-participation
      (if (and (get participated some-participation) (not (get reward-claimed some-participation)))
        (begin
          (unwrap-panic (as-contract (stx-transfer? reward-amount tx-sender voter)))
          (map-set voter-participation
            { voting-id: voting-id, voter: voter }
            (merge some-participation { reward-claimed: true })
          )
          true
        )
        false
      )
      false
    )
  )
)

;; Emergency functions
(define-public (emergency-withdraw (amount uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (try! (as-contract (stx-transfer? amount tx-sender CONTRACT-OWNER)))
    (ok true)
  )
)

;; Initialize contract
(begin
  (var-set voting-counter u0)
  (var-set voter-reward-percentage u70)
  (var-set platform-fee-percentage u30)
)