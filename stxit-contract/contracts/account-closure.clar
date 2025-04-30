;; Account Closure Request Contract
;; Allows users to submit requests to close accounts on centralized platforms

;; Define constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-invalid-fee (err u101))
(define-constant err-invalid-platform (err u102))

;; Define data variables
(define-data-var fee-amount uint u1000000) ;; Fee in microSTX (1 STX = 1,000,000 microSTX)
(define-data-var request-counter uint u0)

;; Define data maps
(define-map closure-requests
  { request-id: uint }
  {
    user: principal,
    platform: (string-ascii 64),
    account-id: (string-ascii 128),
    timestamp: uint,
    status: (string-ascii 20)
  }
)

(define-map user-requests
  { user: principal }
  { request-ids: (list 20 uint) }
)

;; Define public functions

;; Submit a closure request
(define-public (submit-closure-request (platform (string-ascii 64)) (account-id (string-ascii 128)))
  (let
    (
      (current-fee (var-get fee-amount))
      (request-id (+ (var-get request-counter) u1))
      (user-principal tx-sender)
      (current-time (- stacks-block-height u1))
      (user-request-list (default-to { request-ids: (list) } (map-get? user-requests { user: user-principal })))
    )
    ;; Check if platform is valid (simplified validation)
    (asserts! (> (len platform) u0) err-invalid-platform)
    
    ;; Process payment
    (try! (stx-transfer? current-fee tx-sender (as-contract tx-sender)))
    
    ;; Store request
    (map-set closure-requests
      { request-id: request-id }
      {
        user: user-principal,
        platform: platform,
        account-id: account-id,
        timestamp: current-time,
        status: "pending"
      }
    )
    
    ;; Update user's request list
    (map-set user-requests
      { user: user-principal }
      { request-ids: (unwrap-panic (as-max-len? (append (get request-ids user-request-list) request-id) u20)) }
    )
    
    ;; Increment request counter
    (var-set request-counter request-id)
    
    ;; Return success with request ID
    (ok request-id)
  )
)

;; Get request details
(define-read-only (get-request-details (request-id uint))
  (map-get? closure-requests { request-id: request-id })
)

;; Get all requests for a user
(define-read-only (get-user-requests (user principal))
  (map-get? user-requests { user: user })
)

;; Update request status (admin only)
(define-public (update-request-status (request-id uint) (new-status (string-ascii 20)))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (match (map-get? closure-requests { request-id: request-id })
      request-data (begin
        (map-set closure-requests
          { request-id: request-id }
          (merge request-data { status: new-status })
        )
        (ok true)
      )
      (err u404)
    )
  )
)

;; Update fee amount (admin only)
(define-public (update-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set fee-amount new-fee)
    (ok true)
  )
)

;; Withdraw funds (admin only)
(define-public (withdraw-funds (amount uint) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (as-contract (stx-transfer? amount tx-sender recipient))
  )
)