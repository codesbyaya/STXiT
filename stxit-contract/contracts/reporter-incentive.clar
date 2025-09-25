;; Reporter Incentives Contract
;; Provides token-based rewards for users who report inactive accounts

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-REPORT (err u101))
(define-constant ERR-ALREADY-REPORTED (err u102))
(define-constant ERR-INSUFFICIENT-FUNDS (err u103))
(define-constant ERR-REPORT-NOT-FOUND (err u104))
(define-constant ERR-REPORT-ALREADY-PROCESSED (err u105))
(define-constant ERR-INVALID-ACCOUNT (err u106))
(define-constant ERR-COOLDOWN-ACTIVE (err u107))

;; Data Variables
(define-data-var next-report-id uint u1)
(define-data-var base-reward uint u1000000) ;; 1 STX in microSTX
(define-data-var min-inactive-period uint u2016) ;; ~2 weeks in blocks
(define-data-var report-cooldown uint u144) ;; ~1 day in blocks
(define-data-var contract-balance uint u0)

;; Data Maps
(define-map reports
  uint
  {
    reporter: principal,
    reported-account: principal,
    report-block: uint,
    status: (string-ascii 20), ;; "pending", "verified", "rejected"
    reward-amount: uint,
    processed-block: (optional uint)
  }
)

(define-map account-last-activity
  principal
  uint ;; block height of last activity
)

(define-map reporter-stats
  principal
  {
    total-reports: uint,
    verified-reports: uint,
    total-rewards: uint,
    last-report-block: uint
  }
)

(define-map account-reports
  principal
  (list 10 uint) ;; list of report IDs for this account
)

;; Authorization map for admins who can verify reports
(define-map authorized-verifiers principal bool)

;; Read-only functions
(define-read-only (get-report (report-id uint))
  (map-get? reports report-id)
)

(define-read-only (get-reporter-stats (reporter principal))
  (default-to 
    {total-reports: u0, verified-reports: u0, total-rewards: u0, last-report-block: u0}
    (map-get? reporter-stats reporter)
  )
)

(define-read-only (get-account-last-activity (account principal))
  (map-get? account-last-activity account)
)

(define-read-only (get-base-reward)
  (var-get base-reward)
)

(define-read-only (get-contract-balance)
  (var-get contract-balance)
)

(define-read-only (is-account-inactive (account principal))
  (let ((last-activity (map-get? account-last-activity account)))
    (match last-activity
      activity-block (>= (- block-height activity-block) (var-get min-inactive-period))
      true ;; No activity recorded means inactive
    )
  )
)

(define-read-only (can-report (reporter principal))
  (let ((stats (get-reporter-stats reporter)))
    (>= (- block-height (get last-report-block stats)) (var-get report-cooldown))
  )
)

(define-read-only (calculate-reward (reporter principal))
  (let (
    (stats (get-reporter-stats reporter))
    (base (var-get base-reward))
    (accuracy-bonus (if (> (get total-reports stats) u0)
                      (/ (* (get verified-reports stats) u500000) (get total-reports stats))
                      u0))
  )
    (+ base accuracy-bonus)
  )
)

;; Public functions
(define-public (submit-report (reported-account principal))
  (let (
    (report-id (var-get next-report-id))
    (reporter tx-sender)
    (current-stats (get-reporter-stats reporter))
  )
    ;; Validate inputs
    (asserts! (not (is-eq reporter reported-account)) ERR-INVALID-REPORT)
    (asserts! (can-report reporter) ERR-COOLDOWN-ACTIVE)
    (asserts! (is-account-inactive reported-account) ERR-INVALID-ACCOUNT)
    
    ;; Check if account was already reported recently
    (asserts! (is-none (get-recent-report reported-account)) ERR-ALREADY-REPORTED)
    
    ;; Create report
    (map-set reports report-id {
      reporter: reporter,
      reported-account: reported-account,
      report-block: block-height,
      status: "pending",
      reward-amount: (calculate-reward reporter),
      processed-block: none
    })
    
    ;; Update reporter stats
    (map-set reporter-stats reporter {
      total-reports: (+ (get total-reports current-stats) u1),
      verified-reports: (get verified-reports current-stats),
      total-rewards: (get total-rewards current-stats),
      last-report-block: block-height
    })
    
    ;; Add to account reports list
    (update-account-reports reported-account report-id)
    
    ;; Increment report ID
    (var-set next-report-id (+ report-id u1))
    
    (ok report-id)
  )
)

(define-public (verify-report (report-id uint) (is-valid bool))
  (let ((report (unwrap! (map-get? reports report-id) ERR-REPORT-NOT-FOUND)))
    ;; Only authorized verifiers can verify reports
    (asserts! (or (is-eq tx-sender CONTRACT-OWNER) 
                  (default-to false (map-get? authorized-verifiers tx-sender))) 
              ERR-NOT-AUTHORIZED)
    
    ;; Check if report is still pending
    (asserts! (is-eq (get status report) "pending") ERR-REPORT-ALREADY-PROCESSED)
    
    (if is-valid
      (begin
        ;; Mark as verified and distribute reward
        (map-set reports report-id (merge report {
          status: "verified",
          processed-block: (some block-height)
        }))
        (try! (distribute-reward (get reporter report) (get reward-amount report)))
        (update-reporter-verified-stats (get reporter report))
      )
      ;; Mark as rejected
      (map-set reports report-id (merge report {
        status: "rejected",
        processed-block: (some block-height)
      }))
    )
    
    (ok true)
  )
)

(define-public (update-account-activity (account principal))
  (begin
    (map-set account-last-activity account block-height)
    (ok true)
  )
)

(define-public (fund-contract)
  (let ((amount (stx-get-balance tx-sender)))
    (asserts! (> amount u0) ERR-INSUFFICIENT-FUNDS)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (var-set contract-balance (+ (var-get contract-balance) amount))
    (ok amount)
  )
)

(define-public (set-base-reward (new-reward uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (var-set base-reward new-reward)
    (ok true)
  )
)

(define-public (set-min-inactive-period (new-period uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (var-set min-inactive-period new-period)
    (ok true)
  )
)

(define-public (add-verifier (verifier principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set authorized-verifiers verifier true)
    (ok true)
  )
)

(define-public (remove-verifier (verifier principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-delete authorized-verifiers verifier)
    (ok true)
  )
)

;; Private functions
(define-private (get-recent-report (account principal))
  (let ((reports-list (default-to (list) (map-get? account-reports account))))
    (fold check-recent-report reports-list none)
  )
)

(define-private (check-recent-report (report-id uint) (found (optional uint)))
  (match found
    existing-report (some existing-report)
    (let ((report (map-get? reports report-id)))
      (match report
        report-data (if (and (is-eq (get status report-data) "pending")
                            (< (- block-height (get report-block report-data)) u144))
                       (some report-id)
                       none)
        none
      )
    )
  )
)

(define-private (update-account-reports (account principal) (report-id uint))
  (let ((current-reports (default-to (list) (map-get? account-reports account))))
    (map-set account-reports account 
      (unwrap-panic (as-max-len? (append current-reports report-id) u10)))
  )
)

(define-private (distribute-reward (reporter principal) (amount uint))
  (let ((contract-bal (var-get contract-balance)))
    (asserts! (>= contract-bal amount) ERR-INSUFFICIENT-FUNDS)
    (try! (as-contract (stx-transfer? amount tx-sender reporter)))
    (var-set contract-balance (- contract-bal amount))
    (ok true)
  )
)

(define-private (update-reporter-verified-stats (reporter principal))
  (let ((current-stats (get-reporter-stats reporter)))
    (map-set reporter-stats reporter (merge current-stats {
      verified-reports: (+ (get verified-reports current-stats) u1),
      total-rewards: (+ (get total-rewards current-stats) (calculate-reward reporter))
    }))
  )
)

;; Initialize contract
(begin
  (map-set authorized-verifiers CONTRACT-OWNER true)
)
