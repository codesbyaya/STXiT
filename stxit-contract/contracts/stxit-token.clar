;; STXit Governance Token Contract
;; Simple SIP-010 compliant token for governance

(impl-trait .sip-010-trait.sip-010-trait)

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-UNAUTHORIZED (err u100))
(define-constant ERR-NOT-TOKEN-OWNER (err u101))
(define-constant ERR-INSUFFICIENT-BALANCE (err u102))

;; Token definition
(define-fungible-token stxit-token)

;; Data variables
(define-data-var token-name (string-ascii 32) "STXit Governance Token")
(define-data-var token-symbol (string-ascii 10) "STXIT")
(define-data-var token-decimals uint u6)
(define-data-var token-uri (optional (string-utf8 256)) none)

;; SIP-010 Functions

(define-public (transfer (amount uint) (sender principal) (recipient principal) (memo (optional (buff 34))))
    (begin
        (asserts! (or (is-eq tx-sender sender) (is-eq contract-caller sender)) ERR-NOT-TOKEN-OWNER)
        (ft-transfer? stxit-token amount sender recipient)
    )
)

(define-read-only (get-name)
    (ok (var-get token-name))
)

(define-read-only (get-symbol)
    (ok (var-get token-symbol))
)

(define-read-only (get-decimals)
    (ok (var-get token-decimals))
)

(define-read-only (get-balance (who principal))
    (ok (ft-get-balance stxit-token who))
)

(define-read-only (get-total-supply)
    (ok (ft-get-supply stxit-token))
)

(define-read-only (get-token-uri)
    (ok (var-get token-uri))
)

;; Mint function (only owner)
(define-public (mint (amount uint) (recipient principal))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
        (ft-mint? stxit-token amount recipient)
    )
)

;; Burn function
(define-public (burn (amount uint) (owner principal))
    (begin
        (asserts! (or (is-eq tx-sender owner) (is-eq contract-caller owner)) ERR-NOT-TOKEN-OWNER)
        (ft-burn? stxit-token amount owner)
    )
)
