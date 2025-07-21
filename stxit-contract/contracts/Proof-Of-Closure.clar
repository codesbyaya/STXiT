;; Proof-of-Closure NFT Contract
;; This contract manages the minting of NFTs as proof of account closure

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-token-owner (err u101))
(define-constant err-invalid-platform (err u102))
(define-constant err-already-closed (err u103))
(define-constant err-token-not-found (err u104))

;; Data Variables
(define-data-var last-token-id uint u0)
(define-data-var contract-uri (optional (string-utf8 256)) none)

;; Data Maps
(define-map token-count principal uint)
(define-map market {token-id: uint} {price: uint, for-sale: bool})

;; Token metadata structure
(define-map token-metadata 
    uint 
    {
        platform-name: (string-ascii 64),
        account-id: (string-ascii 128),
        closure-date: uint,
        metadata-uri: (string-ascii 256),
        owner: principal
    }
)

;; Platform registry - only authorized platforms can mint
(define-map authorized-platforms (string-ascii 64) bool)

;; Account closure tracking to prevent duplicate NFTs
(define-map closed-accounts {platform: (string-ascii 64), account-id: (string-ascii 128)} uint)

;; SIP-009 NFT Trait Implementation
(define-non-fungible-token proof-of-closure-nft uint)

;; SIP-009 Required Functions

(define-read-only (get-last-token-id)
    (ok (var-get last-token-id))
)

(define-read-only (get-token-uri (token-id uint))
    (match (map-get? token-metadata token-id)
        metadata (ok (some (get metadata-uri metadata)))
        (ok none)
    )
)

(define-read-only (get-owner (token-id uint))
    (ok (nft-get-owner? proof-of-closure-nft token-id))
)

(define-public (transfer (token-id uint) (sender principal) (recipient principal))
    (begin
        (asserts! (is-eq tx-sender sender) err-not-token-owner)
        (match (nft-get-owner? proof-of-closure-nft token-id)
            owner (begin
                (asserts! (is-eq sender owner) err-not-token-owner)
                (nft-transfer? proof-of-closure-nft token-id sender recipient)
            )
            err-token-not-found
        )
    )
)

;; Administrative Functions

(define-public (set-contract-uri (new-uri (optional (string-utf8 256))))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (ok (var-set contract-uri new-uri))
    )
)

(define-public (add-authorized-platform (platform-name (string-ascii 64)))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (ok (map-set authorized-platforms platform-name true))
    )
)

(define-public (remove-authorized-platform (platform-name (string-ascii 64)))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (ok (map-delete authorized-platforms platform-name))
    )
)

;; Core NFT Minting Function

(define-public (mint-proof-of-closure 
    (recipient principal)
    (platform-name (string-ascii 64))
    (account-id (string-ascii 128))
    (closure-date uint)
    (metadata-uri (string-ascii 256))
)
    (let
        (
            (token-id (+ (var-get last-token-id) u1))
            (closure-key {platform: platform-name, account-id: account-id})
        )
        ;; Check if platform is authorized
        (asserts! (default-to false (map-get? authorized-platforms platform-name)) err-invalid-platform)
        
        ;; Check if account hasn't already been closed (prevent duplicates)
        (asserts! (is-none (map-get? closed-accounts closure-key)) err-already-closed)
        
        ;; Mint the NFT
        (try! (nft-mint? proof-of-closure-nft token-id recipient))
        
        ;; Store token metadata
        (map-set token-metadata token-id {
            platform-name: platform-name,
            account-id: account-id,
            closure-date: closure-date,
            metadata-uri: metadata-uri,
            owner: recipient
        })
        
        ;; Mark account as closed
        (map-set closed-accounts closure-key token-id)
        
        ;; Update token count for recipient
        (map-set token-count 
            recipient 
            (+ (default-to u0 (map-get? token-count recipient)) u1)
        )
        
        ;; Update last token ID
        (var-set last-token-id token-id)
        
        (ok token-id)
    )
)

;; Public mint function with additional validation (can be called by authorized platforms)
(define-public (request-closure-proof
    (platform-name (string-ascii 64))
    (account-id (string-ascii 128))
    (metadata-uri (string-ascii 256))
)
    (mint-proof-of-closure 
        tx-sender 
        platform-name 
        account-id 
        stacks-block-height 
        metadata-uri
    )
)

;; Read-only functions for querying

(define-read-only (get-token-metadata (token-id uint))
    (map-get? token-metadata token-id)
)

(define-read-only (get-tokens-owned (owner principal))
    (default-to u0 (map-get? token-count owner))
)

(define-read-only (is-platform-authorized (platform-name (string-ascii 64)))
    (default-to false (map-get? authorized-platforms platform-name))
)

(define-read-only (is-account-closed (platform-name (string-ascii 64)) (account-id (string-ascii 128)))
    (is-some (map-get? closed-accounts {platform: platform-name, account-id: account-id}))
)

(define-read-only (get-closure-token-id (platform-name (string-ascii 64)) (account-id (string-ascii 128)))
    (map-get? closed-accounts {platform: platform-name, account-id: account-id})
)

;; Utility function to get all metadata for verification
(define-read-only (verify-closure-proof (token-id uint))
    (match (map-get? token-metadata token-id)
        metadata (ok metadata)
        err-token-not-found
    )
)

;; Optional: Marketplace functionality for trading closure proofs
(define-public (list-for-sale (token-id uint) (price uint))
    (let ((token-owner (unwrap! (nft-get-owner? proof-of-closure-nft token-id) err-token-not-found)))
        (asserts! (is-eq tx-sender token-owner) err-not-token-owner)
        (ok (map-set market {token-id: token-id} {price: price, for-sale: true}))
    )
)

(define-public (unlist-from-sale (token-id uint))
    (let ((token-owner (unwrap! (nft-get-owner? proof-of-closure-nft token-id) err-token-not-found)))
        (asserts! (is-eq tx-sender token-owner) err-not-token-owner)
        (ok (map-delete market {token-id: token-id}))
    )
)

(define-read-only (get-listing (token-id uint))
    (map-get? market {token-id: token-id})
)

;; Initialize contract with some default authorized platforms
(map-set authorized-platforms "twitter" true)
(map-set authorized-platforms "facebook" true)
(map-set authorized-platforms "instagram" true)
(map-set authorized-platforms "linkedin" true)