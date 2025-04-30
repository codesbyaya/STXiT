
;; title: user-profile
;; version:
;; summary:
;; description:

;; title: user-profile.clar
;; version: 1.0
;; summary: User Registration and Profile Management
;; description: This contract allows users to create and manage their profiles, including linking centralized platform accounts.

;; Define constants
(define-constant contract-owner tx-sender)
(define-constant err-not-found (err u404))
(define-constant err-already-registered (err u409))
(define-constant err-unauthorized (err u401))

;; Define data maps
(define-map users 
  { address: principal } 
  {
    username: (string-ascii 50),
    twitter: (optional (string-ascii 50)),
    instagram: (optional (string-ascii 50))
  }
)

;; Public functions

;; Register a new user
(define-public (register-user (username (string-ascii 50)))
  (let ((user-principal tx-sender))
    (asserts! (is-none (get-user user-principal)) err-already-registered)
    (ok (map-set users 
      { address: user-principal } 
      {
        username: username,
        twitter: none,
        instagram: none
      }
    ))
  )
)

;; Update username
(define-public (update-username (new-username (string-ascii 50)))
  (let ((user-principal tx-sender))
    (match (get-user user-principal)
      user (ok (map-set users 
        { address: user-principal }
        (merge user { username: new-username })
      ))
      err-not-found
    )
  )
)

;; Link Twitter account
(define-public (link-twitter (twitter-handle (string-ascii 50)))
  (let ((user-principal tx-sender))
    (match (get-user user-principal)
      user (ok (map-set users 
        { address: user-principal }
        (merge user { twitter: (some twitter-handle) })
      ))
      err-not-found
    )
  )
)

;; Link Instagram account
(define-public (link-instagram (instagram-handle (string-ascii 50)))
  (let ((user-principal tx-sender))
    (match (get-user user-principal)
      user (ok (map-set users 
        { address: user-principal }
        (merge user { instagram: (some instagram-handle) })
      ))
      err-not-found
    )
  )
)

;; Unlink Twitter account
(define-public (unlink-twitter)
  (let ((user-principal tx-sender))
    (match (get-user user-principal)
      user (ok (map-set users 
        { address: user-principal }
        (merge user { twitter: none })
      ))
      err-not-found
    )
  )
)

;; Unlink Instagram account
(define-public (unlink-instagram)
  (let ((user-principal tx-sender))
    (match (get-user user-principal)
      user (ok (map-set users 
        { address: user-principal }
        (merge user { instagram: none })
      ))
      err-not-found
    )
  )
)

;; Read-only functions

;; Get user profile
(define-read-only (get-user (user-principal principal))
  (map-get? users { address: user-principal })
)

;; Check if a user is registered
(define-read-only (is-user-registered (user-principal principal))
  (is-some (get-user user-principal))
)

;; Get linked Twitter account
(define-read-only (get-linked-twitter (user-principal principal))
  (match (get-user user-principal)
    user (get twitter user)
    none
  )
)

;; Get linked Instagram account
(define-read-only (get-linked-instagram (user-principal principal))
  (match (get-user user-principal)
    user (get instagram user)
    none
  )
)