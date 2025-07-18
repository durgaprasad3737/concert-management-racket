#lang racket/gui

;; Concert Listing System
;; A GUI application for managing concert listings with bands and fans

(require racket/class
         racket/list
         racket/string
         racket/date
         racket/gui)

;; ============================================================================
;; DATA STRUCTURES
;; ============================================================================

;; User structure: Stores user ID, username, password, and type (Band/Fan)
(struct user (id username password type) #:mutable)
;; Concert structure: Stores concert details
(struct concert (id band-id band-name date time venue cost status description genre) #:mutable)

;; Global data storage
;; Using hash tables for efficient lookup and management of users and concerts
(define users (make-hash))
(define concerts (make-hash))
;; Stores fan's selected concerts: (fan-id . (list-of-concert-ids))
(define selected-concerts (make-hash))

;; Current session state
(define current-user #f) ; Stores the currently logged-in user struct
(define next-user-id 1) ; Auto-incrementing ID for new users
(define next-concert-id 1) ; Auto-incrementing ID for new concerts
(define current-search-results '()) ; Stores results of the last concert search

;; ============================================================================
;; UTILITY FUNCTIONS
;; ============================================================================

;; Generates a unique ID for a new user
(define (generate-user-id)
  (let ((id next-user-id))
    (set! next-user-id (+ next-user-id 1))
    id))

;; Generates a unique ID for a new concert
(define (generate-concert-id)
  (let ((id next-concert-id))
    (set! next-concert-id (+ next-concert-id 1))
    id))

;; Authenticates a user based on username and password
;; Returns the user struct if found, #f otherwise
(define (authenticate username password)
  (findf (lambda (u)
           (and (string=? (user-username u) username)
                (string=? (user-password u) password)
                ))
         (hash-values users)))

;; Checks if a username already exists
;; Returns the user struct if found, #f otherwise
(define (username-exists? username)
  (findf (lambda (u) (string=? (user-username u) username)) (hash-values users)))

;; Creates a new concert and adds it to the global 'concerts' hash table
;; Returns the ID of the newly created concert
(define (create-concert band-id band-name date time venue cost description genre)
  (let ((id (generate-concert-id)))
    (hash-set! concerts id (concert id band-id band-name date time venue cost "Active" description genre))
    id))

;; Retrieves all concerts associated with a specific band ID
(define (get-band-concerts band-id)
  (filter (lambda (c) (= (concert-band-id c) band-id)) (hash-values concerts)))

;; Updates a specific field of a concert by its ID
(define (update-concert concert-id field value)
  (let ((c (hash-ref concerts concert-id #f)))
    (when c
      (cond
        [(string=? field "band-name") (set-concert-band-name! c value)]
        [(string=? field "date") (set-concert-date! c value)]
        [(string=? field "time") (set-concert-time! c value)]
        [(string=? field "venue") (set-concert-venue! c value)]
        [(string=? field "cost") (set-concert-cost! c value)]
        [(string=? field "status") (set-concert-status! c value)]
        [(string=? field "description") (set-concert-description! c value)]
        [(string=? field "genre") (set-concert-genre! c value)]
        [else (void)])))) ; Do nothing for unknown fields

;; Searches for concerts based on band name, date, and/or genre
;; All criteria are optional (empty string means no filter)
(define (search-concerts band-name date genre)
  (filter (lambda (c)
            (and (if (string=? band-name "") #t (string-contains? (string-downcase (concert-band-name c)) (string-downcase band-name)))
                 (if (string=? date "") #t (string=? (concert-date c) date))
                 (if (string=? genre "") #t (string-contains? (string-downcase (concert-genre c)) (string-downcase genre)))
                 (string=? (concert-status c) "Active"))) ; Only show active concerts in search results
          (hash-values concerts)))

;; Adds a concert to a fan's selected list
(define (add-to-selected fan-id concert-id)
  (let ((current-selected (hash-ref selected-concerts fan-id '())))
    (unless (member concert-id current-selected) ; Prevent duplicates
      (hash-set! selected-concerts fan-id (cons concert-id current-selected)))))

;; Removes a concert from a fan's selected list
(define (remove-from-selected fan-id concert-id)
  (let ((current-selected (hash-ref selected-concerts fan-id '())))
    (hash-set! selected-concerts fan-id (remove concert-id current-selected))))

;; Retrieves the actual concert structs for a fan's selected concert IDs
(define (get-selected-concerts fan-id)
  (filter-map (lambda (id) (hash-ref concerts id #f)) ; filter-map removes #f (non-existent concerts)
              (hash-ref selected-concerts fan-id '())))

;; ============================================================================
;; GUI COMPONENTS (Definitions)
;; ============================================================================

(define main-frame (new frame% [label "Concert Listing System"] [width 800] [height 600]))

;; Login Panel
(define login-panel (new vertical-panel% [parent main-frame]))
(define login-title (new message% [parent login-panel] [label "Hey, music lovers and bands!"]))
(define username-field (new text-field% [parent login-panel] [label "Username:"]))
(define password-field (new text-field% [parent login-panel] [label "Password:"] [style '(single password)]))
(define login-button-panel (new horizontal-panel% [parent login-panel]))
(define login-button #f) ; Placeholder, instantiated later
(define register-button #f) ; Placeholder, instantiated later
(define login-message (new message% [parent login-panel] [label "Write your username and password."]))

;; Register Panel
(define register-panel (new vertical-panel% [parent main-frame]))
(define register-title (new message% [parent register-panel] [label "Create Account"]))
(define reg-username-field (new text-field% [parent register-panel] [label "Username:"]))
(define reg-password-field (new text-field% [parent register-panel] [label "Password:"] [style '(single password)]))
(define user-type-choice (new choice% [parent register-panel] [label "User Type:"] [choices '("Band" "Fan")]))
(define reg-button-panel (new horizontal-panel% [parent register-panel]))
(define create-account-button #f) ; Placeholder, instantiated later
(define back-to-login-button #f) ; Placeholder, instantiated later
(define register-message (new message% [parent register-panel] [label ""]))

;; Main Panel (visible after login)
(define main-panel (new vertical-panel% [parent main-frame] [stretchable-height #t] [stretchable-width #t]))
(define top-bar-panel (new horizontal-panel% [parent main-panel] [alignment '(left center)] [stretchable-width #t]))
(define welcome-message (new message% [parent top-bar-panel] [label ""] [stretchable-width #t]))
(define logout-button #f) ; Placeholder, instantiated later
(define remove-selected-from-top-bar-button #f) ; Placeholder for button in top bar
;; Removed quick-search-button as search is now always visible for fans

;; Band Panel (for Band users)
(define band-panel (new vertical-panel% [parent main-frame] [stretchable-height #t] [stretchable-width #t]))
(define band-notebook (new tab-panel% [parent band-panel] [choices '("My Concerts" "Create Concert")] [stretchable-height #t] [stretchable-width #t]))

;; Band - My Concerts Tab
(define band-concerts-panel (new vertical-panel% [parent band-notebook] [stretchable-height #t] [stretchable-width #t]))
(define band-concerts-list (new list-box% [parent band-concerts-panel] [label "My Concerts:"] [choices '()] [stretchable-height #t] [stretchable-width #t]))
(define band-actions-panel (new horizontal-panel% [parent band-concerts-panel]))
(define edit-concert-button #f) ; Placeholder
(define cancel-concert-button #f) ; Placeholder
(define mark-booked-button #f) ; Placeholder
(define remove-cancel-button #f) ; Placeholder for new button
(define remove-booked-button #f) ; Placeholder for new button

;; Band - Create Concert Tab
(define create-concert-panel (new vertical-panel% [parent band-notebook] [stretchable-height #t] [stretchable-width #t]))
(define concert-band-name-field (new text-field% [parent create-concert-panel] [label "Band Name:"]))
(define concert-date-field (new text-field% [parent create-concert-panel] [label "Date (YYYY-MM-DD):"]))
(define concert-time-field (new text-field% [parent create-concert-panel] [label "Time:"]))
(define concert-venue-field (new text-field% [parent create-concert-panel] [label "Venue:"]))
(define concert-cost-field (new text-field% [parent create-concert-panel] [label "Cost:"]))
(define concert-genre-field (new text-field% [parent create-concert-panel] [label "Genre:"]))
(define concert-description-field (new text-field% [parent create-concert-panel] [label "Description:"] [style '(multiple)] [stretchable-height #t]))
(define create-concert-button #f) ; Placeholder
(define create-concert-message (new message% [parent create-concert-panel] [label ""]))

;; Fan Panel (for Fan users)
;; This panel will now contain the horizontal layout of search and selected areas
(define fan-main-content-layout (new horizontal-panel% [parent main-panel] [stretchable-height #t] [stretchable-width #t]))

;; Fan - Search Concerts Area (now a direct child of fan-main-content-layout)
(define search-panel (new vertical-panel% [parent fan-main-content-layout] [alignment '(center center)] [stretchable-height #t] [stretchable-width #t]))

;; This acts as the framed container for the search form and results
(define search-content-wrapper
  (new group-box-panel%
       [parent search-panel]
       [label "Concert Search & Results"] ; Added a label for the group box
       [stretchable-height #t]
       [stretchable-width #t] ; Allow stretching horizontally within its side
       [min-width 350])) ; Adjusted min-width for side-by-side view

;; This inner panel holds the form and results, and manages its own vertical spacing
(define search-inner-content-panel
  (new vertical-panel%
       [parent search-content-wrapper]
       [stretchable-height #t]
       [stretchable-width #t]))

(define search-form-panel (new vertical-panel% [parent search-inner-content-panel] [stretchable-width #t] [stretchable-height #f]))
(define search-band-field (new text-field% [parent search-form-panel] [label "Band Name:"]))
(define search-date-field (new text-field% [parent search-form-panel] [label "Date (YYYY-MM-DD):"]))
(define search-genre-field (new text-field% [parent search-form-panel] [label "Genre:"]))
(define search-button-panel (new horizontal-panel% [parent search-form-panel]))
(define search-button #f) ; Placeholder

;; Band Existence Message
(define band-existence-message (new message% [parent search-form-panel] [label ""] [font (make-font #:size 10 #:style 'italic)]))

;; Panel to dynamically display search results (concerts with Add/Details buttons)
(define search-results-display-panel #f) ; Instantiated later as child of search-inner-content-panel
(define search-actions-panel (new horizontal-panel% [parent search-panel])) ; Not used for buttons, but for messages
(define search-message (new message% [parent search-panel] [label ""]))

;; Fan - Selected Concerts Area (now a direct child of fan-main-content-layout)
(define selected-panel (new vertical-panel% [parent fan-main-content-layout]
                             [min-width 350] ; Adjusted min-width for side-by-side view
                             [min-height 400]
                             [stretchable-width #t]
                             [stretchable-height #t]
                             [border 1])) ; Add border for visual separation

;; Added a message% as a title for the selected concerts panel
(define selected-concerts-title (new message% [parent selected-panel] [label "My Selected Concerts"]))

(define selected-concerts-list (new list-box% [parent selected-panel] [label ""] [choices '()] [stretchable-width #t] [stretchable-height #t] [style '(single)]))
(define selected-actions-panel (new horizontal-panel% [parent selected-panel]))
(define remove-selected-button #f) ; Placeholder
(define view-selected-details-button #f) ; Placeholder
;; show-all-concerts-button will be instantiated here

;; Dialogs (defined globally to manage their state)
(define details-dialog #f)
(define edit-dialog #f)
(define edit-concert-id #f) ; Stores the ID of the concert being edited

;; ============================================================================
;; EVENT HANDLERS (Functions that respond to GUI events)
;; ============================================================================

;; Handles user login attempt
(define (handle-login b e)
  (let ((u (send username-field get-value))
        (p (send password-field get-value)))
    (if (or (string=? u "") (string=? p ""))
        (send login-message set-label "Please enter username and password!")
        (let ((user (authenticate u p)))
          (if user
              (begin
                (set! current-user user)
                (send login-message set-label "Login successful!")
                (sleep 0.5) ; Short delay for user to see message
                (show-main-interface)
                )
              (send login-message set-label "Invalid username or password!"))))))

;; Switches to the registration panel
(define (handle-register b e)
  (send register-panel show #t)
  (send login-panel show #f)
  (send login-message set-label "")) ; Clear login message

;; Handles new account creation
(define (handle-create-account b e)
  (let ((u (send reg-username-field get-value))
        (p (send reg-password-field get-value))
        (t (send user-type-choice get-string-selection)))
    (cond
      [(string=? u "") (send register-message set-label "Username cannot be empty!")]
      [(string=? p "") (send register-message set-label "Password cannot be empty!")]
      [(username-exists? u) (send register-message set-label "Username already exists!")]
      [else
       (let ((id (generate-user-id)))
         (hash-set! users id (user id u p t))
         (send register-message set-label "Account created successfully!")
         (send reg-username-field set-value "") ; Clear fields
         (send reg-password-field set-value "")
         (sleep 0.5) ; Short delay
         (handle-back-to-login #f #f))]))) ; Go back to login after successful registration

;; Switches back to the login panel
(define (handle-back-to-login b e)
  (send login-panel show #t)
  (send register-panel show #f)
  (send register-message set-label "") ; Clear register message
  (send login-message set-label "Write your username and password.") ; Reset login message
  (send reg-username-field set-value "") ; Clear register fields
  (send reg-password-field set-value ""))

;; Shows the main application interface based on user type
(define (show-main-interface)
  (send login-panel show #f)
  (send register-panel show #f)
  (send main-panel show #t)
  (send welcome-message set-label (format "Welcome, ~a (~a)" (user-username current-user) (user-type current-user)))

  (if (string=? (user-type current-user) "Band")
      (begin
        (send band-panel show #t)
        (send fan-main-content-layout show #f) ; Hide fan layout for band
        (send remove-selected-from-top-bar-button show #f) ; Hide for band
        (refresh-band-concerts)) ; Load band's concerts
      (begin
        (send fan-main-content-layout show #t) ; Show fan layout for fan
        (send band-panel show #f)
        (send remove-selected-from-top-bar-button show #t) ; Show for fan
        (refresh-selected-concerts) ; Load fan's selected concerts
        ;; Ensure search results panel exists and is refreshed
        (unless search-results-display-panel
          (set! search-results-display-panel (new vertical-panel% [parent search-inner-content-panel]
                                                         [alignment '(left top)]
                                                         [stretchable-width #t]
                                                         [stretchable-height #t]
                                                         [style '(auto-vscroll)])))
        (handle-show-all-concerts #f #f) ; Show all active concerts by default
        (send search-message set-label "")))) ; Clear search message

;; Handles user logout
(define (handle-logout b e)
  (set! current-user #f)
  (send main-panel show #f)
  (send login-panel show #t)
  (send username-field set-value "") ; Clear login fields
  (send password-field set-value "")
  (send login-message set-label "Write your username and password.")
  (send welcome-message set-label "")
  (send remove-selected-from-top-bar-button show #f) ; Hide on logout
  (send fan-main-content-layout show #f)) ; Hide fan layout on logout

;; Handles creation of a new concert by a band
(define (handle-create-concert b e)
  (let ((bn (send concert-band-name-field get-value))
        (d (send concert-date-field get-value))
        (t (send concert-time-field get-value))
        (v (send concert-venue-field get-value))
        (c (send concert-cost-field get-value))
        (g (send concert-genre-field get-value))
        (desc (send concert-description-field get-value)))
    (if (or (string=? bn "") (string=? d "") (string=? t "") (string=? v "") (string=? c ""))
        (send create-concert-message set-label "Please fill in all required fields!")
        (begin
          (create-concert (user-id current-user) bn d t v c desc g)
          (send create-concert-message set-label "Concert created successfully!")
          (clear-concert-form) ; Clear form fields
          (refresh-band-concerts))))) ; Update band's concert list

;; Clears the concert creation form fields
(define (clear-concert-form)
  (for-each (lambda (field) (send field set-value ""))
            (list concert-band-name-field
                  concert-date-field
                  concert-time-field
                  concert-venue-field
                  concert-cost-field
                  concert-genre-field
                  concert-description-field)))

;; Refreshes the list of concerts for the current band user
(define (refresh-band-concerts)
  (let ((band-concerts (get-band-concerts (user-id current-user))))
    (send band-concerts-list set
          (map (lambda (c)
                 (format "~a - ~a ~a (~a) - ~a"
                         (concert-band-name c)
                         (concert-date c)
                         (concert-time c)
                         (concert-status c)
                         (concert-venue c)))
               band-concerts))))

;; Helper to perform an action on a selected concert from the band's list
(define (with-selected-band-concert callback)
  (let ((selection (send band-concerts-list get-selection)))
    (if (number? selection)
        (let* ((band-concerts (get-band-concerts (user-id current-user)))
               (concert (list-ref band-concerts selection)))
          (callback concert))
        (message-box "Selection Error" "Please select a concert from the list first."))))

;; Handles opening the edit dialog for a selected concert
(define (handle-edit-concert b e) (with-selected-band-concert show-edit-dialog))

;; Handles canceling a selected concert
(define (handle-cancel-concert b e)
  (with-selected-band-concert (lambda (c)
                                (update-concert (concert-id c) "status" "Cancelled")
                                (refresh-band-concerts))))

;; Handles marking a selected concert as fully booked
(define (handle-mark-booked b e)
  (with-selected-band-concert (lambda (c)
                                (update-concert (concert-id c) "status" "Fully Booked")
                                (refresh-band-concerts))))

;; Handles reverting a 'Cancelled' concert back to 'Active'
(define (handle-remove-cancel button event)
  (with-selected-band-concert
   (lambda (c)
     (if (string=? (concert-status c) "Cancelled")
         (begin
           (update-concert (concert-id c) "status" "Active")
           (refresh-band-concerts)
           (message-box "Success" (format "Concert '~a' status reverted to Active." (concert-band-name c))))
         (message-box "Info" "Selected concert is not marked as Cancelled.")))))

;; Handles reverting a 'Fully Booked' concert back to 'Active'
(define (handle-remove-booked button event)
  (with-selected-band-concert
   (lambda (c)
     (if (string=? (concert-status c) "Fully Booked")
         (begin
           (update-concert (concert-id c) "status" "Active")
           (refresh-band-concerts)
           (message-box "Success" (format "Concert '~a' status reverted to Active." (concert-band-name c))))
         (message-box "Info" "Selected concert is not marked as Fully Booked.")))))

;; Handles searching for concerts based on criteria
(define (handle-search b e)
  (let ((bn (send search-band-field get-value))
        (d (send search-date-field get-value))
        (g (send search-genre-field get-value)))
    (if (and (string=? bn "") (string=? d "") (string=? g ""))
        (begin
          (set! current-search-results '())
          (refresh-search-results)
          (send search-message set-label "Please enter at least one search criterion.")
          (send band-existence-message set-label "")) ; Clear band existence message
        (begin ; Explicit begin for the else branch
          (let ((results (search-concerts bn d g)))
            (set! current-search-results results)
            (refresh-search-results)
            (send search-message set-label "") ; Clear previous general message

            (cond
              [(string=? bn "") ; No band name entered, general search
               (if (empty? results)
                   (send band-existence-message set-label "No concerts found matching your criteria.")
                   (send band-existence-message set-label ""))] ; Clear if results found for general search
              [else ; Band name entered
               (if (empty? results)
                   (send band-existence-message set-label (format "No active concerts found for '~a' matching your criteria." bn))
                   (send band-existence-message set-label (format "Band '~a' found with active concerts matching your criteria." bn)))]
              ) ; Closes cond
            ) ; Closes let ((results ...))
          ) ; Closes explicit begin
        ) ; Closes if
    ) ; Closes let ((bn ...))
  ) ; Closes define


;; Handles showing all active concerts
(define (handle-show-all-concerts b e)
  (send search-band-field set-value "") ; Clear search fields
  (send search-date-field set-value "")
  (send search-genre-field set-value "")
  (let ((results (search-concerts "" "" ""))) ; Search with empty criteria to get all active
    (set! current-search-results results)
    (refresh-search-results)
    (send search-message set-label "")
    (send band-existence-message set-label "") ; Clear band existence message
    (when (empty? results)
      (send search-message set-label "No active concerts available."))))

;; Refreshes the display of search results
(define (refresh-search-results)
  ;; Correctly delete children by iterating and calling delete on each
  (for-each (lambda (child) (send child delete))
            (send search-results-display-panel get-children))
  ;; Add new result panels
  (when (pair? current-search-results) ; If there are results
    (for-each (lambda (c) (make-concert-result-panel c search-results-display-panel))
              current-search-results))
  (send search-results-display-panel refresh) ; Force GUI refresh on the results panel
  )

;; Creates a panel for a single concert result in the search list
(define (make-concert-result-panel concert-obj parent-panel)
  (let* ((h-panel (new horizontal-panel% [parent parent-panel]
                                         [alignment '(left center)]
                                         [stretchable-width #t]
                                         [border 1] ; Visual separator
                                         [horiz-margin 2]
                                         [vert-margin 2]))
         (c-text (format "~a - ~a ~a - ~a (£~a) - ~a"
                         (concert-band-name concert-obj)
                         (concert-date concert-obj)
                         (concert-time concert-obj)
                         (concert-venue concert-obj)
                         (concert-cost concert-obj)
                         (concert-genre concert-obj)))
         (msg (new message% [parent h-panel] [label c-text] [stretchable-width #t])))
    (new button% [parent h-panel] [label " Add"] [min-width 60] [min-height 20]
         [callback (lambda (b e) (handle-star-concert concert-obj))])
    (new button% [parent h-panel] [label "View Details"] [min-width 90] [min-height 20]
         [callback (lambda (b e) (show-details-dialog concert-obj))])
    h-panel))

;; Handles adding a concert to the fan's selected list
(define (handle-star-concert concert-obj)
  (add-to-selected (user-id current-user) (concert-id concert-obj))
  (refresh-selected-concerts)
  (send search-message set-label (format "Added '~a' to selected concerts!" (concert-band-name concert-obj)))
  (sleep 1) ; Short delay
  (send search-message set-label "")) ; Clear message

;; Helper to perform an action on a selected concert from the fan's selected list
(define (with-selected-fan-concert callback)
  (let ((selection (send selected-concerts-list get-selection)))
    (if (number? selection)
        (let* ((selected (get-selected-concerts (user-id current-user)))
               (concert (list-ref selected selection)))
          (callback concert))
        (message-box "Selection Error" "Please select a concert to remove."))))

;; Handles removing a concert from the fan's selected list
(define (handle-remove-selected b e)
  (with-selected-fan-concert
   (lambda (c)
     (remove-from-selected (user-id current-user) (concert-id c))
     (refresh-selected-concerts)
     (message-box "Removed" (format "Removed '~a' from selected concerts." (concert-band-name c)))
     (send search-message set-label (format "Removed '~a' from selected concerts." (concert-band-name c))) ; Also update search message for consistency
     (sleep 1)
     (send search-message set-label ""))))

;; Refreshes the list of selected concerts for the current fan user
(define (refresh-selected-concerts)
  (let ((selected (get-selected-concerts (user-id current-user))))
    (send selected-concerts-list set
          (map (lambda (c)
                 (format "~a - ~a ~a - ~a (£~a)"
                         (concert-band-name c)
                         (concert-date c)
                         (concert-time c)
                         (concert-venue c)
                         (concert-cost c)))
               selected))))

;; Displays a dialog with full details of a concert
(define (show-details-dialog concert)
  (when details-dialog (send details-dialog show #f)) ; Close previous dialog if open
  (set! details-dialog (new dialog% [label "Concert Details"] [width 400] [height 300]))
  (let ((panel (new vertical-panel% [parent details-dialog] [alignment '(center center)] [stretchable-height #t] [stretchable-width #t])))
    (new message% [parent panel] [label (format "Band: ~a" (concert-band-name concert))])
    (new message% [parent panel] [label (format "Date: ~a" (concert-date concert))])
    (new message% [parent panel] [label (format "Time: ~a" (concert-time concert))])
    (new message% [parent panel] [label (format "Venue: ~a" (concert-venue concert))])
    (new message% [parent panel] [label (format "Cost: £~a" (concert-cost concert))])
    (new message% [parent panel] [label (format "Genre: ~a" (concert-genre concert))])
    (new message% [parent panel] [label (format "Status: ~a" (concert-status concert))])
    (new message% [parent panel] [label (format "Description: ~a" (concert-description concert))])
    (new button% [parent panel] [label "Close"] [callback (lambda (b e) (send details-dialog show #f))]))
  (send details-dialog show #t))

;; Handles viewing details of a selected concert from the fan's list
(define (handle-view-selected-details b e)
  (with-selected-fan-concert show-details-dialog))

;; Displays a dialog for editing concert details
(define (show-edit-dialog concert)
  (when edit-dialog (send edit-dialog show #f)) ; Close previous dialog if open
  (set! edit-concert-id (concert-id concert)) ; Store ID of the concert being edited
  (set! edit-dialog (new dialog% [label "Edit Concert"] [width 400] [height 400]))
  (let ((panel (new vertical-panel% [parent edit-dialog] [stretchable-height #t] [stretchable-width #t])))
    ;; Text fields pre-filled with current concert data
    (define e-bn (new text-field% [parent panel] [label "Band Name:"] [init-value (concert-band-name concert)]))
    (define e-d (new text-field% [parent panel] [label "Date:"] [init-value (concert-date concert)]))
    (define e-t (new text-field% [parent panel] [label "Time:"] [init-value (concert-time concert)]))
    (define e-v (new text-field% [parent panel] [label "Venue:"] [init-value (concert-venue concert)]))
    (define e-c (new text-field% [parent panel] [label "Cost:"] [init-value (number->string (concert-cost concert))])) ; Convert cost to string for text-field
    (define e-g (new text-field% [parent panel] [label "Genre:"] [init-value (concert-genre concert)]))
    (define e-desc (new text-field% [parent panel] [label "Description:"] [init-value (concert-description concert)] [style '(multiple)] [stretchable-height #t]))
    (let ((bp (new horizontal-panel% [parent panel])))
      (new button% [parent bp] [label "Save"]
           [callback (lambda (b e)
                       ;; Update each field of the concert
                       (update-concert edit-concert-id "band-name" (send e-bn get-value))
                       (update-concert edit-concert-id "date" (send e-d get-value))
                       (update-concert edit-concert-id "time" (send e-t get-value))
                       (update-concert edit-concert-id "venue" (send e-v get-value))
                       (update-concert edit-concert-id "cost" (string->number (send e-c get-value))) ; Convert cost back to number
                       (update-concert edit-concert-id "genre" (send e-g get-value))
                       (update-concert edit-concert-id "description" (send e-desc get-value))
                       (refresh-band-concerts) ; Update the list display
                       (send edit-dialog show #f))]) ; Close dialog
      (new button% [parent bp] [label "Cancel"] [callback (lambda (b e) (send edit-dialog show #f))])))
  (send edit-dialog show #t))

;; ============================================================================
;; BUTTON CALLBACKS - Instantiated after event handlers are defined
;; ============================================================================

(set! login-button (new button% [parent login-button-panel] [label "Login"] [callback handle-login]))
(set! register-button (new button% [parent login-button-panel] [label "Register"] [callback handle-register]))
(set! create-account-button (new button% [parent reg-button-panel] [label "Create Account"] [callback handle-create-account]))
(set! back-to-login-button (new button% [parent reg-button-panel] [label "Back to Login"] [callback handle-back-to-login]))
(set! logout-button (new button% [parent top-bar-panel] [label "Logout"] [callback handle-logout]))
(set! edit-concert-button (new button% [parent band-actions-panel] [label "Edit Selected"] [callback handle-edit-concert]))
(set! cancel-concert-button (new button% [parent band-actions-panel] [label "Cancel Concert"] [callback handle-cancel-concert]))
(set! mark-booked-button (new button% [parent band-actions-panel] [label "Mark as Booked"] [callback handle-mark-booked]))
;; New button instantiations for status removal
(set! remove-cancel-button (new button% [parent band-actions-panel] [label "Remove Cancel"] [callback handle-remove-cancel]))
(set! remove-booked-button (new button% [parent band-actions-panel] [label "Remove Booked"] [callback handle-remove-booked]))
(set! create-concert-button (new button% [parent create-concert-panel] [label "Create Concert"] [callback handle-create-concert]))
(set! search-button (new button% [parent search-button-panel] [label "Search"] [callback handle-search]))

;; Moved show-all-concerts-button to selected-actions-panel and defined it directly
(define show-all-concerts-button (new button% [parent selected-actions-panel] [label "Show All Concerts"] [callback handle-show-all-concerts]))

(set! remove-selected-button (new button% [parent selected-actions-panel] [label "Remove Selected"] [callback handle-remove-selected]))
(set! view-selected-details-button (new button% [parent selected-actions-panel] [label "View Details"] [callback handle-view-selected-details]))

;; Top-bar "Remove from Selected" button
(set! remove-selected-from-top-bar-button
  (new button% [parent top-bar-panel]
               [label "Remove from Selected"]
               [callback handle-remove-selected]
               [horiz-margin 5]))
(send remove-selected-from-top-bar-button show #f) ; Explicitly hide it after creation

;; ============================================================================
;; INITIALIZATION (Initial setup and default data)
;; ============================================================================

;; Hide panels initially
(send register-panel show #f)
(send main-panel show #f)
(send band-panel show #f)
(send fan-main-content-layout show #f) ; Hide the new fan layout initially

;; Add initial users
(hash-set! users (generate-user-id) (user (sub1 next-user-id) "rockband1" "pass123" "Band"))
(hash-set! users (generate-user-id) (user (sub1 next-user-id) "metalfan" "pass456" "Fan"))
(hash-set! users (generate-user-id) (user (sub1 next-user-id) "jazzband" "pass789" "Band"))

;; Add initial concerts
(create-concert 1 "The Rock Stars" "2025-08-15" "20:00" "Madison Square Garden" 45.00 "High energy rock concert" "Rock")
(create-concert 1 "The Rock Stars" "2025-09-20" "19:30" "Wembley Stadium" 55.00 "Stadium tour finale" "Rock")
(create-concert 3 "Jazz Collective" "2025-08-10" "21:00" "Blue Note" 25.00 "Intimate jazz session" "Jazz")
(create-concert 3 "Blues Breakers" "2025-10-05" "18:00" "The Cavern" 30.00 "Classic blues rock" "Blues")
(create-concert 1 "Pop Sensations" "2025-11-01" "22:00" "O2 Arena" 60.00 "Chart-topping hits" "Pop")

;; Add some selected concerts for the initial fan user (ID 2: "metalfan")
(add-to-selected 2 1) ; metalfan selects Concert ID 1
(add-to-selected 2 3) ; metalfan selects Concert ID 3
(add-to-selected 2 5) ; metalfan selects Concert ID 5

;; Show the main frame to start the application
(send main-frame show #t)
