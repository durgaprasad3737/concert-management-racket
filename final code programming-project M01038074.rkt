#lang racket/gui

;; Load necessary Racket libraries for building the GUI,
;; working with lists, strings, dates, and hash tables.
(require racket/class
         racket/list
         racket/string
         racket/date
         racket/gui)

;; --- 1. DATA STRUCTURES ---
;; This section defines the blueprints for the main pieces of information
;; my application will handle: users and concerts.

;; The 'user' struct defines what a user object looks like.
;; '#:mutable' means that the fields of a 'user' can be changed after creation.
;; R1.1: Users are either Bands or Fans - This is implemented by the 'type' field,
;;       which will store either "Band" or "Fan". This field dictates the user's permissions
;;       and the interface they see after logging in.
(struct user (id username password type) #:mutable)

;; The 'concert' struct defines what a concert object looks like.
;; '#:mutable' allows concert details to be updated (e.g., status, venue).
;; R2.2: A concert listing must contain (at least) band name, date and time, venue, cost.
;;       These required fields (band-name, date, time, venue, cost) are explicitly defined here
;;       as core components of every concert. Additional fields like 'status', 'description',
;;       and 'genre' provide richer detail and functionality.
(struct concert (id band-id band-name date time venue cost status description genre) #:mutable)

;; Global Hash Tables: These act as your in-memory "database" to store all the application's data.
;; Hash tables are efficient for looking up items by a unique key (like an ID), providing
;; quick access to user and concert information.
(define users (make-hash))       ; Stores all user objects, keyed by their unique user ID.
(define concerts (make-hash))    ; Stores all concert objects, keyed by their unique concert ID.

;; 'selected-concerts' maps a fan's ID to a list of concert IDs they've selected.
;; This hash table is central to managing a fan's personalized concert interests.
;; R3.2: Fans should be able to add items found by searching to a list of Selected Concerts for future reference.
;; R3.3: Fans should be able to view their list of Selected Concerts.
;; R3.4: Fans should be able to remove items from their Selected Concerts.
;;       The structure and manipulation of this hash table directly support these fan functionalities.
(define selected-concerts (make-hash))

;; Session State: These variables keep track of the current application state,
;; such as which user is logged in and what data is currently being displayed.
(define current-user #f)        ; Stores the user object of the currently logged-in user. #f when no one is logged in.
(define next-user-id 1)         ; Counter for generating unique user IDs, ensuring each new user has a distinct identifier.
(define next-concert-id 1)      ; Counter for generating unique concert IDs, ensuring each concert is uniquely identifiable.
(define current-search-results '()) ; Stores the list of concerts returned by the last search operation,
                                  ; which is then used to populate the search results display in the GUI.

;; --- 2. UTILITY FUNCTIONS (Core Logic) ---
;; These functions encapsulate the main business logic of the application.
;; They operate on the data structures (like 'users' and 'concerts' hash tables)
;; but generally do not interact with the GUI directly. They are the "backend" operations.

;; Generates a unique ID for a new user and increments the counter.
;; This ensures that every new user account has a distinct identifier.
(define (generate-user-id)
  (let ((id next-user-id))
    (set! next-user-id (+ next-user-id 1))
    id))

;; Generates a unique ID for a new concert and increments the counter.
;; Similar to user IDs, this provides a unique identifier for each concert listing.
(define (generate-concert-id)
  (let ((id next-concert-id))
    (set! next-concert-id (+ next-concert-id 1))
    id))

;; Checks if provided username and password match any stored user in the 'users' hash table.
;; Returns the user object if authentication is successful, #f otherwise.
;; This function is the core of the login process.
(define (authenticate username password)
  (findf (lambda (u)
           (and (string=? (user-username u) username)
                (string=? (user-password u) password)))
          (hash-values users))) ; 'hash-values' gets a list of all user objects to search through.

;; Checks if a username already exists in the system.
;; This is crucial for preventing duplicate usernames during registration.
(define (username-exists? username)
  (findf (lambda (u) (string=? (user-username u) username))
          (hash-values users)))

;; Creates a new concert listing and adds it to the 'concerts' hash table.
;; It defaults the concert's status to "Active" upon creation.
;; R2.1: Bands must be able to create concert listings - This function performs the actual data creation.
;; R2.2: A concert listing must contain (at least) band name, date and time, venue, cost.
;;       These are all parameters to this function, ensuring they are provided when a concert is created.
(define create-concert
  (lambda (band-id band-name date time venue cost description genre)
    (let ((id (generate-concert-id)))
      (hash-set! concerts id (concert id band-id band-name date time venue cost "Active" description genre))
      id)))


;; Retrieves all concerts created by a specific band.
;; This is used by the Band interface to display "My Concerts."
(define get-band-concerts
  (lambda (band-id)
    (filter (lambda (c) (= (concert-band-id c) band-id)) 
            (hash-values concerts))))


;; Updates a specific field of a concert identified by its ID.
;; This is a versatile function used for all concert modifications.
;; R2.3: Bands must be able to edit the details of their own listings. This function enables the actual data modification.
;;       (The GUI later ensures only *their own* concerts are presented for editing).
;; R2.4: Bands must be able to edit their own listings to indicate if a concert has been cancelled or is fully booked.
;;       The "status" field is mutable and can be updated using this function, changing the concert's state.
(define (update-concert concert-id field value)
  (let ((c (hash-ref concerts concert-id #f))) ; Get the concert object by its ID.
    (when c ; If the concert exists:
      (cond
        [(string=? field "band-name") (set-concert-band-name! c value)]
        [(string=? field "date") (set-concert-date! c value)]
        [(string=? field "time") (set-concert-time! c value)]
        [(string=? field "venue") (set-concert-venue! c value)]
        [(string=? field "cost") (set-concert-cost! c value)]
        [(string=? field "status") (set-concert-status! c value)] ; Directly updates concert status here.
        [(string=? field "description") (set-concert-description! c value)]
        [(string=? field "genre") (set-concert-genre! c value)]
        [else (void)]))))

;; Searches for concerts based on band name, date, and/or genre.
;; It only returns concerts with an "Active" status.
;; R3.1: Fans should be able to search listings using appropriate search criteria (band, date, etc).
;;       This function implements the core search logic, allowing for partial, case-insensitive matches
;;       for band name and genre, and exact matches for date.
(define (search-concerts band-name date genre)
  (filter (lambda (c)
            (and (if (string=? band-name "") #t ; If band-name field is empty, don't filter by band name.
                     (string-contains? (string-downcase (concert-band-name c)) (string-downcase band-name))) ; Case-insensitive partial match.
                 (if (string=? date "") #t ; If date field is empty, don't filter by date.
                     (string=? (concert-date c) date)) ; Exact date match.
                 (if (string=? genre "") #t ; If genre field is empty, don't filter by genre.
                     (string-contains? (string-downcase (concert-genre c)) (string-downcase genre))) ; Case-insensitive partial match.
                 (string=? (concert-status c) "Active"))) ; Only display concerts with "Active" status, hiding cancelled/booked ones.
          (hash-values concerts))) ; Iterate through all concerts in the system.

;; Adds a concert ID to a fan's list of selected concerts in the 'selected-concerts' hash table.
;; It prevents adding the same concert multiple times.
;; R3.2: Fans should be able to add items found by searching to a list of Selected Concerts for future reference.
(define (add-to-selected fan-id concert-id)
  (let ((current-selected (hash-ref selected-concerts fan-id '()))) ; Get current list, or empty list if none for this fan.
    (unless (member concert-id current-selected) ; Only add if not already in the list.
      (hash-set! selected-concerts fan-id (cons concert-id current-selected))))) ; Prepend new ID to the fan's list.

;; Removes a concert ID from a fan's list of selected concerts.
;; R3.4: Fans should be able to remove items from their Selected Concerts.
(define (remove-from-selected fan-id concert-id)
  (let ((current-selected (hash-ref selected-concerts fan-id '())))
    (hash-set! selected-concerts fan-id (remove concert-id current-selected)))) ; 'remove' creates a new list without the specified item.

;; Retrieves the actual concert objects for a fan's selected concert IDs.
;; This function translates the list of IDs stored for a fan into a list of full concert details for display.
;; R3.3: Fans should be able to view their list of Selected Concerts. This function provides the structured data to display.
(define (get-selected-concerts fan-id)
  (filter-map (lambda (id) (hash-ref concerts id #f)) ; For each ID, get the concert object if it exists.
              (hash-ref selected-concerts fan-id '()))) ; Get the list of IDs for the specific fan.

;; --- 3. GUI COMPONENTS (Definitions) ---
;; This section defines all the visual elements (widgets) of your application,
;; such as frames, panels, buttons, text fields, and list boxes.
;; They are defined as global variables so that event handlers can reference and manipulate them.

(define main-frame (new frame% [label "Concert Listing System"] [width 800] [height 600]))

;; Login Panel: The initial screen users see when they launch the application.
(define login-panel (new vertical-panel% [parent main-frame]))
(define login-title (new message% [parent login-panel] [label "Hey, music lovers and bands!"]))
(define username-field (new text-field% [parent login-panel] [label "Username:"]))
(define password-field (new text-field% [parent login-panel] [label "Password:"] [style '(single password)])) ; 'password' style hides input characters.
(define login-button-panel (new horizontal-panel% [parent login-panel]))
(define login-button #f)    ; Placeholder for the Login button, initialized later.
(define register-button #f) ; Placeholder for the Register button, initialized later.
(define login-message (new message% [parent login-panel] [label "Write your username and password."])) ; Feedback message for login.

;; Register Panel: For new user account creation. This panel appears when a user clicks "Register."
(define register-panel (new vertical-panel% [parent main-frame]))
(define register-title (new message% [parent register-panel] [label "Create Account"]))
(define reg-username-field (new text-field% [parent register-panel] [label "Username:"]))
(define reg-password-field (new text-field% [parent register-panel] [label "Password:"] [style '(single password)]))
;; R1.1: Users are either Bands or Fans - This 'choice%' widget allows the user to select their account type
;;       during registration, directly linking to the user struct's 'type' field.
;; R1.2: Users should be able to create an account of the appropriate type (band or fan) - This widget facilitates
;;       type selection during the account creation process.
(define user-type-choice (new choice% [parent register-panel] [label "User Type:"] [choices '("Band" "Fan")]))
(define reg-button-panel (new horizontal-panel% [parent register-panel]))
(define create-account-button #f) ; Button to finalize account creation.
(define back-to-login-button #f)  ; Button to return to the login screen.
(define register-message (new message% [parent register-panel] [label ""])) ; Feedback message for registration.

;; Main Panel: This panel becomes visible after a successful login and acts as a container
;; for either the band-specific or fan-specific interface, determined by the logged-in user's type.
(define main-panel (new vertical-panel% [parent main-frame]
                                         [stretchable-height #t] ; Allows panel to grow vertically.
                                         [stretchable-width #t]  ; Allows panel to grow horizontally.
                                         [alignment '(center center)] ; Centers its content within the frame.
                                         ))

;; Top bar: A common element across both Band and Fan interfaces, containing a welcome message and a logout button.
(define top-bar-panel (new horizontal-panel% [parent main-panel] [alignment '(left center)] [stretchable-width #t]))
(define welcome-message (new message% [parent top-bar-panel] [label ""] [stretchable-width #t]))
(define logout-button #f)

;; Band Panel: This panel contains the interface specific to 'Band' type users.
;; It uses a 'tab-panel%' to organize different band functionalities.
(define band-panel (new vertical-panel% [parent main-panel] [stretchable-height #t] [stretchable-width #t]))
;; 'tab-panel%' organizes content into tabs, in this case, "My Concerts" and "Create Concert."
(define band-notebook (new tab-panel% [parent band-panel] [choices '("My Concerts" "Create Concert")] [stretchable-height #t] [stretchable-width #t]))

;; Band - My Concerts Tab content: Displays a list of concerts created by the logged-in band.
(define band-concerts-panel (new vertical-panel% [parent band-notebook] [stretchable-height #t] [stretchable-width #t]))
;; 'list-box%' displays the band's concerts, allowing selection for editing or status changes.
(define band-concerts-list (new list-box% [parent band-concerts-panel] [label "My Concerts:"] [choices '()] [stretchable-height #t] [stretchable-width #t]))
(define band-actions-panel (new horizontal-panel% [parent band-concerts-panel]))
(define edit-concert-button #f)     ; Button to open the edit dialog for a selected concert.
(define cancel-concert-button #f)   ; Button to mark a selected concert as "Cancelled."
(define mark-booked-button #f)      ; Button to mark a selected concert as "Fully Booked."
(define remove-cancel-button #f)    ; Button to revert a "Cancelled" concert back to "Active."
(define remove-booked-button #f)    ; Button to revert a "Fully Booked" concert back to "Active."

;; Band - Create Concert Tab content: Provides forms for bands to create new concert listings.
(define create-concert-panel (new vertical-panel% [parent band-notebook] [stretchable-height #t] [stretchable-width #t]))
;; R2.2: A concert listing must contain (at least) band name, date and time, venue, cost.
;;       These 'text-field%' widgets are where the band enters the required concert information.
;;       The labels guide the user on what data to input.
(define concert-band-name-field (new text-field% [parent create-concert-panel] [label "Band Name:"]))
(define concert-date-field (new text-field% [parent create-concert-panel] [label "Date (YYYY-MM-DD):"]))
(define concert-time-field (new text-field% [parent create-concert-panel] [label "Time:"]))
(define concert-venue-field (new text-field% [parent create-concert-panel] [label "Venue:"]))
(define concert-cost-field (new text-field% [parent create-concert-panel] [label "Cost:"]))
(define concert-genre-field (new text-field% [parent create-concert-panel] [label "Genre:"]))
(define concert-description-field (new text-field% [parent create-concert-panel] [label "Description:"] [style '(multiple)] [stretchable-height #t]))
(define create-concert-button #f) ; Button to submit the new concert details.
(define create-concert-message (new message% [parent create-concert-panel] [label ""])) ; Feedback message for concert creation.

;; Fan Panel: This panel contains the interface specific to 'Fan' type users.
;; It uses a 'horizontal-panel%' to split the view into a search area (left) and selected concerts (right).
(define fan-main-content-layout (new horizontal-panel% [parent main-panel]
                                         [stretchable-height #t]
                                         [stretchable-width #t]
                                         [alignment '(center center)]
                                         ))

;; Fan - Search Area (left side of the fan interface): Contains search input fields and displays search results.
(define search-panel (new vertical-panel% [parent fan-main-content-layout]
                             [alignment '(center center)]
                             [stretchable-height #t]
                             [stretchable-width #t]))

(define search-content-wrapper
  (new group-box-panel% ; A panel with a border and a title for better visual organization.
       [parent search-panel]
       [label "Concert Search & Results"]
       [stretchable-height #t]
       [stretchable-width #t]
       [min-width 350])) ; Minimum width to ensure content is visible and readable.

(define search-inner-content-panel
  (new vertical-panel%
       [parent search-content-wrapper]
       [stretchable-height #t]
       [stretchable-width #t]))

;; R3.1: Fans should be able to search listings using appropriate search criteria (band, date, etc).
;;       These 'text-field%' widgets provide the input fields for fans to enter their search criteria.
(define search-form-panel (new vertical-panel% [parent search-inner-content-panel] [stretchable-width #t] [stretchable-height #f]))
(define search-band-field (new text-field% [parent search-form-panel] [label "Band Name:"]))
(define search-date-field (new text-field% [parent search-form-panel] [label "Date (YYYY-MM-DD):"]))
(define search-genre-field (new text-field% [parent search-form-panel] [label "Genre:"]))
(define search-button-panel (new horizontal-panel% [parent search-form-panel]))
(define search-button #f)           ; Button to initiate the search based on entered criteria.
(define show-all-concerts-button #f) ; Button to display all active concerts (a special case of search).
(define remove-selected-button #f) ; ***THE FIX: This button is defined here for the main search panel.***
(define search-results-display-panel #f) ; This will be dynamically created later for scrollability of results.
(define search-message (new message% [parent search-panel] [label ""])) ; Feedback message for search operations.

;; Fan - Selected Concerts Area (right side of the fan interface): Displays concerts added to a fan's personal list.
(define selected-panel (new vertical-panel% [parent fan-main-content-layout]
                             [min-width 350] ; Minimum width for the selected concerts panel.
                             [min-height 400]
                             [stretchable-width #t]
                             [stretchable-height #t]
                             [alignment '(center center)]
                             [border 1])) ; Adds a visual border to separate it from the search area.

(define selected-concerts-title (new message% [parent selected-panel] [label "My Selected Concerts"]))
;; R3.3: Fans should be able to view their list of Selected Concerts.
;;       This 'list-box%' is the primary display for the fan's currently selected concerts.
(define selected-concerts-list (new list-box% [parent selected-panel] [label ""] [choices '()] [stretchable-width #t] [stretchable-height #t] [style '(single)]))
(define selected-actions-panel (new horizontal-panel% [parent selected-panel]))
;; The 'remove-selected-button' is declared globally but its physical location is now in `search-button-panel` for combined view.
(define view-selected-details-button #f) ; Button to view full details of a selected concert.

;; Dialogs (Pop-up Windows): Used for displaying detailed information or editing forms,
;; appearing on top of the main application window.
(define details-dialog #f) ; Pop-up for viewing concert details (read-only).
(define edit-dialog #f)    ; Pop-up for editing concert details (for bands).
(define edit-concert-id #f) ; Stores the ID of the concert currently being edited in the edit dialog.

;; --- 4. EVENT HANDLERS ---
;; These functions are the "controllers" that respond to user actions (like button clicks, text input changes).
;; They read input from the GUI, call utility functions to process data, and then update the GUI to reflect changes.

;; Handles the login button click. It authenticates user credentials.
(define (handle-login b e)
  (let ((u (send username-field get-value)) ; Get username from text field.
        (p (send password-field get-value))) ; Get password from text field.
    (if (or (string=? u "") (string=? p ""))
        (send login-message set-label "Please enter username and password!")
        (let ((user (authenticate u p))) ; Call utility function to authenticate.
          (if user
              (begin
                (set! current-user user) ; Store the logged-in user object for session management.
                (send login-message set-label "Login successful!")
                (sleep 0.5) ; Small delay for message visibility.
                (show-main-interface)) ; Transition to the appropriate main interface (Band or Fan).
              (send login-message set-label "Invalid username or password!"))))))

;; Handles the "Register" button click from the login screen.
;; R1.2: Users should be able to create an account of the appropriate type (band or fan).
;;       This handler switches the GUI from the login panel to the registration panel.
(define (handle-register b e)
  (send register-panel show #t) ; Show the registration panel.
  (send login-panel show #f)    ; Hide the login panel.
  (send login-message set-label "")) ; Clear any previous login messages.

;; Handles the "Create Account" button click in the registration panel.
;; R1.2: Users should be able to create an account of the appropriate type (band or fan).
;;       This function gathers user input (username, password, and chosen type), validates it,
;;       and then creates a new user account in the 'users' hash table.
(define (handle-create-account b e)
  (let ((u (send reg-username-field get-value))
        (p (send reg-password-field get-value))
        (t (send user-type-choice get-string-selection))) ; Get the selected user type ("Band" or "Fan").
    (cond
      [(string=? u "") (send register-message set-label "Username cannot be empty!")]
      [(string=? p "") (send register-message set-label "Password cannot be empty!")]
      [(username-exists? u) (send register-message set-label "Username already exists!")]
      [else
       (let ((id (generate-user-id)))
         (hash-set! users id (user id u p t)) ; Store the new user with their chosen type.
         (send register-message set-label "Account created successfully!")
         (send reg-username-field set-value "") ; Clear the registration form fields.
         (send reg-password-field set-value "")
         (sleep 0.5)
         (handle-back-to-login #f #f))]))) ; Automatically go back to login after successful registration.

;; Handles the "Back to Login" button, switching back to the login screen.
(define (handle-back-to-login b e)
  (send login-panel show #t)
  (send register-panel show #f)
  (send register-message set-label "")
  (send login-message set-label "Write your username and password.")
  (send reg-username-field set-value "")
  (send reg-password-field set-value ""))

;; Shows the appropriate main interface (Band or Fan) after a successful login.
;; R1.1: Users are either Bands or Fans - This conditional logic (`if (string=? (user-type current-user) "Band")`)
;;       is crucial for displaying the correct, type-specific interface based on the user's role.
(define (show-main-interface)
  (send login-panel show #f)
  (send register-panel show #f)
  (send main-panel show #t) ; Show the overarching main content area.
  (send welcome-message set-label (format "Welcome, ~a (~a)" (user-username current-user) (user-type current-user)))

  (if (string=? (user-type current-user) "Band") ; Check the logged-in user's type.
      (begin
        (send band-panel show #t) ; Show the band-specific panels.
        (send fan-main-content-layout show #f) ; Hide fan-specific panels.
        (refresh-band-concerts)) ; R2.1: Bands see their concerts refreshed upon login to view their listings.
      (begin ; Else (if the logged-in user is a "Fan"):
        (send fan-main-content-layout show #t) ; Show the fan-specific panels.
        (send band-panel show #f) ; Hide band-specific panels.
        (refresh-selected-concerts) ; R3.3: Fans see their selected concerts refreshed upon login.
        ;; Dynamically create the search results display panel if it doesn't exist.
        ;; This allows for scrollability of results without defining a fixed-size scroll area upfront.
        (unless search-results-display-panel
          (set! search-results-display-panel (new vertical-panel% [parent search-inner-content-panel]
                                                     [alignment '(left top)]
                                                     [stretchable-width #t]
                                                     [stretchable-height #t]
                                                     [style '(auto-vscroll)])))
        (handle-show-all-concerts #f #f) ; R3.1: Fans initially see all active concerts by default to start Browse.
        (send search-message set-label ""))))

;; Handles the logout button click.
(define (handle-logout b e)
  (set! current-user #f) ; Clear the current user session, effectively logging them out.
  (send main-panel show #f) ; Hide the main application content.
  (send login-panel show #t) ; Return to the login screen.
  (send username-field set-value "") ; Clear login form fields.
  (send password-field set-value "")
  (send login-message set-label "Write your username and password.") ; Reset login message.
  (send welcome-message set-label "") ; Clear welcome message.
  (send fan-main-content-layout show #f)) ; Ensure fan layout is hidden on logout.

;; Handles the "Create Concert" button click in the Band interface.
;; R2.1: Bands must be able to create concert listings - This handler collects data from the form fields
;;       and calls the `create-concert` utility function to save the new listing.
;; R2.2: A concert listing must contain (at least) band name, date and time, venue, cost.
;;       Input fields are validated to ensure required data is present before creating the concert.
(define (handle-create-concert b e)
  (let ((bn (send concert-band-name-field get-value))
        (d (send concert-date-field get-value))
        (t (send concert-time-field get-value))
        (v (send concert-venue-field get-value))
        (c-str (send concert-cost-field get-value)) ; Cost is read as a string and converted.
        (g (send concert-genre-field get-value))
        (desc (send concert-description-field get-value)))
    (if (or (string=? bn "") (string=? d "") (string=? t "") (string=? v "") (string=? c-str ""))
        (send create-concert-message set-label "Please fill in all required fields!") ; Basic input validation.
        (let ((c (string->number c-str))) ; Convert cost to a number.
          (if (number? c)
              (begin
                (create-concert (user-id current-user) bn d t v c desc g) ; Call the utility function to store the concert.
                (send create-concert-message set-label "Concert created successfully!")
                (clear-concert-form) ; Clear input fields for the next creation.
                (refresh-band-concerts)) ; Update the "My Concerts" list to show the new concert.
              (send create-concert-message set-label "Cost must be a valid number!"))))))

;; Clears all the input fields in the create concert form after a successful submission.
(define (clear-concert-form)
  (for-each (lambda (field) (send field set-value "")) ; Iterates through a list of fields to clear their values.
            (list concert-band-name-field
                  concert-date-field
                  concert-time-field
                  concert-venue-field
                  concert-cost-field
                  concert-genre-field
                  concert-description-field)))

;; Updates the 'My Concerts' list-box for the currently logged-in band.
;; This function is called whenever concert data might have changed (e.g., after creation, editing, or status update).
;; R2.3: Bands must be able to edit the details of their own listings - This refresh ensures updates are shown.
;; R2.4: Bands must be able to edit their own listings to indicate if a concert has been cancelled or is fully booked.
;;       The status is displayed in the list, so changes are visible after refresh.
(define (refresh-band-concerts)
  (let ((band-concerts (get-band-concerts (user-id current-user)))) ; Get only concerts belonging to this specific band.
    (send band-concerts-list set ; Update the list-box's choices with formatted concert strings.
          (map (lambda (c)
                  (format "~a - ~a ~a (~a) - ~a"
                          (concert-band-name c)
                          (concert-date c)
                          (concert-time c)
                          (concert-status c) ; Display the current status clearly.
                          (concert-venue c)))
                band-concerts))))

;; A helper function to safely get the currently selected concert from the band's list.
;; It prevents errors if no item is selected by displaying a message box.
(define (with-selected-band-concert callback)
  (let ((selection (send band-concerts-list get-selection))) ; Get the index of the selected item.
    (if (number? selection) ; If an item is actually selected (selection is a number).
        (let* ((band-concerts (get-band-concerts (user-id current-user)))
               (concert (list-ref band-concerts selection))) ; Get the actual concert object using the index.
          (callback concert)) ; Call the provided function with the selected concert object.
        (message-box "Selection Error" "Please select a concert from the list first."))))

;; Handles the "Edit Selected" button click.
;; R2.3: Bands must be able to edit the details of their own listings - This triggers the display of the edit dialog.
(define (handle-edit-concert b e) (with-selected-band-concert show-edit-dialog))

;; Handles the "Cancel Concert" button click.
;; R2.4: Bands must be able to edit their own listings to indicate if a concert has been cancelled.
;;       This function updates the concert's status to "Cancelled" via the `update-concert` utility function.
(define (handle-cancel-concert b e)
  (with-selected-band-concert (lambda (c)
                                (update-concert (concert-id c) "status" "Cancelled")
                                (refresh-band-concerts)))) ; Refresh to show the updated status in the list.

;; Handles the "Mark as Booked" button click.
;; R2.4: Bands must be able to edit their own listings to indicate if a concert is fully booked.
;;       This function updates the concert's status to "Fully Booked" in the data.
(define (handle-mark-booked b e)
  (with-selected-band-concert (lambda (c)
                                (update-concert (concert-id c) "status" "Fully Booked")
                                (refresh-band-concerts))))

;; Handles "Remove Cancel" button click, reverting status from "Cancelled" to "Active".
;; R2.4: Bands must be able to edit their own listings to change status back to active.
(define (handle-remove-cancel button event)
  (with-selected-band-concert
   (lambda (c)
     (if (string=? (concert-status c) "Cancelled")
         (begin
           (update-concert (concert-id c) "status" "Active")
           (refresh-band-concerts)
           (message-box "Success" (format "Concert '~a' status reverted to Active." (concert-band-name c))))
         (message-box "Info" "Selected concert is not marked as Cancelled.")))))

;; Handles "Remove Booked" button click, reverting status from "Fully Booked" to "Active".
;; R2.4: Bands must be able to edit their own listings to change status back to active.
(define (handle-remove-booked button event)
  (with-selected-band-concert
   (lambda (c)
     (if (string=? (concert-status c) "Fully Booked")
         (begin
           (update-concert (concert-id c) "status" "Active")
           (refresh-band-concerts)
           (message-box "Success" (format "Concert '~a' status reverted to Active." (concert-band-name c))))
         (message-box "Info" "Selected concert is not marked as Fully Booked.")))))

;; Handles the "Search" button click in the Fan interface.
;; R3.1: Fans should be able to search listings using appropriate search criteria (band, date, etc).
;;       This handler gathers search inputs from the text fields and calls the `search-concerts` utility
;;       to retrieve matching concerts.
(define (handle-search b e)
  (let ((bn (send search-band-field get-value))
        (d (send search-date-field get-value))
        (g (send search-genre-field get-value)))
    (if (and (string=? bn "") (string=? d "") (string=? g "")) ; Check if all search fields are empty.
        (begin
          (set! current-search-results '()) ; Clear previous results if no criteria entered.
          (refresh-search-results)
          (send search-message set-label "Please enter at least one search criterion."))
        (let ((results (search-concerts bn d g))) ; Call the search utility to filter concerts.
          (set! current-search-results results) ; Store the results.
          (refresh-search-results) ; Update the display with search results.
          (send search-message set-label "")
          (when (empty? results)
            (send search-message set-label "No concerts found matching your criteria."))))))

;; Handles the "Show All Concerts" button click. Effectively performs an empty search to show everything active.
;; R3.1: Fans should be able to search listings - "Show All" is a special case of search where no criteria are applied,
;;       displaying all currently "Active" concerts.
(define (handle-show-all-concerts b e)
  (send search-band-field set-value "") ; Clear search fields for a "show all" action.
  (send search-date-field set-value "")
  (send search-genre-field set-value "")
  (let ((results (search-concerts "" "" ""))) ; Search with empty criteria to get all active concerts.
    (set! current-search-results results)
    (refresh-search-results)
    (send search-message set-label "")
    (when (empty? results)
      (send search-message set-label "No active concerts available."))))

;; Refreshes the display area for search results.
;; It dynamically creates individual panels for each concert result returned by a search.
(define (refresh-search-results)
  ;; Clear existing panels to avoid duplicates or old results.
  (for-each (lambda (child) (send child delete))
            (send search-results-display-panel get-children))
  (when (pair? current-search-results) ; If there are results to display.
    (for-each (lambda (c) (make-concert-result-panel c search-results-display-panel)) ; Create a panel for each concert.
              current-search-results))
  (send search-results-display-panel refresh)) ; Force the panel to redraw itself.

;; Creates a small panel for a single concert result within the search results area.
;; Each panel includes "Add" and "View Details" buttons for fan interaction.
(define (make-concert-result-panel concert-obj parent-panel)
  (let* ((h-panel (new horizontal-panel% [parent parent-panel]
                                         [alignment '(left center)]
                                         [stretchable-width #t]
                                         [border 1] ; Adds a thin border around each result for visual separation.
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
    ;; R3.2: Fans should be able to add items found by searching to a list of Selected Concerts.
    ;;       This "Add" button triggers the 'handle-star-concert' function for adding.
    (new button% [parent h-panel] [label " Add"] [min-width 60] [min-height 20]
         [callback (lambda (b e) (handle-star-concert concert-obj))])
    (new button% [parent h-panel] [label "View Details"] [min-width 90] [min-height 20]
         [callback (lambda (b e) (show-details-dialog concert-obj))])
    h-panel))

;; Handles the "Add" button click on a search result, adding it to the fan's selected list.
;; R3.2: Fans should be able to add items found by searching to a list of Selected Concerts.
;;       This function calls the 'add-to-selected' utility and then refreshes the fan's
;;       'My Selected Concerts' display.
(define (handle-star-concert concert-obj)
  (add-to-selected (user-id current-user) (concert-id concert-obj)) ; Add to fan's selected list.
  (refresh-selected-concerts) ; Update the 'My Selected Concerts' display to show the newly added item.
  (send search-message set-label (format "Added '~a' to selected concerts!" (concert-band-name concert-obj)))
  (sleep 1) ; Brief delay for the message to be visible.
  (send search-message set-label "")) ; Clear the message.

;; Helper to safely get the selected concert from the fan's 'Selected Concerts' list-box.
(define (with-selected-fan-concert callback)
  (let ((selection (send selected-concerts-list get-selection)))
    (if (number? selection)
        (let* ((selected (get-selected-concerts (user-id current-user)))
               (concert (list-ref selected selection)))
          (callback concert))
        (message-box "Selection Error" "Please select a concert to remove."))))

;; Handles the "Remove Selected" button click (for fans).
;; R3.4: Fans should be able to remove items from their Selected Concerts.
;;       This calls the `remove-from-selected` utility function and then refreshes the display.
(define (handle-remove-selected b e)
  (with-selected-fan-concert
   (lambda (c)
     (remove-from-selected (user-id current-user) (concert-id c)) ; Remove the concert from the fan's selected list.
     (refresh-selected-concerts) ; Update the 'My Selected Concerts' display to reflect the removal.
     (message-box "Removed" (format "Removed '~a' from selected concerts." (concert-band-name c)))
     (sleep 1))))

;; Handles the "View Details" button click for a selected concert in the fan's "My Selected Concerts" list.
(define (handle-view-selected-details b e)
  (with-selected-fan-concert show-details-dialog))

;; Refreshes the 'My Selected Concerts' list-box for the current fan.
;; This function populates the 'list-box%' with the details of the fan's currently selected concerts.
;; R3.3: Fans should be able to view their list of Selected Concerts.
(define (refresh-selected-concerts)
  (let ((selected (get-selected-concerts (user-id current-user)))) ; Get actual concert objects for display.
    (send selected-concerts-list set ; Update the list-box with formatted strings for each selected concert.
          (map (lambda (c)
                  (format "~a - ~a ~a - ~a (£~a)"
                          (concert-band-name c)
                          (concert-date c)
                          (concert-time c)
                          (concert-venue c)
                          (concert-cost c)))
                selected))))

;; Shows a pop-up dialog with detailed information about a specific concert.
;; This dialog is read-only and displays all attributes of the concert.
(define (show-details-dialog concert)
  (when details-dialog (send details-dialog show #f)) ; Close any previously open detail dialog.
  (set! details-dialog (new dialog% [label "Concert Details"] [width 400] [height 300]))
  (let ((panel (new vertical-panel% [parent details-dialog] [alignment '(center center)] [stretchable-height #t] [stretchable-width #t])))
    ;; Display all concert fields using message% widgets for static text.
    (new message% [parent panel] [label (format "Band: ~a" (concert-band-name concert))])
    (new message% [parent panel] [label (format "Date: ~a" (concert-date concert))])
    (new message% [parent panel] [label (format "Time: ~a" (concert-time concert))])
    (new message% [parent panel] [label (format "Venue: ~a" (concert-venue concert))])
    (new message% [parent panel] [label (format "Cost: £~a" (concert-cost concert))])
    (new message% [parent panel] [label (format "Genre: ~a" (concert-genre concert))])
    (new message% [parent panel] [label (format "Status: ~a" (concert-status concert))])
    (new message% [parent panel] [label (format "Description: ~a" (concert-description concert))])
    (new button% [parent panel] [label "Close"] [callback (lambda (b e) (send details-dialog show #f))])) ; Close button for the dialog.
  (send details-dialog show #t)) ; Show the details dialog.

;; Shows a pop-up dialog for editing concert details (for Band users).
;; R2.3: Bands must be able to edit the details of their own listings.
;;       This dialog provides input fields initialized with the current concert data,
;;       allowing the band to modify any attribute.
(define (show-edit-dialog concert)
  (when edit-dialog (send edit-dialog show #f)) ; Close any existing edit dialog.
  (set! edit-concert-id (concert-id concert)) ; Store the ID of the concert being edited for update.
  (set! edit-dialog (new dialog% [label "Edit Concert"] [width 400] [height 400]))
  (let ((panel (new vertical-panel% [parent edit-dialog] [stretchable-height #t] [stretchable-width #t])))
    ;; Create text fields for each editable concert property, pre-filled with current values.
    (define e-bn (new text-field% [parent panel] [label "Band Name:"] [init-value (concert-band-name concert)]))
    (define e-d (new text-field% [parent panel] [label "Date:"] [init-value (concert-date concert)]))
    (define e-t (new text-field% [parent panel] [label "Time:"] [init-value (concert-time concert)]))
    (define e-v (new text-field% [parent panel] [label "Venue:"] [init-value (concert-venue concert)]))
    (define e-c (new text-field% [parent panel] [label "Cost:"] [init-value (number->string (concert-cost concert))]))
    (define e-g (new text-field% [parent panel] [label "Genre:"] [init-value (concert-genre concert)]))
    (define e-desc (new text-field% [parent panel] [label "Description:"] [init-value (concert-description concert)] [style '(multiple)] [stretchable-height #t]))
    (let ((bp (new horizontal-panel% [parent panel])))
      (new button% [parent bp] [label "Save"]
           [callback (lambda (b e)
                       ;; R2.3: Bands must be able to edit the details of their own listings.
                       ;;       These calls to 'update-concert' apply the changes from the form fields to the concert object in the data.
                       (update-concert edit-concert-id "band-name" (send e-bn get-value))
                       (update-concert edit-concert-id "date" (send e-d get-value))
                       (update-concert edit-concert-id "time" (send e-t get-value))
                       (update-concert edit-concert-id "venue" (send e-v get-value))
                       (update-concert edit-concert-id "cost" (string->number (send e-c get-value))) ; Convert cost back to number.
                       (update-concert edit-concert-id "genre" (send e-g get-value))
                       (update-concert edit-concert-id "description" (send e-desc get-value))
                       (refresh-band-concerts) ; Refresh the main list to show updates instantly.
                       (send edit-dialog show #f))]) ; Close the dialog after saving.
      (new button% [parent bp] [label "Cancel"] [callback (lambda (b e) (send edit-dialog show #f))])))
  (send edit-dialog show #t)) ; Show the edit dialog.

;; --- 5. BUTTON CALLBACKS ---
;; This section links the GUI buttons (which were initially placeholders, `#f`)
;; to their corresponding event handler functions defined above.
;; This must happen *after* both the buttons and the handlers are defined.

(set! login-button (new button% [parent login-button-panel] [label "Login"] [callback handle-login]))
(set! register-button (new button% [parent login-button-panel] [label "Register"] [callback handle-register]))
(set! create-account-button (new button% [parent reg-button-panel] [label "Create Account"] [callback handle-create-account]))
(set! back-to-login-button (new button% [parent reg-button-panel] [label "Back to Login"] [callback handle-back-to-login]))
(set! logout-button (new button% [parent top-bar-panel] [label "Logout"] [callback handle-logout]))

;; Band-specific action buttons
(set! edit-concert-button (new button% [parent band-actions-panel] [label "Edit Selected"] [callback handle-edit-concert]))
(set! cancel-concert-button (new button% [parent band-actions-panel] [label "Cancel Concert"] [callback handle-cancel-concert]))
(set! mark-booked-button (new button% [parent band-actions-panel] [label "Mark as Booked"] [callback handle-mark-booked]))
(set! remove-cancel-button (new button% [parent band-actions-panel] [label "Remove Cancel"] [callback handle-remove-cancel]))
(set! remove-booked-button (new button% [parent band-actions-panel] [label "Remove Booked"] [callback handle-remove-booked]))
(set! create-concert-button (new button% [parent create-concert-panel] [label "Create Concert"] [callback handle-create-concert]))

;; Fan-specific action buttons
(set! search-button (new button% [parent search-button-panel] [label "Search"] [callback handle-search]))
(set! show-all-concerts-button (new button% [parent search-button-panel] [label "Show All Concerts"] [callback handle-show-all-concerts]))
;; R3.4: Fans should be able to remove items from their Selected Concerts.
;; ***THE FIX: The `remove-selected-button` is now placed within the `search-button-panel`,
;;             making it accessible from the main search interface for fans.***
(set! remove-selected-button (new button% [parent search-button-panel] [label "Remove Selected"] [callback handle-remove-selected]))
(set! view-selected-details-button (new button% [parent selected-actions-panel] [label "View Details"] [callback handle-view-selected-details]))


;; --- 6. INITIALIZATION ---
;; This section runs when the program starts. It sets up the initial visibility of panels
;; (only the login screen is visible at startup) and populates some sample data for testing.

;; Hide all main application panels initially, only showing the login.
(send register-panel show #f)
(send main-panel show #f)
(send band-panel show #f)
(send fan-main-content-layout show #f)

;; Added some sample users to the system for immediate testing.
;; R1.1: Users are either Bands or Fans - Demonstrates creation of different user types.
;; R1.2: Users should be able to create an account of the appropriate type (band or fan).
(hash-set! users (generate-user-id) (user (sub1 next-user-id) "rockband1" "pass123" "Band"))    ; User ID 1 (Band account)
(hash-set! users (generate-user-id) (user (sub1 next-user-id) "metalfan" "pass456" "Fan"))      ; User ID 2 (Fan account)
(hash-set! users (generate-user-id) (user (sub1 next-user-id) "jazzband" "pass789" "Band"))     ; User ID 3 (Band account)

;; Added some sample concert listings to populate the system.
;; R2.1: Bands must be able to create concert listings - These are examples of such listings.
;; R2.2: A concert listing must contain (at least) band name, date and time, venue, cost.
;;       All required fields are provided in these sample concert creations, demonstrating the data structure.
(create-concert 1 "The Rock Stars" "2025-08-15" "20:00" "Madison Square Garden" 45.00 "High energy rock concert" "Rock")
(create-concert 1 "The Rock Stars" "2025-09-20" "19:30" "Wembley Stadium" 55.00 "Stadium tour finale" "Rock")
(create-concert 3 "Jazz Collective" "2025-08-10" "21:00" "Blue Note" 25.00 "Intimate jazz session" "Jazz")
(create-concert 3 "Blues Breakers" "2025-10-05" "18:00" "The Cavern" 30.00 "Classic blues rock" "Blues")
(create-concert 1 "Pop Sensations" "2025-11-01" "22:00" "O2 Arena" 60.00 "Chart-topping hits" "Pop")
(create-concert 3 "Folk Fusion" "2025-09-01" "19:00" "The Green Room" 20.00 "A blend of traditional and modern folk" "Folk")
(create-concert 1 "Electro Beats" "2025-10-20" "23:00" "The Warehouse" 35.00 "Pulsating electronic dance music" "Electronic")
(create-concert 3 "Classical Harmony" "2025-11-15" "19:00" "Concert Hall" 70.00 "An evening of orchestral masterpieces" "Classical")

;; Added some selected concerts for the sample fan (metalfan, ID 2).
;; This pre-populates a fan's "Selected Concerts" list, useful for testing the display and removal features.
;; R3.2: Fans should be able to add items found by searching to a list of Selected Concerts.
;; R3.3: Fans should be able to view their list of Selected Concerts.
(add-to-selected 2 1) ; metalfan selects "The Rock Stars" (concert ID 1)
(add-to-selected 2 3) ; metalfan selects "Jazz Collective" (concert ID 3)
(add-to-selected 2 5) ; metalfan selects "Pop Sensations" (concert ID 5)

;; Finally, show the main application window (which starts on the login panel).
(send main-frame show #t)