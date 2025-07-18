# concert-management-racket
: Project Overview

- This project is a concert listing and discovery application developed using the Racket programming language and its GUI toolkit. The system facilitates two distinct user roles: Bands, who can create and manage concert events, and Fans, who can search, view, and save concerts of interest.
Designed with a functional programming approach, this application demonstrates event-driven GUI programming concepts, state management, and user interaction workflows.

; Features

; - Band Users

Create new concert listings with attributes including date, time, venue, cost, and genre.
Modify existing concert details.
Manage concert status: Active, Cancelled, Fully Booked, with the ability to revert to Active.
View and select concerts from a consolidated listing panel.

; - Fan Users

Search concerts by band name, date, or genre.
Browse search results displayed in a scrollable, detailed list.
Add and remove concerts to/from a personalized “Selected Concerts” list.
View detailed concert information via pop-up dialogs.


; Technologies and Tools

- Programming Language: Racket
GUI Framework: racket/gui
Core Concepts: Functional programming, event-driven interfaces, GUI widgets, stateful list management.

; Getting Started

- Prerequisites
Racket installation: Download and install from https://racket-lang.org.

; Running the Application

Open the main source file (main.rkt) in DrRacket.
Execute the program by clicking Run.
Register or log in as either a Band or Fan user.
Access the appropriate dashboard to interact with the application features.

; Usage Instructions

- Upon launch, users must register or log in.
Bands can create and manage concert listings via the dashboard.
Fans can search for concerts using various filters and manage their personal concert selections.
Use provided buttons and menus for navigation and performing actions.
Pop-up dialogs display detailed information and confirm actions such as deletions or cancellations.
