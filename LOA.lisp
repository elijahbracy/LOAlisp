
#|/* *********************************************
Source Code to print things on the screen
********************************************* */|#


#|/* *********************************************************************
Function Name: print-board

Purpose: this function prints the board to the screen

Parameters: board, a board in list form. optionalparams row-index and row-coordinate used in recursive calls

Return Value: none

Algorithm: 1) first time called print top border
           2) last time print bottom borders 
           3) base case, print side borders, call print-row to handle printing individual rows

Reference: ChatGPT for default params syntax and format t for printing literals
********************************************************************* */|#
(defun print-board (board &optional (row-index -1) (row-coordinate 8))
;; first time called row-index = -1, print top border and continue with incremented row-index
  (cond ((null board)
         (princ "no board available")
         (terpri))
        ((= row-index -1)
         (format t " +-----------------+~%")
         (print-board board (1+ row-index)))
         ;; last time called, row index = 8, print bottom border and column coordinates
        ((= row-index (length board))
         (format t " +-----------------+~%")
         (format t "   A B C D E F G H~%"))
        (t
        ;; base case, print row-coordinate, side border, print the row, and closing side border, continue with incremented row index and decremented row coordinate
         (format t "~d| " row-coordinate)
         (print-row (nth row-index board))
         (format t "|~%")
         (print-board board (1+ row-index) (1- row-coordinate)))))


#|/* *********************************************************************
Function Name: print-row

Purpose: this function prints the boards rows to the screen

Parameters: row, a row of a board in list form. 

Return Value: none

Algorithm:

Reference: none
********************************************************************* */|#
(defun print-row (row)
    (cond ((null row)
            nil)
            ((equal (first row) 'X)
             (format t ". ")
             (print-row (rest row)))
            (t
            (format t "~a " (first row))
            (print-row (rest row)))))



#|/* *********************************************************************
Function Name: announce-tournament-result

Purpose: this function prints the results of a tournament to the screen

Parameters: game-state, a list 

Return Value: none

Algorithm:

Reference: chatGPT for quit as way to immediately exit
********************************************************************* */|#
(defun announce-tournament-result (game-state)
  (let ((board (nth 0 game-state))
        (h-rounds-won (nth 1 game-state))
        (h-score (nth 2 game-state))
        (c-rounds-won (nth 3 game-state))
        (c-score (nth 4 game-state))
        (cur-player (nth 5 game-state))
        (cur-player-color (nth 6 game-state)))
    (cond ((eq h-rounds-won c-rounds-won)
           (princ "Tournament ended in draw.")
           (terpri)
           (format t "Both players won ~a rounds" h-rounds-won)
           (terpri)
           (format t "Human scored ~a points." h-score)
           (terpri)
           (format t "Computer scored ~a points." c-score)
           (terpri))
          ((> h-rounds-won c-rounds-won)
           (princ "Human won the tournament.")
           (terpri)
           (format t "Human won ~a rounds" h-rounds-won)
           (terpri)
           (format t "Human scored ~a points." h-score)
           (terpri))
          (t
           (princ "Computer won the tournament.")
           (terpri)
           (format t "Computer won ~a rounds" c-rounds-won)
           (terpri)
           (format t "Computer scored ~a points." c-score)
           (terpri)))
    (princ "Thanks for playing!")
    (terpri)
    (quit)))

#|/* *********************************************************************
Function Name: score-round

Purpose: this function scores a round, called after a winner is determined

Parameters: game-state, a list 

Return Value: updated game state

Algorithm: 1) function is always called with curplayer = winner so just announce cur-player as winner and list points they scored
           2) if the cur-player == human, return a new game state incrementing h-rounds-won and h-score
           3) otherwise increment c-rounds-won and c-score

Reference: none
********************************************************************* */|#
(defun score-round (game-state)
  (let ((board (nth 0 game-state))
        (h-rounds-won (nth 1 game-state))
        (h-score (nth 2 game-state))
        (c-rounds-won (nth 3 game-state))
        (c-score (nth 4 game-state))
        (cur-player (nth 5 game-state))
        (cur-player-color (nth 6 game-state)))
    (format t "~a won the round!" cur-player) 
    (terpri)
    (format t "~a scored: ~a points" cur-player (get-score board cur-player-color))
    (terpri)
    (cond ((eq cur-player 'human)
           (list board (+ h-rounds-won 1) (+ h-score (get-score board cur-player-color)) c-rounds-won c-score cur-player cur-player-color))
          (T
           (list board h-rounds-won h-score (+ c-rounds-won 1) (+ c-score (get-score board cur-player-color)) cur-player cur-player-color)))))


#|/* *********************************************************************
Function Name: print-moves

Purpose: this function prints list of moves in human friendly format to screen

Parameters: moves, a list

Return Value: none

Algorithm: 

Reference: none
********************************************************************* */|#
(defun print-moves (moves)
 (let* ((move (first moves))
        (move-origin (first move))
        (move-destination (second move))) 
  (cond ((null move)
         nil)
        (T
         (princ (indices-to-notation move-origin))
         (princ "->")
         (princ (indices-to-notation move-destination))
         (terpri)
         (print-moves (rest moves))) )))


#|/* *********************************************
Source Code to ask the human player for input
********************************************* */|#


#|/* *********************************************************************
Function Name: begin

Purpose: this function begins the game, welcoming player and asking if they want to load a game

Parameters: none

Return Value: a call to gameloop with either a restored gamestate or a fresh gamestate using round-start

Algorithm: 

Reference: none
********************************************************************* */|#
(defun begin ()
  (princ "Welcome to LOA")
  (terpri)
  (princ "---------------")
  (terpri)
  (princ "Would you like to load a game? (y/n): ")
  (terpri)
  (let ((load-game (yes-no-validate (read))))
    (cond ((equal load-game 'Y)
           (let ((file-name (get-file-path)))
            (gameloop (restore-game-state file-name))) )
           
          (t
           (gameloop (round-start))
            ))))


#|/* *********************************************************************
Function Name: round-start

Purpose: this function initializes a round, either with provided params or default ones if the first round of a tournament

Parameters: optional, h-rounds-won, number of human rounds won
                      h-score, human score
                      c-rounds-won, number of computer rounds won
                      c-score, computer score

Return Value: a call to gameloop with default board and either the deault or provided params

Algorithm: 1) if rounds won for both player equal, toss coin and set curplayer to winner
          2) otherwise set curplayer to player with more rounds won

Reference: none
********************************************************************* */|#
(defun round-start (&optional (h-rounds-won 0) (h-score 0) (c-rounds-won 0) (c-score 0))
  (let ((board '(
                  ( x b b b b b b x ) ;; condense groups board
                  ( w x x x x x x w )
                  ( w x x x x x x w )
                  ( w x x x x x x w )
                  ( w x x x x x x w )
                  ( w x x x x x x w )
                  ( w x x x x x x w )
                  ( x b b b b b b x ))))
    (cond ((eq h-rounds-won c-rounds-won)
           (princ "toss coin for first player, make call. (h/t)")
           (terpri)
           (let ((toss-result (coin-toss (read)))) ;cur-player-and-cur-player-color
             (gameloop (list board h-rounds-won h-score c-rounds-won c-score (first toss-result) (first (rest toss-result))))))
          ((> h-rounds-won c-rounds-won)
           (gameloop (list board h-rounds-won h-score c-rounds-won c-score 'human 'black)))
          (t
           (gameloop (list board h-rounds-won h-score c-rounds-won c-score 'computer 'black))))))



#|/* *********************************************************************
Function Name: restore-game-state

Purpose: restores game state from text file

Parameters: path to text file

Return Value: contents of text file

Algorithm:

Reference: Professor Kumar for read, ChatGPT for working with files/streams
********************************************************************* */|#
 (defun restore-game-state (path)
   (with-open-file (stream path :direction :input)
     (read stream)))



#|/* *********************************************************************
Function Name: pause-game

Purpose: asks the player if they want to pause the game and save the current game state

Parameters: game-state, a list

Return Value: none

Algorithm:

Reference: none
********************************************************************* */|#
(defun pause-game (game-state)
  (princ "Would you like to pause game (data will be saved)? (y/n): ")
  (terpri)
  (let ((pause (yes-no-validate (read)))) 
    (cond ((eq pause 'y)
           (save-game-state game-state)
           (princ "Thanks for playing!")
           (quit))
          (T
           nil))))



#|/* *********************************************************************
Function Name: play-again

Purpose: asks the player if they want to play another round

Parameters: game-state, a list

Return Value: none

Algorithm:

Reference: none
********************************************************************* */|#
(defun play-again (game-state)
  (princ "play again? (y/n)")
  (terpri)
  (let ((play-again (yes-no-validate (read)))
        (h-rounds-won (nth 1 game-state))
        (h-score (nth 2 game-state))
        (c-rounds-won (nth 3 game-state))
        (c-score (nth 4 game-state)))
    (cond ((eq play-again 'y)
           (round-start h-rounds-won h-score c-rounds-won c-score)) 
          (T
           (announce-tournament-result game-state)))))



#|/* *********************************************************************
Function Name: coin-toss

Purpose: asks the player if they want to play another round

Parameters: game-state, a list

Return Value: none

Algorithm:
    1. Get the user's input for their guess (either 'H' or 'T').
    2. Generate a random number (0 or 1) representing the result of the coin toss.
    3. If the user's guess is not 'H' or 'T', prompt them for valid input.
    4. If the user's guess matches the result of the coin toss, they play first as black.
    5. If the user's guess does not match the result of the coin toss, the computer plays first as black.
    6. Return a list indicating whether the player is human or computer and the color they are playing (black).

Reference:
********************************************************************* */|#
(defun coin-toss (user-input)
  (let* ((guess (char (string-upcase user-input) 0))
         (number (random 2 (make-random-state t)))
         (coin (cond ((= number 0) #\H)
                     (t #\T))))
    (cond ((not (or (char= guess #\H)
                    (char= guess #\T)))
           (princ "Invalid input. Please enter H or T: ")
           (terpri)
           (coin-toss (read)))
          ((char= guess coin)
           (princ "correct, you play first. color = black")
           (terpri)
           '(human black))
          (t
           (princ "incorrect, computer first. your color = white")
           (terpri)
           '(computer black)))))


#|/* *********************************************************************
Function Name: save-game-state

Purpose: Saves the current state of the game to a file.

Parameters: game-state - The current state of the game, usually a list representing the game state.

Return Value: None

Algorithm:
    1. Obtain the filename for saving the game state by calling the function get-save-file.
    2. Open the file specified by save-file for writing, creating it if it does not exist and replacing it if it does.
    3. Write the string representation of the game-state to the opened file.
    4. Close the file stream.

Reference: chatGPT for working with streams/file I/O
********************************************************************* */|#
(defun save-game-state (game-state)
  (let ((save-file (get-save-file))) 
    (with-open-file (stream save-file :direction :output
                                      :if-exists :supersede)
      (format stream "~a" game-state)
      )))



#|/* *********************************************************************
Function Name: get-save-file

Purpose: Prompts the user to enter the name of the file they want to save to and returns the file path.

Parameters: None

Return Value: A string representing the file path.

Algorithm:
    1. Print a message asking the user to enter the name of the file they want to save to.
    2. Move to the next line for user input.
    3. Read the input provided by the user.
    4. Check if the file extension ".txt" is already present in the input path.
    5. If the file extension is not present, append ".txt" to the input path.
    6. Create a file stream using the modified path for writing.
    7. If the file stream creation is successful, write a test message to the file.
    8. Print "success" to indicate successful file creation.
    9. Move to the next line.
    10. Return the new path.
    11. If an error occurs during file creation, handle the error by printing an error message.
    12. Recursively call get-save-file to prompt the user for input again.

Reference: None
********************************************************************* */|#
(defun get-save-file ()
  (princ "Please enter the name of the file you would like to save to")
  (terpri)
  (let* ((path (read-line))
         (new-path (cond ((null (search ".txt" path :from-end t))
                          (concatenate 'string path ".txt"))
                         (t path)))
         (file-stream (make-pathname :name new-path :defaults *default-pathname-defaults*)))
    (handler-case
      (with-open-file (stream file-stream
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
        (format stream "Test")
        (princ "success")
        (terpri)
        new-path)
      (error (e)
        (format t "Error: ~a" e)
        (terpri)
        (get-save-file)))))



#|/* *********************************************************************
Function Name: get-file-path

Purpose: Prompts the user to enter the full filename with the file extension and returns the file path.

Parameters: None

Return Value: A string representing the file path.

Algorithm:
    1. Print a message asking the user to enter the full filename with the file extension.
    2. Move to the next line for user input.
    3. Print a message asking the user to specify the entire path if the file is in another directory.
    4. Move to the next line for user input.
    5. Read the input provided by the user.
    6. Check if the file specified by the input path exists using probe-file.
    7. If the file exists, return the input path.
    8. If the file does not exist, print an error message indicating that the file does not exist.
    9. Move to the next line.
    10. Recursively call get-file-path to prompt the user for input again.

Reference: chatGPT for the fact read-line returns a string, which was better for this purpose
********************************************************************* */|#
(defun get-file-path ()
  (princ "Please enter the full filename with file extension (e.g., file.txt) ")
  (terpri) 
  (princ "If the file is in another directory, please specify the entire path (e.g., /path/to/your/file.txt)")
  (terpri) 
  (let ((path (read-line)))
    (cond ((not (null (probe-file path)))
           path)
          (T
           (format t "Error: File ~a does not exist.~%" path)
           (terpri)
           (get-file-path)))))


#|/* *********************************************************************
Function Name: validate-user-move

Purpose: Validates the user's move on the game board.

Parameters:
    - board: The current state of the game board.
    - color: The color of the player making the move.
    - move: The move to validate, represented as a list of two positions (origin and destination).

Return Value: 
    - If the move is valid, returns a list of two positions (origin and destination).
    - If the move is invalid, prompts the user for a new move input.

Algorithm:
    1. Extract the origin and destination positions from the move.
    2. Convert the algebraic notation of positions to their corresponding indices on the game board.
    3. Retrieve the piece at the origin position on the board.
    4. Retrieve the piece at the destination position on the board.
    5. Calculate the distance between the origin and destination positions.
    6. Check if there are any pieces on the straight line path between the origin and destination.
    7. Check if the move is vertical, horizontal, or diagonal.
    8. Validate that the origin position contains a piece of the player's color.
    9. Validate that the destination position is empty or contains an enemy piece.
    10. Validate that the distance between the origin and destination matches the number of pieces on the straight line path.
    11. Validate that there are no enemy pieces blocking the path between the origin and destination.
    12. If any validation check fails, prompt the user for a new move input.
    13. If all validation checks pass, return a list of the origin and destination positions.

Reference: chatGPT for let* over let for variables dependant on previous variables in the same let block
********************************************************************* */|#
(defun validate-user-move (board color move)
  (let* ((origin (nth 0 move))
         (destination (nth 1 move))
         (origin-indices (notation-to-indices origin))
         (origin-piece (print-piece-at-index board origin-indices))
         (destination-indices (notation-to-indices destination))
         (destination-piece (print-piece-at-index board destination-indices))
         (distance (get-distance origin-indices destination-indices))
         (pieces-on-line (pieces-on-line board origin-indices destination-indices))
         (is-straight (is-vertical-horizontal-diagonal board origin-indices destination-indices)))
         
         ;; origin must be same as cur-color
    (cond ((not (equal origin-piece color))
           (princ "Not a valid move. The origin does not contain a piece of your color.")
           (terpri)
           (validate-user-move board color (get-move-user-input)))
           ;; destination must be empty or contain enemy piece
          ((equal destination-piece color)
           (princ "not valid. dest piece == cur-color")
           (terpri)
           (validate-user-move board color (get-move-user-input)))

          ;; move must be verticle, horizontal, or diagonal
          ((not (eq is-straight t))
           (princ "not valid. pieces can only move in straight line.")
           (terpri)
           (validate-user-move board color (get-move-user-input)))

           ;; distance must be equal to pieces on line
          ((not (equal distance pieces-on-line))
           (princ "not valid, distance != pieces on line")
           (terpri)
           (validate-user-move board color (get-move-user-input)))
           
           
          ;; can't have enemy pieces on path
          ((equal (pieces-in-way board origin-indices destination-indices color) t)
           (princ "not valid, pieces in way")
           (terpri)
           (validate-user-move board color (get-move-user-input)))

          (t
           (list origin-indices destination-indices)))))



#|/* *********************************************************************
Function Name: yes-no-validate

Purpose: Validates user input as either 'Y' or 'N'.

Parameters:
    - user-input: The input provided by the user.

Return Value:
    - If the input is valid ('Y' or 'N'), returns the input.
    - If the input is invalid, prompts the user for a new input.

Algorithm:

Reference: None
********************************************************************* */|#
(defun yes-no-validate (user-input)
  (cond ;; TODO: make sure input is 1 char
        ((not (or (char= (char (string-upcase user-input) 0) #\Y)
                  (char= (char (string-upcase user-input) 0) #\N)))
         (princ "Invalid input. Please enter Y or N: ")
         (terpri)
         (yes-no-validate (read)))
        (t 
          ;(princ "valid")
          (terpri)
          user-input)))


#|/* *********************************************
Source Code for game loop
********************************************* */|#


#|/* *********************************************************************
Function Name: play

Purpose: Manages the gameplay by processing player moves and updating the game state accordingly.

Parameters:
    - game-state: A list representing the current state of the game, including the game board, scores, current player, etc.

Return Value:
    - Returns an updated game state after processing the player's move.

Algorithm:
    1. Extract relevant data from the game-state, such as the game board, scores, and current player.
    2. Print information about the current player and the game board.
    3. If the current player is human, prompt the user to enter help mode.
    4. If help mode is enabled, generate a suggested move using a strategy and display it to the user.
    5. Prompt the user to enter their move and validate it.
    6. Update the game board based on the validated move.
    7. If the current player is the computer, generate a move using a strategy and update the game board accordingly.
    8. Print information about the computer's move.
    9. Return the updated game state.

Reference: chatGPT for string-upcase and correct extraction of move-origin/destination with first/rest
********************************************************************* */|#
(defun play (game-state)
  (let ((board (nth 0 game-state))
        (h-rounds-won (nth 1 game-state))
        (h-score (nth 2 game-state))
        (c-rounds-won (nth 3 game-state))
        (c-score (nth 4 game-state))
        (cur-player (nth 5 game-state))
        (cur-player-color (cond ((eq (nth 6 game-state) 'black) 'b)
                                 (t 'w))))
    (format t "current player: ~a" cur-player)
    (terpri)
    (format t "color: ~a" (nth 6 game-state))
    (terpri)
    (print-board board)
    (cond ((eq cur-player 'human) 
           (format t "Enter help mode? (y/n): ")
           (terpri)
           (let* ((help-mode (char (string-upcase (yes-no-validate (read))) 0))
                  (move-reason (cond ((char= help-mode #\N) nil)
                                     (t (strategy game-state))))
                  (move-origin (cond ((null move-reason) nil)
                                     (t (first (first move-reason)))))
                  (move-destination (cond ((null move-reason) nil)
                                     (t (first (rest (first move-reason))))))
                  (reason (cond ((null move-reason) nil)
                                     (t (rest move-reason)))))
             (cond ((char= help-mode #\Y)
                    (format t "suggested move: ~a->~a ~a" (indices-to-notation move-origin) (indices-to-notation move-destination) reason)
                    (terpri)
                    (print-board board)
                    (terpri))
                   (T
                    )))
           (format t "Enter move: ")
           (terpri)
           (let* ((move (validate-user-move board cur-player-color (get-move-user-input)))
                  (origin (nth 0 move))
                  (destination (nth 1 move) )
                  (board (update-board board origin 'x))
                  (board (update-board board destination cur-player-color))
                  (game-state (cons board (rest game-state))))
            (print-board board)
            game-state))
          ((eq cur-player 'computer)
            (terpri)
            (let* ((move-reason (strategy game-state))
                   (move-origin (first (first move-reason)))
                   (move-destination (first (rest (first move-reason))))
                   (reason (rest move-reason))
                   (new-board (update-board board move-origin 'x))
                   (new-board (update-board new-board move-destination cur-player-color))
                   (game-state (cons new-board (rest game-state))))
              (print-board board)
              (print-board new-board)
              (format t "computer moved: ~a->~a ~a" (indices-to-notation move-origin) (indices-to-notation move-destination) reason)
              (terpri)
              game-state)))))



#|/* *********************************************************************
Function Name: gameloop

Purpose: Manages the main game loop, allowing players to take turns and handling game state transitions.

Parameters:
    - game-state: A list representing the current state of the game, including the game board, scores, current player, etc.

Return Value: if no one has won, call to gameloop with switched players, otherwise call to play again with winner as current player

Algorithm:
    1. Play the current game state by calling the play function.
    2. Check if a player has won the game. If so, proceed to handling win conditions.
    3. If no player has won yet, ask player if they want to pause the game, switch to the next player's turn.
    4. Recursively call gameloop with the updated game state to continue the game loop.

Reference: None
********************************************************************* */|#
(defun gameloop (game-state)
  ;; play
  ;; check win, if win leave
  (let* ((new-game-state (play game-state)) 
         (board (nth 0 new-game-state))
         (h-rounds-won (nth 1 new-game-state))
         (h-score (nth 2 new-game-state))
         (c-rounds-won (nth 3 new-game-state))
         (c-score (nth 4 new-game-state))
         (cur-player (nth 5 new-game-state))
         (cur-player-color (cond ((eq (nth 6 game-state) 'black) 'b)
                                 (t 'w)))
         (won (is-win board)))
    (cond ((eq won nil) 
           (pause-game (switch-players new-game-state))
           (gameloop (switch-players new-game-state)))
          ((eq won cur-player-color)
           (play-again (score-round new-game-state)))
          (T
           (play-again (score-round (switch-players new-game-state)))))))






#|/* *********************************************
Source Code for strategy
********************************************* */|#


#|/* *********************************************************************
Function Name: strategy

Purpose: Determines the best move for the current player based on the game state.

Parameters:
    - game-state: A list representing the current state of the game, including the game board and current player's color.

Return Value: A list containing the best move and a reason for the move.

Algorithm:
    1. Extract relevant data from the game-state, such as the game board and current player's color.
    2. Generate all possible moves for the current player.
    3. Generate all possible moves for the opponent.
    4. Check for various strategic scenarios, such as potential wins, threats from the opponent, good captures, etc.
    5. Prioritize moves based on the strategic importance of each scenario.
    6. Return the best move along with a reason for choosing it.

Reference: None
********************************************************************* */|#
(defun strategy (game-state)
  (let* ((board (nth 0 game-state))
         (cur-player-color (cond ((eq (nth 6 game-state) 'black) 'b)
                                 (t 'w)))
         (opponent-color (cond ((eq cur-player-color 'b) 'w)
                                (t 'b)))
         (moves (get-all-moves board cur-player-color (get-origins board cur-player-color)))
         (opponent-moves (get-all-moves board opponent-color (get-origins board opponent-color)))
         (current-player-win (has-winning-move board moves cur-player-color))
         (opponent-win (has-winning-move board opponent-moves opponent-color))
         (thwart (cond ((null opponent-win) nil)
                       (t (has-thwart board moves cur-player-color))))
         (good-capture (has-good-capture board moves cur-player-color opponent-color))
         (capture (has-capture board moves cur-player-color opponent-color))
         (delay (cond ((null current-player-win) nil)
                      (t (has-delay board moves cur-player-color opponent-color))))
         (condense (has-condense board moves cur-player-color))
         (connect (has-connect board moves cur-player-color))
         (stall (has-stall board moves cur-player-color))
         (blok (has-block board moves cur-player-color opponent-color))
         (any-move (first moves)))
    (princ "Possible moves: ")
    (terpri)
    (print-moves (sort moves #'compare-coordinates)) ;; just here for quality of life improvement during demo
    (terpri)
    (cond   ;;delay
          ((not (null delay))
            ;(princ current-player-win)
           (cons delay "to delay win and increase score"))
    
           ;; win
          ((not (null current-player-win))
           (cons current-player-win "to win the game"))

           ;; thwart, meaning opponent has to have win
          ((not (null thwart))
           (cons thwart "to thwart opponent"))

           ;; connect groups
          ((not (null connect))
           (cons connect "to reduce number of groups by 2"))

           ;; condense groups
          ((not (null condense))
           (cons condense "to reduce number of groups"))

           ;; blocks
          ((not (null blok))
           (cons blok "to block opponent"))

           ;; "good" capture
          ((not (null good-capture))
           (cons good-capture "to capture opponents piece"))
          
           ;; "bad" capture
          ((not (null capture))
           (cons capture "to capture opponents piece as last resort"))

           ;; stall 
          ((not (null stall))
           (cons stall "to stall defeat"))

          (t (cons any-move "for lack of any better strategy"))
          )))


#|/* *********************************************************************
Function Name: has-block

Purpose: Checks if a move can block the opponent's piece against the ultimate row or column of the board.

Parameters:
    - board: The current state of the game board.
    - moves: A list of possible moves for the current player.
    - color: The color of the current player.
    - opponent-color: The color of the opponent player.

Return Value: A move that can block the opponent's potential winning move, or nil if no such move is found.

Algorithm:
    1. Iterate through the list of possible moves for the current player.
    2. For each move, determine the destination position.
    3. Check if the destination position is on the edge of the board (penultimate row or column).
    4. If the destination position is on the edge of the board, update the board with the move and check if it gives the opponent an oppurtunity to win.
    5. If the move doesn't give opponent a winning move and is placed on the penultimate row or column, return the move.
    6. Otherwise, continue iterating through the list of possible moves.
    7. If no move is found that can block the opponent's winning move, return nil.

Reference: None
********************************************************************* */|#
(defun has-block (board moves color opponent-color)
 (let* ( (move (first moves))
         (move-destination (cond ((null move) nil)
                                 (t (second move))))
         (destination-row (first move-destination))
         (destination-col (second move-destination))
         (penultimate (cond ((or (eq destination-row 1)
                                 (eq destination-row 6)
                                 (eq destination-col 1)
                                 (eq destination-col 6)) t)
                            (t nil)))
         (new-board (cond ((null move) nil)
                          (t (update-board board move-destination color))))
         (new-board (cond ((null new-board) nil)
                          (t (update-board new-board (first move) 'x))))
         (thwart (has-thwart board (list move) color))) 
    (cond ((null move) nil)
          ((and (not (null thwart)) (not (null penultimate)))
           (cond ((and (eq destination-row 1) (eq (print-piece-at-index board (list 0 destination-col)) opponent-color))
                  move)
                 ((and (eq destination-row 6) (eq (print-piece-at-index board (list 7 destination-col)) opponent-color))
                  move)
                 ((and (eq destination-col 1) (eq (print-piece-at-index board (list destination-row 0)) opponent-color))
                  move)
                 ((and (eq destination-col 6) (eq (print-piece-at-index board (list destination-row 7)) opponent-color))
                  move)))
          (t
           (has-block board (rest moves) color opponent-color)))))


#|/* *********************************************************************
Function Name: has-stall

Purpose: Checks if a move doesn't give the opponent an oppurtunity to win.

Parameters:
    - board: The current state of the game board.
    - moves: A list of possible moves for the current player.
    - color: The color of the current player.

Return Value: A move that will not result in a board where the opponent has a winning move, or nil if no such move is found.

Algorithm:

Reference: None
********************************************************************* */|#
(defun has-stall (board moves color)
 (let* ( (move (first moves))
         (thwart (has-thwart board (list move) color)) )
    
    (cond ((null move) nil)
          ((not (null thwart))
           move)
          (t
           (has-stall board (rest moves) color)))))


#|/* *********************************************************************
Function Name: has-connect

Purpose: Checks if a move can connect multiple groups of the same color on the game board.

Parameters:
    - board: The current state of the game board.
    - moves: A list of possible moves for the current player.
    - color: The color of the current player.

Return Value: A move that can connect multiple groups of the same color (reducing total groups by 2), or nil if no such move is found.

Algorithm:
    1. Iterate through the list of possible moves for the current player.
    2. For each move, update the game board with the move and count the number of groups of the same color before and after the move.
    3. If the move results in a decrease of two or more groups of the same color and also doesn't give the opponent a potential winning move, return the move.
    4. Otherwise, continue iterating through the list of possible moves.
    5. If no move is found that can connect multiple groups of the same color, return nil.

Reference: None
********************************************************************* */|#
(defun has-connect (board moves color)
 (let* ( (move (first moves))
         (move-destination (cond ((null move) nil)
                                 (t (second move))))
         (new-board (cond ((null move) nil)
                          (t (update-board board move-destination color))))
         (new-board (cond ((null new-board) nil)
                          (t (update-board new-board (first move) 'x))))
         (thwart (has-thwart board (list move) color))  
         (pre-move-groups (count-groups board color))
         (post-move-groups (cond ((null move) nil)
                                 (t (count-groups new-board color)))))
         
    (cond ((null move) nil)
          ((and (>= (- pre-move-groups post-move-groups) 2) (not (null thwart)))
           move)
          (t
           (has-connect board (rest moves) color)))))


#|/* *********************************************************************
Function Name: has-condense

Purpose: Checks if a move can reduce total groups of the same color on the game board by 1.

Parameters:
    - board: The current state of the game board.
    - moves: A list of possible moves for the current player.
    - color: The color of the current player.

Return Value: A move that can condense multiple groups of the same color, or nil if no such move is found.

Algorithm:
    1. Iterate through the list of possible moves for the current player.
    2. For each move, update the game board with the move and count the number of groups of the same color before and after the move.
    3. If the move results in a decrease in the number of groups of the same color and also thwarts the opponent's potential winning move, return the move.
    4. Otherwise, continue iterating through the list of possible moves.
    5. If no move is found that can condense multiple groups of the same color, return nil.

Reference: None
********************************************************************* */|#
(defun has-condense (board moves color)
 (let* ( (move (first moves))
         (move-destination (cond ((null move) nil)
                                 (t (second move))))
         (new-board (cond ((null move) nil)
                          (t (update-board board move-destination color))))
         (new-board (cond ((null new-board) nil)
                          (t (update-board new-board (first move) 'x))))
         (thwart (has-thwart board (list move) color))  
         (pre-move-groups (count-groups board color))
         (post-move-groups (cond ((null move) nil)
                                 (t (count-groups new-board color)))))
         
    (cond ((null move) nil)
          ((and (< post-move-groups pre-move-groups) (not (null thwart)))
           move)
          (t
           (has-condense board (rest moves) color)))))



#|/* *********************************************************************
Function Name: has-delay

Purpose: Checks if a move can delay a player's own winning move while also capturing opponent's piece to potentially increase score.

Parameters:
    - board: The current state of the game board.
    - moves: A list of possible moves for the current player.
    - color: The color of the current player.
    - opponent-color: The color of the opponent player.

Return Value: A move that can capture opponents piece while not giving opponent an opportunity to win, or nil if no such move is found.

Algorithm:
    1. Iterate through the list of possible moves for the current player.
    2. For each move, check if it results in a capture, doesn't give opponent a winning move, and does not result in an immediate win for the current player.
    3. If the move satisfies these conditions, return it.
    4. Otherwise, continue iterating through the list of possible moves.
    5. If no move is found that can delay the opponent's winning move while setting up a capture, return nil.

Reference: None
********************************************************************* */|#
(defun has-delay (board moves color opponent-color)
 (let* ( (move (first moves))
         (capture (has-capture board (list move) color opponent-color))
         (thwart (has-thwart board (list move) color)) 
         (winning-move (has-winning-move board (list move) color)))
    (cond ((null move) nil)
          ((and (not (null capture)) (not (null thwart)) (null winning-move)) 
           move)
          (t
           (has-delay board (rest moves) color opponent-color)))))


#|/* *********************************************************************
Function Name: has-good-capture

Purpose: Checks if a move results in capturing opponent's pieces and reduces their piece count while not giving opponent opportunity to win.

Parameters:
    - board: The current state of the game board.
    - moves: A list of possible moves for the current player.
    - color: The color of the current player.
    - opponent-color: The color of the opponent player.

Return Value: A move that results in capturing opponent's pieces and reduces their piece count while not giving opponent winning move, or nil if no such move is found.

Algorithm:
    1. Iterate through the list of possible moves for the current player.
    2. For each move, update the game board with the move and count the number of opponent's pieces before and after the move.
    3. If the move results in a decrease in the number of opponent's pieces and also doesn't give opponent a winning move, return the move.
    4. Otherwise, continue iterating through the list of possible moves.
    5. If no move is found that results in a good capture while also thwarting the opponent's winning move, return nil.

Reference: None
********************************************************************* */|#
(defun has-good-capture (board moves color opponent-color)
(let* ( (move (first moves))
         (move-destination (cond ((null move) nil)
                                 (t (second move))))
         (new-board (cond ((null move) nil)
                          (t (update-board board move-destination color))))
         (new-board (cond ((null new-board) nil)
                          (t (update-board new-board (first move) 'x))))
         (pre-move-count (count-pieces-on-board board opponent-color))
         (post-move-count (cond ((null new-board) nil)
                               (t (count-pieces-on-board new-board opponent-color))))
         (thwart (has-thwart board (list move) color))) 
    (cond ((null move) nil)
          ((and (< post-move-count pre-move-count) (not (null thwart)))
           move)
          (t
           (has-good-capture board (rest moves) color opponent-color)))))


#|/* *********************************************************************
Function Name: has-capture

Purpose: Checks if a move results in capturing opponent's pieces.

Parameters:
    - board: The current state of the game board.
    - moves: A list of possible moves for the current player.
    - cur-player-color: The color of the current player.
    - opponent-color: The color of the opponent player.

Return Value: A move that results in capturing opponent's pieces, or nil if no such move is found.

Algorithm:
    1. Iterate through the list of possible moves for the current player.
    2. For each move, update the game board with the move and count the number of opponent's pieces before and after the move.
    3. If the move results in a decrease in the number of opponent's pieces, return the move.
    4. Otherwise, continue iterating through the list of possible moves.
    5. If no move is found that results in capturing opponent's pieces, return nil.

Reference: None
********************************************************************* */|#
(defun has-capture (board moves cur-player-color opponent-color)
 (let* ( (move (first moves))
         (move-destination (cond ((null move) nil)
                                 (t (second move))))
         (new-board (cond ((null move) nil)
                          (t (update-board board move-destination cur-player-color))))
         (new-board (cond ((null new-board) nil)
                          (t (update-board new-board (first move) 'x))))
         (pre-move-count (count-pieces-on-board board opponent-color))
         (post-move-count (cond ((null new-board) nil)
                               (t (count-pieces-on-board new-board opponent-color))))) 
    (cond ((null move) nil)
          ((< post-move-count pre-move-count)
           move)
          (t
           (has-capture board (rest moves) cur-player-color opponent-color)))))


#|/* *********************************************************************
Function Name: has-thwart

Purpose: Checks if a move can thwart the opponent's potential winning move.

Parameters:
    - board: The current state of the game board.
    - moves: A list of possible moves for the current player.
    - color: The color of the current player.

Return Value: A move that can thwart the opponent's potential winning move, or nil if no such move is found.

Algorithm:
    1. Iterate through the list of possible moves for the current player.
    2. For each move, update the game board with the move and check if the opponent has a potential winning move on the updated board.
    3. If the opponent does not have a potential winning move on the updated board, return the move.
    4. Otherwise, continue iterating through the list of possible moves.
    5. If no move is found that can thwart the opponent's potential winning move, return nil.

Reference: None
********************************************************************* */|#
(defun has-thwart (board moves color)
  (let* ((move (first moves))
         (move-destination (cond ((null move) nil)
                                 (t (second move))))
         (new-board (cond ((null move) nil)
                          (t (update-board board move-destination color))))
         (new-board (cond ((null new-board) nil)
                          (t (update-board new-board (first move) 'x))))
         (opponent-color (cond ((eq color 'b) 'w)
                                (t 'b)))
         (new-opponent-moves (get-all-moves new-board opponent-color (get-origins new-board opponent-color)))
         (opponent-win (has-winning-move new-board new-opponent-moves opponent-color))) 
    (cond ((null move) nil)
          ((eq opponent-win nil)
           move)
          (t
           (has-thwart board (rest moves) color)))))


#|/* *********************************************************************
Function Name: has-winning-move

Purpose: Checks if a move can win the game for a player.

Parameters:
    - board: The current state of the game board.
    - moves: A list of possible moves for the current player.
    - color: The color of the current player.

Return Value: A move that will win player the game, or nil if no such move is found.

Algorithm:
    1. Iterate through the list of possible moves for the current player.
    2. For each move, update the game board with the move and count the number of groups of pieces of the player's color on the updated board.
    3. If the number of groups becomes 1 after the move, indicating a winning position, return the move.
    4. Otherwise, continue iterating through the list of possible moves.
    5. If no move is found that leads to a winning position, return nil.

Reference: None
********************************************************************* */|#
(defun has-winning-move (board moves color)
  (let* ((move (first moves))
         (move-destination (cond ((null move) nil)
                                 (t (second move))))
         (new-board (cond ((null move) nil)
                          (t (update-board board move-destination color))))
         (new-board (cond ((null new-board) nil)
                          (t (update-board new-board (first move) 'x)))))
    (cond ((null move) 
           nil)
          ((eq (count-groups new-board color) 1) 
           move) 
          (t
           (has-winning-move board (rest moves) color)))))


#|/* *********************************************************************
Function Name: compare-coordinates

Purpose: Compares two sets of coordinates to determine their order. Used with sort just for the demo since sort is destructive.

Parameters:
    - coord1: The first set of coordinates.
    - coord2: The second set of coordinates.

Return Value: True if coord1 should come before coord2 in the ordering, false otherwise.

Algorithm:
    1. Extract the row and column values from both sets of coordinates.
    2. Compare the column values. If the column value of coord1 is less than the column value of coord2, coord1 comes before coord2.
    3. If the column values are equal, compare the row values. If the row value of coord1 is greater than the row value of coord2, coord1 comes before coord2.
    4. If both column and row values are equal, compare the destination column values. If the destination column value of coord1 is less than the destination column value of coord2, coord1 comes before coord2.
    5. If all column, row, and destination column values are equal, compare the destination row values. If the destination row value of coord1 is greater than the destination row value of coord2, coord1 comes before coord2.
    6. Return true if coord1 should come before coord2 in the ordering, false otherwise.

Reference: None
********************************************************************* */|#
(defun compare-coordinates (coord1 coord2)
  (let* ((origin1 (first coord1))
         (origin2 (first coord2))
         (dest1 (second coord1))
         (dest2 (second coord2))
         (row1 (first origin1))
         (col1 (second origin1))
         (row2 (first origin2))
         (col2 (second origin2))
         (dest-row1 (first dest1))
         (dest-col1 (second dest1))
         (dest-row2 (first dest2))
         (dest-col2 (second dest2)))
    (or (< col1 col2)
        (and (= col1 col2)
             (> row1 row2))
        (and (= col1 col2)
             (= row1 row2)
             (< dest-col1 dest-col2))
        (and (= col1 col2)
             (= row1 row2)
             (= dest-col1 dest-col2)
             (> dest-row1 dest-row2)))))



#|/* *********************************************************************
Function Name: get-all-moves

Purpose: Retrieves all possible valid moves for a given player color from the current board state.

Parameters:
    - board: The current state of the game board.
    - color: The color of the player for whom to retrieve moves.
    - origins: A list of all possible starting positions (origins) from which moves can be made.
    - row: (Optional) The current row index during iteration (default value: 0).
    - col: (Optional) The current column index during iteration (default value: 0).
    - moves: (Optional) A list to accumulate the valid moves (default value: '()).

Return Value: A list containing all possible valid moves for the given player color.

Algorithm:
    1. Iterate over each origin position in the list of origins.
    2. For each origin position, determine the destination position based on the current row and column indices.
    3. Validate the move from the origin to the destination, considering the current board state and player color.
    4. If a valid move is found, add it to the list of moves.
    5. Continue iterating through rows and columns to exhaustively search for all valid moves.
    6. Return the list of accumulated valid moves.

Reference: None
********************************************************************* */|#
(defun get-all-moves (board color origins &optional (row 0) (col 0) (moves '()))
  (let* ((origin (first origins)) ; pull first origin
         (destination (cond ((not (null (nth col (nth row board)))) ; make destination, if invalid nil
                             (list row col))
                            (T
                             nil)))
         (move (cond ((or (null destination) (null origin))
                      nil)
                     (t
                      (validate-move board color (list origin destination))))))

    (cond ((null origin)
           moves) ;; base case, went through all origins

          ((= row 8) ;; is row is 8, call again with next origin
           (get-all-moves board color (rest origins) 0 0 moves))

          ((and (not (null move)) (<= col 7)) ; move found, continue iterating through cols
           (get-all-moves board color origins row (+ col 1) (cons move moves)))

          ((and (not (null move)) (<= row 7)) ; move found, continue iterating through rows, reset col
           (get-all-moves board color origins (+ row 1) 0 (cons move moves)))

          ((<= col 7) ; move not found, continue iterating through cols
           (get-all-moves board color origins row (+ col 1) moves))

          (t ; move not found, continue iterating through rows, reset col
           (get-all-moves board color origins (+ row 1) 0 moves)))))


#|/* *********************************************************************
Function Name: get-origins

Purpose: Retrieves all positions (origins) on the board where a player of the specified color has a piece.

Parameters:
    - board: The current state of the game board.
    - color: The color of the player for whom to retrieve origin positions.
    - row: (Optional) The current row index during iteration (default value: 0).
    - col: (Optional) The current column index during iteration (default value: 0).
    - origins: (Optional) A list to accumulate the origin positions (default value: '()).

Return Value: A list containing all positions (origins) on the board where a player of the specified color has a piece.

Algorithm:
    1. Iterate over each position on the board, represented by row and column indices.
    2. If a piece of the specified color is found at the current position, add the position to the list of origins.
    3. Continue iterating through rows and columns to search for all positions where the specified color has a piece.
    4. Return the list of accumulated origin positions.

Reference: None
********************************************************************* */|#
(defun get-origins (board color &optional (row 0) (col 0) (origins '()))
  (let ((piece (cond ((not (null (nth col (nth row board))))
                      (nth col (nth row board)))
                     (T
                      nil))))
    (cond 
      ((= row 8) origins) ; Base case: If row is 8, return origins
      ((and (eq piece color) (<= col 7))
       (get-origins board color row (1+ col) (cons (list row col) origins))) ; Add current position to origins
      ((and (eq piece color) (<= row 7))
       (get-origins board color (1+ row) 0 (cons (list row col) origins))) ; Add current position to origins
      ((<= col 7)
       (get-origins board color row (1+ col) origins)) ; Move to the next column
      (t
       (get-origins board color (1+ row) 0 origins)))))


#|/* *********************************************
Source Code for board logic/validation
********************************************* */|#


#|/* *********************************************************************
Function Name: is-vertical-horizontal-diagonal

Purpose: Checks if the move from the origin to the destination is vertical, horizontal, or diagonal.

Parameters:
    - board: The current state of the game board.
    - origin: The position (row, column) from which the move originates.
    - destination: The position (row, column) to which the move is intended.

Return Value: 
    - Returns true if the move is vertical, horizontal, or diagonal; otherwise, returns false.

Algorithm:
    1. Determine the row and column indices of the origin and destination positions.
    2. Calculate the absolute differences in rows and columns between the origin and destination.
    3. If the row difference is equal to the column difference or either difference is zero, the move is considered vertical, horizontal, or diagonal.
    4. Otherwise, the move is not considered vertical, horizontal, or diagonal.

Reference: None
********************************************************************* */|#
(defun is-vertical-horizontal-diagonal (board origin destination)
  (let* ((origin-row (nth 0 origin))
         (origin-col (nth 1 origin))
         (destination-row (nth 0 destination))
         (destination-col (nth 1 destination))
         (row-diff (abs (- destination-row origin-row)))
         (col-diff (abs (- destination-col origin-col)))) 
    (cond ((and (not (= row-diff col-diff)) 
                (not (= row-diff 0))
                (not (= col-diff 0)))
           nil)
          (T
           t) )))


#|/* *********************************************************************
Function Name: pieces-in-way

Purpose: Checks if there are any pieces in the path between the origin and destination positions.

Parameters:
    - board: The current state of the game board.
    - origin: The position (row, column) from which the move originates.
    - destination: The position (row, column) to which the move is intended.
    - cur-color: The color of the current player's pieces.

Return Value: 
    - Returns true if there are pieces in the path between the origin and destination; otherwise, returns false.

Algorithm:
    1. Determine the row and column indices of the origin and destination positions.
    2. Calculate the differences in rows and columns between the origin and destination.
    3. Determine the direction of movement (row-step and col-step) towards the destination.
    4. Move from the origin towards the destination, checking each position for pieces in the way.
    5. If a piece of the calling color or an empty space is encountered, continue moving.
    6. If a piece of the enemy player's color is encountered, return true indicating pieces are in the way.
    7. If the destination is reached without encountering pieces of the enemy player's color, return false.

Reference: None
********************************************************************* */|#
(defun pieces-in-way (board origin destination cur-color)
  (let* ((origin-row (nth 0 origin))
         (origin-col (nth 1 origin))
         (destination-row (nth 0 destination))
         (destination-col (nth 1 destination))
         (row-diff (- destination-row origin-row))
         (col-diff (- destination-col origin-col))
         (row-step (cond ((< row-diff 0) -1)
                         ((= row-diff 0) 0)
                         (t 1)))
         (col-step (cond ((< col-diff 0) -1)
                         ((= col-diff 0) 0)
                         (t 1)))
         (dest-x (+ destination-row (* row-step -1)))
         (dest-y (+ destination-col (* col-step -1))))
    (cond ((not (or (equal (nth origin-col (nth origin-row board)) 'X)
                    (equal (nth origin-col (nth origin-row board)) cur-color)))
           t); Piece in the way
           ((and (= origin-row dest-x)
                (= origin-col dest-y)) nil) ; No pieces in the way 
          (t (pieces-in-way board (list (+ origin-row row-step)
                                        (+ origin-col col-step))
                             destination cur-color))))) ; Recur moving origin closer to destination


#|/* *********************************************************************
Function Name: get-distance

Purpose: Calculates the distance (number of steps) between two positions on the game board.

Parameters:
    - origin: The position (row, column) from which the distance is measured.
    - destination: The position (row, column) to which the distance is measured.

Return Value: 
    - Returns the distance (number of steps) between the origin and destination positions.

Algorithm:
    1. Determine the row and column indices of the origin and destination positions.
    2. Calculate the absolute differences in rows and columns between the origin and destination.
    3. Determine the maximum difference between rows and columns.
    4. Return the maximum difference as the distance between the two positions.

Reference: None
********************************************************************* */|#
(defun get-distance (origin destination)
  (let* ((origin-row (nth 0 origin)) ; indices format: 7,0 (row, col)
         (destination-row (nth 0 destination))
         (origin-col (nth 1 origin))
         (destination-col (nth 1 destination))
         (row-diff (abs (- origin-row destination-row)))
         (col-diff (abs (- origin-col destination-col)))
         (distance (max row-diff col-diff)))
    distance))


#|/* *********************************************************************
Function Name: pieces-on-line

Purpose: Counts the number of pieces lying in a straight line between two positions on the game board.

Parameters:
    - board: The game board represented as a list of lists.
    - origin: The position (row, column) from which the line starts.
    - destination: The position (row, column) at which the line ends.

Return Value: 
    - Returns the number of pieces (including the origin and destination) lying in a straight line between the origin and destination positions.

Algorithm:
    1. Determine the row and column indices of the origin and destination positions.
    2. If the origin and destination lie on the same row, count the pieces in that row.
    3. If the origin and destination lie on the same column, count the pieces in that column.
    4. If the origin and destination lie on a diagonal, count the pieces on the diagonal line.
    5. Return the total count of pieces lying in a straight line between the origin and destination positions.

Reference: None
********************************************************************* */|#
(defun pieces-on-line (board origin destination)
  (let* ((origin-row (nth 0 origin))
         (destination-row (nth 0 destination))
         (origin-col (nth 1 origin))
         (destination-col (nth 1 destination)))
    (cond ((equal origin-row destination-row)
           (let ((row (nth origin-row board)))
              (count-line row 0)))
          ((equal origin-col destination-col)
           (let ((col (get-column board origin-col)))
              (count-line col 0)))
          (t 
            (check-diagonal board origin destination)))))


#|/* *********************************************************************
Function Name: count-line

Purpose: Counts the number of pieces present in a line on the game board.

Parameters:
    - line: The line on the game board to be counted, represented as a list.
    - num-pieces: The current count of pieces in the line.

Return Value: 
    - Returns the total count of pieces present in the given line.

Algorithm:

Reference: None
********************************************************************* */|#
(defun count-line (line num-pieces)
  (cond ((null line) num-pieces)
        ((not (equal (first line) 'X))
         (count-line (rest line) (+ num-pieces 1)))
        (t (count-line (rest line) num-pieces))))


#|/* *********************************************************************
Function Name: get-column

Purpose: Retrieves a specific column from the game board.

Parameters:
    - board: The game board represented as a list of lists.
    - col-index: The index of the column to retrieve.

Return Value: 
    - Returns a list containing the elements of the specified column from the game board.

Algorithm:

Reference: None
********************************************************************* */|#
(defun get-column (board col-index)
  (cond ((null board) nil) 
        (t
          (cons (nth col-index (first board))
          (get-column (rest board) col-index)))))


#|/* *********************************************************************
Function Name: check-diagonal

Purpose: Count pieces on the diagonal path between two points on the game board.

Parameters:
    - board: The game board represented as a list of lists.
    - origin: The starting point of the diagonal path.
    - destination: The ending point of the diagonal path.

Return Value: 
    - Returns true if there are no pieces in the diagonal path between the origin and destination points, false otherwise.

Algorithm:
    1. Determine the direction of movement along the rows (up or down) and columns (left or right) based on the positions of the origin and destination points.
    2. Find the starting point of the diagonal path by calling diagonal-origin.
    3. Call the check-diagonal-helper function with the board, diagonal origin, row step, and column step to traverse the diagonal path and count pieces.
    4. Return the result obtained from the check-diagonal-helper function.

Reference: None
********************************************************************* */|#
(defun check-diagonal (board origin destination)
  (let* ((row-step (cond ((< (first origin) (first destination)) 1)
                         (t -1)))
         (col-step (cond ((< (second origin) (second destination)) 1)
                         (t -1)))
         (diagonal-origin (get-diagonal-start board origin row-step col-step))
         )
    (check-diagonal-helper board diagonal-origin row-step col-step )))


#|/* *********************************************************************
Function Name: get-diagonal-start

Purpose: Finds the starting point of a diagonal line between two points on the game board.

Parameters:
    - board: The game board represented as a list of lists.
    - origin: The starting point of the diagonal path.
    - row-step: The direction of movement along the rows (up or down).
    - col-step: The direction of movement along the columns (left or right).

Return Value: 
    - Returns the starting point of the diagonal path.

Algorithm:
    1. Extract the row and column indices of the origin point.
    2. Determine the direction of movement along the rows and columns based on the given row-step and col-step.
    3. Traverse diagonally from the origin point until reaching the edge of the game board (boundary condition).
    4. Return the last valid position reached, which represents the starting point of the diagonal path.

Reference: None
********************************************************************* */|#
(defun get-diagonal-start (board origin row-step col-step)
  (let* ((x (first origin))
         (y (second origin))
         (dx (* row-step -1))
         (dy (* col-step -1)))
    (cond ((or (= x 0) (= x 7)
               (= y 0) (= y 7))
          origin)
          (t
           (get-diagonal-start board (list (+ x dx) (+ y dy)) row-step col-step)))))


#|/* *********************************************************************
Function Name: check-diagonal-helper

Purpose: Helper function to count the number of pieces on a diagonal path between two points on the game board.

Parameters:
    - board: The game board represented as a list of lists.
    - origin: The current position on the diagonal path.
    - row-step: The direction of movement along the rows (up or down).
    - col-step: The direction of movement along the columns (left or right).
    - num-pieces (optional): The count of pieces encountered on the diagonal path (default value: 0).
    - counter (optional): The counter to track the number of steps taken on the diagonal path (default value: 0).

Return Value: 
    - Returns the count of pieces encountered on the diagonal path.

Algorithm:
    1. Extract the row and column indices of the current position on the diagonal path.
    2. Retrieve the piece at the current position from the game board.
    3. If the piece is not valid (out of bounds or empty), return the count of pieces encountered 
          (have to first check for negative values of x/y here because nth only takes unsigned-byte).
    4. If the piece is not empty, increment the count of pieces and the counter.
    5. Recursively call the function with the next position on the diagonal path.
    6. Return the updated count of pieces encountered.

Reference: None
********************************************************************* */|#
(defun check-diagonal-helper (board origin row-step col-step  &optional (num-pieces 0) (counter 0))
  (let* ((x (first origin))
         (y (second origin))
         (piece (cond ((or (< x 0)
                           (< y 0)
                           (null (nth y (nth x board))))
                       nil)
                      (t
                       (nth y (nth x board))))))
    (cond ((null piece)
            num-pieces)
          ((not (eql piece 'X))
           (check-diagonal-helper board (list (+ x row-step) (+ y col-step)) row-step col-step  (1+ num-pieces) (+ counter 1)))
          (t 
           (check-diagonal-helper board (list (+ x row-step) (+ y col-step)) row-step col-step  num-pieces (+ counter 1))))))



#|*********************************************************************
Function Name: update-board

Purpose: Updates the game board with a new piece at the specified location.

Parameters:
    - board: The current game board represented as a list of lists.
    - location: The coordinates (row, column) where the new piece is to be placed.
    - cur-color: The color of the new piece ('b' for black, 'w' for white).

Return Value: 
    - Returns the updated game board with the new piece placed at the specified location.

Algorithm:
    1. Extract the row (x) and column (y) indices from the location.
    2. If the row index (x) is zero, update the first row of the board using the update-row function.
    3. Otherwise, recursively call the update-board function with the rest of the board and the updated location.
    4. Construct the updated board by replacing the first row with the updated row or appending the updated row to the rest of the board.

Reference: None
********************************************************************* |#
(defun update-board (board location cur-color)
  (let* ((x (nth 0 location))
         (y (nth 1 location)))
    (cond ((zerop x) ; Base case: If x is 0, update the first row
           (cons (update-row (first board) y cur-color) (rest board)))
          (t
           (cons (first board) (update-board (rest board) (list (- x 1) y) cur-color))))))


#|*********************************************************************
Function Name: update-row

Purpose: Updates a row in the game board with a new piece at the specified column.

Parameters:
    - row: The current row of the game board represented as a list.
    - y: The column index where the new piece is to be placed.
    - cur-color: The color of the new piece ('b' for black, 'w' for white).

Return Value: 
    - Returns the updated row with the new piece placed at the specified column.

Algorithm:

Reference: None
********************************************************************* |#
(defun update-row (row y cur-color)
  (cond ((zerop y) ; Base case: If y is 0, update the first element of the row
         (cons cur-color (rest row)))
        (t
         (cons (first row) (update-row (rest row) (- y 1) cur-color)))))


#|*********************************************************************
Function Name: print-piece-at-index

Purpose: Retrieves the piece at the specified indices on the game board. (shouldn't be called print piece)

Parameters:
    - board: The game board represented as a nested list.
    - index-list: A list containing the row and column indices of the desired piece.

Return Value: 
    - The piece found at the specified indices on the game board.

Algorithm:
    1. Destructure the index-list to obtain the row and column indices.
    2. Retrieve the piece at the specified indices from the game board.
    3. If no piece is found or the indices are invalid, print an error message.
    4. If the piece is 'X', return dot "." to represent an empty space.
    5. Otherwise, return the piece found at the specified indices.

Reference: chatGPT for destructuring bind, another website to check that it wasn't destructive
********************************************************************* |#
(defun print-piece-at-index (board index-list)
  (destructuring-bind (row-index col-index) index-list
  (let ((piece (nth col-index (nth row-index board))))
    (cond ((null piece)
             (format t "Invalid indices or no piece found at ~a~a~%" (code-char (+ (char-code #\A) col-index)) (- 8 row-index)))
          ((equal piece 'X)
            ".")   
          (t
             piece)))))


#|*********************************************************************
Function Name: get-move-user-input

Purpose: Prompts the user to enter the origin and destination locations for a move.

Parameters: None

Return Value: 
    - Returns a list containing the origin and destination locations entered by the user.

Algorithm:
    1. Display a prompt asking the user to enter the origin location in the format (e.g., A1).
    2. Get the origin location input from the user using the `get-location-user-input` function.
    3. Display a prompt asking the user to enter the destination location in the format (e.g., A1).
    4. Get the destination location input from the user using the `get-location-user-input` function.
    5. Return a list containing the origin and destination locations entered by the user.

Reference: None
********************************************************************* |#
(defun get-move-user-input ()
  (format t "Enter the origin location (e.g., A1): ")
  (terpri)
  (let ((origin (get-location-user-input))) ; Get origin location
    (format t "Enter the destination location (e.g., A1): ")
    (terpri)
    (let ((destination (get-location-user-input))) ; Get destination location
      (list origin destination)))) ; Return a list containing origin and destination


#|*********************************************************************
Function Name: get-location-user-input

Purpose: Gets valid user input for a board location on the board.

Parameters: None

Return Value: 
    - Returns a string representing a valid board location (e.g., "A1").

Algorithm:
    1. Read a line of input from the user.
    2. Check if the input is of length 2 and if both characters are within valid ranges ('A' to 'H' and '1' to '8').
    3. If the input is valid, return it.
    4. If the input is invalid, print an error message and ask the user to enter a valid location.
    5. Repeat steps 1-4 until a valid input is provided.

Reference: chatGPT for checking if an input was only certain chars
********************************************************************* |#
(defun get-location-user-input ()
  (let ((input (read-line))) ; Read a line of input from the user
    (cond ((and (= (length input) 2)
                (char<= #\A (char (string-upcase input) 0) #\H) ; Check if the first character is between A and H
                (char<= #\1 (char (string-upcase input) 1) #\8)) ; Check if the second character is between 1 and 8
           input) ; Return the valid location
          (t
           (format t "Invalid input. Please enter a valid location (e.g., A1).~%")
           (get-location-user-input))))) ; Ask again if the input is invalid


#|*********************************************************************
Function Name: get-score

Purpose: Calculates the score for a given player based on the pieces on the board.

Parameters:
    - board: A 2D list representing the current state of the board.
    - color: The color of the player ('black' or 'white') for whom the score is calculated.

Return Value: 
    - Returns an integer representing the score of the player.

Algorithm:

Reference: None
********************************************************************* |#
(defun get-score (board color)
  (cond ((eq color 'black)
         (- (count-pieces-on-board board 'b) (count-pieces-on-board board 'w)))
        (t
          (- (count-pieces-on-board board 'w) (count-pieces-on-board board 'b)) ) ))


#|*********************************************************************
Function Name: count-pieces-on-board

Purpose: Counts the number of pieces of a specified color on the board.

Parameters:
    - board: A 2D list representing the current state of the board.
    - color: The color of the pieces to count ('black' or 'white').
    - row: Optional parameter representing the current row being processed (default value is 0).
    - col: Optional parameter representing the current column being processed (default value is 0).
    - total-pieces: Optional parameter representing the total number of pieces counted so far (default value is 0).

Return Value: 
    - Returns an integer representing the total number of pieces of the specified color on the board.

Algorithm:
    1. Iterate over each cell in the board.
    2. If the cell contains a piece of the specified color, increment the total-pieces count.
    3. Recur with the next column if within bounds, or move to the next row if at the end of the row.
    4. Base case: If the row index is 8, return the total-pieces count.

Reference: None
********************************************************************* |#
(defun count-pieces-on-board (board color &optional (row 0) (col 0) (total-pieces 0))
  (let ((piece (cond ((not (null (nth col (nth row board)))) (nth col (nth row board)))
                     (T nil))))
    (cond 
      ((= row 8) total-pieces) ; Base case: If row is 8, return total-pieces
      ((and (eq piece color) (<= col 7))
       (count-pieces-on-board board color row (1+ col) (1+ total-pieces))) ; Move to the next column and increment total-pieces
      ((and (eq piece color) (<= row 7))
       (count-pieces-on-board board color (1+ row) 0 (1+ total-pieces))) ; Move to the next row and increment total-pieces
      ((<= col 7)
       (count-pieces-on-board board color row (1+ col) total-pieces)) ; Move to the next column
      (t
       (count-pieces-on-board board color (1+ row) 0 total-pieces))))) ; Move to the next row
  

#|*********************************************************************
Function Name: switch-players

Purpose: Switches the current player in the game state between human and computer.

Parameters:
    - game-state: A list representing the current state of the game, including the board, scores, and current player information.

Return Value: 
    - Returns a modified game state with the current player switched between human and computer.

Algorithm:

Reference: None
********************************************************************* |#
(defun switch-players (game-state)
  (let ((board (nth 0 game-state))
        (h-rounds-won (nth 1 game-state))
        (h-score (nth 2 game-state))
        (c-rounds-won (nth 3 game-state))
        (c-score (nth 4 game-state))
        (cur-player (nth 5 game-state))
        (cur-player-color (nth 6 game-state)))
    (cond ((and (eq cur-player 'human) (eq cur-player-color 'black))
           (list board h-rounds-won h-score c-rounds-won c-score 'computer 'white))
          ((and (eq cur-player 'human) (eq cur-player-color 'white))
           (list board h-rounds-won h-score c-rounds-won c-score 'computer 'black))
          ((and (eq cur-player 'computer) (eq cur-player-color 'black))
           (list board h-rounds-won h-score c-rounds-won c-score 'human 'white))
          (T
           (list board h-rounds-won h-score c-rounds-won c-score 'human 'black)))))


#|*********************************************************************
Function Name: is-win

Purpose: Determines if a player has won the game by checking if there is only one group of pieces left on the board for either black or white.

Parameters:
    - board: A list representing the current state of the board.

Return Value:
    - Returns 'b if there is only one group of black pieces left on the board.
    - Returns 'w if there is only one group of white pieces left on the board.
    - Returns nil if neither player has won the game.

Algorithm:

Reference: None
********************************************************************* |#
(defun is-win (board)
  (cond ((eq (count-groups board 'b) 1)
         'b)
         ((eq (count-groups board 'w) 1)
         'w) 
         (T
          nil)))

#|*********************************************************************
Function Name: count-groups

Purpose: Counts the number of groups of pieces of a given color on the board.

Parameters:
    - board: A list representing the current state of the board.
    - color: The color of the pieces to count the groups for ('b' for black, 'w' for white).
    - groups: An optional parameter representing the number of groups found so far (default is 0).
    - x: An optional parameter representing the current row index (default is 0).
    - y: An optional parameter representing the current column index (default is 0).
    - visited: An optional parameter representing the visited state of each cell on the board (default is a list of lists initialized to 'f' for false).

Return Value:
    - Returns the total number of groups of pieces of the specified color on the board.

Algorithm:
    1. Check if the current position (x, y) is outside the bounds of the board. If x = 8, return the total number of groups.
    2. Check if the current position has been visited. If not, proceed to check if the piece at that position matches the specified color.
    3. If the piece matches the color, increment the groups count and perform a flood-fill to mark all connected pieces as visited.
    4. Recursively call count-groups with updated parameters to continue counting groups.
    5. If the current position has been visited or the piece doesn't match the color, move to the next position.
    6. Recursively call count-groups with updated parameters to continue counting groups.

Reference: None
********************************************************************* |#
(defun count-groups (board color &optional (groups 0) (x 0) (y 0) (visited '((f f f f f f f f)(f f f f f f f f)(f f f f f f f f)(f f f f f f f f)(f f f f f f f f)(f f f f f f f f)(f f f f f f f f)(f f f f f f f f))))
  (let* ((is-visited (nth y (nth x visited)))
         (piece (nth y (nth x board))))
    (cond  ((= x 8) ;; base case, we have iterated through the board, return groups
            groups) 
           ((and (eq is-visited 'f) (eq piece color) (<= y 7)) ;; if location hasnt been visited, the piece matches the color, and we arent at the end of the row, increment groups, update board and call again
            (count-groups board color (+ groups 1) x (+ y 1) (flood-fill x y color board visited))
            )
           ((and (eq is-visited 'f) (eq piece color) (<= x 7)) ;; if location hasnt been visited, the piece matches the color, and we arent at the last row, inrement groups, update board and call again
           (count-groups board color (+ groups 1) (+ x 1) 0 (flood-fill x y color board visited)))
           ((<= y 7) ;; if location has been visited or color doesnt match but we arent at end of row, call again incrementing y
            (count-groups board color groups x (+ y 1) visited))
           (T
            (count-groups board color groups (+ x 1) 0 visited)) ; if location has been visited or color doesnt match but we arent at ultimate row, call again incrementing x
          )))



#|*********************************************************************
Function Name: flood-fill

Purpose: Performs a flood-fill algorithm to mark connected cells with a specific color as visited.

Parameters:
    - row: The current row index.
    - col: The current column index.
    - color: The color to check for and fill.
    - board: A list representing the current state of the board.
    - visited: A list of lists representing the visited state of each cell on the board.

Return Value:
    - Returns the updated visited matrix after performing flood-fill.

Algorithm:
    1. Check if the current position (row, col) is outside the bounds of the board or if the piece at that position doesn't match the specified color or if the position has already been visited. If any of these conditions are met, return the current visited matrix.
    2. Mark the current position as visited by updating the visited matrix.
    3. Recursively call flood-fill for all adjacent cells (up, down, left, right, and diagonals) passing the updated visited matrix.
    4. Continue the recursion until all connected cells of the specified color are visited.
    5. Return the final updated visited matrix.

Reference: chatGPT for explainations on how let worked, and how setf worked, what the difference was, which led to this threaded recursive call approach
********************************************************************* |#
(defun flood-fill (row col color board visited)
;;base case, any of the following conditions are met: 1. row/col out of bounds, 2. piece on board doesnt match color param, 3. space has been visited
  (cond ((or (< row 0) (> row 7) (< col 0) (> col 7))
         visited)
        ((or (not (eq (nth col (nth row board)) color))
             (eq (nth col (nth row visited)) 't))
         visited)
        (t
        ;; recursive case, mark space as visited, call itself for all adjacent cells, passing updated visited matrix
         (let ((new-visited (update-board visited (list row col) 't)))
           (flood-fill (- row 1) col color board
                       (flood-fill (+ row 1) col color board
                                   (flood-fill row (- col 1) color board
                                               (flood-fill row (+ col 1) color board
                                                           (flood-fill (- row 1) (+ col 1) color board
                                                                       (flood-fill (- row 1) (- col 1) color board
                                                                                   (flood-fill (+ row 1) (+ col 1) color board
                                                                                               (flood-fill (+ row 1) (- col 1) color board new-visited))))))))))))


#|*********************************************************************
Function Name: validate-move

Purpose: Validates a move based on several conditions including the color of the piece, the destination, the straightness of the move, the distance traveled, and the presence of enemy pieces on the path.

Parameters:
    - board: A list representing the current state of the board.
    - color: The color of the current player's pieces.
    - move: A list representing the move to be validated, containing origin and destination indices.

Return Value:
    - Returns either nil if the move is invalid or a list containing origin and destination indices if the move is valid.

Algorithm:
    1. Extract the origin and destination indices from the move.
    2. Get the pieces at the origin and destination positions on the board.
    3. Check if the origin piece matches the current player's color.
    4. Check if the destination position is either empty or contains an enemy piece.
    5. Check if the move is in a straight line (vertical, horizontal, or diagonal).
    6. Calculate the distance between the origin and destination positions.
    7. Count the number of pieces on the line of the move.
    8. Ensure that the distance traveled is equal to the number of pieces on the line.
    9. Check if there are any enemy pieces on the path from origin to destination.
    10. If all conditions are met, return a list containing origin and destination indices; otherwise, return nil.

Reference: None
********************************************************************* |#
(defun validate-move (board color move)
  (let* ((origin-indices (nth 0 move))
         (origin-piece (print-piece-at-index board origin-indices))
         (destination-indices (nth 1 move))
         (destination-piece (print-piece-at-index board destination-indices))
         (is-straight (is-vertical-horizontal-diagonal board origin-indices destination-indices))
         (distance (cond ((eq is-straight t)
                          (get-distance origin-indices destination-indices))
                         (T
                          nil)))
         (num-pieces-on-line (cond ((eq is-straight t)
                                    (pieces-on-line board origin-indices destination-indices))
                                   (T
                                    nil)) ))
         ;; origin must be same as cur-color
    (cond ((not (equal origin-piece color))
           nil)
           ;; destination must be empty or contain enemy piece
          ((equal destination-piece color)
           nil)

           ;;must move in straight line
          ((not (eq is-straight t))
           nil)

           ;; distance must be equal to pieces on line
          ((not (equal distance num-pieces-on-line))
           nil)
          
           
          ;; can't have enemy pieces on path
          ((equal (pieces-in-way board origin-indices destination-indices color) t)
           nil)

          (t
           (list origin-indices destination-indices)))))



#|/* *********************************************
Utility functions
********************************************* */|#


#|*********************************************************************
Function Name: notation-to-indices

Purpose: Converts board notation (e.g., "A1") to row and column indices suitable for accessing a board representation.

Parameters:
    - notation: A string representing a chess notation, such as "A1".

Return Value:
    - Returns a list containing row and column indices (0-based) corresponding to the given chess notation.

Algorithm:

Reference: None
********************************************************************* |#
(defun notation-to-indices (notation)
  (let ((col (char-upcase (char notation 0))) ; take first char of notation
        (row (parse-integer (string (char notation 1))))) ; take 2nd char of notation
    (list (- 8 row) ; Convert row char to 0-based index
          (- (char-code col) (char-code #\A))))) ; Convert column number to 0-based index


#|*********************************************************************
Function Name: indices-to-notation

Purpose: Converts row and column indices (0-based) to chess notation (e.g., "A1").

Parameters:
    - indices: A list containing row and column indices (0-based).

Return Value:
    - Returns a string representing chess notation corresponding to the given row and column indices.

Algorithm:

Reference: None
********************************************************************* |#
(defun indices-to-notation (indices)
  (let ((row (- 8 (first indices))) 
        (col (+ (second indices) (char-code #\A)))) 
    (format nil "~a~a" (code-char col) row))) 




;; call to start game
(begin)
