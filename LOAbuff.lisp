

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

 (defun restore-game-state (path)
   (with-open-file (stream path :direction :input)
     (read stream)))

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


(defun save-game-state (game-state)
  (let ((save-file (get-save-file))) 
    (with-open-file (stream save-file :direction :output
                                      :if-exists :supersede)
      (format stream "~a" game-state)
      )))


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


(defun read-file (stream)
  (let ((line (read stream nil 'eof)))
    (cond ((eq line 'eof)
           nil)
          ((not (null line))
           line
           (read-file stream))) ))


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
         ;(print-board board) 
    (cond ((eq won nil) 
           (pause-game (switch-players new-game-state))
           (gameloop (switch-players new-game-state)))
          ((eq won cur-player-color)
           ;(princ "current player wins!")
           
           (play-again (score-round new-game-state)))
          (T
          ;; switch players, play again
           (play-again (score-round (switch-players new-game-state)))))))

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

(defun get-score (board color)
  (cond ((eq color 'black)
         (- (count-pieces-on-board board 'b) (count-pieces-on-board board 'w)))
        (t
          (- (count-pieces-on-board board 'w) (count-pieces-on-board board 'b)) ) ))

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



(defun is-win (board)
  (cond ((eq (count-groups board 'b) 1)
         'b)
         ((eq (count-groups board 'w) 1)
         'w) 
         (T
          nil)))

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
        ;  (princ distance)
        ;  (terpri)
        ;  (princ num-pieces-on-line)
        ;  (terpri)
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
           ;(princ "Move is valid")
           (list origin-indices destination-indices)))))


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
    ; (princ "strategy gamestate: ")
    ; (princ game-state)
    ; (terpri)
    ; (princ "opponent color: ")
    ; (princ capture)
    ; (terpri)
    (princ "Possible moves: ")
    (terpri)
    (print-moves (sort moves #'compare-coordinates))
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
        ;  (princ "dest row: ")
        ;  (princ destination-row)
        ;  (terpri)
        ;  (princ "penult: ")
        ;  (princ penultimate)
        ;  (terpri)
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

(defun has-stall (board moves color)
 (let* ( (move (first moves))
         (thwart (has-thwart board (list move) color)) )
    
    (cond ((null move) nil)
          ((not (null thwart))
           move)
          (t
           (has-stall board (rest moves) color)))))

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

(defun has-winning-move (board moves color)
  (let* ((move (first moves))
         (move-destination (cond ((null move) nil)
                                 (t (second move))))
         (new-board (cond ((null move) nil)
                          (t (update-board board move-destination color))))
         (new-board (cond ((null new-board) nil)
                          (t (update-board new-board (first move) 'x)))))
        ;(print-board new-board)
    (cond ((null move) 
           nil)
          ((eq (count-groups new-board color) 1) 
          ; (princ "new board")
          ; (terpri)
          ; (princ "count-groups: ")
          ; (princ (count-groups new-board color))
          ; (terpri)
          ; (print-board new-board)
           move) 
          (t
           (has-winning-move board (rest moves) color)))))

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




(defun get-all-moves (board color origins &optional (row 0) (col 0) (moves '()))
  ; (print (list 'board board 'color color 'origins origins 'row row 'col col 'moves moves)) ; Add this line
  ; (cond ((null origins)
  ;         moves))
  (let* ((origin (first origins)) ; pull first origin
         (destination (cond ((not (null (nth col (nth row board)))) ; make destination, if invalid nil
                             (list row col))
                            (T
                             nil)))
         (move (cond ((or (null destination) (null origin))
                      nil)
                     (t
                      (validate-move board color (list origin destination))))))
    ;(print (list 'origin origin 'destination destination 'move move)) ; Add this line

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
           (princ "distance: ")
           (princ distance)
           (terpri)
           (princ "pieces on line: ")
           (princ pieces-on-line)
           (terpri)
           (princ "not valid, distance != pieces on line")
           (terpri)
           (validate-user-move board color (get-move-user-input)))
           
           
          ;; can't have enemy pieces on path
          ((equal (pieces-in-way board origin-indices destination-indices color) t)
           (princ "not valid, pieces in way")
           (terpri)
           (validate-user-move board color (get-move-user-input)))

          (t
           ;(princ "Move is valid")
           (list origin-indices destination-indices)))))

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


(defun get-distance (origin destination)
  (let* ((origin-row (nth 0 origin)) ; indices format: 7,0 (row, col)
         (destination-row (nth 0 destination))
         (origin-col (nth 1 origin))
         (destination-col (nth 1 destination))
         (row-diff (abs (- origin-row destination-row)))
         (col-diff (abs (- origin-col destination-col)))
         (distance (max row-diff col-diff)))
    distance))

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

(defun count-line (line num-pieces)
  (cond ((null line) num-pieces)
        ((not (equal (first line) 'X))
         (count-line (rest line) (+ num-pieces 1)))
        (t (count-line (rest line) num-pieces))))

(defun get-column (board col-index)
  (cond ((null board) nil) 
        (t
          (cons (nth col-index (first board))
          (get-column (rest board) col-index)))))

(defun check-diagonal (board origin destination)
  (let* ((row-step (cond ((< (first origin) (first destination)) 1)
                         (t -1)))
         (col-step (cond ((< (second origin) (second destination)) 1)
                         (t -1)))
         (diagonal-origin (get-diagonal-start board origin row-step col-step))
         (max-steps (max (first diagonal-origin) (second diagonal-origin))))
        ;  (princ row-step)
        ;  (terpri)
        ;  (princ col-step)
        ;  (terpri)
        ;  (princ diagonal-origin)
        ;  (terpri)
        ;  (princ max-steps)
        ;  (terpri)
    (check-diagonal-helper board diagonal-origin row-step col-step max-steps)))


(defun get-diagonal-start (board origin row-step col-step)
  (let* ((x (first origin))
         (y (second origin))
         (dx (* row-step -1))
         (dy (* col-step -1)))
    (cond ((or (= x 0) (= x 7)
               (= y 0) (= y 7))
          ;(princ origin)
          origin)
          (t
           (get-diagonal-start board (list (+ x dx) (+ y dy)) row-step col-step)))))


(defun check-diagonal-helper (board origin row-step col-step max-steps &optional (num-pieces 0) (counter 0))
  (let* ((x (first origin))
         (y (second origin))
         (piece (cond ((or (< x 0)
                           (< y 0)
                           (null (nth y (nth x board))))
                       nil)
                      (t
                       (nth y (nth x board))))))
    ; (princ "piece:")
    ; (princ piece)
    ; (terpri)
    (cond ((null piece)
            num-pieces)
          ((not (eql piece 'X))
           (check-diagonal-helper board (list (+ x row-step) (+ y col-step)) row-step col-step max-steps (1+ num-pieces) (+ counter 1)))
          (t 
           (check-diagonal-helper board (list (+ x row-step) (+ y col-step)) row-step col-step max-steps num-pieces (+ counter 1))))))

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
    (cond ((eq cur-player 'human) ;;TODO: change this to only human
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
                    ;(format t "Declined help")
                    )))
           (format t "Enter move: ") ;; remove this line?
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
            ;(print-board board)
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

(defun update-board (board location cur-color)
  (let* ((x (nth 0 location))
         (y (nth 1 location)))
    (cond ((zerop x) ; Base case: If x is 0, update the first row
           (cons (update-row (first board) y cur-color) (rest board)))
          (t
           (cons (first board) (update-board (rest board) (list (- x 1) y) cur-color))))))

(defun update-row (row y cur-color)
  (cond ((zerop y) ; Base case: If y is 0, update the first element of the row
         (cons cur-color (rest row)))
        (t
         (cons (first row) (update-row (rest row) (- y 1) cur-color)))))



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

(defun print-row (row)
    (cond ((null row)
            nil)
            ((equal (first row) 'X)
             (format t ". ")
             (print-row (rest row)))
            (t
            (format t "~a " (first row))
            (print-row (rest row)))))


(defun print-piece-at-index (board index-list)
  (destructuring-bind (row-index col-index) index-list
  (let ((piece (nth col-index (nth row-index board))))
    (cond ((null piece)
             (format t "Invalid indices or no piece found at ~a~a~%" (code-char (+ (char-code #\A) col-index)) (- 8 row-index))) ; complains at char because derived type conflicts with asserted type
          ((equal piece 'X)
            ;(format t "Piece at ~a~a: .~%" (code-char (+ (char-code #\A) col-index)) (- 8 row-index))
            ".")   
          (t
             ;(format t "Piece at ~a~a: ~a~%" (code-char (+ (char-code #\A) col-index)) (- 8 row-index) piece)
             piece)))))


; (defun get-location-user-input ()
;   (format t "Enter the location (e.g., A1): ")
;   (terpri)
;   (let ((input (read-line))) ; Read a line of input from the user
;     (cond ((and (= (length input) 2)
;                 (char<= #\A (char (string-upcase input) 0) #\H) ; Check if the first character is between A and H
;                 (char<= #\1 (char (string-upcase input) 1) #\8)) ; Check if the second character is between 1 and 8
;            input) ; Return the valid origin
;           (t
;            (format t "Invalid input. Please enter a valid location (e.g., A1).~%")
;            (get-location-user-input))))) ; Ask again if the input is invalid

(defun get-move-user-input ()
  (format t "Enter the origin location (e.g., A1): ")
  (terpri)
  (let ((origin (get-location-user-input))) ; Get origin location
    (format t "Enter the destination location (e.g., A1): ")
    (terpri)
    (let ((destination (get-location-user-input))) ; Get destination location
      (list origin destination)))) ; Return a list containing origin and destination

(defun get-location-user-input ()
  (let ((input (read-line))) ; Read a line of input from the user
    (cond ((and (= (length input) 2)
                (char<= #\A (char (string-upcase input) 0) #\H) ; Check if the first character is between A and H
                (char<= #\1 (char (string-upcase input) 1) #\8)) ; Check if the second character is between 1 and 8
           input) ; Return the valid location
          (t
           (format t "Invalid input. Please enter a valid location (e.g., A1).~%")
           (get-location-user-input))))) ; Ask again if the input is invalid


(defun notation-to-indices (notation)
  (let ((col (char-upcase (char notation 0))) ; take first char of notation
        (row (parse-integer (string (char notation 1))))) ; take 2nd char of notation
    ;(format t "Row: ~a, Col: ~a~%" (- 8 row) (- (char-code col) (char-code #\A))) ; check by 
    (list (- 8 row) ; Convert row char to 0-based index
          (- (char-code col) (char-code #\A))))) ; Convert column number to 0-based index

(defun indices-to-notation (indices)
  (let ((row (- 8 (first indices))) ; Convert back to 1-based index
        (col (+ (second indices) (char-code #\A)))) ; Convert back to ASCII character code
    ;(format t "Notation: ~a~a~%" (code-char col) row) ; Check by printing the notation
    (format nil "~a~a" (code-char col) row))) ; Return the notation


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

;(princ (round-start 2 0 0 0))

; (let ((board '(
;         ( b b x x x x x x )
;         ( x b x x w w w x )
;         ( x x x x x w x x )
;         ( b x x w x w x x )
;         ( x x x x x w x x )
;         ( x x x x x w x x )
;         ( x x x x x w x x )
;         ( x x x x x x x b )
;    )
; ))

; )

; (defvar board '(
;         ( b b x x x x x x )
;         ( x b x x w w w x )
;         ( x x x x x w x x )
;         ( b x x w x w x x )
;         ( x x x x x w x x )
;         ( x x x x x w x x )
;         ( x x x x x w x x )
;         ( x x x x x x x b )
;    )
; )

(defvar board '(
                  ( b b x x x x x x ) ;; capture board
                  ( x b x x w w x x )
                  ( x x x x x w w x )
                  ( x b x w x x x x )
                  ( x x x x x w x x )
                  ( x x x x x w x x )
                  ( w x x x x w x x )
                  ( b x x x x x x x )
   )
)
(defvar game-state '(
   ; Board
   (
      ( x x x x x x x x)
      ( w x x x x x x x)
      ( b b b x x x x x)
      ( b x w x x b x x)
      ( w x x b b x x x)
      ( x b w b x w w x)
      ( x x x x x x x w)
      ( x x x x x x x x)
   )

   ; Human:
   0 0 

   ; Computer
   1 -2

   ; Next player
   Computer White
))

;(princ (strategy game-state))
; (princ game-state)
; (princ (count-pieces-on-board board 'black))
;(princ (count-groups board 'black))
;(is-vertical-horizontal-diagonal board '(7 0) '(5 3))
;(print-board board)
;(princ (get-all-moves board 'b '((1 1))))
;(validate-user-move board 'b (get-move-user-input))
; (terpri)
;(princ (get-diagonal-start board '(1 1) -1 -1))
;(princ (validate-move board 'b '((0 1) (1 2))))



;(princ (get-diagonal-start board '(7 7) '(0 0)))
;(princ (check-diagonal board '(7 7) '(0 0)))

;(princ (has-winning-move board (get-all-moves board 'b (get-origins board 'b)) 'b))
;(princ (has-capture board (get-all-moves board 'b (get-origins board 'b)) 'b 'w))
;(princ (has-thwart board (get-all-moves board 'b (get-origins board 'b)) 'b))
;(princ (has-delay board (get-all-moves board 'b (get-origins board 'b)) 'b 'w))
;(princ (has-condense board (get-all-moves board 'b (get-origins board 'b)) 'b))
;(princ (has-connect board (get-all-moves board 'b (get-origins board 'b)) 'b))
;(princ (has-good-capture board (get-all-moves board 'b (get-origins board 'b)) 'b 'w))
;(princ (has-stall board (get-all-moves board 'b (get-origins board 'b)) 'b));


;(terpri)
;  (terpri)
;  (princ (get-distance '(7 0) '(4 3)))
;  (terpri)
; (princ (pieces-on-line board '(7 0) '(4 3)))
; (princ (pieces-in-way board '(0 0) '(2 5) 'b))
; (terpri)
; (princ (get-distance '(0 0) '(2 5)))
;(print-board board)
;(princ (count-groups board 'b))


;(princ (indices-to-notation '(7 7)))
;(get-all-moves board cur-player-color (get-origins board cur-player-color))
;(print-moves (get-all-moves board 'b (get-all-moves board 'b (get-origins board 'b))))
;(princ (count-groups board 'b))
(begin)

;(princ (restore-game-state (get-file-path)))

;(coin-toss (read))
;(validate-move board 'B)

;(print-piece-at-index board (notation-to-indices(read-origin)))
