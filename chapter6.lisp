
;; =======================================
;;  6.3 
;; =======================================

;; (defun game-repl ()
;;   (loop (print (eval (read)))))
;;
;; CL-USER> (game-repl)
;; (look)
;;
;; (YOU ARE IN THE LIVING-ROOM.
;;  A WIZARD IS SNORING LOUDLY ON THE COUCH.
;;  THERE IS A DOOR GOING WEST FROM HERE.
;;  THERE IS A LADDER GOING UPSTAIRS FROM HERE.
;;  YOU SEE A WHISKEY ON THE FLOOR.
;;  YOU SEE A BUCKET ON THE FLOOR.) 

(defun game-read ()
  (let ((cmd (read-from-string
              (concatenate 'string "(" (read-line) ")"))))
    (labels ((quote-it (x)
               (list 'quote x)))
      (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

;; CL-USER> (game-read)
;; walk east
;; (WALK 'EAST)

(defparameter *allowed-commands* '(look walk pickup inventory))

(defun game-eval (sexp)
  (if (member (car sexp) *allowed-commands*)
      (eval sexp)
      '(i do not know that command.)))

;; CL-USER> (game-eval '(look))
;; (YOU ARE IN THE LIVING-ROOM.
;;  A WIZARD IS SNORING LOUDLY ON THE COUCH.
;;  THERE IS A DOOR GOING WEST FROM HERE.
;;  THERE IS A LADDER GOING UPSTAIRS FROM HERE.
;;  YOU SEE A WHISKEY ON THE FLOOR.
;;  YOU SEE A BUCKET ON THE FLOOR.)

(defun tweak-text (lst caps lit)
  (when lst
    (let ((item (car lst))
          (rest (cdr lst)))
      (cond ((eql item #\space) (cons item (tweak-text rest caps lit)))
            ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit)))
            ((eql item #\") (tweak-text rest caps (not lit)))
            (lit (cons item (tweak-text rest nil lit)))
            (caps (cons (char-upcase item) (tweak-text rest nil lit)))
            (t (cons (char-downcase item) (tweak-text rest nil nil)))))))

;; CL-USER> (tweak-text '(#\F #\O #\O #\Space #\B #\A #\R #\. #\Space #\B #\A #\Z #\Space #\" #\q #\u #\X #\") t nil)
;; (#\F #\o #\o #\Space #\b #\a #\r #\. #\Space #\B #\a #\z #\Space #\q #\u #\X)

(defun game-print (lst)
  (princ (coerce
          (tweak-text (coerce (string-trim "()" (prin1-to-string lst)) 'list) t nil)
          'string))
  (fresh-line))

;; CL-USER> (game-print '(not only does this sentence have a "comma," it also mentions the "iPad."))
;; Not only does this sentence have a comma, it also mentions the iPad.

(defun game-repl ()
  (let ((cmd (game-read)))
    (unless (eq (car cmd) 'quit)
      (game-print (game-eval cmd))
      (game-repl))))

;; CL-USER> (game-repl)
;; look
;; You are in the living-room. A wizard is snoring loudly on the couch. There is a door going west from here. There is a ladder going upstairs from here. You see a whiskey on the floor. You see a bucket on the floor.
;; walk west
;; You are in a beautiful garden. There is a well in front of you. There is a door going east from here. You see a frog on the floor. You see a chain on the floor.
;; quit

