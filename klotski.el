;; -*- lexical-binding: t -*-
;;; klotski.el --- Klotski (Hua Rong Dao) Game

;; Author: Huang Ying <huang.ying.caritas@gmail.com>
;; Created: 5 June 2015
;; Keywords: game klotski

;; Copyright (C) 2015 Huang Ying <huang.ying.caritas@gmail.com>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

(require 'cl-lib)

(cl-defun dcons (car cdr cer)
  (vector car cdr cer))

(cl-defun dcar (dcons)
  (and dcons (aref dcons 0)))

(cl-defun dcdr (dcons)
  (and dcons (aref dcons 1)))

(cl-defun dcpr (dcons)
  (and dcons (aref dcons 2)))

(cl-defun set-dcar (dcons value)
  (setf (aref dcons 0) value))
(gv-define-simple-setter dcar set-dcar)

(cl-defun set-dcdr (dcons value)
  (setf (aref dcons 1) value))
(gv-define-simple-setter dcdr set-dcdr)

(cl-defun set-dcpr (dcons value)
  (setf (aref dcons 2) value))
(gv-define-simple-setter dcpr set-dcpr)

(cl-defun dlist-push-head (value dlist)
  (let ((ndcons (dcons value dlist nil)))
    (when dlist
      (setf (dcpr dlist) ndcons))
    ndcons))

(cl-defmacro dlist-push-headf (value dlist)
  (gv-letplace (getter setter) dlist
    (funcall setter `(dlist-push-head ,value ,getter))))

(cl-defun dlist-push-tail (value dlist)
  (let ((ndcons (dcons value nil dlist)))
    (when dlist
      (setf (dcdr dlist) ndcons))
    ndcons))

(cl-defmacro dlist-push-tailf (value dlist)
  (gv-letplace (getter setter) dlist
    (funcall setter `(dlist-push-tail ,value ,getter))))

(cl-defun dlist-pop-head (dlist)
  (if dlist
      (let ((next (dcdr dlist)))
	(if next
	    (setf (dcpr next) nil))
	(list (dcar dlist) next))
    (list nil nil)))

(cl-defmacro dlist-pop-headf (dlist)
  (gv-letplace (getter setter) dlist
    (let ((value-var (cl-gensym))
	  (dlist-var (cl-gensym)))
      `(cl-multiple-value-bind (,value-var ,dlist-var)
	   (dlist-pop-head ,getter)
	 ,(funcall setter dlist-var)
	 ,value-var))))

(cl-defun dlist-pop-tail (dlist)
  (if dlist
      (let ((prev (dcpr dlist)))
	(if prev
	    (setf (dcdr prev) nil))
	(list (dcar dlist) prev))
    (list nil nil)))

(cl-defmacro dlist-pop-tailf (dlist)
  (gv-letplace (getter setter) dlist
    (let ((value-var (cl-gensym))
	  (dlist-var (cl-gensym)))
      `(cl-multiple-value-bind (,value-var ,dlist-var)
	   (dlist-pop-tail ,getter)
	 ,(funcall setter dlist-var)
	 ,value-var))))

(cl-defun dlist-head (dlist)
  (and dlist
       (let ((prev (dcpr dlist)))
	 (while prev
	   (setf dlist prev)
	   (setf prev (dcpr dlist)))
	 dlist)))

(cl-defun dlist-tail (dlist)
  (and dlist
       (let ((next (dcdr dlist)))
	 (while next
	   (setf dlist next)
	   (setf next (dcdr dlist)))
	 dlist)))

(cl-defmacro dlist-nextf (dlist)
  (gv-letplace (getter setter) dlist
    (funcall setter `(dcdr ,getter))))

(cl-defmacro dlist-prevf (dlist)
  (gv-letplace (getter setter) dlist
    (funcall setter `(dcpr ,getter))))

(cl-defun dlist-lastp (dlist)
  (and dlist (null (dcdr dlist))))

(cl-defmacro do-dlist ((var dlist) &body body)
  (let ((dlist-var (cl-gensym)))
    `(let (,var
	   (,dlist-var ,dlist))
      (while ,dlist-var
	(setf ,var (dcar ,dlist-var))
	,@body
	(setf ,dlist-var (dcdr ,dlist-var))))))

(cl-defun list->dlist (list)
  (if (null list)
      nil
    (let* ((dlist (dlist-push-tail (car list) nil))
	   (dpos dlist))
      (dolist (val (cdr list))
	(dlist-push-tailf val dpos))
      dlist)))

(cl-defun dlist->list (dlist)
  (let (list)
    (do-dlist (val dlist)
      (push val list))
    (nreverse list)))

(defconst +klotski-rows+ 5)
(defconst +klotski-cols+ 4)

(defconst +klotski-caocao-char+ ?c)
(defconst +klotski-exit-pos-list+ '((4 1) (4 2)))

(defconst +klotski-buffer-name+ "*klotski*")

(defconst +klotski-setups+
  `(((,+klotski-caocao-char+ (0 1) (0 2) (1 1) (1 2))
     (?g (2 1) (2 2))
     (?z (0 0) (1 0))
     (?a (0 3) (1 3))
     (?m (2 0) (3 0))
     (?h (2 3) (3 3))
     (?1 (4 0))
     (?2 (3 1))
     (?3 (3 2))
     (?4 (4 3)))
    ((,+klotski-caocao-char+ (0 1) (0 2) (1 1) (1 2))
     (?g (2 0) (2 1))
     (?z (0 0) (1 0))
     (?a (3 1) (3 2))
     (?m (3 0) (4 0))
     (?h (3 3) (4 3))
     (?1 (0 3))
     (?2 (1 3))
     (?3 (2 2))
     (?4 (2 3)))
    ((,+klotski-caocao-char+ (0 1) (0 2) (1 1) (1 2))
     (?g (2 1) (2 2))
     (?z (0 0) (1 0))
     (?a (0 3) (1 3))
     (?m (3 1) (3 2))
     (?h (4 1) (4 2))
     (?1 (2 0))
     (?2 (3 0))
     (?3 (2 3))
     (?4 (3 3)))))

(defvar *klotski-setup* (first +klotski-setups+))

(defvar *klotski-empty-cell-string*)

(defvar *klotski-board* (make-vector (* +klotski-cols+ +klotski-rows+) " "))
(defvar *klotski-actors*)
(defvar *klotski-current-actor*)

(defvar *klotski-steps*)
(defvar *klotski-steps-count*)

(defvar *klotski-replay-timer* nil)

(define-derived-mode klotski-mode special-mode "KLOTSKI"
  "Klotski (Hua Rong Dao) Game"
  (read-only-mode)
  (set-buffer-modified-p nil)
  (klotski-define-keys-for-move klotski-mode-map)
  (let ((str " "))
    (put-text-property 0 1 'face 'klotski-normal-actor-face str)
    (setf *klotski-empty-cell-string* str))
  (define-key klotski-mode-map (kbd "C-r") #'klotski-reset)
  (define-key klotski-mode-map (kbd "C-_") #'klotski-undo)
  (define-key klotski-mode-map (kbd "C-+") #'klotski-redo)
  (define-key klotski-mode-map (kbd "C-o") #'klotski-show-steps)
  (define-key klotski-mode-map (kbd "C-t") #'klotski-replay)
  (define-key klotski-mode-map (kbd "C-s") #'klotski-setup))

(define-derived-mode klotski-replay-mode special-mode "KLOTSKI-REPLAY"
  "Klotski (Hua Rong Dao) Game Replay"
  (read-only-mode)
  (set-buffer-modified-p nil)
  (define-key klotski-replay-mode-map (kbd "p") #'klotski-replay-toggle-pause)
  (define-key klotski-replay-mode-map (kbd "q") #'klotski-replay-stop)
  (define-key klotski-replay-mode-map (kbd "C-_") #'klotski-undo)
  (define-key klotski-replay-mode-map (kbd "C-+") #'klotski-redo))

(define-derived-mode klotski-setup-mode special-mode "KLOTSKI-SETUP"
  "Klotski (Hua Rong Dao) Game Setup"
  (read-only-mode)
  (set-buffer-modified-p nil)
  (define-key klotski-setup-mode-map (kbd "q") #'klotski-cancel-setup))

;;;###autoload
(cl-defun klotski-game ()
  (interactive)
  (let ((buffer (get-buffer-create +klotski-buffer-name+)))
    (switch-to-buffer buffer)
    (klotski-mode)
    (klotski-reset)))

(defsubst klotski-index (row col)
  (+ (* +klotski-cols+ row) col))
(defsubst klotski-get-cell (row col)
  (aref *klotski-board* (klotski-index row col)))
(defsubst klotski-set-cell (row col val)
  (setf (aref *klotski-board* (klotski-index row col)) val))
(defsubst klotski-empty-cell-p (row col)
  (null (first (klotski-get-cell row col))))

(defsubst klotski-actor-char (actor)
  (car actor))
(defsubst klotski-actor-pos-list (actor)
  (cdr actor))

(cl-defun klotski-erase-board ()
  (dotimes (row +klotski-rows+)
    (dotimes (col +klotski-cols+)
      (klotski-set-cell row col (list nil nil)))))

(cl-defun klotski-put-actor (actor)
  (cl-loop
   for pos in (klotski-actor-pos-list actor)
   for n upfrom 0
   do (klotski-set-cell (first pos) (second pos)
			(list actor n))))

(defsubst klotski-put-current-actor ()
  (klotski-put-actor *klotski-current-actor*))

(cl-defun klotski-put-normal-actors ()
  (dolist (actor *klotski-actors*)
    (unless (eq actor *klotski-current-actor*)
      (klotski-put-actor actor))))

(cl-defun klotski-set-current-actor (actor)
  (setf *klotski-current-actor* actor)
  (klotski-refresh))

(cl-defun klotski-define-keys-for-actors (map)
  (dolist (kactor *klotski-actors*)
    (let ((actor kactor))
     (define-key map (kbd (make-string 1 (klotski-actor-char actor)))
       (lambda ()
	 (interactive)
	 (klotski-set-current-actor actor))))))

(cl-defun klotski-actor-check-collision (actor)
  (dolist (pos (klotski-actor-pos-list actor))
    (when (or (< (first pos) 0)
	      (>= (first pos) +klotski-rows+)
	      (< (second pos) 0)
	      (>= (second pos) +klotski-cols+)
	      (not (apply #'klotski-empty-cell-p pos)))
      (cl-return-from klotski-actor-check-collision t))))

(cl-defun klotski-actor-move (actor dir)
  (dolist (pos (klotski-actor-pos-list actor))
    (cl-case dir
      ('up (decf (first pos)))
      ('down (incf (first pos)))
      ('left (decf (second pos)))
      ('right (incf (second pos))))))

(defsubst klotski-current-actor-move (dir)
  (klotski-actor-move *klotski-current-actor* dir))

(cl-defun klotski-current-actor-try-move (dir)
  (let ((actor (copy-tree *klotski-current-actor*)))
    (klotski-actor-move actor dir)
    actor))

(cl-defun klotski-move (dir &optional record)
  (klotski-erase-board)
  (klotski-put-normal-actors)
  (let ((actor (klotski-current-actor-try-move dir)))
    (unless (klotski-actor-check-collision actor)
      (klotski-current-actor-move dir)
      (when record
       (klotski-steps-push (cons *klotski-current-actor* dir))))
    (klotski-put-current-actor)
    (klotski-print-game)))

(cl-defun klotski-define-keys-for-move (map)
  (cl-loop
   for adir in '(up down left right)
   for dir-key in '("<up>" "<down>" "<left>" "<right>")
   do (let ((dir adir))
	(define-key map (kbd dir-key)
	  (lambda ()
	    (interactive)
	    (klotski-move dir t))))))

(cl-defun klotski-steps-reset ()
  (setf *klotski-steps* (dcons nil nil nil)
	*klotski-steps-count* 0))

(cl-defun klotski-steps-push (step)
  (dlist-push-tailf step *klotski-steps*)
  (incf *klotski-steps-count*))

(cl-defun klotski-steps-refill (steps)
  (klotski-steps-reset)
  (let ((ksteps *klotski-steps*))
    (dolist (step steps)
      (dlist-push-tailf step ksteps))))

(cl-defun klotski-steps-go-backward ()
  (let ((step (dcar *klotski-steps*)))
    (when step
      (setf *klotski-steps* (dcpr *klotski-steps*))
      (decf *klotski-steps-count*))
    step))

(cl-defun klotski-steps-go-forward ()
  (let ((next (dcdr *klotski-steps*)))
    (and next
	 (progn
	   (setf *klotski-steps* next)
	   (incf *klotski-steps-count*)
	   (dcar next)))))

(cl-defmacro klotski-steps-do-step (var &body body)
  `(do-dlist (,var (dcdr (dlist-head *klotski-steps*)))
	     ,@body))

(cl-defun klotski-steps-at-lastp ()
  (dlist-lastp *klotski-steps*))

(cl-defun klotski-reverse-dir (dir)
  (cl-case dir
    ('up 'down)
    ('down 'up)
    ('left 'right)
    ('right 'left)))

(cl-defun klotski-undo ()
  (interactive)
  (with-current-buffer +klotski-buffer-name+
    (let ((step (klotski-steps-go-backward)))
      (if step
	  (progn
	    (klotski-set-current-actor (car step))
	    (klotski-move (klotski-reverse-dir (cdr step))))
	(message "Already at begin of history!")))))

(cl-defun klotski-redo ()
  (interactive)
  (with-current-buffer +klotski-buffer-name+
    (let ((step (klotski-steps-go-forward)))
     (if step
	 (progn
	   (klotski-set-current-actor (car step))
	   (klotski-move (cdr step)))
       (message "Already at end of history!")))))

(cl-defun klotski-show-steps ()
  (interactive)
  (let ((buffer (get-buffer-create "*klotski steps*"))
	(first-line t))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "(")
      (klotski-steps-do-step step
	(if first-line
	    (setf first-line nil)
	  (insert "\n "))
	(insert (format "(?%c . %s)" (klotski-actor-char (car step))
			(cdr step))))
      (insert ")\n"))
    (display-buffer buffer)))

(cl-defun klotski-replay-check-step ()
  (let ((buffer (get-buffer +klotski-buffer-name+)))
    (if (and buffer (not (klotski-steps-at-lastp)))
	(klotski-redo)
      (klotski-replay-stop))))

(cl-defun klotski-replay-setup-timer ()
  (setf *klotski-replay-timer*
	(run-at-time nil 0.5 #'klotski-replay-check-step)))

(cl-defun klotski-replay-stop-timer ()
  (when *klotski-replay-timer*
    (cancel-timer *klotski-replay-timer*)
    (setf *klotski-replay-timer* nil)))

(cl-defun klotski-replay-toggle-pause ()
  (interactive)
  (with-current-buffer +klotski-buffer-name+
    (when (eq major-mode 'klotski-replay-mode)
      (if *klotski-replay-timer*
	  (klotski-replay-stop-timer)
	(klotski-replay-setup-timer)))))

(cl-defun klotski-replay-stop ()
  (interactive)
  (with-current-buffer +klotski-buffer-name+
    (klotski-replay-stop-timer)
    (klotski-mode)
    (klotski-refresh)))

(cl-defun klotski-replay-check-steps (replay-steps)
  (when (not (listp replay-steps))
    (message "Invalid replay steps: not a list!")
    (cl-return-from klotski-replay-check-steps))
  (dolist (replay-step replay-steps t)
    (unless (consp replay-step)
      (message "Invalid replay steps: item is not a list: %s"
	       (prin1 replay-step))
      (cl-return-from klotski-replay-check-steps))
    (let* ((actor-char (car replay-step))
	   (actor (assoc actor-char *klotski-actors*)))
      (unless actor
	(message "Invalid replay steps: invalid actor char: %c" actor-char)
	(cl-return-from klotski-replay-check-steps)))))

(cl-defun klotski-replay-convert-steps (replay-steps)
  (let (steps)
    (dolist (replay-step replay-steps)
      (let* ((actor-char (car replay-step))
	     (actor (assoc actor-char *klotski-actors*)))
	(push (cons actor (cdr replay-step)) steps)))
    (nreverse steps)))

(cl-defun klotski-replay (file)
  (interactive "fFile to replay: ")
  (let ((replay-steps (with-temp-buffer
			(insert-file-contents file)
			(read (current-buffer)))))
    (when (klotski-replay-check-steps replay-steps)
      (with-current-buffer +klotski-buffer-name+
	(klotski-reset)
	(klotski-replay-mode)
	(klotski-steps-refill (klotski-replay-convert-steps replay-steps))
	(klotski-replay-setup-timer)))))

(defface klotski-current-actor-face
  '((default :foreground "red" :height 2.0))
  "Face for current klotski actor"
  :group 'klotski-faces)

(defface klotski-normal-actor-face
  '((default :height 2.0))
  "Face for normal actor"
  :group 'klotski-faces)

(cl-defun klotski-check-success ()
  (let* ((caocao (assoc +klotski-caocao-char+ *klotski-actors*))
	 (caocao-pos-list (klotski-actor-pos-list caocao)))
    (dolist (pos +klotski-exit-pos-list+ t)
      (unless (member pos caocao-pos-list)
	(cl-return-from klotski-check-success nil)))))

(cl-defun klotski-print-cell (row col)
  (cl-multiple-value-bind (actor npos)
      (klotski-get-cell row col)
    npos
    (insert
     (let ((str (and actor (make-string 1 (klotski-actor-char actor)))))
       (cond
	((null actor) *klotski-empty-cell-string*)
	((eq actor *klotski-current-actor*)
	 (put-text-property 0 1 'face 'klotski-current-actor-face str)
	 str)
	(t (put-text-property 0 1 'face 'klotski-normal-actor-face str)
	   str))))))

(cl-defun klotski-print-board ()
  (dotimes (row +klotski-rows+)
    (insert "  ")
    (dotimes (col +klotski-cols+)
      (klotski-print-cell row col))
    (insert "\n")))

(cl-defun klotski-print-game ()
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert "  Klotski (Hua Rong Dao) Game")
    (if (eq major-mode 'klotski-replay-mode)
	(insert ": Replay"))
    (insert "\n\n")
    (klotski-print-board)
    (insert (format "\nSteps: %d\n" *klotski-steps-count*))
    (when (klotski-check-success)
      (insert "\nCongratulation!  You success!\n\nRestart with C-r\n"))))

(cl-defun klotski-refresh ()
  (klotski-erase-board)
  (klotski-put-normal-actors)
  (klotski-put-current-actor)
  (klotski-print-game))

(cl-defun klotski-reset ()
  (interactive)
  (with-current-buffer +klotski-buffer-name+
   (setf *klotski-actors* (copy-tree *klotski-setup*)
	 *klotski-current-actor* (car *klotski-actors*))
   (klotski-steps-reset)
   (klotski-define-keys-for-actors klotski-mode-map)
   (klotski-refresh)))

(cl-defun klotski-select-setup (no)
  (setf *klotski-setup* (nth no +klotski-setups+))
  (klotski-mode)
  (klotski-reset))

(cl-defun klotski-cancel-setup ()
  (interactive)
  (klotski-mode)
  (klotski-reset))

(cl-defun klotski-print-setup (setup no)
  (insert (format "Setup: %d\n\n" (1+ no)))
  (setf *klotski-actors* (copy-tree setup)
	*klotski-current-actor* nil)
  (klotski-erase-board)
  (klotski-put-normal-actors)
  (klotski-print-board)
  (insert "\n"))

(cl-defun klotski-define-keys-for-setup (no)
  (define-key klotski-setup-mode-map
    (kbd (make-string 1 (+ ?1 no)))
    (lambda ()
      (interactive)
      (klotski-select-setup no))))

(cl-defun klotski-setup ()
  (interactive)
  (with-current-buffer +klotski-buffer-name+
   (let ((inhibit-read-only t))
     (klotski-setup-mode)
     (erase-buffer)
     (cl-loop
      for setup in +klotski-setups+
      for no upfrom 0
      do (progn
	   (klotski-define-keys-for-setup no)
	   (klotski-print-setup setup no)))
     (goto-char 1))))

(provide 'klotski)
