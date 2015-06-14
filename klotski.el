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

(defconst +klotski-rows+ 5)
(defconst +klotski-cols+ 4)

(defconst +klotski-caocao-char+ ?c)
(defconst +klotski-exit-pos-list+ '((4 1) (4 2)))

(defconst +klotski-buffer-name+ "*klotski*")

(defconst +klotski-actors-setup+
  `((,+klotski-caocao-char+ (0 1) (0 2) (1 1) (1 2))
    (?g (2 1) (2 2))
    (?z (0 0) (1 0))
    (?a (0 3) (1 3))
    (?m (2 0) (3 0))
    (?h (2 3) (3 3))
    (?1 (4 0))
    (?2 (3 1))
    (?3 (3 2))
    (?4 (4 3))))

(defvar *klotski-empty-cell-string*)

(defvar *klotski-board* (make-vector (* +klotski-cols+ +klotski-rows+) " "))
(defvar *klotski-actors*)
(defvar *klotski-current-actor*)

(defvar *klotski-steps*)
(defvar *klotski-step-counts*)

(defvar *klotski-replay-steps*)
(defvar *klotski-replay-timer*)

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
  (define-key klotski-mode-map (kbd "C-o") #'klotski-show-steps)
  (define-key klotski-mode-map (kbd "C-t") #'klotski-replay))

(define-derived-mode klotski-replay-mode special-mode "KLOTSKI-REPLAY"
  "Klotski (Hua Rong Dao) Game Replay"
  (read-only-mode)
  (set-buffer-modified-p nil)
  (define-key klotski-replay-mode-map (kbd "p") #'klotski-replay-toggle-pause)
  (define-key klotski-replay-mode-map (kbd "q") #'klotski-replay-stop))

;;;###autoload
(cl-defun klotski-game ()
  (interactive)
  (let ((buffer (get-buffer-create +klotski-buffer-name+)))
    (switch-to-buffer buffer)
    (klotski-mode)
    (klotski-reset)))

(cl-defun klotski-index (row col)
  (+ (* +klotski-cols+ row) col))
(cl-defun klotski-get-cell (row col)
  (aref *klotski-board* (klotski-index row col)))
(cl-defun klotski-set-cell (row col val)
  (setf (aref *klotski-board* (klotski-index row col)) val))

(defsubst klotski-actor-char (actor)
  (car actor))

(defsubst klotski-actor-pos-list (actor)
  (cdr actor))

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
	      (not (eq (apply #'klotski-get-cell pos)
		       *klotski-empty-cell-string*)))
      (cl-return t))))

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

(cl-defun klotski-move (dir &optional undo)
  (klotski-put-normal-actors)
  (let ((actor (klotski-current-actor-try-move dir)))
    (unless (klotski-actor-check-collision actor)
      (klotski-current-actor-move dir)
      (if undo
	  (decf *klotski-step-counts*)
	(incf *klotski-step-counts*)
	(push (cons *klotski-current-actor* dir)
	      *klotski-steps*)))
    (klotski-put-current-actor)
    (klotski-print-board)))

(cl-defun klotski-define-keys-for-move (map)
  (cl-loop
   for adir in '(up down left right)
   for dir-key in '("<up>" "<down>" "<left>" "<right>")
   do (let ((dir adir))
	(define-key map (kbd dir-key)
	  (lambda ()
	    (interactive)
	    (klotski-move dir))))))

(cl-defun klotski-reverse-dir (dir)
  (cl-case dir
    ('up 'down)
    ('down 'up)
    ('left 'right)
    ('right 'left)))

(cl-defun klotski-undo ()
  (interactive)
  (with-current-buffer +klotski-buffer-name+
    (when *klotski-steps*
      (let ((step (pop *klotski-steps*)))
	(klotski-set-current-actor (car step))
	(klotski-move (klotski-reverse-dir (cdr step)) t)))))

(cl-defun klotski-show-steps ()
  (interactive)
  (let ((buffer (get-buffer-create "*klotski steps*"))
	(first-line t))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "(")
      (dolist (step (reverse *klotski-steps*))
	(if first-line
	    (setf first-line nil)
	  (insert "\n "))
	(insert (format "(?%c . %s)" (klotski-actor-char (car step))
			(cdr step))))
      (insert ")\n"))
    (display-buffer buffer)))

(cl-defun klotski-replay-step (step)
  (let ((actor (assoc (car step) *klotski-actors*))
	(dir (cdr step)))
    (when actor
      (klotski-set-current-actor actor)
      (klotski-move dir))))

(cl-defun klotski-replay-check-step ()
  (let ((step (pop *klotski-replay-steps*))
	(buffer (get-buffer +klotski-buffer-name+)))
    (if (and buffer step)
	(with-current-buffer buffer
	  (klotski-replay-step step))
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
    (when *klotski-replay-steps*
      (if *klotski-replay-timer*
	  (klotski-replay-stop-timer)
	(klotski-replay-setup-timer)))))

(cl-defun klotski-replay-stop ()
  (interactive)
  (with-current-buffer +klotski-buffer-name+
    (setf *klotski-replay-steps* nil)
    (klotski-replay-stop-timer)
    (klotski-mode)
    (klotski-refresh)))

(cl-defun klotski-replay-check-steps (steps)
  (and (listp steps)
       (dolist (step steps t)
	 (unless (consp step)
	   (cl-return nil)))))

(cl-defun klotski-replay (file)
  (interactive "fFile to replay: ")
  (let ((steps (with-temp-buffer
		 (insert-file-contents file)
		 (read (current-buffer)))))
    (if (klotski-replay-check-steps steps)
	(with-current-buffer +klotski-buffer-name+
	  (setf *klotski-replay-steps* steps)
	  (klotski-replay-mode)
	  (klotski-reset)
	  (klotski-replay-setup-timer))
      (message "Invalid steps file!"))))

(defface klotski-current-actor-face
  '((default :foreground "red" :height 2.0))
  "Face for current klotski actor"
  :group 'klotski-faces)

(defface klotski-normal-actor-face
  '((default :height 2.0))
  "Face for normal actor"
  :group 'klotski-faces)

(cl-defun klotski-put-current-actor ()
  (let* ((actor *klotski-current-actor*)
	 (str (make-string 1 (klotski-actor-char actor))))
    (put-text-property 0 1 'face 'klotski-current-actor-face str)
    (dolist (pos (klotski-actor-pos-list actor))
      (apply #'klotski-set-cell
	     (append pos (list str))))))

(cl-defun klotski-erase-board ()
  (dotimes (row +klotski-rows+)
    (dotimes (col +klotski-cols+)
      (klotski-set-cell row col *klotski-empty-cell-string*))))

(cl-defun klotski-put-normal-actor (actor)
  (let ((str (make-string 1 (klotski-actor-char actor))))
    (put-text-property 0 1 'face 'klotski-normal-actor-face str)
    (dolist (pos (klotski-actor-pos-list actor))
      (apply #'klotski-set-cell
	     (append pos (list str))))))

(cl-defun klotski-put-normal-actors ()
  (klotski-erase-board)
  (dolist (actor *klotski-actors*)
    (unless (eq actor *klotski-current-actor*)
      (klotski-put-normal-actor actor))))

(cl-defun klotski-check-success ()
  (let* ((caocao (assoc +klotski-caocao-char+ *klotski-actors*))
	 (caocao-pos-list (klotski-actor-pos-list caocao)))
    (dolist (pos +klotski-exit-pos-list+ t)
      (unless (member pos caocao-pos-list)
	(cl-return nil)))))

(cl-defun klotski-print-board ()
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert "  Klotski (Hua Rong Dao) Game")
    (if (eq major-mode 'klotski-replay-mode)
	(insert ": Replay"))
    (insert "\n\n")
    (dotimes (row +klotski-rows+)
      (insert "  ")
      (dotimes (col +klotski-cols+)
	(insert (klotski-get-cell row col)))
      (insert "\n"))
    (insert (format "\nSteps: %d\n" *klotski-step-counts*))
    (when (klotski-check-success)
      (insert "\nCongratulation!  You success!\n\nRestart with C-r\n"))))

(cl-defun klotski-refresh ()
  (klotski-put-normal-actors)
  (klotski-put-current-actor)
  (klotski-print-board))

(cl-defun klotski-reset ()
  (interactive)
  (with-current-buffer +klotski-buffer-name+
   (setf *klotski-actors* (copy-tree +klotski-actors-setup+)
	 *klotski-current-actor* (car *klotski-actors*)
	 *klotski-steps* nil
	 *klotski-step-counts* 0)
   (klotski-define-keys-for-actors klotski-mode-map)
   (klotski-refresh)))

(provide 'klotski-mode)
