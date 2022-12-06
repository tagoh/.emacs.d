;; galign.el --- glib style argument aligner

;; Copyright 2004 Masatake YAMATO
;;
;; Author: Masatake YAMATO<jet@gyve.org>
;; Keywords:
;; X-URL: not distributed yet

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;; Usage:
;;
;; (add-hook 'c-mode-common-hook
;; 	  (lambda () (require 'galign)))
;;
;; Set region then C-c\

;; static void g_variable_constraint_class_init (GVariableConstraintClass *klass);
;; => "static void": rtn
;; => "": ptr
;; => g_variable_constraint_class_init: func
;; => (: openparen
;; => GVariableConstraintClass: argtype
;; => *klass: arg
;; => ); closeparen
;;
(define-key c-mode-map "\C-c\\" 'galign-align)

(defun galign-align (r e)
  (interactive "r")
  (let* ((extractions (galign-extract r e))
	 (rule (galign-gen-rule extractions)))
    (delete-region r e)
    (galign-insert rule extractions)))
	
(defun galign-insert (rule extractions)
  (let (ideal target real)
    (mapc
     (lambda (x)
       (setq ideal  (1+ (nth 0 rule)))
       (setq target (nth 2 (nth 0 x)))
       (setq real   (length target))
       (insert (concat target (make-string (- ideal real) ?\ )))
       (setq ideal  (nth 1 rule))
       (setq target (nth 2 (nth 1 x)))
       (setq real   (length target))
       (insert (concat target (make-string (- ideal real) ?\ )))
;       (setq ideal  (1+ (nth 1 rule)))
       (setq ideal  (nth 2 rule))
       (setq target (nth 2 (nth 2 x)))
       (setq real   (length target))
       (insert (concat target (make-string (- ideal real) ?\ )))
       (insert ?\()
       (galign-insert-args (current-column)(nth 3 rule) (nth 4 rule) (nth 4 x))
       (insert ?\) ?\;)
       (insert "\n"))
     extractions)))

(defun galign-insert-args (base typelen arglen args)
  (let ((i 0)
	(imax (1- (length args)))
	ideal target real )
    (if (null args)
	(insert "void")
      (mapc
       (lambda (x)
	 (setq ideal (1+ typelen))
	 (setq target (nth 2 x))
	 (setq real (length target))
	 (insert (concat
		  (when (not (zerop i)) (make-string base ?\ ))
		  target
		  (make-string (- ideal real) ?\ )))
	 (setq ideal arglen)
	 (setq target (nth 5 x))
	 (setq real (length target))
	 (insert (concat 
		  (unless (equal "*" (substring target 0 1)) " ")
					;(make-string (- ideal real) ?\ )
		  target))
	 (unless (eq i imax) (insert ",\n"))
	 (setq i (1+ i))
	 )
       args)
      )))

(defun galign-gen-rule (extractions)
  (let ((max-rtn 0)
	(max-ptr 0)
	(max-func 0)
	(max-argtype 0)
	(max-arg 0))
    (mapc
     (lambda (x)
       (setq max-rtn  (max max-rtn  (length (nth 2 (nth 0 x)))))
       (setq max-ptr  (max max-ptr  (length (nth 2 (nth 1 x)))))
       (setq max-func (max max-func (length (nth 2 (nth 2 x)))))
       (mapc
	(lambda (y)
	  (setq max-argtype (max max-argtype (length (nth 2 y))))
	  (setq max-arg (max max-arg (length (nth 5 y)))))
	(nth 4 x)))
     extractions)
    (list max-rtn max-ptr max-func max-argtype max-arg)))

(defun galign-extract (b e)
  (interactive "r")
  (let (tmp acum)
    (while (setq tmp (galign-extract-one b e))
      (setq b (nth 5 tmp))
      (setq acum (cons tmp acum)))
    (reverse acum)))

(defun galign-extract-one (b e)
  (let (rtn ptr func openparen closeparen args)
  (goto-char b)
  (when (re-search-forward "[a-zA-Z_]" e t)
    (goto-char (match-beginning 0))
    (setq b (point))
    (setq func (galign-extract-func b e))
    (when func
      (goto-char (nth 0 func))
      (when (re-search-backward "[^ \t]" b t)
	(setq rtn (list b (match-end 0)
			(buffer-substring-no-properties b (match-end 0))))
	(setq ptr (list (nth 0 func) (nth 0 func) ""))
	(unless (zerop (length (nth 2 rtn)))
	    (let* ((str (nth 2 rtn))
		   (rtnnp (replace-regexp-in-string "[ \t]+\\*[ \t]?" "" str))
		   (subst (substring str -1 (length str))))
	      (if (equal "*" subst)
		  (progn
		    (setcar (cddr rtn) rtnnp)
		    (setq ptr (list (- (nth 0 func) 1) (nth 0 func) "*"))))))
	(setq openparen  (galign-extract-openparen b e))
	(setq closeparen (galign-extract-closeparen b e))
	(when (and rtn openparen closeparen)
	  (list rtn
		ptr
		func
		openparen
		(galign-extract-args openparen closeparen)
		closeparen)))))))

(defun galign-extract-openparen (b e)
  (interactive "r")
  (save-excursion
    (goto-char b)
    (when (re-search-forward "(" e t)
      (match-beginning 0))))

(defun galign-extract-closeparen (b e)
  (interactive "r")
  (save-excursion
    (goto-char b)
    (when (re-search-forward ");" e t)
      (match-beginning 0))))

(defun galign-extract-func (b e)
  (interactive "r")
  (save-excursion 
    (goto-char b)
    (let ((open-paren (galign-extract-openparen b e)))
      (when (and open-paren
		 (goto-char open-paren)
		 (re-search-backward "\\_<.*\\_>" b t))
	(list (match-beginning 0)
	      (match-end 0)
	      (match-string 0))))))

(defun galign-extract-args (b e)
  (interactive "r")
  (let (arg args)
    (while (setq arg (galign-extract-type-and-arg b (1+ e)))
      (setq b (nth 4 arg))
      (setq args (cons arg args)))
    (reverse args)))
  
(defun galign-extract-type-and-arg (b e)
  (let (TB TE AB AE)
    (save-excursion 
      (goto-char b)
      (when (re-search-forward "[a-zA-Z_]" e t)
	(setq b (match-beginning 0))
	(setq TB b)
	(goto-char b)
	(when (and (prog1 (re-search-forward "[ \t]*[,)]" e t)
		     (setq AE (match-beginning 0))
		     (goto-char AE)
		     )
		   (re-search-backward "[ \t]" b t))
	  (setq AB (match-end 0))
	  (when (re-search-backward "[^ \t]" b t)
	    (setq TE (match-end 0))
	    (list TB
		  TE
		  (buffer-substring-no-properties TB TE)
		  AB
		  AE
		  (buffer-substring-no-properties AB AE)
		  )))))))

(provide 'galign)
;; arch-tag: da065c7e-46fe-44a5-a58a-685ddccf3241
;; galign.el ends here
