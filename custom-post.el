;;; custom-post.el --- personal overrides    -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;;       Personal configuration loaded after init, overriding Centaur defaults.
;;;       This file is git-ignored, so it survives Centaur updates untouched.
;;; Code:

;; Keep the periodic session auto-save from destroying live buffers.
;;
;; Centaur's `tabspaces--prepare-save-session' (lisp/init-workspace.el) runs as
;; :before advice on `tabspaces--save-session-smart' and kills magit/helpful
;; buffers and deletes posframes so the saved session stays clean.  That is
;; harmless for the exit-time and manual saves it was written for, but Centaur
;; also enables `tabspaces-session-auto-save-delay' (5 min idle), so the same
;; destructive cleanup now fires on a timer against a live session and kills
;; buffers you are actively using (most visibly magit-status).
;;
;; Neutralize just those side effects for the duration of the periodic
;; auto-save.  Exit and manual saves still sanitize the session as before, so
;; the on-disk session stays restorable.
(with-eval-after-load 'tabspaces
  (require 'cl-lib)
  (defun my/tabspaces-auto-save-preserve-buffers (orig &rest args)
    "Run periodic session auto-save without Centaur's exit-time buffer cleanup."
    (cl-letf (((symbol-function 'magit-mode-get-buffers) (lambda (&rest _) nil))
              ((symbol-function 'helpful-kill-buffers)    (lambda (&rest _) nil))
              ((symbol-function 'posframe-delete-all)     (lambda (&rest _) nil)))
      (apply orig args)))
  (advice-add 'tabspaces--session-auto-save :around
              #'my/tabspaces-auto-save-preserve-buffers))

;; Stop `package-selected-packages' from being written into custom.el.
;;
;; Centaur overrides `package--save-selected-packages' (lisp/init-package.el)
;; to suppress this, but Emacs 30.1+ moved the actual `customize-save-variable'
;; call into a new helper, `package--save-selected-packages-1', which the
;; outer override does not cover.  On a load-order path (package activation
;; before the override installs) the real helper gets queued on
;; `after-init-hook' and writes the list into custom.el.  Neutralize the
;; writer itself so no path can persist it, matching Centaur's intent without
;; editing any Centaur file.  `load-custom-post-file' runs ahead of the queued
;; helper on `after-init-hook', so this override is in place before it fires.
(when (fboundp 'package--save-selected-packages-1)
  (advice-add 'package--save-selected-packages-1 :override #'ignore))

(provide 'custom-post)
;;; custom-post.el ends here
