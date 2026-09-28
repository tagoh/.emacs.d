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

;; Keep tabspaces + tabspaces-ext after Centaur's upstream switch to project-x.
;;
;; Upstream Centaur (lisp/init-workspace.el on master) replaced tabspaces with
;; project-x and enables `project-x-tabs-mode', which takes over tab-bar tab
;; lifecycle and per-tab buffer isolation -- the same job tabspaces does.  Both
;; cannot drive the tab-bar at once, and my whole workflow (tabspaces-ext: magit
;; worktree tabs, treemacs/popterm per-tab sync, window-state-plus session
;; restore) is built on tabspaces.  So once project-x is present we stop it from
;; managing tabs/sessions and stand tabspaces back up.
;;
;; Guarded on project-x being installed: on a checkout that still ships tabspaces
;; (pre-merge) this whole block is a no-op and Centaur's own config is used.
;;
;; NOTE on timing: `load-custom-post-file' runs *from* `after-init-hook', and
;; project-x's `(after-init . project-x-mode)' hook runs before it -- so both
;; project-x modes are already ON here.  We must disable them actively, and
;; enable `tabspaces-mode' explicitly (re-adding an after-init hook won't fire).
(when (locate-library "project-x")
  ;; 1. Stop project-x owning tabs/sessions. Turn the modes off (already on),
  ;;    and drop the hooks so nothing re-enables them.
  (remove-hook 'after-init-hook #'project-x-mode)
  (remove-hook 'project-x-mode-hook #'project-x-tabs-mode)
  (when (bound-and-true-p project-x-tabs-mode) (project-x-tabs-mode -1))
  (when (bound-and-true-p project-x-mode)      (project-x-mode -1))

  ;; 2. Revive tabspaces. Mirrors the pre-project-x lisp/init-workspace.el that
  ;;    upstream deletes; :ensure pulls it back in. `tabspaces-ext' (configured
  ;;    in custom.el with `:after tabspaces') activates when this loads it.
  (use-package tabspaces
    :ensure t
    :bind (:map tabspaces-command-map
           ("C-r"   . tabspaces-restore-session)
           ("C-M-r" . tabspaces-restore-session-alt)
           ("C-s"   . tabspaces-save-session))
    :custom
    (tab-bar-show nil)
    (tab-bar-history-limit 30)
    (tabspaces-use-filtered-buffers-as-default t)
    (tabspaces-exclude-buffers '())
    (tabspaces-session (not centaur-dashboard))
    (tabspaces-session-auto-restore (not centaur-dashboard))
    (tabspaces-session-file (locate-user-emacs-file "tabspaces/tabsession.el"))
    (tabspaces-session-project-session-store (locate-user-emacs-file "tabspaces/"))
    (tabspaces-session-auto-save-delay 300)
    :config
    (defun tabspaces-restore-session-alt ()
      "Select file to restore tabspaces session."
      (interactive)
      (let ((project-or-session-file
             (read-file-name "Select project or session file: "
                             tabspaces-session-project-session-store)))
        (tabspaces-restore-session project-or-session-file)))

    (with-no-warnings
      ;; Filtered buffer list for consult-buffer.
      (with-eval-after-load 'consult
        (consult-customize consult-source-buffer :hidden t :default nil)
        (defvar consult-source-workspace
          (list :name     "Workspace Buffer"
                :narrow   ?w
                :history  'buffer-name-history
                :category 'buffer
                :state    #'consult--buffer-state
                :default  t
                :items    (lambda ()
                            (consult--buffer-query
                             :predicate #'tabspaces--local-buffer-p
                             :sort 'visibility
                             :as #'buffer-name)))
          "Set workspace buffer list for consult-buffer.")
        (add-to-list 'consult-buffer-sources 'consult-source-workspace))

      ;; Backup + cleanup tabspaces sessions before saving.
      (defconst tabspaces--keep-days 14
        "How long (days) to keep tabspaces sessions.")
      (defun tabspaces--delete-old-files (dir days)
        "Delete backup files of DIR with timestamp suffix older than DAYS days."
        (let ((cutoff (time-subtract (current-time)
                                     (seconds-to-time (* days 24 60 60)))))
          (dolist (file (directory-files dir 'full "\\.[0-9]\\{8\\}\\'"))
            (when-let* ((ts (substring file (string-match "\\([0-9]\\{8\\}\\)\\'" file))))
              (when (time-less-p (date-to-time ts) cutoff)
                (delete-file file 'trash))))))
      (defun tabspaces--prepare-save-session (&rest _)
        "Prepare for saving session."
        (when tabspaces-session
          (let ((dir (locate-user-emacs-file "tabspaces")))
            (unless (file-exists-p dir) (mkdir dir))
            (tabspaces--delete-old-files dir tabspaces--keep-days))
          (when (file-exists-p tabspaces-session-file)
            (copy-file tabspaces-session-file
                       (format "%s.%s" tabspaces-session-file
                               (format-time-string "%Y%m%d"))
                       t)))
        (when (fboundp 'helpful-kill-buffers) (helpful-kill-buffers))
        (when (fboundp 'magit-mode-get-buffers)
          (mapc #'kill-buffer (magit-mode-get-buffers)))
        (when (fboundp 'posframe-delete-all) (posframe-delete-all)))
      (advice-add #'tabspaces--save-session-smart :before
                  #'tabspaces--prepare-save-session)))

  ;; after-init is already running; enable now rather than via :hook.
  (tabspaces-mode 1)
  (tab-bar-history-mode 1))

(provide 'custom-post)
;;; custom-post.el ends here
