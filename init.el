;;; init.el --- A Fancy and Fast Emacs Configuration.	-*- lexical-binding: t no-byte-compile: t -*-

;; Copyright (C) 2006-2025 Vincent Zhang

;; Author: Vincent Zhang <seagle0128@gmail.com>
;; URL: https://github.com/seagle0128/.emacs.d
;; Version: 8.2.1
;; Keywords: .emacs.d centaur

;;
;;                          `..`
;;                        ````+ `.`
;;                    /o:``   :+ ``
;;                .+//dho......y/..`
;;                `sdddddhysso+h` ``
;;                  /ddd+`..` +. .`
;;                 -hos+    `.:```
;;               `./dddyo+//osso/:`
;;             `/o++dddddddddddddod-
;;            `// -y+:sdddddsddsy.dy
;;                /o   `..```h+`y+/h+`
;;                .s       `++``o:  ``
;;                        `:- `:-
;;
;;   CENTAUR EMACS - Enjoy Programming & Writing

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;;
;; Centaur Emacs - A Fancy and Fast Emacs Configuration.
;;


;;; Code:

(when (version< emacs-version "28.1")
  (error "This requires Emacs 28.1 and above!"))

;;
;; Speed up Startup Process
;;

;; Optimize Garbage Collection for Startup
(setq gc-cons-threshold most-positive-fixnum)

;; Prevent flash of unstyled mode line
(setq-default mode-line-format nil)

;; Optimize `auto-mode-alist`
(setq auto-mode-case-fold nil)

(unless (or (daemonp) noninteractive init-file-debug)
  ;; Temporarily suppress file-handler processing to speed up startup
  (let ((default-handlers file-name-handler-alist))
    (setq file-name-handler-alist nil)
    ;; Recover handlers after startup
    (add-hook 'emacs-startup-hook
              (lambda ()
                (setq file-name-handler-alist
                      (delete-dups (append file-name-handler-alist default-handlers))))
              101)))

;;
;; Configure Load Path
;;

;; Add "lisp" and "site-lisp" to the beginning of `load-path`
(defun update-load-path (&rest _)
  "Update the `load-path` to prioritize personal configurations."
  (dolist (dir '("site-lisp" "lisp"))
    (push (expand-file-name dir user-emacs-directory) load-path)))

;; Add subdirectories inside "site-lisp" to `load-path`
(defun add-subdirs-to-load-path (&rest _)
  "Recursively add subdirectories in `site-lisp` to `load-path`.

Avoid placing large files like EAF in `site-lisp` to prevent slow startup."
  (let ((default-directory (expand-file-name "site-lisp" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path)))

;; Ensure these functions are called after `package-initialize`
(advice-add #'package-initialize :after #'update-load-path)
(advice-add #'package-initialize :after #'add-subdirs-to-load-path)

;; Initialize load paths explicitly
(update-load-path)

;; Requisites
(require 'init-const)
(require 'init-custom)
(require 'init-funcs)

;; Packages
;; Without this comment Emacs25 adds (package-initialize) here
(require 'init-package)

;; Preferences
(require 'init-base)
(require 'init-hydra)

(require 'init-ui)
(require 'init-edit)
(require 'init-completion)
(require 'init-snippet)

(require 'init-bookmark)
(require 'init-calendar)
(require 'init-dashboard)
(require 'init-dired)
(require 'init-highlight)
(require 'init-ibuffer)
(require 'init-kill-ring)
(require 'init-workspace)
(require 'init-window)
(require 'init-treemacs)

(require 'init-eshell)
(require 'init-shell)

(require 'init-markdown)
(require 'init-org)
(require 'init-reader)

(require 'init-dict)
(require 'init-docker)
(require 'init-player)
(require 'init-utils)

;;(require 'init-tabs)

;; Programming
(require 'init-vcs)
(require 'init-check)
(require 'init-lsp)
(require 'init-dap)

(require 'init-prog)
(require 'init-elisp)
(require 'init-c)
(require 'init-go)
(require 'init-rust)
(require 'init-python)
(require 'init-ruby)
(require 'init-elixir)
(require 'init-web)

(require 'init-ai)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
