;; init-ai.el --- Initialize AI configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2026 Vincent Zhang

;; Author: Vincent Zhang <seagle0128@gmail.com>
;; URL: https://github.com/seagle0128/.emacs.d

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
;; AI configurations.
;;

;;; Code:

(eval-when-compile
  (require 'init-const))

;; Interact with ChatGPT or other LLMs
(use-package gptel
  :functions gptel-make-openai
  :custom
  (gptel-model 'gpt-4o)
  ;; Put the apikey to `auth-sources'
  ;; Format: "machine {HOST} login {USER} password {APIKEY}"
  ;; The LLM host is used as HOST, and "apikey" as USER.
  (gptel-backend (gptel-make-openai "Github Models"
                   :host "models.inference.ai.azure.com"
                   :endpoint "/chat/completions?api-version=2024-05-01-preview"
                   :stream t
                   :key 'gptel-api-key
                   :models '(gpt-4o))))

;; Generate commit messages for magit
(use-package gptel-magit
  :hook (magit-mode . gptel-magit-install))

;; A native shell experience to interact with ACP agents
(when emacs/>=29p
  (use-package agent-shell
    :diminish agent-shell-ui-mode))

(use-package aidermacs
  :bind (("C-c C-a" . aidermacs-transient-menu))
  :config
                                        ; Set API_KEY
  (setenv "OPENAI_API_BASE" "http://0.0.0.0:8080")
  (setenv "OPENAI_API_KEY" "xxxx")
  :custom
  (setq aidermacs-default-model "")
  (setq aidermacs-architect-model "")
  (setq aidermacs-editor-model "")
  (setq aidermacs-use-architect-mode t)
  (setq aidermacs-weak-model "")
                                        ;(setq aidermacs-auto-accept-architect t)
  (setq aidermacs-backend 'vterm)
  (setq aidermacs-vterm-multiline-newline-key "S-<return>"))

(use-package ellama
  :ensure t
  :bind ("C-c M-e" . ellama)
  :hook (org-ctrl-c-ctrl-c-final . ellama-chat-send-last-message)
  :init
  (setopt ellama-auto-scroll t)
  (require 'llm-openai)
  (declare-function make-llm-openai-compatible "ext:llm-openai")
  (setopt ellama-provider
          (make-llm-openai-compatible
           :url "http://0.0.0.0:8080/v1/"))
  :config
  (ellama-context-header-line-global-mode +1)
  (ellama-session-header-line-global-mode +1))

(provide 'init-ai)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ai.el ends here
