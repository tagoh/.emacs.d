;;; init-ai.el --- Initialize AI related packages
;;; Copyright (C) 2025 Akira TAGOH
;;; SPDX-License-Identifier: MIT
;;;
;;; Commentary:
;;;  blah blah blah
;;; Code:
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
;;; init-ai.el ends here
