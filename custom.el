;;; custom.el --- user customization file    -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;;       Add or change the configurations in custom.el, then restart Emacs.
;;;       Put your own configurations in custom-post.el to override default configurations.
;;; Code:

;; (setq centaur-logo nil)                        ; Logo file or nil (official logo)
;; (setq centaur-full-name "user name")           ; User full name
;; (setq centaur-mail-address "user@email.com")   ; Email address
;; (setq centaur-proxy "127.0.0.1:7890")          ; HTTP/HTTPS proxy
;; (setq centaur-socks-proxy "127.0.0.1:7890")    ; SOCKS proxy
;; (setq centaur-server nil)                      ; Enable `server-mode' or not: t or nil
;; (setq centaur-icon nil)                        ; Display icons or not: t or nil
(setq centaur-package-archives 'melpa)         ; Package repo: melpa, emacs-cn, bfsu, netease, sjtu, tencent, tuna or ustc
;; (setq centaur-theme 'auto)                     ; Color theme: auto, random, system, default, pro, dark, light, warm, cold, day or night
;; (setq centaur-completion-style 'minibuffer)    ; Completion display style: minibuffer or childframe
;; (setq centaur-dashboard nil)                   ; Display dashboard at startup or not: t or nil
;; (setq centaur-lsp 'lsp-mode)                   ; Set LSP client: lsp-mode, eglot or nil
;; (setq centaur-lsp-format-on-save t)            ; Auto format buffers on save: t or nil
;; (setq centaur-lsp-format-on-save-ignore-modes '(c-mode c++-mode python-mode markdown-mode)) ; Ignore format on save for some languages
;; (setq centaur-tree-sitter nil)                 ; Enable tree-sitter or not: t or nil. Only available in 29+.
;; (setq centaur-chinese-calendar t)              ; Support Chinese calendar or not: t or nil
;; (setq centaur-player t)                        ; Enable players or not: t or nil
;; (setq centaur-prettify-symbols-alist nil)      ; Alist of symbol prettifications. Nil to use font supports ligatures.
;; (setq centaur-prettify-org-symbols-alist nil)  ; Alist of symbol prettifications for `org-mode'

;; For Emacs devel
;; (setq package-user-dir (locate-user-emacs-file (format "elpa-%s" emacs-major-version)))
;; (setq desktop-base-file-name (format ".emacs-%s.desktop" emacs-major-version))
;; (setq desktop-base-lock-name (format ".emacs-%s.desktop.lock" emacs-major-version))

(defun image-supported-file-p (file)
  "Say whether Emacs has native support for displaying TYPE.
The value is a symbol specifying the image type, or nil if type
cannot be determined (or if Emacs doesn't have built-in support
for the image type)."
  (let ((case-fold-search t)
   type)
   (catch 'found
   (dolist (elem image-type-file-name-regexps)
	 (when (and (string-match-p (car elem) file)
      (image-type-available-p (setq type (cdr elem))))
	 (throw 'found type))))))

;; Fonts
(defun centaur-setup-fonts ()
  "Setup fonts."
  (when (display-graphic-p)
    ;; Set default font
    (cl-loop for font in '("Cascadia Code" "Fira Code" "Jetbrains Mono"
                           "SF Mono" "Hack" "Source Code Pro" "Menlo"
                           "Monaco" "DejaVu Sans Mono" "Consolas")
             when (font-installed-p font)
             return (set-face-attribute 'default nil
                                        :family font
                                        :height (cond (sys/macp 130)
                                                      (sys/win32p 110)
                                                      (t 100))))

    ;; Set mode-line font
    ;; (cl-loop for font in '("Menlo" "SF Pro Display" "Helvetica")
    ;;          when (font-installed-p font)
    ;;          return (progn
    ;;                   (set-face-attribute 'mode-line nil :family font :height 120)
    ;;                   (when (facep 'mode-line-active)
    ;;                     (set-face-attribute 'mode-line-active nil :family font :height 120))
    ;;                   (set-face-attribute 'mode-line-inactive nil :family font :height 120)))

    ;; Specify font for all unicode characters
    (cl-loop for font in '("Segoe UI Symbol" "Symbola" "Symbol")
             when (font-installed-p font)
             return (if (< emacs-major-version 27)
                        (set-fontset-font "fontset-default" 'unicode font nil 'prepend)
                      (set-fontset-font t 'symbol (font-spec :family font) nil 'prepend)))

    ;; Emoji
    (cl-loop for font in '("Noto Color Emoji" "Apple Color Emoji" "Segoe UI Emoji")
             when (font-installed-p font)
             return (cond
                     ((< emacs-major-version 27)
                      (set-fontset-font "fontset-default" 'unicode font nil 'prepend))
                     ((< emacs-major-version 28)
                      (set-fontset-font t 'symbol (font-spec :family font) nil 'prepend))
                     (t
                      (set-fontset-font t 'emoji (font-spec :family font) nil 'prepend))))

    ;; Specify font for Chinese characters
    (cl-loop for font in '("LXGW Neo Xihei" "WenQuanYi Micro Hei Mono" "LXGW WenKai Screen"
                           "LXGW WenKai Mono" "PingFang SC" "Microsoft Yahei UI" "Simhei")
             when (font-installed-p font)
             return (progn
                      (setq face-font-rescale-alist `((,font . 1.3)))
                      (set-fontset-font t 'han (font-spec :family font))))))

(centaur-setup-fonts)
(add-hook 'window-setup-hook #'centaur-setup-fonts)
(add-hook 'server-after-make-frame-hook #'centaur-setup-fonts)

;; Mail
;; (setq message-send-mail-function 'smtpmail-send-it
;;       smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
;;       smtpmail-auth-credentials '(("smtp.gmail.com" 587
;;                                    user-mail-address nil))
;;       smtpmail-default-smtp-server "smtp.gmail.com"
;;       smtpmail-smtp-server "smtp.gmail.com"
;;       smtpmail-smtp-service 587)

;; Calendar
;; Set location , then press `S' can show the time of sunrise and sunset
;; (setq calendar-location-name "Chengdu"
;;       calendar-latitude 30.67
;;       calendar-longitude 104.07)

;; Misc.
;; (setq confirm-kill-emacs 'y-or-n-p)

;; Enable proxy
;; (proxy-http-enable)
;; (proxy-socks-enable)

;; Display on the specified monitor
;; (when (and (> (length (display-monitor-attributes-list)) 1)
;;            (> (display-pixel-width) 1920))
;;   (set-frame-parameter nil 'left 1920))

;; (put 'cl-destructuring-bind 'lisp-indent-function 'defun)
;; (put 'pdf-view-create-image 'lisp-indent-function 'defun)
;; (put 'treemacs-create-theme 'lisp-indent-function 'defun)

;; (if window-system
;;     (progn
;;       (if (> (display-pixel-width) 1920)
;;           (set-frame-width nil (/ (display-pixel-width) 4)))
;;       (set-frame-height nil (display-pixel-height))))
(set-frame-parameter nil 'left 0)

(setq kill-whole-line t)
(setq user-mail-address "akira@tagoh.org")
;(menu-bar-mode 1)
(blink-cursor-mode -1)
;(cua-mode)
(savehist-mode t)
(setq default-input-method "japanese-anthy-unicode")

;; cc-mode
(defun my-c-mode-common-hook ()
  (c-set-style "PERSONAL")
  (setq tab-width 8
        indent-tabs-mode t
        c-basic-offset 8))
(c-add-style "PERSONAL"
             '((c-tab-always-indent . t)
               (c-offsets-alist
                (substatement-open . 0)
                (case-label . 4))))
(c-add-style "fontconfig"
	         '((c-basic-offset . 4)	; Guessed value
	           (c-offsets-alist
		        (block-close . 0)	; Guessed value
		        (brace-entry-open . 0)	; Guessed value
		        (brace-list-close . 0)	; Guessed value
		        (brace-list-entry . 0)	; Guessed value
		        (brace-list-intro . +)	; Guessed value
		        (case-label . 0)	; Guessed value
		        (class-close . 0)	; Guessed value
		        (defun-block-intro . +)	; Guessed value
		        (defun-close . 0)	; Guessed value
		        (defun-open . 0)	; Guessed value
		        (else-clause . 0)	; Guessed value
		        (inclass . +)		; Guessed value
		        (label . 0)		; Guessed value
		        (statement . 0)		; Guessed value
		        (statement-block-intro . +) ; Guessed value
		        (statement-case-intro . +) ; Guessed value
		        (substatement . +)	   ; Guessed value
		        (substatement-open . 0)	; Guessed value
		        (topmost-intro . 0)	; Guessed value
		        (topmost-intro-cont . 0) ; Guessed value
		        (access-label . -)
		        (annotation-top-cont . 0)
		        (annotation-var-cont . +)
		        (arglist-close . c-lineup-close-paren)
		        (arglist-cont c-lineup-gcc-asm-reg 0)
		        (arglist-cont-nonempty . c-lineup-arglist)
		        (arglist-intro . +)
		        (block-open . 0)
		        (brace-list-open . 0)
		        (c . c-lineup-C-comments)
		        (catch-clause . 0)
		        (class-open . 0)
		        (comment-intro . c-lineup-comment)
		        (composition-close . 0)
		        (composition-open . 0)
		        (cpp-define-intro c-lineup-cpp-define +)
		        (cpp-macro . -1000)
		        (cpp-macro-cont . +)
		        (do-while-closure . 0)
		        (extern-lang-close . 0)
		        (extern-lang-open . 0)
		        (friend . 0)
		        (func-decl-cont . +)
		        (incomposition . +)
		        (inexpr-class . +)
		        (inexpr-statement . +)
		        (inextern-lang . +)
		        (inher-cont . c-lineup-multi-inher)
		        (inher-intro . +)
		        (inlambda . c-lineup-inexpr-block)
		        (inline-close . 0)
		        (inline-open . +)
		        (inmodule . +)
		        (innamespace . +)
		        (knr-argdecl . 0)
		        (knr-argdecl-intro . +)
		        (lambda-intro-cont . +)
		        (member-init-cont . c-lineup-multi-inher)
		        (member-init-intro . +)
		        (module-close . 0)
		        (module-open . 0)
		        (namespace-close . 0)
		        (namespace-open . 0)
		        (objc-method-args-cont . c-lineup-ObjC-method-args)
		        (objc-method-call-cont c-lineup-ObjC-method-call-colons c-lineup-ObjC-method-call +)
		        (objc-method-intro .
				                   [0])
		        (statement-case-open . 0)
		        (statement-cont . +)
		        (stream-op . c-lineup-streamop)
		        (string . -1000)
		        (substatement-label . 2)
		        (template-args-cont c-lineup-template-args +))))
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;; nerd-icons
(eval-after-load 'nerd-icons
  '(add-to-list 'nerd-icons-extension-icon-alist
                '("xz" nerd-icons-octicon "nf-oct-file_binary" :face nerd-icons-lmaroon)))

;; galign
(add-hook 'c-mode-common-hook
          (lambda ()
            (require 'galign)))

;; imenu
(eval-after-load 'lsp-ui
  '(load "lsp-mode-imenu-show-focus"))

;; ;; irony
;; (eval-after-load 'irony
;;   '(progn
;;      (add-hook 'c-mode-hook 'irony-mode)
;;      (add-hook 'c++-mode-hook 'irony-mode)
;;      (add-hook 'objc-mode-hook 'irony-mode)
;;      (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)))

;; license
(require 'fedora-license)
(global-set-key (kbd "C-c M-l") 'license-validate-map)

;; projectile
;; (eval-after-load 'projectile
;;   '(progn
;;      (setq projectile-generic-command "rg -L --files . | cut -c3- | tr '\\n' '\\0'"
;;            projectile-enable-caching t)))

;; rpm-spec-mode
(setq rpm-spec-user-mail-address "tagoh@redhat.com")
(setq rpm-spec-user-full-name "Akira TAGOH")

;; spdx
(eval-after-load 'use-package
  '(progn
     (use-package "spdx"
	   :ensure t
	   :bind
	   (:map prog-mode-map
	    ("C-c M-l i" . spdx-insert-spdx-copyright))
	   :custom
	   (spdx-copyright-holder 'auto)
	   (spdx-project-detection 'auto))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(browse-url-browser-function 'browse-url-chrome)
 '(browse-url-chrome-program "google-chrome")
 '(safe-local-variable-values '((indent-tab-mode))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; custom.el ends here
