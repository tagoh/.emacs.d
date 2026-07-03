;;; custom.el --- user customization file    -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;;       Add or change the configurations in custom.el, then restart Emacs.
;;;       Put your own configurations in custom-post.el to override default configurations.
;;; Code:

;; (setq centaur-logo nil)                        ; Logo file or nil (official logo)
;; (setq centaur-full-name "user name")           ; User full name
;; (setq centaur-mail-address "user@email.com")   ; Email address
;; (setq centaur-proxy "127.0.0.1:7897")          ; HTTP/HTTPS proxy
;; (setq centaur-socks-proxy "127.0.0.1:7897")    ; SOCKS proxy

;; (setq centaur-use-exec-path-from-shell nil)    ; Use `exec-path-from-shell' or not. If using emacs-plus with path ejection, set to nil
;; (setq centaur-icon nil)                        ; Display icons or not: t or nil
(setq centaur-package-archives 'melpa)         ; Package repo: melpa, emacs-cn, bfsu, netease, sjtu, tencent, tuna or ustc
;; (setq centaur-theme 'auto)                     ; Color theme: auto, random, system, default, pro, dark, light, warm, cold, day or night
;; (setq centaur-completion-style 'minibuffer)    ; Completion display style: minibuffer or childframe
(setq centaur-frame-maximized-on-startup nil)  ; Maximize frame on startup or not: t or nil
;; (setq centaur-dashboard nil)                   ; Display dashboard at startup or not: t or nil
;; (setq centaur-lsp 'lsp-mode)                   ; Set LSP client: lsp-mode, eglot or nil
;; (setq centaur-lsp-format-on-save t)            ; Auto format buffers on save: t or nil
;; (setq centaur-lsp-format-on-save-ignore-modes '(c-mode c++-mode python-mode markdown-mode)) ; Ignore format on save for some languages
;; (setq centaur-tree-sitter nil)                 ; Enable tree-sitter or not: t or nil. Only available in 29+.
;; (setq centaur-chinese-calendar t)              ; Support Chinese calendar or not: t or nil
;; (setq centaur-player t)                        ; Enable players or not: t or nil
(setq centaur-prettify-symbols-alist nil)      ; Alist of symbol prettifications. Nil to use font supports ligatures.

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
    (cl-loop for font in '("FiraCode Nerd Font" "CaskaydiaCove Nerd Font"
                           "Fira Code" "Cascadia Code" "Jetbrains Mono"
                           "SF Mono" "Menlo" "Hack" "Source Code Pro"
                           "Monaco" "DejaVu Sans Mono" "Consolas")
             when (font-available-p font)
             return (set-face-attribute 'default nil
                                        :family font
                                        :height (cond (sys/macp 130)
                                                      (sys/win32p 110)
                                                      (t 100))))

    ;; Set mode-line font
    ;; (cl-loop for font in '("Arial" "Helvetica" "Times New Roman")
    ;;          when (font-available-p font)
    ;;          return (progn
    ;;                   (set-face-attribute 'mode-line nil :family font :inherit 'variable-pitch)
    ;;                   (set-face-attribute 'mode-line-inactive nil :family font :inherit 'variable-pitch)))

    ;; Specify font for all unicode characters
    (cl-loop for font in '("Apple Symbols" "Segoe UI Symbol" "Symbola" "Symbol")
             when (font-available-p font)
             return (set-fontset-font t 'symbol (font-spec :family font) nil 'prepend))

    ;; Specify font for Emoji characters
    (cl-loop for font in '("Noto Color Emoji" "Apple Color Emoji" "Segoe UI Emoji")
             when (font-available-p font)
             return (set-fontset-font t 'emoji (font-spec :family font) nil 'prepend))

    ;; Specify font for Chinese characters
    ;; (cl-loop for font in '("LXGW Neo Xihei" "LXGW WenKai Mono" "WenQuanYi Micro Hei Mono"
    ;;                        "PingFang SC" "Microsoft Yahei UI" "Simhei")
    ;;          when (font-available-p font)
    ;;          return (progn
    ;;                   (setq face-font-rescale-alist `((,font . 1.3)))
    ;;                   (set-fontset-font t 'han (font-spec :family font))))
    ))

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
;; (setq package-check-signature nil)
;; (setq trusted-content ':all)

;; Enable proxy
;; (enable-http-proxy)
;; (enable-socks-proxy)

;; Display on the specified monitor
;; (when (and (> (length (display-monitor-attributes-list)) 1)
;;            (> (display-pixel-width) 1920))
;;   (set-frame-parameter nil 'left 288))

;; (put 'cl-destructuring-bind 'lisp-indent-function 'defun)
;; (put 'pdf-view-create-image 'lisp-indent-function 'defun)
;; (put 'treemacs-create-theme 'lisp-indent-function 'defun)

;; For compat
;; (add-hook 'emacs-lisp-mode-hook
;;           (defun compat-add-to-imenu ()
;;             "Add to imenu list."
;;             (add-to-list
;;              'imenu-generic-expression
;;              '(nil
;;                "^\\s-*(\\(compat-def\\(?:un\\|macro\\|alias\\)\\)\\s-+\\(\\(?:\\w\\|\\s_\\|\\\\.\\)+\\)"
;;                2))
;;             (add-to-list
;;              'imenu-generic-expression
;;              '("Packages" "^\\s-*(\\(compat-require\\)\\s-+\\(\\(?:\\w\\|\\s_\\|\\\\.\\)+\\)" 2))
;;             (add-to-list
;;              'imenu-generic-expression
;;              '("Variables"
;;                "^\\s-*(\\(compat-def\\(?:var\\|const\\)\\)?\\s-+\\(\\(?:\\w\\|\\s_\\|\\\\.\\)+\\)[[:space:]\n]+[^)]"
;;                2))))

;; (if window-system
;;     (progn
;;       (if (> (display-pixel-width) 1920)
;;           (set-frame-width nil (/ (display-pixel-width) 4)))
;;       (set-frame-height nil (display-pixel-height))))
;;(set-frame-parameter nil 'left 0)

(setq kill-whole-line t)
(setq user-mail-address "akira@tagoh.org")
                                        ;(menu-bar-mode 1)
(blink-cursor-mode -1)
                                        ;(cua-mode)
(savehist-mode t)
(setq default-input-method "japanese-anthy-unicode")

;; cc-mode
(eval-after-load 'c-ts-mode
  (add-hook 'c-ts-mode-hook
            (lambda ()
              (setq-local c-ts-mode-indent-style 'linux)
              (setq-local c-ts-mode-indent-offset 4)
              (setq-local tab-width 8))))
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
(global-set-key (kbd "C-c M-l") license-validate-map)

;; projectile
;; (eval-after-load 'projectile
;;   '(progn
;;      (setq projectile-generic-command "rg -L --files . | cut -c3- | tr '\\n' '\\0'"
;;            projectile-enable-caching t)))

;; rpm-spec-mode
(setq rpm-spec-user-mail-address "tagoh@redhat.com")
(setq rpm-spec-user-full-name "Akira TAGOH")

;; spdx
(with-eval-after-load 'init-package
  (define-key license-validate-map (kbd "i") #'spdx-insert-spdx-copyright)
  (setq spdx-copyright-holder 'auto)
  (setq spdx-project-detection 'auto))

;; magit
(with-eval-after-load 'magit
  (defun my/magit-worktree-ensure-tabspace ()
    (when (and (derived-mode-p 'magit-mode)
               default-directory
               (not (bound-and-true-p my/in-tabspace-switching)))
      (let* ((project-root (expand-file-name default-directory))
             (current-tab-name (alist-get 'name (tab-bar--current-tab)))
             (expected-tab-name (file-name-nondirectory (directory-file-name project-root))))
        (when (and expected-tab-name
                   (not (string= current-tab-name expected-tab-name)))
          (let ((my/in-tabspace-switching t))
            (when (fboundp 'project-forget-project)
              (project-forget-project project-root))

            (let ((all-tabs (mapcar (lambda (tab) (alist-get 'name tab))
                                    (funcall tab-bar-tabs-function))))
              (if (member expected-tab-name all-tabs)
                  (tab-bar-switch-to-tab expected-tab-name)
                (tab-bar-new-tab)
                (tab-bar-rename-tab expected-tab-name)))

            (setq-local project-current-directory project-root))))))
  (add-hook 'magit-post-display-buffer-hook #'my/magit-worktree-ensure-tabspace))

(with-eval-after-load 'magit-worktree
  (defun my/magit-worktree-delete-and-clean-tab (orig-fun &rest args)
    (let* ((current-tab (tab-bar--current-tab))
           (current-tab-name (alist-get 'name current-tab))
           (all-tabs (mapcar (lambda (tab) (alist-get 'name tab)) (funcall tab-bar-tabs-function)))
           (remaining-tabs (cl-remove current-tab-name all-tabs :test #'string=))
           (fallback-tab (or (car (cl-remove "Default" remaining-tabs :test #'string=))
                             (car remaining-tabs))))

      (apply orig-fun args)
      (when (and current-tab-name (not (string= current-tab-name "Default")))
        (tab-bar-close-tab-by-name current-tab-name)
        (when fallback-tab
          (tab-bar-switch-to-tab fallback-tab)
          (magit-refresh)))))

  (advice-add 'magit-worktree-delete :around #'my/magit-worktree-delete-and-clean-tab))

;; tabspaces
(setq tabspaces-use-filters t
      tabspaces-use-filtered-buffers-as-default t)

(defun my/tabspaces-kill-buffers-before-close (tab)
  (let* ((name (cdr (assq 'name tab)))
         (unless (string= name "Default")
           (let* ((tabs (funcall tab-bar-tabs-function))
                  (tab-index (cl-position-if (lambda (t-obj) (string= (alist-get 'name t-obj) name)) tabs))
                  (buffers (if tab-index
                               (tabspaces--buffer-list nil (1+ tab-index))
                             (tabspaces--buffer-list))))
             (dolist (buf buffers)
               (when (buffer-live-p buf)
                 (let ((buf-name (buffer-name buf)))
                   (unless (member buf-name '("*scratch*", "*Messages*"))
                     (kill-buffer buf))))))))))

(add-hook 'tab-bar-tab-prevent-close-functions
          (lambda (tab arg)
            (my/tabspaces-kill-buffers-before-close tab)
            nil))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(browse-url-browser-function 'browse-url-chrome)
 '(browse-url-chrome-program "google-chrome")
 '(safe-local-variable-values '((encoding . utf-8) (indent-tab-mode))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; custom.el ends here
