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
(setq centaur-dashboard nil)                   ; Display dashboard at startup or not: t or nil
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

;; ============================================================================
;; Git-aware tabspaces integration
;; ============================================================================
;; Automatically creates and manages tabspaces sessions for git repositories
;; and worktrees with naming format: "project@branch"

;; Helper functions
(defun my/get-all-tab-names ()
  "Get list of all tab names."
  (mapcar (lambda (tab) (alist-get 'name tab))
          (funcall tab-bar-tabs-function)))

(defun my/get-current-tab-name ()
  "Get current tab name."
  (alist-get 'name (tab-bar--current-tab)))

(defun my/find-tab-index (tab-name)
  "Find the index of tab with TAB-NAME."
  (cl-position-if
   (lambda (tab) (string= (alist-get 'name tab) tab-name))
   (funcall tab-bar-tabs-function)))

(defun my/is-system-buffer-p (buffer-name)
  "Return t if BUFFER-NAME is a system buffer."
  (or (member buffer-name '("*scratch*" "*Messages*"))
      (string-prefix-p " *Minibuf" buffer-name)))

(defun my/switch-or-create-tab (tab-name project-root)
  "Switch to existing tab TAB-NAME or create new one for PROJECT-ROOT."
  (if (member tab-name (my/get-all-tab-names))
      (tab-bar-switch-to-tab tab-name)
    (tab-bar-new-tab)
    (tab-bar-rename-tab tab-name)
    (add-to-list 'tabspaces-project-tab-map (cons project-root tab-name))))

(defun my/kill-worktree-buffers (project-root)
  "Kill all buffers associated with PROJECT-ROOT worktree."
  (let ((worktree-name (file-name-nondirectory
                        (directory-file-name project-root))))
    (dolist (buf (buffer-list))
      (when (buffer-live-p buf)
        (let ((buf-file (buffer-file-name buf))
              (buf-dir (ignore-errors
                         (buffer-local-value 'default-directory buf)))
              (buf-name (buffer-name buf)))
          ;; Kill buffers from deleted worktree (but not system buffers)
          (when (and (not (my/is-system-buffer-p buf-name))
                     (or (and buf-file
                              (string-prefix-p project-root
                                               (expand-file-name buf-file)))
                         (and buf-dir
                              (string-prefix-p project-root
                                               (expand-file-name buf-dir)))
                         (and (string-prefix-p " *Old buffer" buf-name)
                              (string-match-p (regexp-quote worktree-name)
                                              buf-name))))
            (kill-buffer buf)))))))

(defun my/get-git-project-name ()
  "Get project name from git remote URL or directory name.
Works for both regular repos and worktrees."
  (when (and (fboundp 'magit-get) (fboundp 'magit-gitdir))
    (or
     ;; Try to extract from remote URL
     (when-let ((remote-url (magit-get "remote.origin.url")))
       (cond
        ;; SSH: git@github.com:user/project.git
        ((string-match ":\\([^/]+\\)/\\([^/\\.]+\\)\\(\\.git\\)?$" remote-url)
         (match-string 2 remote-url))
        ;; HTTPS: https://github.com/user/project.git
        ((string-match "/\\([^/\\.]+\\)\\(\\.git\\)?$" remote-url)
         (match-string 1 remote-url))))
     ;; Fallback to repository directory name
     (when-let ((git-dir (magit-gitdir)))
       (file-name-nondirectory
        (directory-file-name
         (if (string-match "/\\.git/?$" git-dir)
             (replace-regexp-in-string "/\\.git/?$" "" git-dir)
           default-directory)))))))

(defun my/get-git-branch-name (&optional worktree-path)
  "Get current git branch name.
For detached HEAD (e.g., during rebase), tries to infer from git state
or WORKTREE-PATH directory name."
  (let ((default-directory (or worktree-path default-directory)))
    (or
     ;; Try normal branch name
     (and (fboundp 'magit-get-current-branch)
          (magit-get-current-branch))
     ;; If rebasing, get branch from rebase state
     (when (file-exists-p ".git/rebase-merge/head-name")
       (let ((head-name (with-temp-buffer
                          (insert-file-contents ".git/rebase-merge/head-name")
                          (string-trim (buffer-string)))))
         (when (string-match "refs/heads/\\(.+\\)" head-name)
           (match-string 1 head-name))))
     ;; Extract from worktree directory name: "main_branch-name" -> "branch-name"
     (when worktree-path
       (let ((dir-name (file-name-nondirectory
                        (directory-file-name worktree-path))))
         (when (string-match "^[^_]+_\\(.+\\)$" dir-name)
           (match-string 1 dir-name)))))))

(defun my/get-git-tab-name (&optional worktree-path)
  "Get tab name in 'project@branch' format.
Optional WORKTREE-PATH for worktree-specific branch detection."
  (let* ((default-directory (or worktree-path default-directory))
         (project-name (my/get-git-project-name))
         (branch-name (my/get-git-branch-name worktree-path)))
    (if (and project-name branch-name)
        (format "%s@%s" project-name branch-name)
      (or project-name
          (file-name-nondirectory
           (directory-file-name default-directory))))))

;; ============================================================================
;; Magit integration
;; ============================================================================

(with-eval-after-load 'magit
  (defun my/magit-worktree-ensure-tabspace ()
    "Automatically switch to or create tabspace for git worktree in magit buffers."
    (when (and (derived-mode-p 'magit-mode)
               default-directory
               (not (get-buffer "*tabspaces--placeholder*")))
      (let* ((project-root (expand-file-name default-directory))
             (current-tab-name (my/get-current-tab-name))
             (expected-tab-name (my/get-git-tab-name))
             (magit-buffer (current-buffer)))
        (when (and expected-tab-name
                   (not (string= current-tab-name expected-tab-name)))
          ;; Switch to or create the appropriate tab
          (if (member expected-tab-name (my/get-all-tab-names))
              ;; Switch to existing tab
              (progn
                (tab-bar-switch-to-tab expected-tab-name)
                (switch-to-buffer magit-buffer))
            ;; Create new tab
            (tab-bar-new-tab)
            (tab-bar-rename-tab expected-tab-name)
            (add-to-list 'tabspaces-project-tab-map
                         (cons project-root expected-tab-name)))
          (setq-local project-current-directory project-root)))))

  (add-hook 'magit-post-display-buffer-hook #'my/magit-worktree-ensure-tabspace))

(with-eval-after-load 'magit-worktree
  (defun my/magit-worktree-status-with-tab (orig-fun worktree)
    "Switch to or create dedicated tabspace when visiting a worktree via 'Z g'."
    (let* ((worktree-list (if (consp worktree)
                              worktree
                            (cl-find-if (lambda (wt) (string= (car wt) worktree))
                                        (magit-list-worktrees))))
           (worktree-path (expand-file-name (car worktree-list)))
           (expected-tab-name
            (let ((default-directory worktree-path))
              (my/get-git-tab-name worktree-path)))
           (current-tab-name (my/get-current-tab-name)))

      ;; If already in correct tab or no expected name, just call original function
      (if (or (not expected-tab-name)
              (string= current-tab-name expected-tab-name))
          (funcall orig-fun worktree)
        ;; Handle tab switching/creation
        (if (member expected-tab-name (my/get-all-tab-names))
            ;; Tab exists: switch and show status
            (progn
              (tab-bar-switch-to-tab expected-tab-name)
              (let ((default-directory worktree-path))
                (magit-status-setup-buffer)))
          ;; Tab doesn't exist: create and visit
          (tab-bar-new-tab)
          (tab-bar-rename-tab expected-tab-name)
          (add-to-list 'tabspaces-project-tab-map
                       (cons worktree-path expected-tab-name))
          ;; Temporarily remove hook to prevent duplicate tab
          (remove-hook 'magit-post-display-buffer-hook
                       #'my/magit-worktree-ensure-tabspace)
          (unwind-protect
              (funcall orig-fun worktree)
            (add-hook 'magit-post-display-buffer-hook
                      #'my/magit-worktree-ensure-tabspace))))))

  (advice-add 'magit-worktree-status :around #'my/magit-worktree-status-with-tab)

  (defun my/magit-worktree-delete-and-clean-tab (orig-fun &rest args)
    "Delete worktree and clean up associated tab and buffers."
    (let ((current-tab-name (my/get-current-tab-name))
          (project-root (when default-directory
                          (expand-file-name default-directory))))

      (if (or (not current-tab-name)
              (string= current-tab-name "Default"))
          ;; No special tab handling needed
          (apply orig-fun args)

        ;; Delete worktree and manage tab
        (apply orig-fun args)

        ;; Clean up buffers associated with the deleted worktree
        (when project-root
          (my/kill-worktree-buffers project-root))

        ;; Forget project
        (when (and project-root (fboundp 'project-forget-project))
          (project-forget-project project-root))

        ;; Close the tab
        (when (and current-tab-name
                   (stringp current-tab-name)
                   (not (string-empty-p current-tab-name)))
          (let ((tab-bar-tab-prevent-close-functions nil))
            (tab-bar-close-tab-by-name current-tab-name)))

        ;; Refresh magit if still in a magit buffer
        (when (and (derived-mode-p 'magit-mode) (magit-gitdir))
          (magit-refresh)))))

  (advice-add 'magit-worktree-delete :around #'my/magit-worktree-delete-and-clean-tab))

;; ============================================================================
;; Tabspaces configuration
;; ============================================================================

(with-eval-after-load 'tabspaces
  (defun my/tabspaces-generate-descriptive-tab-name-advice
      (orig-fun project-path existing-tab-names)
    "Use 'project@branch' format for git repositories."
    (if (get-buffer "*tabspaces--placeholder*")
        ;; Don't interfere during session restoration
        (funcall orig-fun project-path existing-tab-names)
      (let ((default-directory project-path))
        (if (and (fboundp 'magit-toplevel)
                 (condition-case nil (magit-toplevel) (error nil)))
            ;; Git repository: use project@branch format
            (let ((tab-name (my/get-git-tab-name)))
              (add-to-list 'tabspaces-project-tab-map
                           (cons project-path tab-name))
              tab-name)
          ;; Not a git repo: use default behavior
          (funcall orig-fun project-path existing-tab-names)))))

  (advice-add 'tabspaces-generate-descriptive-tab-name :around
              #'my/tabspaces-generate-descriptive-tab-name-advice)

  (defun my/tabspaces-cleanup-placeholder-tabs ()
    "Close placeholder tabs left after session restoration.
Fixes tabspaces bug where placeholder tabs aren't automatically cleaned up."
    (dolist (tab (funcall tab-bar-tabs-function))
      (let ((tab-name (alist-get 'name tab)))
        (when (and tab-name
                   (stringp tab-name)
                   (string-prefix-p "*tabspaces--" tab-name))
          (let ((tab-bar-tab-prevent-close-functions nil))
            (tab-bar-close-tab-by-name tab-name))))))

  (advice-add 'tabspaces-restore-session :after
              (lambda (&rest _) (my/tabspaces-cleanup-placeholder-tabs))))

(defun my/tabspaces-kill-buffers-before-close (tab)
  "Kill buffers unique to a tab when closing it."
  (let ((name (cdr (assq 'name tab))))
    (when (and name
               (stringp name)
               (not (string= name "Default"))
               (not (get-buffer "*tabspaces--placeholder*")))
      (let ((tab-index (my/find-tab-index name)))
        (when tab-index
          (let* ((tabs (funcall tab-bar-tabs-function))
                 (buffers (tabspaces--buffer-list nil (1+ tab-index)))
                 (other-tabs-buffers
                  (cl-loop for idx from 0 below (length tabs)
                           unless (= idx tab-index)
                           append (tabspaces--buffer-list nil (1+ idx)))))
            (dolist (buf buffers)
              (when (buffer-live-p buf)
                (let ((buf-name (buffer-name buf)))
                  (unless (or (my/is-system-buffer-p buf-name)
                              (member buf other-tabs-buffers))
                    (kill-buffer buf)))))))))))

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
