;; fedora-license.el --- Fedora License related lisp -*- lexical-binding: t -*-
;;
;; Copyright (C) 2022 Akira TAGOH
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; (require 'fedora-license)
;; (global-set-key (kdb "C-c M-l" 'license-validate-map))
;;
(eval-after-load 'use-package
  '(use-package "posframe"))

(defvar license-validate-posframe-last-position nil)
(defvar license-validate-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "c") #'license-convert)
    (define-key keymap (kbd "v") #'license-validate)
    keymap))
(fset 'license-validate-map license-validate-map)


(defun license-validate-posframe-check-position ()
  (equal license-validate-posframe-last-position
         (setq license-validate-posframe-last-position
               (list (current-buffer) (buffer-modified-tick) (point)))))

(defun license-validate-posframe-hidehandler (_info)
  (not (license-validate-posframe-check-position)))

(defun license-validate (&optional string)
  "Validate license string if it follows SPDX License Identifier.

If no string is given to this function, tries to find out from a region
in current buffer.  Otherwise tries to ask a License text at minibuffer."
  (interactive)
  (let ((text (if string
                  string
                (if (use-region-p)
                    (buffer-substring (region-beginning) (region-end))
                  (read-from-minibuffer "License text: "))))
        (buf (get-buffer-create "* license-validate process *")))
    (license-validate-posframe-check-position)
    (make-process
     :name "license-validate"
     :buffer buf
     :command (list "license-validate" text)
     :sentinel '(lambda (process event)
                  (save-match-data
                    (if (use-region-p)
                        (deactivate-mark))
                    (cond ((string= "finished\n" event)
                           (message "Validating license... OK!"))
                          ((string-match "\\(deleted\\|exited\\|failed\\|signal\\|connection\\).*" event)
                           (message "Validating license... failed!!")
                           ))
                    (set-process-sentinel process nil)
                    (let* ((buf (get-buffer "* license-validate process *"))
                           (text (with-current-buffer buf
                                   (buffer-string))))
                      (kill-buffer buf)
                      (delete-process process)
                      (posframe-show "* license-validate *"
                                     :string text
                                     :foreground-color "red"
                                     :hidehandler #'license-validate-posframe-hidehandler)))))))

(defun license-convert (&optional string)
  "Convert Fedora-specific License Identifier to SPDX.

If no string is given to this function, tries to find out from a region
in current buffer.  Otherwise tries to ask a License tag at minibuffer.
If string is read from a region and it is valid License, will ask to
replace Fedora License Identifier to SPDX License Identifier."
  (interactive)
  (let ((text (if string
                  string
                (if (use-region-p)
                    (buffer-substring (region-beginning) (region-end))
                  (read-from-minibuffer "License text to convert: "))))
        (buf (get-buffer-create "* license-convert process *")))
    (license-validate-posframe-check-position)
    (make-process
     :name "license-convert"
     :buffer buf
     :command (list "license-fedora2spdx" text)
     :sentinel (lambda (process event)
                 (let* ((buf (get-buffer "* license-convert process *"))
                        (result (with-current-buffer buf
                                  (buffer-string))))
                   (save-match-data
                     (cond ((string= "finished\n" event)
                            (message "Converting license... done!")
                            (if (string-match "Warning" result)
                                (progn
				                  (message "Converting license... warning!")
                                  (posframe-show "* license-convert *"
                                                 :string result
                                                 :foreground-color "red"
                                                 :hidehandler #'license-validate-posframe-hidehandler))
                              (let ((r (replace-regexp-in-string "\n$" "" result)))
                                (if (use-region-p)
                                    (if (y-or-n-p (format "Do you want to replace '%s' to '%s'? " text r))
                                        (save-excursion
                                          (delete-region (region-beginning) (region-end))
                                          (deactivate-mark)
                                          (insert r))
                                      (message "Converting license... canceled"))
                                  (message r)))))
                           ((string-match "\\(deleted\\|exited\\|failed\\|signal\\|connection\\).*" event)
                            (message "Converting license... failed!!")
                            (posframe-show "* license-convert *"
                                           :string result
                                           :foreground-color "red"
                                           :hidehandler #'license-validate-posframe-hidehandler))))
                   (set-process-sentinel process nil)
                   (kill-buffer buf)
                   (delete-process process))))))

(provide 'fedora-license)
