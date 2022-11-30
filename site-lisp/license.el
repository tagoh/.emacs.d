;;
;;
;; (require 'license)
;; (global-set-key (kdb "C-c M-l" 'license-validate-map))
;;
(require 'cl)
(eval-after-load 'use-package
  '(use-package "posframe"))

(defvar license-validate-posframe-last-position nil)
(defvar license-validate-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "v") #'license-validate)
    (define-key keymap (kbd "c") #'license-convert)
    keymap))
(fset 'license-validate-map license-validate-map)

(defun license-validate-posframe-check-position ()
  (equal license-validate-posframe-last-position
         (setq license-validate-posframe-last-position
               (list (current-buffer) (buffer-modified-tick) (point)))))

(defun license-validate-posframe-hidehandler (_info)
  (not (license-validate-posframe-check-position)))

(defun license-validate (&optional string)
  (interactive)
  (let ((text (if string
                  string
                (if (use-region-p)
                    (buffer-substring (region-beginning) (region-end))
                  (read-from-minibuffer "License text: "))))
        (buf (get-buffer-create "* license-validate process *")))
    (make-process
     :name "license-validate"
     :buffer buf
     :command (list "license-validate" text)
     :sentinel '(lambda (process event)
                  (save-match-data
                    (cond ((string= "finished\n" event)
                           (message "Validating license... done!"))
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
                                     :hidehandler #'license-validate-posframe-hidehandler)))))
    (message "Validating license...")))

(defun license-convert (&optional string)
  (interactive)
  (let ((text (if string
                  string
                (if (use-region-p)
                    (buffer-substring (region-beginning) (region-end))
                  (read-from-minibuffer "License text to convert: "))))
        (buf (get-buffer-create "* license-convert process *")))
    (make-process
     :name "license-convert"
     :buffer buf
     :command (list "license-fedora2spdx" text)
     :sentinel (lexical-let ((text text))
                 (lambda (process event)
                   (let* ((buf (get-buffer "* license-convert process *"))
                          (result (with-current-buffer buf
                                    (buffer-string))))
                     (save-match-data
                       (cond ((string= "finished\n" event)
                              (message "Converting license... done!")
                              (if (string-match "Warning" result)
                                  (posframe-show "* license-convert *"
                                                 :string result
                                                 :hidehandler #'license-validate-posframe-hidehandler)
                                (if (use-region-p)
                                    (let ((r (replace-regexp-in-string "\n$" "" result)))
                                      (if (y-or-n-p (format "Do you want to replace '%s' to '%s'? " text r))
                                          (save-excursion
                                            (delete-region (region-beginning) (region-end))
                                            (insert r))
                                        (message "Converting license... canceled")))
                                  (message result))))
                             ((string-match "\\(deleted\\|exited\\|failed\\|signal\\|connection\\).*" event)
                              (message "Converting license... failed!!")
                              (posframe-show "* license-convert *"
                                             :string result
                                             :hidehandler #'license-validate-posframe-hidehandler))))
                     (set-process-sentinel process nil)
                     (kill-buffer buf)
                     (delete-process process)))))))

(provide 'license)
