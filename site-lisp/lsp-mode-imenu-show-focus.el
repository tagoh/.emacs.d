(defun my-imenu-get-nearest ()
    (interactive)
    "Returns `nil' or `(name . marker)' pair of the nearest item on `imenu'"

    ;; Thanks: https://emacs.stackexchange.com/questions/30673/next-prev-imenu-item-function
    (imenu--make-index-alist)

    (let ((alist imenu--index-alist)
          (minoffset (point-max))
          offset pair mark imstack result)

        ;; Element = ("name" . marker)
        ;;         | ("submenu" ("name" . marker) ... )
        (while (or alist imstack)
            (if alist
                    (progn
                        (setq pair (car-safe alist)
                              alist (cdr-safe alist))
                        (cond
                         ((atom pair)) ;; Skip anything not a cons.

                         ((imenu--subalist-p pair)
                          (setq imstack   (cons alist imstack)
                                alist     (cdr pair)))

                         ((number-or-marker-p (setq mark (cdr pair)))
                          ;; REMARK: Allow zero, search direction = -1 (up)
                          (when (>= (setq offset (* (- mark (point)) -1)) 0)
                              (when (< offset minoffset) ;; Find the closest item.
                                  (setq minoffset offset
                                        result pair))))))

                ;; pop
                (setq alist   (car imstack)
                      imstack (cdr imstack))))

        result))

(defun my-lsp-imenu-update-focus ()
    (interactive)
    "Move the `*lsp-ui-imenu*' buffer's point to the current item."
    (let ((window (get-buffer-window "*lsp-ui-imenu*")))
        (when window

            ;; get the name of the current item
            (let ((pair (my-imenu-get-nearest)))
                (when pair
                    (let ((pattern (concat "┃ " (car pair) "$")))

                        ;; search in the imenu buffer
                        (with-selected-window window
                            (goto-char 0)
                            (re-search-forward pattern nil 'no-error)

                            (move-beginning-of-line 1)
                            (scroll-right 1000)

                            (hl-line-mode 1)
                            (hl-line-highlight))))))))

(add-hook 'post-command-hook #'my-lsp-imenu-update-focus)
