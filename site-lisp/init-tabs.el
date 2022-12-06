(require 'init-custom)
(require 'init-funcs)

(message "Loading... init-tabs.el")
(use-package centaur-tabs
  :demand
  :config
  (setq centaur-tabs-style "box"
        centaur-tabs-height 32
        centaur-tabs-set-icons t
        centaur-tabs-gray-out-icons 'buffer
        centaur-tabs-set-bar 'under
        overline-margin 5
        x-underline-at-descent-line t
        centaur-tabs-adjust-buffer-order t)
  (centaur-tabs-headline-match)
  (centaur-tabs-enable-buffer-alphabetical-reordering)
  (centaur-tabs-group-by-projectile-project)
  (centaur-tabs-mode t)
  :bind
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward))

(provide 'init-tabs)
