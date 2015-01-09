(setq ring-bell-function 'ignore)

;; Browsing search history doesn't move to minibuffer to edit search string
(setq search-ring-update t)

;; Initialize "Plan 9 smart shell" as described
;; [here](http://www.masteringemacs.org/article/complete-guide-mastering-eshell)
(require 'eshell)
(require 'em-smart)
(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)
(add-hook 'eshell-mode-hook 'eshell-smart-initialize)
