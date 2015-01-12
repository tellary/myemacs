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

;; No menus as [Steve Yegge advices](https://sites.google.com/site/steveyegge2/effective-emacs)
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; Russian input method for dvorak layout
;; Got from [here](http://www.emacswiki.org/emacs/WritingRussianWithDvorak)
(load-file "~/.emacs.d/cyrillic-dvorak.el")
(setq default-input-method "cyrillic-dvorak")
