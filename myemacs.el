;; No noise from emacs
(setq ring-bell-function 'ignore)

;; Use java mode for Gosu files
(add-to-list 'auto-mode-alist '("\\.gs$" . java-mode))

(load-theme 'zenburn)

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

;; From [here](http://stackoverflow.com/a/27908343/1060693)
(defun eshell/clear ()
  "Clear terminal"
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))


;; [groovy-emacs-mode](http://groovy.codehaus.org/Emacs+Groovy+Mode).
(load "~/.emacs.d/groovy-mode/groovy-mode.el")
(add-to-list 'auto-mode-alist '("\\.gradle$" . groovy-mode))
(add-to-list 'auto-mode-alist '("\\.groovy$" . groovy-mode))

;; http://ergoemacs.org/emacs/emacs_package_system.html
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  )

;; Promised to be faster than the default `scp`
;; as said [here](http://www.emacswiki.org/emacs/TrampMode)
(setq tramp-default-method "ssh")

;; http://thinkinghard.com/software/rules-editing-mode/index.html
(load "~/.emacs.d/drools-mode.el")
(add-to-list 'auto-mode-alist '("\\.drl$" . drools-mode))

;; Make auto-revert mode quiet
(setq auto-revert-verbose nil)

(require 'column-marker)
(add-hook 'prog-mode-hook 
          (lambda () 
            (interactive) 
            (column-marker-1 80)
            (column-marker-3 120)))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(setq column-number-mode t)

(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))
