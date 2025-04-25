(setenv "TERM" "dumb")
;; No noise from emacs
(setq ring-bell-function 'ignore)

;; Use java mode for Gosu files
(add-to-list 'auto-mode-alist '("\\.gs$" . java-mode))

(defun my-indent-setup ()
  (c-set-offset 'arglist-intro '+)
  (c-set-offset 'arglist-close nil)
  (c-set-offset 'case-label '+)
  (c-set-offset 'arglist-cont-nonempty '+)
  (c-set-offset 'topmost-intro-cont '++)
  (c-set-offset 'statement-cont '+)
  )
(add-hook 'java-mode-hook 'my-indent-setup)

(setq myemacs-dir (file-name-directory load-file-name))
(add-to-list 'custom-theme-load-path myemacs-dir)
(load-theme 'zenburn t)

(load "meta-bindings/meta-bindings")

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
(load "cyrillic-dvorak")
(setq default-input-method "cyrillic-dvorak")

;; From [here](http://stackoverflow.com/a/27908343/1060693)
(defun eshell/clear ()
  "Clear terminal"
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))


;; [groovy-emacs-mode](http://groovy.codehaus.org/Emacs+Groovy+Mode).
(add-to-list 'auto-mode-alist '("\\.gradle$" . groovy-mode))
(add-to-list 'auto-mode-alist '("\\.groovy$" . groovy-mode))

;; http://ergoemacs.org/emacs/emacs_package_system.html
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Promised to be faster than the default `scp`
;; as said [here](http://www.emacswiki.org/emacs/TrampMode)
(setq tramp-default-method "ssh")

;; http://thinkinghard.com/software/rules-editing-mode/index.html
(load "drools-mode")
(add-to-list 'auto-mode-alist '("\\.drl$" . drools-mode))

;; Make auto-revert mode quiet
(setq auto-revert-verbose nil)

(load "column-marker")
(add-hook 'prog-mode-hook 
          (lambda () 
            (interactive) 
            (column-marker-1 80)
            (column-marker-3 120)))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default nxml-child-indent 4)
(setq-default nxml-attribute-indent 8)

(setq column-number-mode t)

(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))

(setq c-basic-offset 4)

(add-hook 'haskell-mode-hook
          (lambda()
            (haskell-indentation-mode)))

(setq dabbrev-case-replace case-replace)
(setq dabbrev-case-distinction nil)

;; Set it to nil to halt display redraw if any input event is detected.
;; This is an "old" performance optimization irrelevant
;; this days as described 
;; [here](https://www.masteringemacs.org/article/improving-performance-emacs-display-engine),
;; but I still want to use it.
(setq redisplay-dont-pause nil)

;; Indent long arguments properly
;; http://stackoverflow.com/a/6952408/1060693
(c-set-offset 'arglist-intro '+)


;; Copied from [github](https://github.com/atomontage/xterm-color)
;; from commit 1bc4ddb0e1bf7562cbf4b6b3bdd2ce3f9b596b39
(load "xterm-color")
;; Setup somewhat zenburn colors for xterm-colors
(setq xterm-color-names
  ["#192033"    ; black
   "#E89393"    ; red
   "#9ECE9E"    ; green
   "#F0DFAF"    ; yellow
   "#8CD0D3"    ; blue
   "#C0BED1"    ; magenta
   "#DFAF8F"    ; cyan
   "#EFEFEF"]   ; white
  )
(setq xterm-color-names-bright
  ["#192033"    ; black
   "#E89393"    ; red
   "#9ECE9E"    ; green
   "#F0DFAF"    ; yellow
   "#8CD0D3"    ; blue
   "#C0BED1"    ; magenta
   "#DFAF8F"    ; cyan
   "#EFEFEF"]   ; white
  )

;; comint install
(progn (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter)
       (setq comint-output-filter-functions (remove 'ansi-color-process-output comint-output-filter-functions))
       (setq font-lock-unfontify-region-function 'xterm-color-unfontify-region))

;; You can also use it with eshell (and thus get color output from system ls):
(add-hook 'eshell-mode-hook
          (lambda ()
            (setq xterm-color-preserve-properties t)))

(add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
(setq eshell-output-filter-functions (remove 'eshell-handle-ansi-color eshell-output-filter-functions))

;; A workaround as emacs sets to "dumb" in `normal-top-level` function
(add-hook 'eshell-mode-hook (lambda() (setenv "TERM" "xterm-256color")))

(package-install 'yasnippet)
(require 'yasnippet)
(add-hook 'prog-mode-hook 'yas-minor-mode)
(setq yas-snippet-dirs
      (list
       (concat
        (file-name-directory load-file-name)
        "snippets")
       yas-installed-snippets-dir))
(yas-reload-all)

(add-hook 'eshell-mode-hook
          (lambda ()
            (add-to-list
             'eshell-command-aliases-list
             '("gradle" "TERM=dumb gradle $*"))
            (add-to-list
             'eshell-command-aliases-list
             '("./gradlew" "TERM=dumb ./gradlew $*"))))

(defun set-window-width (n)
  "Set the selected window's width."
  (window-resize (selected-window) (- n (window-width)) t))

(defun set-80-columns ()
  "Set the selected window to 80 columns."
  (interactive)
  (set-window-width 80))

(add-to-list 'yank-excluded-properties 'xterm-color)
(add-to-list 'yank-excluded-properties 'face)

(defun main-frame-27-inch()
  (interactive)
  (set-frame-font "Monospace 14"))
(defun second-frame-27-inch ()
  (interactive)
  (let ((f (make-frame)))
    (set-frame-font "Monospace 14" nil (list f))
    f))
(defun second-frame-24-inch ()
  (interactive)
  (let ((f (make-frame)))
    (set-frame-font "Monospace 12" nil (list f))))
(defun main-frame-mbp15()
  (interactive)
  (set-face-attribute 'default (selected-frame) :height 150))
(defun second-frame-mbp15 ()
  (interactive)
  (let ((f (make-frame)))
    (set-face-attribute 'default f :height 150)))

(defun clear-colors ()
    (interactive)
    (remove-text-properties (region-beginning) (region-end) '(xterm-color nil face nil)))

(defun quote-line-break ()
  "Function to split pasted text into lines with quotation on tokens found"
  (interactive)
  (let* (
        (linestart (eq (point) (line-beginning-position)))
        (emptyline (and linestart (eq (point) (line-end-position)))))
    (if emptyline
        (progn
          (insert "> ")
          (next-line)
          (beginning-of-line)
          (quote-line-break))
      (unless linestart
        (unless (eq (char-before) (string-to-char " "))
            (backward-word))
        (newline))
      (insert "> "))))

(defun add-tags (dir)
  (add-to-list 'tags-table-list dir))

(defun new-eshell (name dir)
  (let (d default-directory)
    (cd dir)
  (eshell)
  (rename-buffer name)
  (unless (not d) (cd d))))

(package-install 'markdown-mode)
(require 'markdown-mode)
(add-hook 'markdown-mode-hook
          (lambda () (yas-minor-mode)))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(define-key markdown-mode-map "\t" 'indent-relative)
(define-key markdown-mode-map "\M-\r" 'markdown-insert-list-item)
;; Making `comment-style` buffer local by supplying `t`
;; to the `LOCAL` argument of `add-hook` doesn`t work.
;; The hook isn't executed then.
(add-hook 'markdown-mode-hook
          (lambda()
            (setq comment-start ">")
            (setq comment-end ">")
            (setq comment-continue ">")
            (make-local-variable 'comment-style)
            (setq comment-style 'extra-line)))
(add-hook 'markdown-mode-hook
          (lambda()
            (visual-line-mode)))

(package-install 'dockerfile-mode)

(put 'upcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)

;; The LANG variable is necessary to let hunspell know
;; a dictionary to use as default.
(setenv "LANG" "en_US.UTF-8")
(setq ispell-program-name "hunspell")

;; Dirty hacks to init dictionaries manually
(setq ispell-local-dictionary "english")
(setq ispell-local-dictionary-alist
      '(("english" "[[:alpha:]]" "[^[:alpha:]]" "[']"
         nil ("-d" "en_US") nil utf-8)
        ("russian" "[А-Яа-я]" "[^А-Яа-я]" "[']"
         nil ("-d" "ru_RU") nil utf-8)
        ))
(condition-case nil
    (ispell-change-dictionary "english")
  (error nil))


(setenv "SHELL" "/bin/bash")

;; Enable xclip support
(package-install 'xclip)
(xclip-mode)

(package-install 'haskell-mode)
(package-install 'haskell-snippets)

(org-mode)

(load "etags-select")

(package-install 'groovy-mode)
(package-install 'gradle-mode)

(package-install 'nodejs-repl)

(package-install 'typescript-mode)

(package-install 'nodejs-repl)

(package-install 'typescript-mode)

;; (package-install 'purescript-mode)

;; As suggested [here](https://github.com/epost/psc-ide-emacs/issues/60#issuecomment-228606407)
(add-hook 'purescript-mode-hook
 (lambda ()
  (turn-on-purescript-indentation)))

(setq compilation-ask-about-save nil)

(add-to-list 'auto-mode-alist '("\\.libsonnet\\'" . jsonnet-mode))


(defun copy-file-name-to-clipboard--filename (full-filename copy-full-path)
  (when full-filename
    (if copy-full-path full-filename
      (if (equal "/" (substring full-filename -1))
          (file-name-nondirectory (substring full-filename 0 -1))
        (file-name-nondirectory full-filename)
        )
      )
    )
  )

;; Derived
;; [here](http://emacsredux.com/blog/2013/03/27/copy-filename-to-the-clipboard/)
(defun copy-filename (arg)
  "Copy the current buffer file name to the clipboard."
  (interactive "P")
  (let ((copy-full-path arg)
        (full-filename
         (if (member major-mode '(dired-mode eshell-mode shell-mode))
             default-directory
           (buffer-file-name))))
    (let ((filename (copy-file-name-to-clipboard--filename
                     full-filename copy-full-path)))
      (when filename
        (kill-new filename)
        (message
         "Copied buffer file name '%s' to the clipboard." filename)))
    )
  )

(defalias 'cpfn 'copy-filename)

; Emacs workspace (ws-*) functions
(defun ws-open-eshell (buffer-name location)
  (if (not (get-buffer buffer-name))
      (progn
        (eshell)
        (insert location)
        (eshell-send-input)
        (rename-buffer buffer-name))))

(defun ws-project (buffer-name location)
  (message "ws-project %s %s" buffer-name location)
  (ws-open-eshell buffer-name location)
  (let ((file (concat location "/TAGS")))
    (and
     (file-exists-p file)
     (visit-tags-table-buffer file))))

(defun ws-visit-tags-table (f)
  (message "ws-visit-tags-table %s" f)
  (visit-tags-table f)
  )

; Doing this, because `package-install` stopped working
(let ((php-mode-autoloads (concat
                           (file-name-directory load-file-name)
                           "php-mode/php-mode-autoloads.el")))
  (if (file-exists-p php-mode-autoloads)
      (load php-mode-autoloads)
    (error
     (format "Git Init 'php-mode' submodule and run 'make'. %s file doesn't exist"
             php-mode-autoloads))
    )
  )

(setq haskell-interactive-popup-errors nil)

(use-package daml-mode
  :mode "\\.daml\'"
  :config
  (require 'lsp-daml)
  )
(require 'daml-mode)
(add-hook 'daml-mode-hook
          (lambda ()
            (display-line-numbers-mode)))

(package-install 'avy)
(require 'avy)

;; https://github.com/skuro/puml-mode
;; Using a deprecated one as it supports Emacs 24
(load "puml-mode")
(add-to-list 'auto-mode-alist '("\\.pu$" . puml-mode))

(load "tasktags/task-tags-mode.el")

(server-start)
(setenv "EDITOR" (concat "emacsclient -s " server-socket-dir "/server"))

(setenv "PAGER" "cat")
