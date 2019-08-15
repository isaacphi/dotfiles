;;;; init --- My emacs config

;;; Commentary:
;;; based on https://www.sandeepnambiar.com/my-minimal-emacs-setup/

;;; Code:

(setq user-full-name "phil"
      user-mail-address "isaac.phil@gmail.com")

;; memory limit for garbage collection
(setq gc-cons-threshold 50000000)
;; large file warning size
(setq large-file-warning-threshold 100000000)

;; Encoding. This shouldn't be necessary
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; package management
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

;; hide visual clutter
(menu-bar-mode -1)
(when (display-graphic-p)
  (scroll-bar-mode -1))
(tool-bar-mode -1)

;; highlight current line
(global-hl-line-mode +1)
;; line numbers
(line-number-mode +1)
(when (display-graphic-p)
  (global-display-line-numbers-mode 1))
(column-number-mode t)
;; file size in mode line
(size-indication-mode t)

;; hide startup screen
(setq inhibit-startup-screen t)

;; display full file name in mode line
(setq frame-title-format
      '((:eval (if (buffer-file-name)
       (abbreviate-file-name (buffer-file-name))
       "%b"))))

;; scroll behaviour
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; font
(set-frame-font "Hack 12" nil t)

;; disable bell
(use-package doom-themes
  :ensure t
  :config
  (doom-themes-visual-bell-config))

;; theme
(use-package solarized-theme
  :ensure t
  :config
  (load-theme 'solarized-light t))

;; mode line
(use-package smart-mode-line
  :ensure t
  :config
  (add-hook 'after-init-hook 'sml/setup))

;; put temporary files in one directory
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; replace yes/no with y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; auto reload files if edited elsewhere
(global-auto-revert-mode t)

;; tabs are 4 spaces
(setq-default tab-width 4
              indent-tabs-mode nil)

;; remove extra whitespace on save
(add-hook 'before-save-hook 'whitespace-cleanup)

;; hide minor modes from mode line
(use-package diminish
  :ensure t)

;; parenthesis management
(use-package smartparens
  :ensure t
  :if window-system
  :diminish smartparens-mode
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode 1)
    (show-paren-mode t)))

;; expand region selection
(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

;; useful hotkeys
(use-package crux
  :ensure t
  :bind
  ("C-k" . crux-smart-kill-line)
  ("C-c n" . crux-cleanup-buffer-or-region)
  ("C-c f" . crux-recentf-find-file)
  ("C-a" . crux-move-beginning-of-line))

;; hotkey suggestions
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode +1))

;; jump to char in view
(use-package avy
  :ensure t
  :bind
  ("C-_" . avy-goto-char)
  :config
  (setq avy-background t))

;; auto complete
(use-package company
  :ensure t
  :diminish company-mode
  :config
  (add-hook 'after-init-hook #'global-company-mode))

;; syntax checking
(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

;; git
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
         ("C-M-g" . magit-status)))

;; helm: dwim autocomplete
(use-package helm
  :ensure t
  :defer 2
  :bind
  ("M-x" . helm-M-x)
  ("C-x C-f" . helm-find-files)
  ("M-y" . helm-show-kill-ring)
  ("C-x b" . helm-mini)
  :config
  (require 'helm-config)
  (helm-mode 1)
  (setq helm-split-window-inside-p t
    helm-move-to-line-cycle-in-source t)
  (setq helm-autoresize-max-height 0)
  (setq helm-autoresize-min-height 45)
  (helm-autoresize-mode 1)
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
  (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
  (eval-when-compile
    (defvar helm-boring-file-regexp-list)
    (defvar helm-ff-skip-boring-files))
  (helm-mode 1)
  (add-to-list 'helm-boring-file-regexp-list "\\.~$")
  (add-to-list 'helm-boring-file-regexp-list "#.+#$")
  (setq helm-ff-skip-boring-files t)
  )

;; Manage projects
(use-package projectile
  :ensure t
  :diminish projectile-mode
  :bind
  (("C-x p f" . helm-projectile-find-file)
   ("M-p"     . helm-projectile)
   ("C-S-f"   . helm-projectile-ag)
   ("C-x p h" . helm-projectile)
   ("C-x p p" . helm-projectile-switch-project)
   ("C-x p s" . projectile-save-project-buffers))
  :config
  (projectile-mode +1)
  (setq projectile-indexing-method 'hybrid)
  ;; hack to not fail on parse errors. needed for thm
  (setq projectile-git-submodule-command "git submodule --quiet foreach 'echo $path' 2>/dev/null | tr '\\n' '\\0'")
)
(use-package helm-projectile
  :if window-system
  :ensure t
  :config
  (helm-projectile-on))

;; scrollbar
(use-package yascroll
  :ensure t
  :init
  (require 'cl)
  :config
  (global-yascroll-bar-mode))

;; Comment current line override
(defun comment-dwim-line (&optional arg)
  "Replacement for the 'comment-dwim' command (ARG)."
  (interactive "*P")
  (comment-normalize-vars)
  (if (not (region-active-p))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))
(global-set-key (kbd "C-;") 'comment-dwim-line)

;; tree
(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  ;; (global-set-key [f8] 'treemacs-select-window)
  (progn
    (setq treemacs-collapse-dirs                 (if (executable-find "python3") 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-follow-delay             0.2
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-desc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5)

    (treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null (executable-find "python3"))))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ([f8]        . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

;; markdown
;; disable M-p because it conflicts with projectile
(use-package markdown-mode
  :ensure t
  :config
  (eval-when-compile (defvar markdown-mode-map))
  (add-hook 'markdown-mode-hook (lambda () (define-key markdown-mode-map (kbd "M-p") nil))))

;; named frames for projects
;; open projects in new frames
(use-package nameframe
  :ensure t)
(use-package nameframe-projectile
  :ensure t
  :config
  (nameframe-projectile-mode t)
  (global-set-key (kbd "M-P") 'nameframe-switch-frame))

;; window resize and move
;; shift + arrow to switch moves
(use-package windresize
  :ensure t)
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; dumb jump (go to defn)
(use-package dumb-jump
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g p" . dumb-jump-back)
         ("M-g j" . dumb-jump-go)
         ("M-g i" . dumb-jump-go-prompt))
  :config (setq dumb-jump-selector 'helm)
  :ensure t)

;; subword movement
(global-subword-mode)

;; imenu (list of important things in current buffer)
;;
(global-set-key (kbd "M-i") 'imenu)

;; yaml
(use-package yaml-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yml.j2\\'" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml.j2\\'" . yaml-mode)))

;; term
(use-package helm-mt
  :ensure t)
(use-package multi-term
  :ensure t
  :config
  (setq multi-term-program "/bin/zsh")
  :bind(("C-S-t" . helm-mt)))

;; diff in sidebar
(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode 1)
  :bind (("C-}" . diff-hl-next-hunk)
         ("C-{" . diff-hl-previous-hunk)))

;; move text
;; M-<up> M-<down>
(use-package move-text
  :ensure t
  :config
  (move-text-default-bindings))

;; transpose-frame
(use-package transpose-frame
  :ensure t)

;; mac settings
(when (memq window-system '(mac ns))
  (setq mac-command-modifier 'meta)
  (add-to-list 'exec-path "/usr/local/bin/"))

;; misc mappings
;;
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-h") 'backward-kill-word)
;; unset keys
(dolist (key '("\M-c" "\C-z"))
  (global-unset-key key))

;; daemon mode
;;(require 'server)
;;(if (not (server-running-p)) (server-start))

;; NOTES
;;
;; there's a spotify plugin!
;; C-x C-e excecutes previous lisp command
;; C-x z is redo
;; M-x customize-group lets you customize plugins
;; use helm-mark-ring and helm-all-mark-rings, also use helm-bookmarks
;; (setq debug-on-error t) ;; Display errors



;; OLD STUFF

;; ;; syntax checks
;; ;;
;; ;; mostly from http://codewinds.com/blog/2015-04-02-emacs-flycheck-eslint-jsx.html
;; (require 'flycheck)
;; (add-hook 'after-init-hook #'global-flycheck-mode)
;; ;; disable jshint since we prefer eslint checking
;; (setq-default flycheck-disabled-checkers
;;   (append flycheck-disabled-checkers
;;     '(javascript-jshint)))
;; (flycheck-add-mode 'javascript-eslint 'web-mode)
;; (setq-default flycheck-temp-prefix ".flycheck")
;; ;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
;; (defun my/use-eslint-from-node-modules ()
;;   "Use eslint in the local node_modules."
;;   (let* ((root (locate-dominating-file
;;                 (or (buffer-file-name) default-directory)
;;                 "node_modules"))
;;          (eslint (and root
;;                       (expand-file-name "node_modules/eslint/bin/eslint.js"
;;                                         root))))
;;     (when (and eslint (file-executable-p eslint))
;;       (setq-local flycheck-javascript-eslint-executable eslint))))
;; (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)
;; (add-to-list 'auto-mode-alist '("\\.js$" . web-mode))

;; ;; python
;; ;;
;; (elpy-enable)

;; ;; javascript
;; ;;
;; (require 'js2-mode)
;; (require 'js2-refactor)
;; (require 'xref-js2)
;; (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
;; (add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
;; (add-hook 'js2-mode-hook #'js2-refactor-mode)
;; (js2r-add-keybindings-with-prefix "C-c C-r")
;; (define-key js2-mode-map (kbd "C-k") #'js2r-kill)
;; ;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so unbind it.
;; (define-key js-mode-map (kbd "M-.") nil)
;; (add-hook 'js2-mode-hook (lambda ()
;;   (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))
;; (setq-default indent-tabs-mode nil)
;; (setq-default js2-basic-offset 4)
;; (setq-default js-indent-level 4)
;; (setq-default sgml-basic-offset 4)
;; (eval-after-load 'rjsx-mode
;;     '(progn
;;        (add-hook 'rjsx-mode-hook #'add-node-modules-path)
;;        (add-hook 'after-save-hook 'eslint-fix nil t)))
;; (eval-after-load 'js2-mode
;;     '(add-hook 'js2-mode-hook (lambda () (add-hook 'after-save-hook 'eslint-fix nil t))))


;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("d91ef4e714f05fff2070da7ca452980999f5361209e679ee988e3c432df24347" "0598c6a29e13e7112cfbc2f523e31927ab7dce56ebb2016b567e1eff6dc1fd4f" "a3fa4abaf08cc169b61dea8f6df1bbe4123ec1d2afeb01c17e11fdc31fc66379" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" default)))
 '(package-selected-packages
   (quote
    (transpose-frame markdown-mode helm-mt multi-term avy which-key crux smartparens diminish smart-mode-line-powerline-theme doom-themes use-package yascroll yaml-imenu xref-js2 windresize web-mode spotify solarized-theme smooth-scrolling smooth-scroll rust-mode rjsx-mode prettier-js neotree nameframe-projectile
                     (custom-set-faces)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
