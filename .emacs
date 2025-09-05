
;; install packages automatically on startup
(require 'package)
(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/"))

(require 'use-package)
(setq use-package-always-ensure t)

;; interface
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1))

;; customize file
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file :no-error-if-file-is-missing)

;; sudo apt-get install fonts-inconsolata -y
;; sudo fc-cache -fv
(if (eq system-type 'windows-nt) (set-frame-font "Consolas-14"))
(if (eq system-type 'gnu/linux) (set-frame-font "Inconsolata-14"))
(if (eq system-type 'darwin) (set-frame-font "Menlo-18"))

;; text editing
(fset 'yes-or-no-p 'y-or-n-p)

(require 'saveplace)
(require 'paren)
(require 'uniquify)

(global-font-lock-mode 1)
(global-subword-mode 1)
(savehist-mode 1)
(show-paren-mode 1)
(transient-mark-mode 1)

(prefer-coding-system 'utf-8)
(setq frame-title-format (list "%f"))
(setq history-length 1000)
(setq inhibit-splash-screen 1)
(setq recentf-max-menu-items 100)
(setq undo-limit (* 32 1024 1024))
(setq undo-outer-limit (* 16 1024 1024))
(setq undo-strong-limit (* 64 1024 1024))
(setq uniquify-buffer-name-style 'post-forward)
(setq-default save-place 1)
(setq-default show-trailing-whitespace 1)

(use-package neotree)
(global-set-key [f8] 'neotree-toggle)

;; tip: C-u C-x = to get a name of face under cursor and some additional info
;; tip: M-x customize-themes to browse themes
;; tip: select "linux console" color theme in terminal if background is grey

;; ivy, swiper, and counsel
(use-package ivy)
(use-package swiper)
(use-package counsel)

(ivy-mode 1)
(setq enable-recursive-minibuffers t)
(setq ivy-use-virtual-buffers t)
(setq ivy-height 20)
(setq ivy-wrap t)

(define-key ivy-minibuffer-map (kbd "C-j") 'ivy-immediate-done)
(define-key ivy-minibuffer-map (kbd "RET") 'ivy-alt-done)

;; make remembering key sequences a bit easier
(use-package which-key)
(which-key-mode)
(which-key-setup-side-window-right)

;; key bindings
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)

(global-set-key (kbd "M-;") 'windmove-up)
(global-set-key (kbd "M-'") 'windmove-down)
(global-set-key (kbd "M-,") 'windmove-left)
(global-set-key (kbd "M-.") 'windmove-right)
(global-set-key (kbd "M-/") 'other-window)

(global-set-key (kbd "C-x f") 'projectile-find-file)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-x C-r") 'counsel-recentf)

;; press C-q to query-replace once in swiper mode
(global-set-key (kbd "C-r") 'ivy-resume)
(global-set-key (kbd "C-s") 'swiper)

;; sudo apt-get install clangd
;; pip3 install 'python-lsp-server[all]'
;; cargo install rls
;; rustup component add rls
(use-package company)
(use-package eglot)
(use-package py-autopep8)
(use-package rust-mode)

(add-hook 'after-init-hook 'global-company-mode)

;; based on https://raw.github.com/google/styleguide/gh-pages/google-c-style.el
(defconst my-cc-style
  `((c-recognize-knr-p . nil)
    (c-basic-offset . 4)
    (indent-tabs-mode . nil)
    (c-comment-only-line-offset . 0)
    (c-hanging-braces-alist . ((defun-open after)
                               (defun-close before after)
                               (class-open after)
                               (class-close before after)
                               (inexpr-class-open after)
                               (inexpr-class-close before)
                               (namespace-open after)
                               (inline-open after)
                               (inline-close before after)
                               (block-open after)
                               (block-close . c-snug-do-while)
                               (extern-lang-open after)
                               (extern-lang-close after)
                               (statement-case-open after)
                               (substatement-open after)))
    (c-hanging-colons-alist . ((case-label)
                               (label after)
                               (access-label after)
                               (member-init-intro before)
                               (inher-intro)))
    (c-hanging-semi&comma-criteria
     . (c-semi&comma-no-newlines-for-oneline-inliners
        c-semi&comma-inside-parenlist
        c-semi&comma-no-newlines-before-nonblanks))
    (c-indent-comments-syntactically-p . t)
    (comment-column . 40)
    (c-indent-comment-alist . ((other . (space . 2))))
    (c-cleanup-list . (brace-else-brace
                       brace-elseif-brace
                       brace-catch-brace
                       empty-defun-braces
                       defun-close-semi
                       list-close-comma
                       scope-operator))
    (c-offsets-alist . ((arglist-intro . ++)
                        (func-decl-cont . ++)
                        (member-init-intro . ++)
                        (inher-intro . ++)
                        (comment-intro . 0)
                        (arglist-close . c-lineup-arglist)
                        (topmost-intro . 0)
                        (block-open . 0)
                        (inline-open . 0)
                        (substatement-open . 0)
                        (statement-cont
                         .
                         (,(when (fboundp 'c-no-indent-after-java-annotations)
                             'c-no-indent-after-java-annotations)
                          ,(when (fboundp 'c-lineup-assignments)
                             'c-lineup-assignments)
                          ++))
                        (label . -)
                        (case-label . 0)
                        (statement-case-open . +)
                        (statement-case-intro . +) ; case w/o {
                        (access-label . -)
                        (innamespace . 0)))))

(c-add-style "my-cc-style" my-cc-style)

(defun my-programming-modes-hook ()
  (setq-local linum-format (if window-system "%4d" "%4d "))

  (setq-default indent-tabs-mode nil)
  (setq tab-width 4)

  (setq flycheck-check-syntax-automatically '(mode-enabled save))

  (eglot-ensure)
  (setq eglot-report-progress nil)

  (local-set-key (kbd "TAB") 'company-indent-or-complete-common)
  (local-set-key (kbd "RET") 'newline-and-indent)

  (local-set-key (kbd "C-c S") 'eglot-list-connections)
  (local-set-key (kbd "C-c a") 'eglot-code-actions)
  (local-set-key (kbd "C-c c") 'company-complete)
  (local-set-key (kbd "C-c d") 'xref-find-definitions)
  (local-set-key (kbd "C-c f") 'eglot-format-buffer)
  (local-set-key (kbd "C-c g") 'counsel-imenu)
  (local-set-key (kbd "C-c i") 'eglot-find-implementation)
  (local-set-key (kbd "C-c j") 'eglot-find-declaration)
  (local-set-key (kbd "C-c r") 'eglot-rename)
  (local-set-key (kbd "C-c x") 'xref-find-references)

  (local-set-key (kbd "M-q") 'ff-find-other-file))

(defun my-c-c++-mode-hook ()
  (setq compilation-scroll-output 'first-error)
  (c-set-style "my-cc-style"))

(defun my-python-mode-hook ()
  (local-set-key (kbd "C-c <") 'python-indent-shift-left)
  (local-set-key (kbd "C-c >") 'python-indent-shift-right)
  (local-set-key (kbd "C-c C-f") 'py-autopep8))

(add-hook 'c-mode-common-hook 'my-programming-modes-hook)
(add-hook 'c-mode-hook 'my-c-c++-mode-hook)
(add-hook 'c++-mode-hook 'my-c-c++-mode-hook)
(add-hook 'lisp-mode-hook 'my-programming-modes-hook)
(add-hook 'python-mode-hook 'my-programming-modes-hook)
(add-hook 'python-mode-hook 'my-python-mode-hook)
(add-hook 'sh-mode-hook 'my-programming-modes-hook)
(add-hook 'rust-mode-hook 'my-programming-modes-hook)
