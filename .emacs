

;; install packages automatically on startup
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; interface
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1))

(if (eq system-type 'windows-nt) (set-frame-font "Consolas-14"))
(if (eq system-type 'gnu/linux) (set-frame-font "Inconsolata-14"))

;; text editing
(fset 'yes-or-no-p 'y-or-n-p)

(require 'saveplace)
(require 'paren)
(require 'uniquify)
(require 'recentf)
(use-package expand-region)

(global-font-lock-mode 1)
(global-subword-mode 1)
(recentf-mode 1)
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

;; tip: C-u C-x = to get a name of face under cursor and some additional info
;; tip: M-x customize-themes to browse themes
(add-to-list 'custom-theme-load-path "~/etc/darkokai")
(setq darkokai-mode-line-padding 1)
(load-theme 'darkokai t)

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

;; key bindings
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)

(global-set-key (kbd "C-<down>") 'windmove-down)
(global-set-key (kbd "C-<left>") 'windmove-left)
(global-set-key (kbd "C-<right>") 'windmove-right)
(global-set-key (kbd "C-<up>") 'windmove-up)
(global-set-key (kbd "M-[ a") 'windmove-up)
(global-set-key (kbd "M-[ b") 'windmove-down)
(global-set-key (kbd "M-[ c") 'windmove-right)
(global-set-key (kbd "M-[ d") 'windmove-left)

(global-set-key (kbd "M-=") 'er/expand-region)

(global-set-key (kbd "C-x f") 'projectile-find-file)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-x C-r") 'counsel-recentf)

(global-set-key (kbd "C-r") 'ivy-resume)
(global-set-key (kbd "C-s") 'swiper)

;; org mode
(require 'org)
(define-prefix-command 'my-org-mode-map)
(global-set-key (kbd "C-o") 'my-org-mode-map)
(global-set-key (kbd "C-o a") 'org-agenda)
(global-set-key (kbd "C-o c") 'org-capture)
(global-set-key (kbd "C-o l") 'org-store-link)
(global-set-key (kbd "C-o t") 'org-todo-list)

(setq org-directory "~/org")
(setq org-default-notes-file (concat org-directory "/capture.org"))
(setq org-list-description-max-indent 5)
(setq org-adapt-indentation nil)
(setq org-log-done 'time)

;; sudo apt install git cmake libclang-dev clang clang-format exuberant-ctags global libncurses5-dev liblua5.3-dev libssl-dev python\*-virtualenv python\*-\*pep8
;; cd ~/etc/rtags && cmake . && make
;; cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON ./ && ~/etc/rtags/bin/rc -J .
;; tip: add SET(CMAKE_CXX_STANDARD 17) in CMakeLists.txt if you see strange errors.
(require 'cl)
(require 'cc-mode)

(use-package cmake-mode)

(use-package clang-format)
(use-package projectile)
(use-package counsel-projectile)
(use-package magit)

(use-package rtags)
(use-package ggtags)

(use-package anaconda-mode)

(use-package realgud)
(use-package popup)

(use-package company)
(use-package company-anaconda)
(use-package company-rtags)

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

(global-company-mode 1)
(setq company-minimum-prefix-length 3)
(setq company-idle-delay 0.1)

(setq rtags-path "~/etc/rtags/bin/")
(add-to-list 'load-path "~/etc/rtags/src/")

(setq whitespace-style '(face trailing tabs))
(global-whitespace-mode 1)

(projectile-global-mode 1)
(setq projectile-completion-system 'ivy)
(global-set-key (kbd "<f7>") 'projectile-compile-project)
(global-set-key (kbd "M-c") 'projectile-command-map)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cc\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

(defun my-programming-modes-hook ()
  (setq-local linum-format (if window-system "%4d" "%4d "))

  (setq-default indent-tabs-mode nil)
  (setq tab-width 2)

  (local-set-key (kbd "TAB") 'company-indent-or-complete-common)
  (local-set-key (kbd "RET") 'newline-and-indent)

  (ggtags-mode 1)
  (push 'company-gtags company-backends)

  (local-set-key (kbd "M-/") 'company-complete)
  (local-set-key (kbd "C-c c") 'comment-region)
  (local-set-key (kbd "C-c g") 'counsel-git-grep)
  (local-set-key (kbd "C-c t") 'projectile-regenerate-tags)
  (local-set-key (kbd "C-c u") 'uncomment-region)
  (local-set-key (kbd "M-q") 'ff-find-other-file))

(defun my-c-c++-mode-hook ()
  (setq compilation-scroll-output 'first-error)
  (c-set-style "my-cc-style")

  (setq rtags-completions-enabled 1)
  (setq rtags-display-current-error-as-tooltip 1)
  (setq rtags-display-result-backend 'ivy)
  (rtags-start-process-unless-running)
  (rtags-diagnostics)
  (push 'company-rtags company-backends)

  (setq company-minimum-prefix-length 5)
  (setq company-idle-delay 0.5)
  (setq company-rtags-begin-after-member-access 1)

  (local-set-key (kbd "C-c d") 'realgud:gdb)
  (local-set-key (kbd "C-c f") 'clang-format)
  (local-set-key (kbd "C-c i") 'rtags-print-symbol-info)
  (local-set-key (kbd "C-c j") 'rtags-find-symbol-at-point)
  (local-set-key (kbd "C-c m") 'rtags-imenu)
  (local-set-key (kbd "C-c n") 'rtags-next-match)
  (local-set-key (kbd "C-c p") 'rtags-previous-match)
  (local-set-key (kbd "C-c v") 'rtags-find-virtuals-at-point)
  (local-set-key (kbd "C-c x") 'rtags-find-references-at-point))

(defun my-python-mode-hook ()
  (anaconda-mode 1)

  (setq python-indent-offset 4)
  (setq tab-width 4)

  (push 'company-anaconda company-backends)

  (local-set-key (kbd "C-c a") 'anaconda-mode-find-assignments)
  (local-set-key (kbd "C-c d") 'realgud:pdb)
  (local-set-key (kbd "C-c f") 'py-autopep8)
  (local-set-key (kbd "C-c i") 'anaconda-mode-show-doc)
  (local-set-key (kbd "C-c j") 'anaconda-mode-find-definitions)
  (local-set-key (kbd "C-c p") 'anaconda-mode-go-back)
  (local-set-key (kbd "C-c x") 'anaconda-mode-find-references))

(add-hook 'c-mode-common-hook 'my-programming-modes-hook)
(add-hook 'c-mode-hook 'my-c-c++-mode-hook)
(add-hook 'c++-mode-hook 'my-c-c++-mode-hook)
(add-hook 'lisp-mode-hook 'my-programming-modes-hook)
(add-hook 'python-mode-hook 'my-programming-modes-hook)
(add-hook 'python-mode-hook 'my-python-mode-hook)
(add-hook 'sh-mode-hook 'my-programming-modes-hook)
