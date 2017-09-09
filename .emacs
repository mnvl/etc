
;; global settings
(require 'saveplace)
(require 'paren)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(fset 'yes-or-no-p 'y-or-n-p)
(global-font-lock-mode 1)
(setq gdb-many-windows 1)
(setq inhibit-splash-screen 1)
(setq transient-mark-mode 1)
(setq-default save-place 1)
(show-paren-mode 1)
(setq frame-title-format (list "%f"))
(global-subword-mode 1)

(setq undo-limit (* 32 1024 1024))
(setq undo-strong-limit (* 64 1024 1024))
(setq undo-outer-limit (* 16 1024 1024))

;; melpa
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;; coding system and input method
(prefer-coding-system 'utf-8)

;; fonts
(if (eq system-type 'windows-nt) (set-frame-font "Consolas-14"))
(if (eq system-type 'gnu/linux) (set-frame-font "Inconsolata-14"))

;; remember recent files
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 100)

;; tip: C-u C-x = to get a name of face under cursor and some additional info
;; tip: M-x customize-themes to browse themes
(add-to-list 'custom-theme-load-path "~/etc/darkokai")
(setq darkokai-mode-line-padding 1)
(load-theme 'darkokai t)

;; programming modes
(require 'cc-mode)
(require 'gud)

;; based on https://raw.github.com/google/styleguide/gh-pages/google-c-style.el
(defconst my-cc-style
  `((c-recognize-knr-p . nil)
    (c-enable-xemacs-performance-kludge-p . t)
    (c-basic-offset . 2)
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
                        (label . /)
                        (case-label . +)
                        (statement-case-open . +)
                        (statement-case-intro . +) ; case w/o {
                        (access-label . /)
                        (innamespace . 0)))))

(c-add-style "my-cc-style" my-cc-style)

;; sudo apt install git cmake libclang-dev clang clang-format libncurses5-dev liblua5.3-dev libssl-dev python\*-virtualenv python\*-\*pep8
;; mkdir ~/.emacs.d/lisp; cd $_
;; git clone --recursive https://github.com/Andersbakken/rtags.git && cd rtags && git checkout tags/vX.X && cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=1 . && make && cd ..
;; M-x package-install clang-format, flycheck, cmake-mode, protobuf-mode, company, projectile, anaconda-mode, company-anaconda
;; cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON ./ && ~/.emacs.d/lisp/rtags/bin/rc -J .
(require 'company)
(global-company-mode 1)
(setq company-minimum-prefix-length 1)
(setq company-idle-delay 0.1)

(setq rtags-path "~/.emacs.d/lisp/rtags/bin/")
(add-to-list 'load-path "~/.emacs.d/lisp/rtags/src/")
(require 'rtags)
(require 'flycheck-rtags)

(setq whitespace-style '(face trailing tabs))
(global-whitespace-mode 1)

(projectile-global-mode)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cc\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

(defun my-programming-modes-hook ()
  (setq-local linum-format (if window-system "%4d" "%4d "))

  (setq-default indent-tabs-mode nil)
  (setq tab-width 2)

  (local-set-key (kbd "M-/") 'company-complete)
  (local-set-key (kbd "C-c c") 'company-complete)

  (local-set-key (kbd "C-c #") 'comment-region)
  (local-set-key (kbd "C-c 3") 'comment-region)
  (local-set-key (kbd "C-c $") 'uncomment-region)
  (local-set-key (kbd "C-c 4") 'uncomment-region))

(defun my-c-c++-mode-hook ()
  (setq compilation-scroll-output 'first-error)
  (c-set-style "my-cc-style")

  (setq company-clang-arguments '("--std=c++14"))

  (rtags-start-process-unless-running)
  (setq rtags-autostart-diagnostics 1)
  (rtags-diagnostics)
  (setq rtags-completions-enabled 1)
  (setq rtags-display-current-error-as-tooltip 1)
  (setq company-backends
        '((company-files
           company-keywords
           company-rtags
           company-yasnippet)
          (company-abbrev
           company-dabbrev)))
  (flycheck-mode 1)

  (setq company-minimum-prefix-length 5)
  (setq company-idle-delay 0.5)
  (setq company-rtags-begin-after-member-access 1)

  (local-set-key (kbd "RET") 'newline-and-indent)

  (local-set-key (kbd "M-q") 'ff-find-other-file)
  (local-set-key (kbd "C-c f") 'clang-format)
  (local-set-key (kbd "C-c g") 'gdb)
  (local-set-key (kbd "C-c j") 'rtags-find-symbol-at-point)
  (local-set-key (kbd "C-c m") 'rtags-imenu)
  (local-set-key (kbd "C-c n") 'rtags-next-match)
  (local-set-key (kbd "C-c p") 'rtags-previous-match)
  (local-set-key (kbd "C-c s") 'rtags-print-symbol-info)
  (local-set-key (kbd "C-c t") 'projectile-find-test-file)
  (local-set-key (kbd "C-c x") 'rtags-find-all-references-at-point))

(defun my-python-mode-hook ()
  (anaconda-mode 1)

  (setq python-indent-offset 4)
  (setq tab-width 4)

  (push 'company-anaconda company-backends)

  (local-set-key (kbd "C-c a") 'anaconda-mode-find-assignments)
  (local-set-key (kbd "C-c d") 'anaconda-mode-show-doc)
  (local-set-key (kbd "C-c f") 'py-autopep8)
  (local-set-key (kbd "C-c j") 'anaconda-mode-find-definitions)
  (local-set-key (kbd "C-c p") 'anaconda-mode-go-back)
  (local-set-key (kbd "C-c s") 'anaconda-mode-complete)
  (local-set-key (kbd "C-c x") 'anaconda-mode-find-references))

(add-hook 'c-mode-common-hook 'my-programming-modes-hook)
(add-hook 'c-mode-hook 'my-c-c++-mode-hook)
(add-hook 'c++-mode-hook 'my-c-c++-mode-hook)
(add-hook 'python-mode-hook 'my-programming-modes-hook)
(add-hook 'python-mode-hook 'my-python-mode-hook)

;; ido
(ido-mode 1)
(setq ido-everywhere 1)
(setq ido-enable-flex-matching 1)
(setq ido-use-filename-at-point 'guess)

;; navigation key bindings
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)

(global-set-key (kbd "M-o") 'other-window)

(global-set-key (kbd "C-<up>") 'windmove-up)
(global-set-key (kbd "M-[ a") 'windmove-up)
(global-set-key (kbd "C-<down>") 'windmove-down)
(global-set-key (kbd "M-[ b") 'windmove-down)
(global-set-key (kbd "C-<left>") 'windmove-left)
(global-set-key (kbd "M-[ d") 'windmove-left)
(global-set-key (kbd "C-<right>") 'windmove-right)
(global-set-key (kbd "M-[ c") 'windmove-right)

(global-set-key (kbd "C-x g") 'projectile-grep)
(global-set-key (kbd "C-x f") 'projectile-find-file)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

;; search & replace key bindings
(global-set-key (kbd "C-r") 'replace-regexp)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "M-r") 'replace-string)
(global-set-key (kbd "M-s") 'isearch-backward-regexp)
