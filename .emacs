
;; global settings
(require 'saveplace)
(require 'paren)

(if (not window-system) (menu-bar-mode 0))
(tool-bar-mode 0)

(fset 'yes-or-no-p 'y-or-n-p)
(global-font-lock-mode 1)
(setq gdb-many-windows 1)
(setq inhibit-splash-screen 1)
(setq transient-mark-mode 1)
(setq-default save-place 1)
(show-paren-mode 1)
(setq frame-title-format (list "%f"))
(global-subword-mode 1)

;; melpa
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;; coding system and input method
(prefer-coding-system 'utf-8)

;; fonts
(if (eq system-type 'windows-nt) (set-frame-font "Consolas-12"))
(if (eq system-type 'gnu/linux) (set-frame-font "Monospace-12"))

;; tip: C-u C-x = to get a name of face under cursor and some additional info
;; tip: M-x customize-themes to browse themes
(if (window-system) (load-theme 'tango))

;; programming modes
(require 'cc-mode)

;; sudo apt install libclang-dev clang clang-format libncurses5-dev liblua5.3-dev
;; clone from github.com and compile company-mode and rtags in ~/.emacs.d/lisp
;; M-x package-install clang-format & flycheck
;; cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON ./ && ~/.emacs.d/lisp/rtags/bin/rc -J .
(add-to-list 'load-path "~/.emacs.d/lisp/company-mode/")
(require 'company)

(setq rtags-path "~/.emacs.d/lisp/rtags/bin/")
(add-to-list 'load-path "~/.emacs.d/lisp/rtags/src/")
(require 'rtags)
(require 'flycheck-rtags)

(add-to-list 'auto-mode-alist '("\\CMakeLists.txt\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.proto\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cc\\'" . c++-mode))

(defun my-prog-mode-hook ()
  (setq-local linum-format (if window-system "%4d" "%4d "))

  (setq c-basic-offset 2)

  (setq-default indent-tabs-mode nil)
  (column-number-mode 1)
  (which-function-mode 1)

  (setq-local whitespace-style '(face trailing tabs))
  (whitespace-mode 1)

  (setq compilation-scroll-output 'first-error)

  (rtags-start-process-unless-running)
  (setq rtags-autostart-diagnostics 1)
  (setq rtags-completions-enabled 1)
  (push 'company-rtags company-backends)
  (delete 'company-clang company-backends)

  (setq company-idle-delay 0)
  (company-mode 1)
  (flycheck-mode 1)

  (local-set-key (kbd "M-q") 'ff-find-other-file)
  (local-set-key (kbd "C-c # c") 'comment-region)
  (local-set-key (kbd "C-c # u") 'uncomment-region)
  (local-set-key (kbd "C-c c") 'company-complete)
  (local-set-key (kbd "C-c s") 'rtags-print-symbol-info)
  (local-set-key (kbd "C-c x") 'rtags-find-all-references-at-point)
  (local-set-key (kbd "C-c j") 'rtags-find-symbol-at-point)
  (local-set-key (kbd "C-c m") 'rtags-imenu)
  (local-set-key (kbd "C-c p") 'rtags-previous-match)
  (local-set-key (kbd "C-c n") 'rtags-next-match)
  (local-set-key (kbd "C-c f") 'clang-format)
  (local-set-key (kbd "RET") 'newline-and-indent))

(add-hook 'c-mode-common-hook 'my-prog-mode-hook)
(add-hook 'emacs-lisp-mode-hook 'my-prog-mode-hook)
(add-hook 'lisp-mode-hook 'my-prog-mode-hook)
(add-hook 'python-mode-hook 'my-prog-mode-hook)
(add-hook 'sh-mode-hook 'my-prog-mode-hook)
(add-hook 'text-mode-hook 'my-prog-mode-hook)

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
(global-set-key (kbd "C-<down>") 'windmove-down)
(global-set-key (kbd "C-<left>") 'windmove-left)
(global-set-key (kbd "C-<right>") 'windmove-right)

;; search & replace key bindings
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(define-prefix-command 'my-search-bindings)
(global-set-key (kbd "M-s") 'my-search-bindings)
(global-set-key (kbd "M-s b") 'regexp-builder)
(global-set-key (kbd "M-s g") 'grep)
(global-set-key (kbd "M-s h") 'highlight-lines-matching-regexp)
(global-set-key (kbd "M-s o") 'occur)
(global-set-key (kbd "M-s r") 'isearch-forward-regexp)
(global-set-key (kbd "M-s s") 'isearch-forward)
(global-set-key (kbd "M-s w") 'isearch-forward-word)

(global-set-key (kbd "C-r") 'query-replace-regexp)
(define-prefix-command 'my-replace-bindings)
(global-set-key (kbd "M-r") 'my-replace-bindings)
(global-set-key (kbd "M-r r") 'replace-regexp)
(global-set-key (kbd "M-r s") 'replace-string)
