
;; global settings
(require 'saveplace)
(require 'paren)

(if (window-system)
    (tool-bar-mode 0)
  (menu-bar-mode 0))

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
(if (eq system-type 'windows-nt) (set-frame-font "Consolas-12"))
(if (eq system-type 'gnu/linux) (set-frame-font "Monospace-12"))

;; tip: C-u C-x = to get a name of face under cursor and some additional info
;; tip: M-x customize-themes to browse themes
(if (window-system) (load-theme 'tango))

;; programming modes
(require 'cc-mode)
(require 'gud)

(defconst my-cc-style
  '("cc-mode"
    (c-offsets-alist . ((innamespace . [0])))))

(c-add-style "my-cc-style" my-cc-style)

;; sudo apt install git cmake libclang-dev clang clang-format libncurses5-dev liblua5.3-dev libssl-dev python3-virtualenv python3-jedi
;; mkdir ~/.emacs.d/lisp; cd $_
;; git clone https://github.com/company-mode/company-mode.git && cd company-mode && make && cd ..
;; git clone --recursive https://github.com/Andersbakken/rtags.git && cd rtags && git checkout tags/vX.X && cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=1 . && make && cd ..
;; M-x package-install clang-format, flycheck, cmake-mode, protobuf-mode, company-jedi
;; cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON ./ && ~/.emacs.d/lisp/rtags/bin/rc -J .
(add-to-list 'load-path "~/.emacs.d/lisp/company-mode/")
(require 'company)
(setq company-minimum-prefix-length 1)
(setq company-idle-delay 0.1)
(setq company-rtags-begin-after-member-access 1)
(global-company-mode 1)

(setq rtags-path "~/.emacs.d/lisp/rtags/bin/")
(add-to-list 'load-path "~/.emacs.d/lisp/rtags/src/")
(require 'rtags)
(require 'flycheck-rtags)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cc\\'" . c++-mode))

(defun my-c-mode-common-hook ()
  (setq-local linum-format (if window-system "%4d" "%4d "))
  (setq-default indent-tabs-mode nil)
  (column-number-mode 1)
  (setq-local whitespace-style '(face trailing tabs))
  (whitespace-mode 1))

(defun my-c-c++-mode-hook ()
  (setq c-basic-offset 2)
  (setq compilation-scroll-output 'first-error)
  (c-set-style "my-cc-style")

  (setq company-clang-arguments '("--std=c++14"))

  (rtags-start-process-unless-running)
  (setq rtags-autostart-diagnostics 1)
  (rtags-diagnostics)
  (setq rtags-completions-enabled 1)
  (setq rtags-display-current-error-as-tooltip 1)
  (push 'company-rtags company-backends)

  (flycheck-mode 1)

  (local-set-key (kbd "M-q") 'ff-find-other-file)
  (local-set-key (kbd "C-c C") 'uncomment-region)
  (local-set-key (kbd "C-c c") 'company-complete)
  (local-set-key (kbd "C-c f") 'clang-format)
  (local-set-key (kbd "C-c g") 'gdb)
  (local-set-key (kbd "C-c j") 'rtags-find-symbol-at-point)
  (local-set-key (kbd "C-c m") 'rtags-imenu)
  (local-set-key (kbd "C-c n") 'rtags-next-match)
  (local-set-key (kbd "C-c p") 'rtags-previous-match)
  (local-set-key (kbd "C-c r") 'comment-region)
  (local-set-key (kbd "C-c s") 'rtags-print-symbol-info)
  (local-set-key (kbd "C-c x") 'rtags-find-all-references-at-point)
  (local-set-key (kbd "RET") 'newline-and-indent))

(defun my-python-mode-hook ()
  (setq python-indent-offset 2)
  (setq jedi:environment-root "jedi")
  (setq jedi:environment-virtualenv
        (append python-environment-virtualenv
                '("--python" "/usr/bin/python3")))
  (jedi:setup)
  (push 'company-jedi company-backends))

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
(add-hook 'c-mode-hook 'my-c-c++-mode-hook)
(add-hook 'c++-mode-hook 'my-c-c++-mode-hook)

(add-hook 'python-mode-hook 'my-c-mode-common-hook)
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

;; search & replace key bindings
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "M-s") 'isearch-backward-regexp)
(global-set-key (kbd "C-r") 'query-replace-regexp)
