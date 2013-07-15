
;; path settings
(add-to-list 'load-path (expand-file-name "~/.emacs.d"))

;; global settings
(tool-bar-mode nil)
(if (not window-system) (menu-bar-mode nil))
(setq transient-mark-mode t)
(global-font-lock-mode t)
(setq inhibit-splash-screen t)
(setq gdb-many-windows t)
(setq compilation-scroll-output 'first-error)
(fset 'yes-or-no-p 'y-or-n-p)
(setq x-select-enable-clipboard t)

;; save place
(require 'saveplace)
(setq-default save-place t)

;; show paren mode
(require 'paren)
(show-paren-mode t)

;; coding system and input method
(prefer-coding-system 'utf-8)

;; fonts
(if (eq system-type 'windows-nt) (set-frame-font "Consolas-14"))
(if (eq system-type 'gnu/linux) (set-frame-font "Monospace-14"))
(set-cursor-color "gray")

;; font lock
;; tip: use C-u C-x = to get name of face under cursor and some additional info
(defconst my-font-lock-faces
  (list
   '(default ((t (:foreground "white" :background "black"))))
   '(font-lock-builtin-face ((t (:foreground "white"))))
   '(font-lock-comment-face ((t (:foreground "green"))))
   '(font-lock-comment-delimiter-face ((t (:foreground "green"))))
   '(font-lock-constant-face ((t (:foreground "gray"))))
   '(font-lock-doc-string-face ((t (:foreground "white"))))
   '(font-lock-function-name-face ((t (:foreground "mediumpurple"))))
   '(font-lock-keyword-face ((t (:foreground "goldenrod"))))
   '(font-lock-preprocessor-face ((t (:foreground "lightblue"))))
   '(font-lock-reference-face ((t (:foreground "white"))))
   '(font-lock-regexp-grouping-backslash ((t (:foreground "white"))))
   '(font-lock-regexp-grouping-construct ((t (:foreground "white"))))
   '(font-lock-string-face ((t (:foreground "gray"))))
   '(font-lock-type-face ((t (:foreground "steelblue"))))
   '(font-lock-variable-name-face ((t (:foreground "white"))))
   '(font-lock-warning-face ((t (:foreground "red"))))
   '(font-lock-warning-name-face ((t (:foreground "red"))))
   '(show-paren-match-face ((t (:foreground "black" :background "green"))))
   '(show-paren-mismatch-face ((t (:foreground "red" :background "green" :bold t))))
   '(cperl-hash-face ((t (:foreground "lightcyan" :background "black"))))
   '(cperl-array-face ((t (:foreground "lightcyan" :background "black"))))
   '(link ((t (:foreground "white" :underline t))))
   '(minibuffer-prompt ((t (:foreground "white"))))
   '(senator-momentary-highlight-face ((t (:background "black"))))
   (if window-system '(which-func ((t (:background "gray" :foreground "black")))) '(which-func ((t (:background "black")))))
   '(linum ((t (:foreground "blue" :background "black"))))))
(apply 'custom-set-faces my-font-lock-faces)

;; cc-mode
(require 'cc-mode)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cc\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.fx\\'" . c++-mode))

(defconst my-c-style
  '(
    (c-hanging-braces-alist     . ((substatement-open after)
                                   (brace-list-open)))
    (c-hanging-colons-alist     . ((member-init-intro after)
                                   (inher-intro)
                                   (case-label after)
                                   (label after)
                                   (access-label after)))
    (c-cleanup-list             . (scope-operator
                                   empty-defun-braces
                                   defun-close-semi))
    (c-offsets-alist            . ((arglist-cont-nonempty . +)
                                   (arglist-close         . c-lineup-arglist)
                                   (substatement-open     . 0)
                                   (case-label            . 0)
                                   (statement-case-intro  . +)
                                   (block-open            . 0)
                                   (knr-argdecl-intro     . -)
                                   (inline-open           . 0)
                                   (member-init-intro     . +)
                                   (topmost-intro         . 0)
                                   (access-label          . -)
                                   (inclass               . +)
                                   (innamespace           . 0)))
    (c-echo-syntactic-information-p . t))
  "my-c-style")

(defun my-c-mode-common-hook ()
  (c-add-style "my-c-style" my-c-style)
  (line-number-mode t)
  (column-number-mode t)
  (setq compilation-scroll-output 'first-error)
  (apply 'custom-set-faces my-font-lock-faces))

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;; python-mode
(defun my-python-mode-hook ()
  (linum-mode t)
  (apply 'custom-set-faces my-font-lock-faces))

(add-hook 'python-mode-hook 'my-python-mode-hook)

;; keys
(global-set-key "\C-s" 'isearch-forward-regexp)
(global-set-key "\C-r" 'isearch-backward-regexp)
(global-set-key "\M-r" 'replace-regexp)
(global-set-key "\C-\M-r" 'replace-string)
(global-set-key "\M-o" 'occur)
(global-set-key "\M-p" 'backward-paragraph)
(global-set-key "\M-n" 'forward-paragraph)

;; semantic
(require 'cedet)
(require 'semantic)
(require 'semantic/analyze/refs)
(require 'semantic/ia)

(global-ede-mode t)

(setq semantic-default-submodes
      '(global-semanticdb-minor-mode
        global-semantic-decoration-mode
        global-semantic-highlight-func-mode
        global-semantic-idle-completions-mode
        global-semantic-idle-scheduler-mode
        global-semantic-mru-bookmark-mode))

(defun my-cedet-hook ()
  (semantic-mode t)

  (local-set-key "\C-q" 'semantic-analyze-proto-impl-toggle)
  (local-set-key "\M-q" 'eassist-switch-h-cpp)
  (local-set-key "\M-j" 'semantic-ia-fast-jump)
  (local-set-key "\M-c" 'semantic-ia-complete-symbol)
  (local-set-key "\M-v" 'semantic-ia-complete-symbol-menu)
  (local-set-key "\M-i" 'semantic-complete-analyze-inline))

(add-hook 'c-mode-common-hook 'my-cedet-hook)

;; auto-complete-mode
(when (require 'auto-complete-config nil 'noerror)
  (setq ac-comphist-file  "~/.emacs.d/ac-comphist.dat")
  (ac-config-default)
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")

  (defun my-use-semantic-in-autocomplete-hook ()
    (setq ac-sources (append ac-sources '(ac-source-semantic ac-source-semantic-raw))))

  (add-hook 'c-mode-common-hook 'my-use-semantic-in-autocomplete-hook))

;; local
(dolist (path (file-expand-wildcards (expand-file-name "~/.emacs-local-*"))) (load-file path))
