
;; path settings
(setq load-path
      (append
       (list (expand-file-name "~/.emacs.d/lisp/"))
       (file-expand-wildcards (expand-file-name "~/.emacs.d/lisp/*/lisp"))
       load-path))

;; global settings
(tool-bar-mode nil)
(global-font-lock-mode t)
(if (not window-system) (menu-bar-mode nil))
(setq compilation-scroll-output 'first-error)
(setq gdb-many-windows t)
(setq inhibit-splash-screen t)
(setq transient-mark-mode t)
(fset 'yes-or-no-p 'y-or-n-p)

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
   '(default ((t (:foreground "gray80" :background "black"))))
   '(ac-candidate-face ((t (:foreground "gray75" :background "DodgerBlue4"))))
   '(ac-completion-face ((t (:foreground "gray80" :background "DeepSkyBlue4"))))
   '(ac-selection-face ((t (:foreground "gray80" :background "SlateGray4"))))
   '(cperl-array-face ((t (:foreground "light cyan"))))
   '(cperl-hash-face ((t (:foreground "light cyan"))))
   '(font-lock-builtin-face ((t (:foreground "sky blue"))))
   '(font-lock-comment-delimiter-face ((t (:foreground "sea green"))))
   '(font-lock-comment-face ((t (:foreground "sea green"))))
   '(font-lock-constant-face ((t (:foreground "coral3"))))
   '(font-lock-doc-string-face ((t (:foreground "sea green"))))
   '(font-lock-function-name-face ((t (:foreground "medium purple"))))
   '(font-lock-keyword-face ((t (:foreground "goldenrod"))))
   '(font-lock-preprocessor-face ((t (:foreground "light blue"))))
   '(font-lock-reference-face ((t (:foreground "plum"))))
   '(font-lock-regexp-grouping-backslash ((t (:foreground "magneta"))))
   '(font-lock-regexp-grouping-construct ((t (:foreground "magneta"))))
   '(font-lock-string-face ((t (:foreground "gray"))))
   '(font-lock-type-face ((t (:foreground "steel blue"))))
   '(font-lock-variable-name-face ((t (:foreground "turquoise4"))))
   '(font-lock-warning-face ((t (:foreground "red"))))
   '(font-lock-warning-name-face ((t (:foreground "red"))))
   '(link ((t (:foreground "CadetBlue1" :underline t))))
   '(linum ((t (:foreground "DeepSkyBlue3" :background "black"))))
   '(minibuffer-prompt ((t (:foreground "gray70"))))
   '(region ((t (:background "gray12"))))
   '(semantic-highlight-func-current-tag-face ((t ())))
   '(semantic-tag-boundary-face ((t ())))
   '(senator-momentary-highlight-face ((t (:background "dark blue"))))
   '(sh-heredoc ((t (:foreground "light sea green"))))
   '(show-paren-match-face ((t (:foreground "black" :background "green"))))
   '(show-paren-mismatch-face ((t (:foreground "red" :background "green" :bold t))))))

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

(add-hook 'c-mode-common-hook
  (lambda ()
    (c-add-style "my-c-style" my-c-style)
    (apply 'custom-set-faces my-font-lock-faces)

    (linum-mode t)
    (column-number-mode t)
    (which-function-mode t)

    (local-set-key "\C-c\ m" 'compile)))

;; python-mode
(add-hook 'python-mode-hook
  (lambda ()
    (apply 'custom-set-faces my-font-lock-faces)))

;; remember recent files
(require 'recentf)
(recentf-mode t)
(setq recentf-max-menu-items 100)

;; global key bindings
(define-prefix-command 'my-keyboard-bindings)
(global-set-key "\C-z" 'my-keyboard-bindings)

(global-set-key "\C-r" 'isearch-backward-regexp)
(global-set-key "\C-s" 'isearch-forward-regexp)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)
(global-set-key "\C-z\ c" 'capitalize-region)
(global-set-key "\C-z\ i" 'indent-region)
(global-set-key "\C-z\ k" 'clipboard-kill-region)
(global-set-key "\C-z\ l" 'downcase-region)
(global-set-key "\C-z\ o" 'occur)
(global-set-key "\C-z\ r" 'replace-regexp)
(global-set-key "\C-z\ s" 'replace-string)
(global-set-key "\C-z\ u" 'upcase-region)
(global-set-key "\C-z\ y" 'clipboard-yank)
(global-set-key "\M-n" 'forward-paragraph)
(global-set-key "\M-p" 'backward-paragraph)
(global-set-key "\M-q" 'ff-find-other-file)

;; semantic
(require 'cedet)
(require 'semantic)
(require 'semantic/analyze/refs)
(require 'semantic/ia)

(semantic-mode t)
(global-ede-mode t)

(setq semantic-default-submodes
  '(global-semanticdb-minor-mode
    global-semantic-decoration-mode
    global-semantic-highlight-func-mode
    global-semantic-idle-completions-mode
    global-semantic-idle-scheduler-mode
    global-semantic-mru-bookmark-mode))

(global-set-key "\C-c\ c" 'semantic-ia-complete-symbol-menu)
(global-set-key "\C-c\ j" 'semantic-ia-fast-jump)
(global-set-key "\C-c\ s" 'semantic-ia-show-summary)
(global-set-key "\C-c\ t" 'semantic-analyze-proto-impl-toggle)

;; auto-complete mode
(when (require 'auto-complete-config nil t)
  (setq ac-comphist-file  "~/.emacs.d/ac-comphist.dat")
  (ac-config-default)

  (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")

  (add-hook 'c-mode-common-hook
    (lambda ()
      (add-to-list 'ac-sources 'ac-source-semantic)
      (add-to-list 'ac-sources 'ac-source-semantic-raw))))

;; yasnippet mode
(when (require 'yasnippet nil t)
  (yas-global-mode t)
  (add-to-list 'ac-sources 'ac-source-yasnippet))

;; local
(dolist (path (file-expand-wildcards (expand-file-name "~/.emacs.d/local/*.el"))) (load-file path))
