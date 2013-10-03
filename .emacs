
;; path settings
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/"))

;; global settings
(require 'saveplace)
(require 'paren)

(fset 'yes-or-no-p 'y-or-n-p)
(global-font-lock-mode 1)
(if (not window-system) (menu-bar-mode 0))
(setq compilation-scroll-output 'first-error)
(setq gdb-many-windows 1)
(setq inhibit-splash-screen 1)
(setq transient-mark-mode 1)
(setq-default save-place 1)
(show-paren-mode 1)
(tool-bar-mode 0)

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
   '(linum ((t (:foreground "SkyBlue4" :background "black"))))
   '(minibuffer-prompt ((t (:foreground "gray70"))))
   '(region ((t (:background "gray20"))))
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
    (c-echo-syntactic-information-p . 1))
  "my-c-style")

(defun my-prog-mode-hook ()
  (c-add-style "my-c-style" my-c-style)
  (apply 'custom-set-faces my-font-lock-faces)

  (linum-mode 1)
  (column-number-mode 1)
  (which-function-mode 1)

  (local-set-key (kbd "C-c C-c") 'comment-region)
  (local-set-key (kbd "C-c C-s") 'speedbar)
  (local-set-key (kbd "C-c C-u") 'uncomment-region)
  (local-set-key (kbd "C-c TAB") 'indent-region)
  (local-set-key (kbd "C-c c") 'semantic-ia-complete-symbol-menu)
  (local-set-key (kbd "C-c j") 'semantic-ia-fast-jump)
  (local-set-key (kbd "C-c q") 'semantic-analyze-proto-impl-toggle)
  (local-set-key (kbd "C-c s") 'semantic-ia-show-summary)
  (local-set-key (kbd "M-q") 'ff-find-other-file)
  (local-set-key (kbd "RET") 'newline-and-indent))

(add-hook 'c-mode-common-hook 'my-prog-mode-hook)
(add-hook 'python-mode-hook 'my-prog-mode-hook)

;; remember recent files
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 100)

;; semantic
(require 'cedet)
(require 'semantic)
(require 'semantic/analyze/refs)
(require 'semantic/ia)

(semantic-mode 1)
(global-ede-mode 1)

(setq semantic-default-submodes
      '(global-semanticdb-minor-mode
	global-semantic-decoration-mode
	global-semantic-highlight-func-mode
	global-semantic-idle-completions-mode
	global-semantic-idle-scheduler-mode
	global-semantic-mru-bookmark-mode))

;; auto-complete mode
(when (require 'auto-complete-config nil 1)
  (setq ac-comphist-file  "~/.emacs.d/ac-comphist.dat")
  (ac-config-default)

  (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")

  (add-hook 'c-mode-common-hook
	    (lambda ()
	      (setq ac-sources
		    '(ac-source-abbrev
		      ac-source-dictionary
		      ac-source-features
		      ac-source-filename
		      ac-source-files-in-current-dir
		      ac-source-functions
		      ac-source-symbols
		      ac-source-variables
		      ac-source-words-in-all-buffer))))

  (global-set-key (kbd "C-c a") 'auto-complete)
  (define-key ac-completing-map (kbd "RET") 'ac-expand)

  (setq ac-auto-show-menu 0.1)

  (add-to-list 'ac-modes 'sh-mode)
  (add-to-list 'ac-modes 'conf-colon-mode))

;; global key bindings
(define-prefix-command 'my-key-bindings)
(global-set-key (kbd "C-z") 'my-key-bindings)

(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)
(global-set-key (kbd "C-z ,") 'beginning-of-buffer)
(global-set-key (kbd "C-z .") 'end-of-buffer)
(global-set-key (kbd "C-z C-d") 'dired-current-directory)
(global-set-key (kbd "C-z C-k") 'clipboard-kill-ring-save)
(global-set-key (kbd "C-z C-s") 'sort-lines)
(global-set-key (kbd "C-z C-w") 'delete-trailing-whitespace)
(global-set-key (kbd "C-z C-y") 'clipboard-yank)
(global-set-key (kbd "C-z c") 'capitalize-region)
(global-set-key (kbd "C-z g") 'goto-line)
(global-set-key (kbd "C-z l") 'downcase-region)
(global-set-key (kbd "C-z o") 'occur)
(global-set-key (kbd "C-z r") 'replace-regexp)
(global-set-key (kbd "C-z s") 'replace-string)
(global-set-key (kbd "C-z u") 'upcase-region)
(global-set-key (kbd "M-<down>") 'next-buffer)
(global-set-key (kbd "M-<left>") 'next-multiframe-window)
(global-set-key (kbd "M-<right>") 'previous-multiframe-window)
(global-set-key (kbd "M-<up>") 'previous-buffer)
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-o") 'next-multiframe-window)
(global-set-key (kbd "M-p") 'backward-paragraph)

;; local
(dolist (path (file-expand-wildcards (expand-file-name "~/.emacs.d/auto/*.el"))) (load-file path))
