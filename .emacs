
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

(defun my-programming-mode-hook ()
  (c-add-style "my-c-style" my-c-style)
  (apply 'custom-set-faces my-font-lock-faces)

  (linum-mode 1)
  (column-number-mode 1)
  (which-function-mode 1))

(add-hook 'c-mode-common-hook 'my-programming-mode-hook)
(add-hook 'python-mode-hook 'my-programming-mode-hook)

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
		      ac-source-semantic
		      ac-source-semantic-raw
		      ac-source-symbols
		      ac-source-variables
		      ac-source-words-in-all-buffer))))

  (global-set-key (kbd "C-c a") 'auto-complete)
  (define-key ac-completing-map (kbd "RET") 'ac-expand)

  (setq ac-auto-show-menu 0.1)

  (add-to-list 'ac-modes 'sh-mode)
  (add-to-list 'ac-modes 'conf-colon-mode))

;; key bindings
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

(define-prefix-command 'my-key-bindings)
(defvar my-keys-mode-map (make-keymap) "my-keys-mode-map")
(define-key my-keys-mode-map (kbd "C-z") 'my-key-bindings)
(define-key my-keys-mode-map (kbd "C-c '") 'uncomment-region)
(define-key my-keys-mode-map (kbd "C-c ;") 'comment-region)
(define-key my-keys-mode-map (kbd "C-c C-s") 'speedbar)
(define-key my-keys-mode-map (kbd "C-c TAB") 'indent-region)
(define-key my-keys-mode-map (kbd "C-c c") 'semantic-ia-complete-symbol-menu)
(define-key my-keys-mode-map (kbd "C-c j") 'semantic-ia-fast-jump)
(define-key my-keys-mode-map (kbd "C-c s") 'semantic-ia-show-summary)
(define-key my-keys-mode-map (kbd "C-c 1") 'semantic-analyze-proto-impl-toggle)
(define-key my-keys-mode-map (kbd "C-r") 'isearch-backward-regexp)
(define-key my-keys-mode-map (kbd "C-s") 'isearch-forward-regexp)
(define-key my-keys-mode-map (kbd "C-z ,") 'beginning-of-buffer)
(define-key my-keys-mode-map (kbd "C-z .") 'end-of-buffer)
(define-key my-keys-mode-map (kbd "C-z C-d") 'dired-current-directory)
(define-key my-keys-mode-map (kbd "C-z C-k") 'clipboard-kill-ring-save)
(define-key my-keys-mode-map (kbd "C-z C-s") 'sort-lines)
(define-key my-keys-mode-map (kbd "C-z C-w") 'delete-trailing-whitespace)
(define-key my-keys-mode-map (kbd "C-z C-y") 'clipboard-yank)
(define-key my-keys-mode-map (kbd "C-z c") 'capitalize-region)
(define-key my-keys-mode-map (kbd "C-z g") 'goto-line)
(define-key my-keys-mode-map (kbd "C-z l") 'downcase-region)
(define-key my-keys-mode-map (kbd "C-z o") 'occur)
(define-key my-keys-mode-map (kbd "C-z r") 'replace-regexp)
(define-key my-keys-mode-map (kbd "C-z s") 'replace-string)
(define-key my-keys-mode-map (kbd "C-z u") 'upcase-region)
(define-key my-keys-mode-map (kbd "M-<down>") 'next-buffer)
(define-key my-keys-mode-map (kbd "M-<left>") 'next-multiframe-window)
(define-key my-keys-mode-map (kbd "M-<right>") 'previous-multiframe-window)
(define-key my-keys-mode-map (kbd "M-<up>") 'previous-buffer)
(define-key my-keys-mode-map (kbd "M-n") 'forward-paragraph)
(define-key my-keys-mode-map (kbd "M-o") 'next-multiframe-window)
(define-key my-keys-mode-map (kbd "M-p") 'backward-paragraph)
(define-key my-keys-mode-map (kbd "M-q") 'ff-find-other-file)
(define-key my-keys-mode-map (kbd "RET") 'newline-and-indent)

(define-minor-mode my-keys-minor-mode "my-keys-minor-mode" nil " my-keys" my-keys-mode-map)

(add-hook 'c-mode-common-hook (lambda () (my-keys-minor-mode 1)))
(add-hook 'python-mode-hook (lambda () (my-keys-minor-mode 1)))
(add-hook 'sh-mode-hook (lambda () (my-keys-minor-mode 1)))
(add-hook 'conf-mode-hook (lambda () (my-keys-minor-mode 1)))

;; local
(dolist (path (file-expand-wildcards (expand-file-name "~/.emacs.d/auto/*.el"))) (load-file path))
