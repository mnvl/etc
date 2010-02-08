
;; path settings
(add-to-list 'load-path (expand-file-name "~/.emacs.d"))

;; global settings
(menu-bar-mode nil)
(if window-system (tool-bar-mode nil))
(setq transient-mark-mode 't)
(global-font-lock-mode 't)
(setq inhibit-splash-screen 't)
(setq gdb-many-windows 't)
(iswitchb-mode 't)

;; save place
(require 'saveplace)
(setq-default save-place 't)

;; show paren mode
(require 'paren)
(show-paren-mode 't)

;; coding system and input method
(prefer-coding-system 'utf-8)
(setq default-input-method "cyrillic-jcuken")

;; use tabs for indentation
(setq indent-tabs-mode 't)

;; trailing whitespaces highlighter
(defface trailing-whitespace-face '((t (:background "red")))
  "*Face for highlighting whitespace at line ends in Font-Lock mode."
  :group 'Show-Whitespace :group 'font-lock :group 'faces)

(defun highlight-trailing-whitespace ()
  "Highlight whitespace characters at line ends."
  (font-lock-add-keywords nil '(("[\040\t]+$" (0 'trailing-whitespace-face t)))))

;; (taken from http://dizzyd.com/blog/post/116)
;; Association list of extension -> inverse extension
(setq my-other-exts '(("cpp" . ("hpp" "h" "hxx" "h++" "H"))
		      ("c"   . ("hpp" "h" "hxx" "h++" "H"))
		      ("cxx" . ("hpp" "h" "hxx" "h++" "H"))
		      ("c++" . ("hpp" "h" "hxx" "h++" "H"))
		      ("C"   . ("hpp" "h" "hxx" "h++" "H"))
		      ("hpp" . ("cpp" "c" "cxx" "c++" "C"))
		      ("h"   . ("cpp" "c" "cxx" "c++" "C"))
		      ("hxx" . ("cpp" "c" "cxx" "c++" "C"))
		      ("h++" . ("cpp" "c" "cxx" "c++" "C"))
		      ("H"   . ("cpp" "c" "cxx" "c++" "C"))))

;; Process the association list of extensions and find the last file
;; that exists
(defun my-find-other-file (fname fext) (dolist (value (cdr (assoc fext my-other-exts)) result) (if (file-exists-p (concat fname "." value)) (setq result (concat fname "." value)))))

;; Toggle function that uses the current buffer name to open/find the
;; other file
(defun my-toggle-header-buffer() (interactive) (let ((ext (file-name-extension buffer-file-name)) (fname (file-name-sans-extension buffer-file-name))) (find-file (my-find-other-file fname ext))))

;; Bind the toggle function to a global key
(global-set-key "\M-q" 'my-toggle-header-buffer)

;; font lock
;; tip: use C-u C-x = to get name of face under cursor and some additional info
(if window-system
    ;; x11, windows etc.
    (defconst my-font-lock-faces
      (list
       '(default ((t (:foreground "black"))))))
  ;; console
  (defconst my-font-lock-faces
    (list
     '(default ((t (:foreground "white"))))
     '(font-lock-builtin-face ((t (:foreground "white"))))
     '(font-lock-comment-face ((t (:foreground "green"))))
     '(font-lock-comment-delimiter-face ((t (:foreground "green"))))
     '(font-lock-constant-face ((t (:foreground "gray"))))
     '(font-lock-doc-string-face ((t (:foreground "white"))))
     '(font-lock-function-name-face ((t (:foreground "purple"))))
     '(font-lock-keyword-face ((t (:foreground "yellow"))))
     '(font-lock-preprocessor-face ((t (:foreground "blue"))))
     '(font-lock-reference-face ((t (:foreground "white"))))
     '(font-lock-regexp-grouping-backslash ((t (:foreground "white"))))
     '(font-lock-regexp-grouping-construct ((t (:foreground "white"))))
     '(font-lock-string-face ((t (:foreground "gray"))))
     '(font-lock-type-face ((t (:foreground "cyan"))))
     '(font-lock-variable-name-face ((t (:foreground "gray"))))
     '(font-lock-warning-face ((t (:foreground "red"))))
     '(font-lock-warning-name-face ((t (:foreground "red"))))
     '(show-paren-match-face ((t (:foreground "black" :background "green"))))
     '(show-paren-mismatch-face ((t (:foreground "red" :background "green" :bold t))))
     '(cperl-hash-face ((t (:foreground "cyan" :background "black"))))
     '(cperl-array-face ((t (:foreground "cyan" :background "black"))))
     '(link ((t (:foreground "white" :underline t))))
     '(minibuffer-prompt ((t (:foreground "white"))))
     '(semantic-decoration-on-private-members-face ((t)))
     '(semantic-decoration-on-protected-members-face ((t)))
     '(semantic-unmatched-syntax-face ((t (:background "darkred" :foreground "white"))))
     '(semantic-decoration-on-unparsed-includes ((t (:background "darkred" :foreground "white"))))
     '(semantic-decoration-on-unknown-includes ((t (:background "darkred" :foreground "white"))))
     '(semantic-tag-boundary-face ((t)))
     '(semantic-highlight-func-current-tag-face ((t)))
     '(senator-momentary-highlight-face ((t (:background "black"))))
     '(which-func ((t (:background "black"))))

     ;; faces for semantic-ia-fast-jump, retreived by reverse engineering
     '(pulse-highlight-start-face ((t (:background "black"))))
     '(pulse-highlight-face ((t (:background "black"))))
     )
    )
  )

(apply 'custom-set-faces my-font-lock-faces)

;; cedet
;; tip: try to disable my-c-mode-common-hook if cedet does not work!!!
(load-file (expand-file-name "~/.emacs.d/cedet-1.0pre6/common/cedet.el"))

(global-ede-mode t)
(semantic-load-enable-excessive-code-helpers)

(require 'semantic-ia)
(require 'semantic-gcc)

(global-semantic-stickyfunc-mode -1)
(global-semantic-idle-summary-mode -1)
(global-semantic-idle-completions-mode -1)
(which-function-mode -1)

(defun my-cedet-hook ()
  (local-set-key "\M-u" 'semantic-ia-show-summary)
  (local-set-key "\M-i" 'semantic-complete-analyze-inline)
  (local-set-key "\M-o" 'semantic-symref)
  (local-set-key "\M-p" 'semantic-ia-fast-jump)
)

(add-hook 'c-mode-common-hook 'my-cedet-hook)
(add-hook 'lisp-mode-hook 'my-cedet-hook)

;; text-mode
(setq default-major-mode 'text-mode)

(defun my-text-mode-hook ()
  ;; tab stops
  (setq tab-width 4
	;; this will make sure tabs are used instead of spaces
	indent-tabs-mode 't)

  ;; set font lock faces
  (apply 'custom-set-faces my-font-lock-faces)

  ;; linum mode
  (linum-mode 't)
)

(add-hook 'text-mode-hook 'my-text-mode-hook)

;; cc-mode
(require 'cc-mode)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(setq c-basic-offset 4)
(setq c-tab-always-indent 't)

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
    (c-echo-syntactic-information-p . t)
  ) "my-c-style")

(defun my-c-mode-common-hook ()
  ;; my-c-style
  (c-add-style "my-c-style" my-c-style t)

  ;; tab stops
  (setq tab-width 4
	;; this will make sure tabs are used instead of spaces
	indent-tabs-mode 't)

  ;; set font lock faces
  (apply 'custom-set-faces my-font-lock-faces)

  ;; trailing whitespaces
  (add-hook 'font-lock-mode-hook 'highlight-trailing-whitespace)

  ;; linum mode
  (linum-mode 't)
)

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;; cperl-mode
(defun my-cperl-mode-hook ()
  ;; tab stops
  (setq tab-width 4
	;; this will make sure tabs are used instead of spaces
	indent-tabs-mode 't)

  ;; set font lock faces
  (apply 'custom-set-faces my-font-lock-faces)

  ;; indent
  (setq cperl-indent-level 4)
  (setq cperl-brace-offset 0)
  (setq cperl-continued-brace-offset 0)
  (setq cperl-label-offset -4)
  (setq cperl-continued-statement-offset 0)
  (setq cperl-continued-brace-offset 0)
  (setq cperl-merge-trailing-else t)
  (setq cperl-electric-lbrace-space 0)
  (setq cperl-min-label-indent 0)

  ;; trailing whitespaces (use cperl's hack with our face)
  (setq cperl-invalid-face 'trailing-whitespace-face)

  ;; linum mode
  (linum-mode 't)
)

(defalias 'perl-mode 'cperl-mode)

(add-hook 'cperl-mode-hook 'my-cperl-mode-hook)

(add-to-list 'auto-mode-alist '("\\.\\([pP][LlMm]\\|al\\)\\'" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))

;; python-mode
(defun my-python-mode-hook ()
  ;; set font lock faces
  (apply 'custom-set-faces my-font-lock-faces)

  ;; trailing whitespaces
  (add-hook 'font-lock-mode-hook 'highlight-trailing-whitespace)

  ;; linum mode
  (linum-mode 't)
)

(add-hook 'python-mode-hook 'my-python-mode-hook)

;; lua-mode
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode))

(defun my-lua-mode-hook ()
  ;; tab stops
  (setq tab-width 4
	;; this will make sure tabs are used instead of spaces
	indent-tabs-mode 't
	;; set indent level
	lua-indent-level 4)

  ;; set font lock faces
  (apply 'custom-set-faces my-font-lock-faces)

  ;; trailing whitespaces
  (add-hook 'font-lock-mode-hook 'highlight-trailing-whitespace)

  ;; linum mode
  (linum-mode 't)
)

(add-hook 'lua-mode-hook 'my-lua-mode-hook)

;; keys
(global-set-key [f5] 'goto-line)
(global-set-key [f7] 'compile)
(global-set-key [f8] 'recompile)
(global-set-key "\C-z" 'undo)

;; local
(load-file (expand-file-name "~/.emacs-local"))
