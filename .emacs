
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
(setq compilation-scroll-output 'first-error)
(fset 'yes-or-no-p 'y-or-n-p)

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
   '(semantic-decoration-on-private-members-face ((t)))
   '(semantic-decoration-on-protected-members-face ((t)))
   '(semantic-unmatched-syntax-face ((t (:background "darkred" :foreground "white"))))
   '(semantic-decoration-on-unparsed-includes ((t (:background "darkred" :foreground "white"))))
   '(semantic-decoration-on-unknown-includes ((t (:background "darkred" :foreground "white"))))
   '(semantic-tag-boundary-face ((t)))
   '(semantic-highlight-func-current-tag-face ((t)))
   '(senator-momentary-highlight-face ((t (:background "black"))))
(if window-system '(which-func ((t (:background "gray" :foreground "black")))) '(which-func ((t (:background "black")))))
   '(which-func ((t (:foreground "black" :background "white"))))
   '(linum ((t (:foreground "gray" :background "black"))))

   ;; faces for semantic-ia-fast-jump, face names retreived by reverse engineering ;)
   '(pulse-highlight-start-face ((t (:background "black"))))
   '(pulse-highlight-face ((t (:background "black"))))))

(apply 'custom-set-faces my-font-lock-faces)

;; cedet
(setq my-cedet-loader-path (expand-file-name "~/.emacs.d/cedet-1.0/common/cedet.el"))
(if (file-exists-p my-cedet-loader-path) (load-file my-cedet-loader-path) (load-file "cedet-common/cedet.el"))

(global-ede-mode t)
(semantic-load-enable-excessive-code-helpers)

(require 'semantic-ia)
(require 'semantic-gcc)

(global-semantic-stickyfunc-mode nil)
(global-semantic-idle-summary-mode nil)
(global-semantic-idle-completions-mode nil)
(which-function-mode nil)

(defun my-cedet-hook ()
  (local-set-key "\M-u" 'semantic-ia-show-summary)
  (local-set-key "\M-i" 'semantic-complete-analyze-inline)
  (local-set-key "\M-o" 'semantic-symref)
  (local-set-key "\M-p" 'semantic-ia-fast-jump)
  (local-set-key "\M-q" 'eassist-switch-h-cpp)
  (local-set-key "\M-1" 'eassist-list-methods)
)

(add-hook 'c-mode-common-hook 'my-cedet-hook)
(add-hook 'lisp-mode-hook 'my-cedet-hook)

;; text-mode
(setq default-major-mode 'text-mode)

(defun my-text-mode-hook ()
  (setq tab-width 4
	indent-tabs-mode 't)
  (apply 'custom-set-faces my-font-lock-faces)
  (linum-mode 't)
)

(add-hook 'text-mode-hook 'my-text-mode-hook)

;; cc-mode
(require 'cc-mode)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.fx\\'" . c++-mode))
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
  (c-add-style "my-c-style" my-c-style t)
  (setq tab-width 4 indent-tabs-mode 't)
  (apply 'custom-set-faces my-font-lock-faces)
  (add-hook 'font-lock-mode-hook 'highlight-trailing-whitespace)
  (linum-mode 't)
  (setq compilation-scroll-output 'first-error)
)

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;; cperl-mode
(defun my-cperl-mode-hook ()
  (setq tab-width 4
	indent-tabs-mode 't)

  (apply 'custom-set-faces my-font-lock-faces)
  (linum-mode 't)

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
)

(defalias 'perl-mode 'cperl-mode)

(add-hook 'cperl-mode-hook 'my-cperl-mode-hook)

(add-to-list 'auto-mode-alist '("\\.\\([pP][LlMm]\\|al\\)\\'" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))

;; python-mode
(defun my-python-mode-hook ()
  (apply 'custom-set-faces my-font-lock-faces)
  (add-hook 'font-lock-mode-hook 'highlight-trailing-whitespace)
  (linum-mode 't)
)

(add-hook 'python-mode-hook 'my-python-mode-hook)

;; lua-mode
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode))

(defun my-lua-mode-hook ()
  (setq tab-width 4
	indent-tabs-mode 't
	lua-indent-level 4)

  (apply 'custom-set-faces my-font-lock-faces)
  (add-hook 'font-lock-mode-hook 'highlight-trailing-whitespace)
  (linum-mode 't)
)

(add-hook 'lua-mode-hook 'my-lua-mode-hook)

;; octave-mode
(autoload 'octave-mode "octave-mod" nil t)
(setq auto-mode-alist
      (cons '("\\.m$" . octave-mode) auto-mode-alist))

(add-hook 'octave-mode-hook
          (lambda ()
            (abbrev-mode 1)
            (auto-fill-mode 1)
            (if (eq window-system 'x)
                (font-lock-mode 1))
	    (setq octave-block-offset 4)
	  ))

;; keys
(global-set-key "\C-q" 'speedbar)
(global-set-key "\C-z" 'undo)
(global-set-key [f5] 'previous-error)
(global-set-key [f6] 'next-error)
(global-set-key [f7] 'compile)
(global-set-key "\C-\M-y" 'x-clipboard-yank)

;; local
(load-file (expand-file-name "~/.emacs-local"))
