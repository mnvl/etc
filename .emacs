
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
(require 'color-theme)
(if (window-system)
    (load-theme 'tango)
  (load-theme 'tango-dark))

;; CEDET & company
(require 'cedet)
(require 'semantic)
(require 'semantic/analyze/refs)
(require 'semantic/ia)
(require 'ede-compdb)

(global-ede-mode 1)

(setq semantic-default-submodes
      '(global-semanticdb-minor-mode
	global-semantic-decoration-mode
	global-semantic-highlight-func-mode
	global-semantic-idle-completions-mode
	global-semantic-idle-scheduler-mode
	global-semantic-mru-bookmark-mode))

;; programming modes
(require 'cc-mode)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cc\\'" . c++-mode))

(defconst my-c-style
  '((c-recognize-knr-p . nil)
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
    (c-offsets-alist . ((func-decl-cont . ++)
			(member-init-intro . ++)
			(inher-intro . ++)
			(comment-intro . 0)
			(arglist-close . c-lineup-arglist)
			(topmost-intro . 0)
			(block-open . 0)
			(inline-open . 0)
			(substatement-open . 0)
			(statement-cont . ++)
			(label . /)
			(case-label . +)
			(statement-case-open . +)
			(statement-case-intro . +)
			(access-label . 0)
			(innamespace . --)))))

(c-add-style "my-c-style" my-c-style)


(defun my-prog-mode-hook ()
  (setq-local linum-format (if window-system "%4d" "%4d "))
  (setq c-default-style "my-c-style")

  (column-number-mode 1)
  (which-function-mode 1)

  (setq-local whitespace-style '(face trailing tabs))
  (whitespace-mode 1)

  (setq compilation-scroll-output 'first-error)

  (semantic-mode 1)
  (company-mode 1)

  (local-set-key (kbd "M-q") 'ff-find-other-file)
  (local-set-key (kbd "C-c # c") 'comment-region)
  (local-set-key (kbd "C-c # u") 'uncomment-region)
  (local-set-key (kbd "C-c c") 'semantic-ia-complete-symbol-menu)
  (local-set-key (kbd "C-c j") 'semantic-ia-fast-jump)
  (local-set-key (kbd "C-c x") 'semantic-ia-show-summary)
  (local-set-key (kbd "RET") 'newline-and-indent))

(add-hook 'c-mode-common-hook 'my-prog-mode-hook)
(add-hook 'emacs-lisp-mode-hook 'my-prog-mode-hook)
(add-hook 'lisp-mode-hook 'my-prog-mode-hook)
(add-hook 'python-mode-hook 'my-prog-mode-hook)
(add-hook 'sh-mode-hook 'my-prog-mode-hook)
(add-hook 'text-mode-hook 'my-prog-mode-hook)

;; yes, ido
(ido-mode 1)
(setq ido-everywhere 1)
(setq ido-enable-flex-matching 1)
(setq ido-use-filename-at-point 'guess)

;; common key bindings
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)

(global-set-key (kbd "C-<up>") 'windmove-up)
(global-set-key (kbd "C-<down>") 'windmove-down)
(global-set-key (kbd "C-<left>") 'windmove-left)
(global-set-key (kbd "C-<right>") 'windmove-right)

(global-set-key (kbd "M-[ a") 'windmove-up)
(global-set-key (kbd "M-[ b") 'windmove-down)
(global-set-key (kbd "M-[ d") 'windmove-left)
(global-set-key (kbd "M-[ c") 'windmove-right)

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

;; desktop save mode
(require 'desktop)
(setq desktop-save 'if-exists)
(setq desktop-path '("~/.emacs.d/desktop/"))
(let ((x (url-hexify-string (expand-file-name default-directory))))
  (setq desktop-base-file-name (format "%s.desktop" x))
  (setq desktop-base-lock-name (format "%s.lock" x)))

(add-to-list 'desktop-globals-to-save 'buffer-name-history)
(add-to-list 'desktop-globals-to-save 'compile-history)
(add-to-list 'desktop-globals-to-save 'extended-command-history)
(add-to-list 'desktop-globals-to-save 'file-name-history)
(add-to-list 'desktop-globals-to-save 'find-file-history)
(add-to-list 'desktop-globals-to-save 'find-tag-history)
(add-to-list 'desktop-globals-to-save 'grep-history)
(add-to-list 'desktop-globals-to-save 'kill-ring)
(add-to-list 'desktop-globals-to-save 'kill-ring)
(add-to-list 'desktop-globals-to-save 'minibuffer-history)
(add-to-list 'desktop-globals-to-save 'query-replace-history)
(add-to-list 'desktop-globals-to-save 'query-replace-regex-history)
(add-to-list 'desktop-globals-to-save 'read-expression-history)
(add-to-list 'desktop-globals-to-save 'regexp-search-ring)
(add-to-list 'desktop-globals-to-save 'replace-regex-history)
(add-to-list 'desktop-globals-to-save 'replace-string-history)
(add-to-list 'desktop-globals-to-save 'search-ring)
(add-to-list 'desktop-globals-to-save 'shell-command-history)
