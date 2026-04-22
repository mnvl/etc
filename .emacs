
;; install packages automatically on startup
(require 'package)
(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/"))

(require 'use-package)
(setq use-package-always-ensure t)

;; interface
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1))

;; customize file
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file :no-error-if-file-is-missing)

(if (eq system-type 'windows-nt) (set-frame-font "Iosevka-14"))
(if (eq system-type 'gnu/linux) (set-frame-font "Iosevka-14"))
(if (eq system-type 'darwin) (set-frame-font "Iosevka-18"))

;; text editing
(setq use-short-answers t)

(require 'uniquify)

(global-font-lock-mode 1)
(global-subword-mode 1)
(savehist-mode 1)
(show-paren-mode 1)
(transient-mark-mode 1)

(prefer-coding-system 'utf-8)
(setq frame-title-format (list "%f"))
(setq history-length 1000)
(setq inhibit-splash-screen 1)
(setq recentf-max-menu-items 100)
(setq undo-limit (* 32 1024 1024))
(setq undo-outer-limit (* 16 1024 1024))
(setq undo-strong-limit (* 64 1024 1024))
(setq uniquify-buffer-name-style 'post-forward)
(save-place-mode 1)
(setq-default show-trailing-whitespace 1)

(use-package treemacs)
(global-set-key [f8] 'treemacs)
(global-set-key (kbd "C-b") 'treemacs)

;; tip: C-u C-x = to get a name of face under cursor and some additional info
;; tip: M-x customize-themes to browse themes
;; tip: select "linux console" color theme in terminal if background is grey

;; vertico — vertical completion UI (replaces ivy)
(use-package vertico
  :init (vertico-mode)
  :custom
  (vertico-count 20)
  (vertico-cycle t))

(use-package vertico-directory
  :after vertico
  :ensure nil
  :bind (:map vertico-map
              ("DEL" . vertico-directory-delete-word)
              ("RET" . vertico-directory-enter)))

;; orderless — flexible completion matching (replaces flx/ivy-regex)
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; marginalia — rich annotations in completions
(use-package marginalia
  :init (marginalia-mode))

;; consult — search and navigation commands (replaces swiper/counsel)
(use-package consult
  :custom
  (consult-preview-key "M-."))

;; make remembering key sequences a bit easier
(use-package which-key)
(which-key-mode)
(which-key-setup-side-window-right)

;; key bindings
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-p") 'consult-buffer)

(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)

(global-set-key (kbd "M-,") 'windmove-left)
(global-set-key (kbd "M-.") 'windmove-right)
(global-set-key (kbd "M-;") 'previous-buffer)
(global-set-key (kbd "M-'") 'next-buffer)
(global-set-key (kbd "M-h") 'query-replace)
(global-set-key (kbd "M-/") 'other-window)
(global-set-key (kbd "M-o") 'other-window)

(global-set-key (kbd "C-x f") 'project-find-file)
(global-set-key (kbd "C-x C-f") 'find-file)
(global-set-key (kbd "C-x C-r") 'consult-recent-file)

(global-set-key (kbd "C-r") 'query-replace)
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-f") 'consult-line)
(global-set-key (kbd "M-l") 'goto-line)
(global-set-key (kbd "M-g") 'consult-imenu)
(global-set-key (kbd "s-h") 'query-replace)
(global-set-key (kbd "s-p") 'project-find-file)
(global-set-key (kbd "s-P") 'execute-extended-command)

;; testing and debugging workflow (M-t like VSCode alt+t)
(defvar my-test-last-command nil)

(defun my-test--project-root ()
  (or
   (let ((proj (project-current nil)))
     (when proj (project-root proj)))
   (locate-dominating-file default-directory "pyproject.toml")
   (locate-dominating-file default-directory "Cargo.toml")
   (locate-dominating-file default-directory "go.mod")
   (locate-dominating-file default-directory "package.json")
   (locate-dominating-file default-directory "CMakeLists.txt")
   default-directory))

(defun my-test--from-root (root cmd)
  (if (and root (file-directory-p root))
      (format "cd %s && %s" (shell-quote-argument (expand-file-name root)) cmd)
    cmd))

(defun my-test--js-test-command (root)
  (when (and root (file-exists-p (expand-file-name "package.json" root)))
    (cond
     ((file-exists-p (expand-file-name "pnpm-lock.yaml" root)) "pnpm test")
     ((file-exists-p (expand-file-name "yarn.lock" root)) "yarn test")
     (t "npm test"))))

(defun my-test--default-command ()
  (let* ((root (my-test--project-root))
         (has-file (and buffer-file-name root))
         (file-arg (when has-file
                     (shell-quote-argument
                      (file-relative-name buffer-file-name root)))))
    (cond
     ((derived-mode-p 'python-mode)
      (if file-arg
          (my-test--from-root root (format "pytest %s" file-arg))
        (my-test--from-root root "pytest")))
     ((derived-mode-p 'rust-mode)
      (my-test--from-root root "cargo test"))
     ((or (derived-mode-p 'go-mode) (derived-mode-p 'go-ts-mode))
      (my-test--from-root root "go test ./..."))
     ((or (derived-mode-p 'js-mode)
          (derived-mode-p 'js-ts-mode)
          (derived-mode-p 'typescript-mode)
          (derived-mode-p 'typescript-ts-mode))
      (let ((js-cmd (my-test--js-test-command root)))
        (when js-cmd
          (my-test--from-root root js-cmd))))
     ((or (derived-mode-p 'c-mode) (derived-mode-p 'c++-mode))
      (if (and root (file-exists-p (expand-file-name "CMakeLists.txt" root)))
          (my-test--from-root root "ctest --output-on-failure")
        nil))
     (t nil))))

(defun my-test-debug-at-cursor ()
  (interactive)
  (let* ((base (or (my-test--default-command)
                   (read-shell-command "Debug command: ")))
         (cmd (if (string-match-p "pytest" base)
                  (concat base " --pdb")
                base)))
    (setq my-test-last-command cmd)
    (compile cmd)))

(defun my-test-run-current-file ()
  (interactive)
  (let ((cmd (or (my-test--default-command)
                 (read-shell-command "Test command: "))))
    (setq my-test-last-command cmd)
    (compile cmd)))

(defun my-test-debug-last-run ()
  (interactive)
  (if my-test-last-command
      (compile my-test-last-command)
    (call-interactively 'my-test-run-current-file)))

(defun my-debug-start ()
  (interactive)
  (cond
   ((fboundp 'dap-debug) (call-interactively 'dap-debug))
   (t (call-interactively 'gdb))))

(defun my-debug-continue ()
  (interactive)
  (cond
   ((fboundp 'dap-continue) (call-interactively 'dap-continue))
   ((fboundp 'gud-cont) (call-interactively 'gud-cont))
   (t (message "No debug continue command available"))))

(defun my-debug-stop ()
  (interactive)
  (cond
   ((fboundp 'dap-disconnect) (call-interactively 'dap-disconnect))
   ((get-buffer "*compilation*") (kill-compilation))
   (t (message "No active debug/test process"))))

(defun my-debug-toggle-breakpoint ()
  (interactive)
  (cond
   ((fboundp 'dap-breakpoint-toggle) (call-interactively 'dap-breakpoint-toggle))
   ((fboundp 'gud-break) (call-interactively 'gud-break))
   (t (message "No breakpoint command available"))))

(define-prefix-command 'my-test-debug-map)
(global-set-key (kbd "M-t") 'my-test-debug-map)
(define-key my-test-debug-map (kbd "c") 'my-test-debug-at-cursor)
(define-key my-test-debug-map (kbd "f") 'my-test-run-current-file)
(define-key my-test-debug-map (kbd "l") 'my-test-debug-last-run)
(define-key my-test-debug-map (kbd "r") 'my-debug-continue)
(define-key my-test-debug-map (kbd "s") 'my-debug-stop)
(define-key my-test-debug-map (kbd "d") 'my-debug-start)
(define-key my-test-debug-map (kbd "b") 'my-debug-toggle-breakpoint)

;; sudo apt-get install clangd
;; pip3 install pyright
;; rustup component add rust-analyzer
(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  :init (global-corfu-mode))
(use-package eglot)
(use-package apheleia
  :init (apheleia-global-mode))
(use-package rust-mode)
(use-package markdown-mode
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode))
  :custom (markdown-command "pandoc"))

;; based on https://raw.github.com/google/styleguide/gh-pages/google-c-style.el
(defconst my-cc-style
  `((c-recognize-knr-p . nil)
    (c-basic-offset . 4)
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
                        (label . -)
                        (case-label . 0)
                        (statement-case-open . +)
                        (statement-case-intro . +) ; case w/o {
                        (access-label . -)
                        (innamespace . 0)))))

(c-add-style "my-cc-style" my-cc-style)

(defun my-programming-modes-hook ()
  (display-line-numbers-mode 1)

  (setq-default indent-tabs-mode nil)
  (setq tab-width 4)

  (eglot-ensure)
  (setq eglot-report-progress nil)

  (local-set-key (kbd "TAB") 'indent-for-tab-command)
  (local-set-key (kbd "RET") 'newline-and-indent)

  (local-set-key (kbd "M-c S") 'eglot-list-connections)
  (local-set-key (kbd "M-c a") 'eglot-code-actions)
  (local-set-key (kbd "M-c b") 'my-debug-toggle-breakpoint)
  (local-set-key (kbd "M-c c") 'completion-at-point)
  (local-set-key (kbd "M-c d") 'xref-find-definitions)
  (local-set-key (kbd "M-c f") 'eglot-format-buffer)
  (local-set-key (kbd "M-c g") 'consult-imenu)
  (local-set-key (kbd "M-c i") 'eglot-find-implementation)
  (local-set-key (kbd "M-c j") 'eglot-find-declaration)
  (local-set-key (kbd "M-c r") 'eglot-rename)
  (local-set-key (kbd "M-c x") 'xref-find-references)

  (local-set-key (kbd "M-q") 'ff-find-other-file))

(defun my-c-c++-mode-hook ()
  (setq compilation-scroll-output 'first-error)
  (c-set-style "my-cc-style"))

(defun my-python-mode-hook ()
  (local-set-key (kbd "M-c <") 'python-indent-shift-left)
  (local-set-key (kbd "M-c >") 'python-indent-shift-right)
  (local-set-key (kbd "M-c C-f") 'apheleia-format-buffer))

(add-hook 'c-mode-common-hook 'my-programming-modes-hook)
(add-hook 'c-mode-hook 'my-c-c++-mode-hook)
(add-hook 'c++-mode-hook 'my-c-c++-mode-hook)
(add-hook 'c-ts-mode-hook 'my-programming-modes-hook)
(add-hook 'c-ts-mode-hook 'my-c-c++-mode-hook)
(add-hook 'c++-ts-mode-hook 'my-programming-modes-hook)
(add-hook 'c++-ts-mode-hook 'my-c-c++-mode-hook)
(add-hook 'lisp-mode-hook 'my-programming-modes-hook)
(add-hook 'python-mode-hook 'my-programming-modes-hook)
(add-hook 'python-mode-hook 'my-python-mode-hook)
(add-hook 'python-ts-mode-hook 'my-programming-modes-hook)
(add-hook 'python-ts-mode-hook 'my-python-mode-hook)
(add-hook 'sh-mode-hook 'my-programming-modes-hook)
(add-hook 'bash-ts-mode-hook 'my-programming-modes-hook)
(add-hook 'rust-mode-hook 'my-programming-modes-hook)
(add-hook 'rust-ts-mode-hook 'my-programming-modes-hook)
(add-hook 'go-ts-mode-hook 'my-programming-modes-hook)
(add-hook 'js-ts-mode-hook 'my-programming-modes-hook)
(add-hook 'typescript-ts-mode-hook 'my-programming-modes-hook)
