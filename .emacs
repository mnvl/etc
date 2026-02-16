
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

;; sudo apt-get install fonts-inconsolata -y
;; sudo fc-cache -fv
(if (eq system-type 'windows-nt) (set-frame-font "Consolas-14"))
(if (eq system-type 'gnu/linux) (set-frame-font "Inconsolata-14"))
(if (eq system-type 'darwin) (set-frame-font "Menlo-18"))

;; text editing
(fset 'yes-or-no-p 'y-or-n-p)

(require 'saveplace)
(require 'paren)
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
(setq-default save-place 1)
(setq-default show-trailing-whitespace 1)

(use-package neotree)
(global-set-key [f8] 'neotree-toggle)

;; tip: C-u C-x = to get a name of face under cursor and some additional info
;; tip: M-x customize-themes to browse themes
;; tip: select "linux console" color theme in terminal if background is grey

;; ivy, swiper, and counsel
(use-package ivy)
(use-package swiper)
(use-package counsel)

(ivy-mode 1)
(setq enable-recursive-minibuffers t)
(setq ivy-use-virtual-buffers t)
(setq ivy-height 20)
(setq ivy-wrap t)

(define-key ivy-minibuffer-map (kbd "C-j") 'ivy-immediate-done)
(define-key ivy-minibuffer-map (kbd "RET") 'ivy-alt-done)

;; make remembering key sequences a bit easier
(use-package which-key)
(which-key-mode)
(which-key-setup-side-window-right)

;; key bindings
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "C-p") 'ivy-switch-buffer)

(global-set-key (kbd "C-S-x") 'kill-region)
(global-set-key (kbd "C-S-c") 'kill-ring-save)
(global-set-key (kbd "C-S-v") 'yank)

(global-set-key (kbd "M-<up>") 'windmove-up)
(global-set-key (kbd "M-<down>") 'windmove-down)
(global-set-key (kbd "M-<left>") 'windmove-left)
(global-set-key (kbd "M-<right>") 'windmove-right)
(global-set-key (kbd "M-h") 'query-replace)
(global-set-key (kbd "M-/") 'other-window)
(global-set-key (kbd "M-o") 'other-window)

(global-set-key (kbd "C-x f") 'projectile-find-file)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-x C-r") 'counsel-recentf)

;; press C-q to query-replace once in swiper mode
(global-set-key (kbd "C-r") 'ivy-resume)
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-g") 'goto-line)
(global-set-key (kbd "M-g") 'counsel-imenu)
(global-set-key (kbd "<escape>") 'keyboard-quit)

;; testing and debugging workflow (C-t / M-t like VSCode Alt+t)
(defvar my-test-last-command nil)

(defun my-test--project-root ()
  (or
   (when (fboundp 'projectile-project-root)
     (ignore-errors (projectile-project-root)))
   (let ((proj (and (fboundp 'project-current) (project-current nil))))
     (when (and proj (fboundp 'project-roots))
       (car (project-roots proj))))
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
(global-set-key (kbd "C-t") 'my-test-debug-map)
(define-key key-translation-map (kbd "M-t") (kbd "C-t"))
(define-key key-translation-map (kbd "M-c") (kbd "C-c"))
(define-key my-test-debug-map (kbd "c") 'my-test-debug-at-cursor)
(define-key my-test-debug-map (kbd "f") 'my-test-run-current-file)
(define-key my-test-debug-map (kbd "l") 'my-test-debug-last-run)
(define-key my-test-debug-map (kbd "r") 'my-debug-continue)
(define-key my-test-debug-map (kbd "s") 'my-debug-stop)
(define-key my-test-debug-map (kbd "d") 'my-debug-start)
(define-key my-test-debug-map (kbd "b") 'my-debug-toggle-breakpoint)
(global-set-key (kbd "C-c b") 'my-debug-toggle-breakpoint)

;; sudo apt-get install clangd
;; pip3 install pyright
;; cargo install rls
;; rustup component add rls
(use-package company)
(use-package eglot)
(use-package py-autopep8)
(use-package elpy :init (elpy-enable))
(use-package rust-mode)

(add-hook 'after-init-hook 'global-company-mode)

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
  (setq-local linum-format (if window-system "%4d" "%4d "))

  (setq-default indent-tabs-mode nil)
  (setq tab-width 4)

  (setq flycheck-check-syntax-automatically '(mode-enabled save))

  (eglot-ensure)
  (setq eglot-report-progress nil)

  (local-set-key (kbd "TAB") 'company-indent-or-complete-common)
  (local-set-key (kbd "RET") 'newline-and-indent)

  (local-set-key (kbd "C-c S") 'eglot-list-connections)
  (local-set-key (kbd "C-c a") 'eglot-code-actions)
  (local-set-key (kbd "C-c c") 'company-complete)
  (local-set-key (kbd "C-c d") 'xref-find-definitions)
  (local-set-key (kbd "C-c f") 'eglot-format-buffer)
  (local-set-key (kbd "C-c g") 'counsel-imenu)
  (local-set-key (kbd "C-c i") 'eglot-find-implementation)
  (local-set-key (kbd "C-c j") 'eglot-find-declaration)
  (local-set-key (kbd "C-c r") 'eglot-rename)
  (local-set-key (kbd "C-c x") 'xref-find-references)

  (local-set-key (kbd "M-q") 'ff-find-other-file))

(defun my-c-c++-mode-hook ()
  (setq compilation-scroll-output 'first-error)
  (c-set-style "my-cc-style"))

(defun my-python-mode-hook ()
  (local-set-key (kbd "C-c <") 'python-indent-shift-left)
  (local-set-key (kbd "C-c >") 'python-indent-shift-right)
  (local-set-key (kbd "C-c C-f") 'py-autopep8))

(add-hook 'c-mode-common-hook 'my-programming-modes-hook)
(add-hook 'c-mode-hook 'my-c-c++-mode-hook)
(add-hook 'c++-mode-hook 'my-c-c++-mode-hook)
(add-hook 'lisp-mode-hook 'my-programming-modes-hook)
(add-hook 'python-mode-hook 'my-programming-modes-hook)
(add-hook 'python-mode-hook 'my-python-mode-hook)
(add-hook 'sh-mode-hook 'my-programming-modes-hook)
(add-hook 'rust-mode-hook 'my-programming-modes-hook)
