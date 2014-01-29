
(autoload 'php-mode "php-mode" "PHP editing mode" t)
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.lib$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.admin" . php-mode))

(ede-cpp-root-project "server" :name "server" :file "~/gggit/server/CMakeLists.txt"
		      :include-path '("/src" "/libs" "/libs/log4cplus/include" "/script"))

(defun my-c-mode-local-hook () (c-set-offset 'innamespace '+))
(add-hook 'c-mode-common-hook 'my-c-mode-local-hook 't)