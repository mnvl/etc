
(ede-cpp-root-project "frombola"
	:directory "~/frombola"
	:file "~/frombola/CMakeLists.txt"
	:include-path '("/src")
)

(setq compile-command "vcbuild.exe \"d:/home/frombola/build/FROMBOLA.sln\" \"Debug|Win32\" /M8")

(dolist (element (split-string (getenv "INCLUDE") ";"))
	(semantic-add-system-include element 'c++-mode))

(semantic-add-system-include "d:/tools/boost_1_43_0/" 'c++-mode)
(semantic-add-system-include "d:/tools/xerces-c-3.1.0-x86-windows-vc-9.0/include/" 'c++-mode)
(semantic-add-system-include "d:/tools/OgreSDK_vc9_v1-7-1/include/OGRE/" 'c++-mode)

(add-to-list 'semantic-lex-c-preprocessor-symbol-map '("Q_GUI_EXPORT" . ""))
(add-to-list 'semantic-lex-c-preprocessor-symbol-map '("Q_CORE_EXPORT" . ""))

(setq qt4-base-dir "d:/tools/Qt/4.6.2/include")
(setq qt4-gui-dir (concat qt4-base-dir "/QtGui"))
(semantic-add-system-include qt4-base-dir 'c++-mode)
(semantic-add-system-include qt4-gui-dir 'c++-mode)
(add-to-list 'auto-mode-alist (cons qt4-base-dir 'c++-mode))
(add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt4-base-dir "/Qt/qconfig.h"))
(add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt4-base-dir "/Qt/qconfig-large.h"))
(add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt4-base-dir "/Qt/qglobal.h"))
