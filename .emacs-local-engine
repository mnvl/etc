
(ede-cpp-root-project "engine"
	:directory "~/engine"
	:file "~/engine/CMakeLists.txt"
	:include-path '("/src" "/contrib" "/contrib/lua/src" "/contrib/luabind/luabind")
)

(setq compile-command "vcbuild.exe \"d:\\home\\engine\\build\\ENGINE.sln\" \"Debug|Win32\" /M2")

(dolist (element (split-string (getenv "INCLUDE") ";"))
	(semantic-add-system-include element 'c++-mode))

(semantic-add-system-include "d:/tools/boost_1_43_0/" 'c++-mode)
(semantic-add-system-include "d:/tools/xerces-c-3.1.0-x86-windows-vc-9.0/include/" 'c++-mode)
(semantic-add-system-include "d:/tools/OgreSDK_vc9_v1-7-1/include/OGRE/" 'c++-mode)

