@echo off

call "c:\Program Files\Microsoft Visual Studio 9.0\vc\vcvarsall.bat" > NUL

C:
chdir C:\cygwin\bin

rxvt -fg gray -bg black -fn 'Courier New-21' -e bash --login -i
