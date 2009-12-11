@echo off

call "c:\Program Files (x86)\Microsoft Visual Studio 9.0\vc\vcvarsall.bat" x86 > NUL

C:
chdir C:\cygwin\bin

rxvt -e bash --login -i
