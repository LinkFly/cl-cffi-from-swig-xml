start /b build-dll-module.bat
start /b build-xml.bat
sbcl --load test.lisp
