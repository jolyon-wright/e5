#!/bin/bash

sudo apt install sbcl
mkdir -p ~/tmp
curl -o ~/tmp/ql.lisp http://beta.quicklisp.org/quicklisp.lisp
if [ $? -ne 0 ]; then
    echo "failure!"
else
    sbcl --no-sysinit --no-userinit --load ~/tmp/ql.lisp \
         --eval '(quicklisp-quickstart:install :path "~/.quicklisp")' \
         --eval '(ql:add-to-init-file)' \
         --quit
    sbcl --eval '(ql:quickload :quicklisp-slime-helper)' --quit
fi
