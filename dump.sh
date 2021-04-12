#!/bin/sh

MYEMACS=/usr/local/opt/emacs-mac/Emacs.app/Contents/MacOS/Emacs.sh
sh $MYEMACS --batch \
    --load ~/.emacs.d/init.el \
    --execute "(dump-emacs \"mymacs\" \"$MYEMACS\")"
