#!/bin/sh
#
# Workspace launcher
#
# Author: Thomas Frössman  ( thomasf@jossystem.se / http://thomas.jossystem.se )
#

wsname -s && exit 1

exec emacs ~/.emacs.d/lisp/*.el ~/.emacs.d/updateVendor
