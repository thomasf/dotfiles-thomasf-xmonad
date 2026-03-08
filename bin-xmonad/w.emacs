#!/bin/sh
#
# Workspace launcher
#
# Author: Thomas Frössman  ( thomasf@jossystem.se / http://thomas.jossystem.se )
#

wsname -s && exit 1
cd ~/.emacs.d/
exec emacs init.el
