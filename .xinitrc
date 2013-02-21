#!/bin/sh
#
#    xinitrc file, used by all X sessions started by xinit (startx)
#    Man page: xinit
#
#  A good way of dealing with both xinit and xsession is to just source the xsession file from xinit.
#
exec ${HOME}/.config-xmonad/xstart
