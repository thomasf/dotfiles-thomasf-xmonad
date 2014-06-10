#!/bin/bash
#
#    xinitrc file, used by all X sessions started by xinit (startx)
#    Man page: xinit
#
#  A good way of dealing with both xinit and xsession is to just source the xsession file from xinit.
#
if [ ${OSTYPE:0:6} = darwin ]; then
  exec quartz-wm
else
  exec ${HOME}/.config-xmonad/xstart
fi
