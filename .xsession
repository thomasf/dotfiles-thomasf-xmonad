#!/bin/bash
#
#   Xsession file -- used by display managers and xinit (startx)
#   Man page: Xsession
#
#   https://wiki.archlinux.org/index.php/ConsoleKit
#   http://www.jwiltshire.org.uk/content/2010/12/19/the-perfect-gpg-agent-setup/
#   https://wiki.archlinux.org/index.php/SSH_Keys#Using_GnuPG_Agent

# exec ck-launch-session "$HOME/.config-xmonad/xstart"
exec "$HOME/.config-xmonad/xstart"
