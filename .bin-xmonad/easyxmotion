#!/usr/bin/env python
# -*- coding: utf-8 -*-

#############################################################################
##
## Copyright 2012
## Author: Loki Davison <loki.davison@gravityfour.com>
## gravityfour.com and musicfilmcomedy.com
##
## This program is free software; you can redistribute it and/or
## modify it under the terms of the GNU General Public License as
## published by the Free Software Foundation; either version 2 of 
## the License, or (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program.  If not, see <http://www.gnu.org/licenses/>.
##
#############################################################################

"""
A Easy motion style window switcher.

inspired by the vim plugin:
https://github.com/Lokaltog/vim-easymotion

"""
import time, string
import wnck
import pyosd
from Xlib.display import Display
from Xlib import X, XK
import sys, os, signal
import operator

# Config values
# font = "-monotype-arial-bold-r-normal--75-0-0-0-p-0-iso8859-15" # looks prettier but isn't installed everywhere.
#

timestamp = int(time.time())

def display_osd(options):
    s = wnck.screen_get_default()
    s.force_update()
    windows = s.get_windows()
    osds = []
    ws = s.get_active_workspace()

    windows = sorted(windows, key=operator.methodcaller("get_pid"))
    windows = [window for window in windows if window.is_visible_on_workspace(ws)]

    for i, window in enumerate(windows):
        if window.is_visible_on_workspace(ws):
            osd = pyosd.osd(options.font)
            osd.set_timeout(-1)
            osd.set_colour(options.colour)
            osd.set_outline_offset(1)
            osd.set_outline_colour(options.outline_colour)
            osd.set_shadow_offset(2)
            x, y  = window.get_geometry()[:2]
            osd.set_horizontal_offset(x)
            osd.set_vertical_offset(y)
            # XXX explodes if more than 26 windows are visable.
            osd.display(string.lowercase[i])
            osds.append(osd)
    return osds, windows

def main(options):
    # current display
    pid_file = '/var/lock/easyxmotion.pid'
    # kill any old versions that are still running,
    # we do it this way so the current one has input focus.
    # might be a better way to just exit and give focus to the old one.
    try:
        with open(pid_file, 'r') as fp:
            pid = int(fp.read())
            try:
                os.kill(pid, signal.SIGTERM)
            except OSError:
                #other isn't running
                pass
    except IOError:
        # first ever run
        pass
    with open(pid_file, 'w') as fp:
        fp.write(str(os.getpid()))

    osds, windows = display_osd(options)
    disp = Display()
    root = disp.screen().root
    root.change_attributes(event_mask = X.KeyPressMask)
    root.grab_keyboard(False, X.GrabModeAsync, X.GrabModeAsync, X.CurrentTime)

    event = disp.next_event()
    keycode = event.detail
    if event.type == X.KeyPress:
        key = XK.keysym_to_string(disp.keycode_to_keysym(keycode, 0))
        if key and key in string.lowercase and string.lowercase.index(key) < len(windows):
            windows[string.lowercase.index(key)].activate(timestamp)
        disp.ungrab_keyboard(X.CurrentTime)
        sys.exit()

if __name__ == '__main__':
    import optparse

    parser = optparse.OptionParser(
        usage = "%prog [options]",
        description = "A Easy motion style window switcher."
    )

    parser.add_option("-f", "--font",
        dest = "font",
        help = """font specified in old style specification,
as returned by xfontsel or xlsfonts. 
If you're using a patched xosd, then you must use XFT font specs.""",
        default = "-misc-fixed-bold-r-normal--50-0-100-100-c-0-iso8859-2"
    )

    parser.add_option("-c", "--colour",
        dest = "colour",
        help = "set text colour. In hex or X11 style words.",
        default = 'red'
    )

    parser.add_option("-o","--outline",
        dest = "outline_colour",
        help = "set text outline colour. In hex or X11 style words.",
        default = 'black'
    )

    (options, args) = parser.parse_args()

    main(options)
