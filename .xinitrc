#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

if [ -f "$HOME"/.Xmodmap ]; then
    /usr/bin/xmodmap "$HOME"/.Xmodmap
fi

# start emacs daemon
/usr/bin/emacs --daemon &

# start terminal with tmux session
/usr/bin/st -e /usr/bin/tmux &

# set the keyboard repeat delay and rate
/usr/bin/xset r rate 170 30 &

# launch screensaver daemon
/usr/bin/xscreensaver -no-splash &

# exec gnome-session
# exec ck-launch-session startkde
# exec startxfce4
# ...or the Window Manager of your choice
exec /usr/bin/dwm
