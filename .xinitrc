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

# start emacs daemon and shell
emacs --daemon &
st -e tmux &

# exec gnome-session
# exec ck-launch-session startkde
# exec startxfce4
# ...or the Window Manager of your choice
exec dwm
