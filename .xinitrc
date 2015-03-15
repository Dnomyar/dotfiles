# .xinitrc

# Type de curseur
xsetroot -cursor_name left_ptr

# Backgroud screen
feh --bg-scale ~/wallpepper.jpg

# Azerty
setxkbmap fr

# Désactiver touchpad
/home/damien/.xmonad/touchpadHandler.sh

# Coriger bug applet java
export _JAVA_AWT_WM_NONREPARENTING=1


# Temporaire
export NIP=E134038T

ssh-agent /bin/bash &


stalonetray &

if [ -x /usr/bin/nm-applet ] ; then
   nm-applet --sm-disable &
fi

# ScreenSaver
/usr/bin/xscreensaver -no-splash &

xmonad &
xmobar &

# wait for xmonad
wait $!
pkill -HUP dzen2
pkill -HUP -f xmonad-clock
wait
