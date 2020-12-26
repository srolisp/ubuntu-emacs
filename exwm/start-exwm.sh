#!/bin/sh

xrdb ~/.emacs.d/exwm/Xresources

compton &

exec dbus-launch --exit-with-session emacs -mm --debug-init
