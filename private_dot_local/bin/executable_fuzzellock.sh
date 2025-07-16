#!/bin/bash

options=""
options+=" Lock\n"
options+="󰩈 Exit\n"
options+=" Suspend\n"
options+=" Hibernate\n"
options+=" Reboot\n"
options+="⏻ Power Off\n"

choice=$(echo -e "$options" | fuzzel --dmenu --prompt="🗲 Select Power Option ")

case "$choice" in
    " Lock")
        swaylock && swayidle
        ;;
    "󰩈 Exit")
        loginctl terminate-session self
        ;;
    " Suspend")
        swaylock -f && systemctl suspend
        ;;
    " Hibernate")
        swaylock -f && systemctl hibernate
        ;;
    " Reboot")
        systemctl reboot
        ;;
    "⏻ Power Off")
        systemctl poweroff
        ;;
esac
