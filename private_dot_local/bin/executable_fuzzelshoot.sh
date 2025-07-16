#!/bin/bash

options="󰒅 Screenshot Area\n Screenshot Window\n󰹑 Screenshot Screen\n󰍽 Screenshot Active"

choice=$(echo -e "$options" | fuzzel --dmenu --prompt=" Select Screenshot Option: ")

case "$choice" in
    "󰒅 Screenshot Area")
        grimshot --notify savecopy area
        ;;
    " Screenshot Window")
        grimshot --notify savecopy window
        ;;
    "󰹑 Screenshot Screen")
        grimshot --notify savecopy screen
        ;;
    "󰍽 Screenshot Active")
        grimshot --notify savecopy active
        ;;
    *)
        echo "No valid option selected."
        ;;
esac
