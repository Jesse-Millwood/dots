#!/bin/bash
set -eu

export XDG_CONFIG_HOME=${XDG_CONFIG_HOME:=${HOME}/.config}
export ZDOTDIR=${ZDOTDIR:=${XDG_CONFIG_HOME}/zsh}
export ZSH=${ZDOTDIR}/oh-my-zsh
export ZSH_CUSTOM=${ZSH}/custom

if [ ! -f $ZSH_CUSTOM/themes/spaceship.zsh-theme ]; then
    ln -s "$ZSH_CUSTOM/themes/spaceship-prompt/spaceship.zsh-theme" "$ZSH_CUSTOM/themes/spaceship.zsh-theme"
fi
