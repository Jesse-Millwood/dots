#!/bin/bash

zsh_tar_url=https://github.com/robbyrussell/oh-my-zsh/archive/master.tar.gz
flatpak_zsh_url=https://github.com/bilelmoussaoui/flatpak-zsh-completion/archive/master.zip

curl -s -L -o ohmyzsh-master.tar.gz ${zsh_tar_url}
curl -s -L -o flatpak-zsh-completion.zip ${flatpak_zsh_url}
unzip flatpak-zsh-completion.zip

tar -xzf ohmyzsh-master.tar.gz
mv flatpak-zsh-completion-master ohmyzsh-master/plugins/flatpak
tar -czf ohmyzsh.tar.gz ohmyzsh-master

rm flatpak-zsh-completion.zip
rm ohmyzsh-master.tar.gz
rm -r ohmyzsh-master

chezmoi import --strip-components 1 --destination ${HOME}/.oh-my-zsh ohmyzsh.tar.gz
mkdir -p .backups/ohmyzsh
mv ohmyzsh.tar.gz .backups/ohmyzsh/ohmyzsh-$(date +"%m-%d-%Y").tar.gz
