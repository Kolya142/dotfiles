# A special script for publicsony

SH=/home/sony/

mkdir ~/.config/

mkdir ~/Projects
mkdir ~/Probe
mkdir ~/ThirdParty
mkdir ~/NotProjectsButMine
mkdir ~/JustSomeProgramming
ln -s ${SH}Scripts ~/
ln -s ${SH}.config ~/
ln -s ${SH}.config/i3 ~/.config
echo You should setup firefox manually
ln -s ${SH}.emacs.d ~/.config
ln -s ${SH}.XCompose ~/
ln -s ${SH}.xinitrc ~/

