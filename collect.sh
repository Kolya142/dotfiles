. dotfiles.inc.sh

rm -rf $FDIRS

mkdir .emacs.d
mkdir .config
mkdir .config/i3

cp -r ~/Scripts .
cp ~/.emacs.d/init.el .emacs.d
cp ~/.emacs.d/fasm-mode.el .emacs.d
cp ~/.config/i3/config .config/i3
cp ~/.XCompose .
cp ~/.xinitrc .
