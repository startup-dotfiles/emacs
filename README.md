# Dotfiles --- Emacs

```sh
# Backup/Rename
[ -d $HOME/.emacs.d ] && mv $HOME/.emacs.d $HOME/.emacs.d.bak
[ -f $HOME/.emacs.el ] && mv $HOME/.emacs.el .emacs.el.bak
[ -f $HOME/.emacs ] && mv $HOME/.emacs $HOME/.emacs.bak
```

```sh
# See https://www.gnu.org/software/emacs/manual/html_node/emacs/Init-File.html

# Base on XDG Spec (Recommanded)
mkdir -p $HOME/.config/emacs
git clone https://github.com/range4-skyz/emacs $HOME/.config/emacs

# Standard Installation Location
git clone https://github.com/range4-skyz/emacs $HOME/.emacs.d
```
