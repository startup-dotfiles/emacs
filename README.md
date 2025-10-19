# Dotfiles --- Emacs

```sh
# Backup/Rename
[ -d $HOME/.emacs.d ] && mv $HOME/.emacs.d $HOME/.emacs.d.bak
[ -f $HOME/.emacs.el ] && mv $HOME/.emacs.el .emacs.el.bak
[ -f $HOME/.emacs ] && mv $HOME/.emacs $HOME/.emacs.bak

[ -d $HOME/.config/emacs ] mv $HOME/.config/emacs $HOME/.config/emacs.bak
```

```sh
# See https://www.gnu.org/software/emacs/manual/html_node/emacs/Init-File.html

# Option 1: Base on XDG Spec (Recommanded)
mkdir -p $HOME/.config/emacs
git clone https://github.com/startup-dotfiles/emacs $HOME/.config/emacs

# Option 2: Standard Installation Location
mkdir -p $HOME/.emacs.d
git clone https://github.com/startup-dotfiles/emacs $HOME/.emacs.d
```

```sh
# These files can be deleted after installation.
rm -r README.md LICENSE .git/ .gitignore
```
