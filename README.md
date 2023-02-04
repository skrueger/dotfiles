### Overview
This is a repo of Simon Krueger's dotfiles.


[GNU Stow](https://www.gnu.org/software/stow/) is used to symlink these files into a $HOME directory (or where
ever they need to be installed).

e.g.,
```sh
stow --verbose=3 --target=$(readlink -f $HOME) emacs
```

NOTE: If the target is a symlink, it does not seem to work.
Instead give the full path.
This can usually be done with `readlink -f /path/to/symlink`.