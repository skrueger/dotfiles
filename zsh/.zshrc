# $ZDOTDIR/.zshrc
# Used for setting user's interactive shell configuration and executing commands, will be read when starting as an interactive shell.

# I use Grml's zsh configuration (grmlzshrc https://grml.org/zsh/).
# Grml is a bootable live system based on Debian used for system administration and rescue.
# Its name is derived from the "grr" sound  people use when expressing dissatisfaction.
# Grml provides an extensive zsh configuration that's usable in other Linux distributions.
# The grml zshrc file can be downloaded with `wget -O .zshrc https://git.grml.org/f/grml-etc-core/etc/zsh/zshrc`.
# On Arch Linux it can be installed with the official `grml-zsh-config` package.
# Grml and Arch Linux places the file in the global zshrc (/etc/zsh/zshrc).
# See `man grmlzshrc` for more information or its reference card (https://grml.org/zsh/grml-zsh-refcard.pdf).

# vi keybindings
bindkey -v

[[ -e $HOME/.cargo/env ]] && source "$HOME/.cargo/env"
[[ -e /usr/share/nvm/init-nvm.sh ]] && source /usr/share/nvm/init-nvm.sh

# fzf, the general-purpose command-line fuzzy finder.
# fzf provides the following keybindings:
# - Ctrl+t to list files & folders. For example, this is helpful with `git add`.
# - Ctrl+r to search command history.
# - Alt+c to change directories.
#
# fzf is installed on Arch Linux with the fzf package.
# fzf is enabled in zsh by sourcing its keybindings and completion.
f=/usr/share/fzf/key-bindings.zsh ; [[ -e $f ]] && source $f ; unset f
f=/usr/share/fzf/completion.zsh ; [[ -e $f ]] && source $f ; unset f
