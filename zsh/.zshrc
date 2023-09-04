# $ZDOTDIR/.zshrc
# Used for setting user's interactive shell configuration and executing commands, will be read when starting as an interactive shell.

# Use the zsh config provided by https://grml.org/zsh/

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
