# $ZDOTDIR/.zshrc
# Used for setting user's interactive shell configuration and executing commands, will be read when starting as an interactive shell.

# Use the zsh config provided by https://grml.org/zsh/

# vi keybindings
bindkey -v

[[ -e $HOME/.cargo/env ]] && source "$HOME/.cargo/env"
[[ -e /usr/share/nvm/init-nvm.sh ]] && source /usr/share/nvm/init-nvm.sh
