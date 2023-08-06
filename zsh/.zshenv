# $ZDOTDIR/.zshenv
# Used for setting user's environment variables; it should not contain commands that produce output or assume the shell is attached to a TTY. When this file exists it will always be read.

HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
TERM=xterm-256color

# https://zsh.sourceforge.io/Guide/zshguide02.html#l24
typeset -U path PATH
path=(~/.local/bin $path)
