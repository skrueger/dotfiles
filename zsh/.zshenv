# $ZDOTDIR/.zshenv
# Used for setting user's environment variables; it should not contain commands that produce output or assume the shell is attached to a TTY. When this file exists it will always be read.

HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
TERM=xterm-256color

# https://zsh.sourceforge.io/Guide/zshguide02.html#l24
typeset -U path PATH
path=(~/.local/bin $path)

# fzf - fuzzy finder
# I use --layout=reverse to keep what I type close to the command prompt.
# I find this more pleasant.
FZF_DEFAULT_OPTS='--layout=reverse'
