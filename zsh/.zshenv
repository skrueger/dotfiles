# $ZDOTDIR/.zshenv
# Used for setting user's environment variables; it should not contain commands that produce output or assume the shell is attached to a TTY. When this file exists it will always be read.

COMPDUMPFILE="${XDG_CACHE_HOME:-${HOME}/.cache}/zsh/.zcompdump"
GRML_COMP_CACHE_DIR="${XDG_CACHE_HOME:-${HOME}/.cache}/zsh"
HISTFILE="${XDG_STATE_HOME:-${HOME}/.local/state}"/zsh/history
if ! [[ -d $(dirname "$HISTFILE") ]]; then
  mkdir -p $(dirname "$HISTFILE")
fi
HISTSIZE=1000
SAVEHIST=1000
TERM=xterm-256color
INPUTRC=${XDG_CONFIG_HOME:-${HOME}/.config}/readline/inputrc
EDITOR=vim

# https://zsh.sourceforge.io/Guide/zshguide02.html#l24
typeset -U path PATH
path=(~/.local/bin $path)

# fzf - fuzzy finder
# I use --layout=reverse to keep what I type close to the command prompt.
# I find this more pleasant.
FZF_DEFAULT_OPTS='--layout=reverse'

[[ -r "$ZDOTDIR/.zshenv.local" ]] && source $ZDOTDIR/.zshenv.local
