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

source "$ZDOTDIR/grml.zsh"
xsource "$ZDOTDIR/.zshrc.local.pre"

# vi keybindings
bindkey -v

# xsource is a function defined in grmlzshrc that `source`s a file if it's readable.
# It is a terse way to source files.

# Add rust toolchain (rustc, cargo, rustup, etc) to the PATH.
xsource "$HOME/.cargo/env"

# Node Version Manager (NVM).
#
# Add it to PATH and setup completions.
# Arch Linux has an nvm AUR package (https://aur.archlinux.org/packages/nvm).
xsource /usr/share/nvm/init-nvm.sh
# The source code manual install location.
xsource "$HOME/.nvm/nvm.sh"

# fzf, the general-purpose command-line fuzzy finder.
# fzf provides the following keybindings:
# - Ctrl+t to list files & folders. For example, this is helpful with `git add`.
# - Ctrl+r to search command history.
# - Alt+c to change directories.
#
# fzf is installed on Arch Linux with the fzf package.
# fzf is enabled in zsh by sourcing its keybindings and completion.
xsource /usr/share/fzf/{key-bindings,completion}.zsh
# Ubuntu 22.04 places them here
xsource /usr/share/doc/fzf/examples/{key-bindings,completion}.zsh
# Fedora 41
xsource /usr/share/fzf/shell/key-bindings.zsh

# cdr changes the working directory to a previous working directory.
#
# Run `cdr<TAB>` to get a list of recent directories to switch to.
#
# cdr is a user defined function that is bundled with the zsh source code.
#
# For more information see:
# - https://github.com/zsh-users/zsh/blob/master/Functions/Chpwd/cdr
# - https://zsh.sourceforge.io/Doc/Release/User-Contributions.html
autoload -Uz chpwd_recent_dirs cdr add-zsh-hook
add-zsh-hook chpwd chpwd_recent_dirs
zstyle ':completion:*:*:cdr:*:*' menu selection
