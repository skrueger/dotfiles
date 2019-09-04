#
# ~/.bash_profile
#

[[ -f ~/.bashrc ]] && . ~/.bashrc

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*

export PATH="$HOME/.cargo/bin:$PATH"
export PATH="/home/simon/cs140e/aarch64-none-elf/bin:$PATH"


# See terminal capability by running `infocmp`
# See what each capability does by running `man 5 terminfo`
# setaf set_a_foreground
GREEN="\[$(tput setaf 2)\]"
RESET="\[$(tput sgr0)\]"
YELLOW="\[$(tput setaf 3)\]"
BLUE="\[$(tput setaf 4)\]"
PS1="[${GREEN}\u${RESET}@${YELLOW}\h${RESET} ${BLUE}\W${RESET}]\n\$ "

