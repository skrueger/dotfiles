# ~/.bash_profile
# is executed for login shells
echo "~/.bash_profile start"
[[ -f ~/.bashrc ]] && . ~/.bashrc

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*

export PATH="$HOME/.cargo/bin:$PATH"
export PATH="/home/simon/cs140e/aarch64-none-elf/bin:$PATH"

echo "~/.bash_profile end"
