#!/bin/sh

# https://www.gnu.org/software/bash/manual/html_node/The-Set-Builtin.html#The-Set-Builtin
# Exit immediately if a pipeline, which may consist of a single simple command, a list, or a compound command returns a non-zero status.
set -e
set -o xtrace

SCRIPT_PATH=$(readlink -f "$0")
SCRIPT_DIR_PATH=$(dirname $SCRIPT_PATH)
ln -s $SCRIPT_DIR_PATH/.tmux.conf  $HOME/.tmux.conf

