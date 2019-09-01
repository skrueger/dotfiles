#!/bin/sh

# https://www.gnu.org/software/bash/manual/html_node/The-Set-Builtin.html#The-Set-Builtin
# Exit immediately if a pipeline, which may consist of a single simple command, a list, or a compound command returns a non-zero status.
set -e
set -o xtrace

SCRIPT_PATH=$(readlink -f "$0")
SCRIPT_DIR_PATH=$(dirname $SCRIPT_PATH)

FILES=(
  .tmux.conf
  .inputrc
)

for FILE in "${FILES[@]}"
do
  FILE_PATH=$SCRIPT_DIR_PATH/$FILE
  TARGET=$HOME/$FILE
  if ! ln -s $FILE_PATH $TARGET
  then
    echo Skipping $FILE because $TARGET alreasy exists
    if [[ -L $TARGET ]]
    then
       echo $TARGET is a symlink
    else
       echo $TARGET is a NOT symlink
    fi
  else
    echo Symlink created at $TARGET
  fi
done

