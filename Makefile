HOME_DIR = $(shell readlink -f ${HOME})
XDG_CONFIG_HOME = ${HOME_DIR}/.config
STOW_IGNORE = Makefile|README

.PHONY: install uninstall

install:
	stow --ignore="${STOW_IGNORE}" --verbose=3 --target=${XDG_CONFIG_HOME} .

uninstall:
	stow --delete --ignore="${STOW_IGNORE}" --verbose=3 --target=${XDG_CONFIG_HOME} .
