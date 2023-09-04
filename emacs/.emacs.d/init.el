;;; init.el --- Simon Krueger's Emacs Initialization file. -*- mode: elisp -*-

;;; Commentary

;; I want this file to be maintainable and portable.
;; Literate programming is a good idea.
;; Less is more.
;; Simple. Not complex.

;; I tested this configuration on:
;; - GNU Emacs 29.1 with Arch Linux.
;; - GNU Emacs 28.2 with Ubuntu 20.04.
;; - GNU Emacs 27.1 with Ubuntu 22.04.

;; gc-cons-threshold is the number of bytes of consing between garbage collections.
;; It is a variable define din Emacs C source code.
;; The default is 800 kilobytes.  I set it to 64 MB.
(setq gc-cons-threshold (* 64 1024 1024))

;; visible-bell t flashes the frame to represent a bell.
;; This variable is defined in emacs C source code.
(setq visible-bell t)

;; indent-tabs-mode nil indents with spaces.
;; Emacs 28 has an indent-tabs-mode function defined in simple.el.
;; I set the variable so this works in Emacs 27 on Ubuntu 22.04.
(setq-default indent-tabs-mode nil)

;; After startup, I message the initialization time and number of garbage collections.
;; emacs-startup-hook is a built-in inside of startup.el.
;; startup.el is not a package that can be loaded by use-package.
(add-hook 'emacs-startup-hook
          (lambda ()
            (let ((init-duration-time (time-subtract after-init-time before-init-time)))
              (message "Emacs initialized in %s with %d garbage collections."
                       (format "%.2f seconds" (float-time init-duration-time))
                       gcs-done))))

;; package.el is a package manager that is built into GNU Emacs since version 24.
;; require is a built-in function that loads a Lisp file if it has not already been loaded.
;; It does this with the built-in load function that executes Lisp code.
;; Emacs should already have package.el loaded, but I require it here anyway for clarity.
(require 'package)

;; package-archives is a list of repositories that package.el fetches from.
;; Elements of package-archives have the form (ID . LOCATION).
;; The ID is the archive name and the LOCATION is the URL of the repository.
;; By default package-archives contains "GNU Emacs Lisp Package Archive" (https://elpa.gnu.org).
;; GNU ELPA is the default package repository for GNU Emacs.
;; Starting with Emacs 28, package-archive also contains NonGNU Emacs Lisp Package Archive (https://elpa.nongnu.org).
;; I don't quite understand the exact difference between GNU ELPA and NonGNU ELPA.
;; I vaguely understand that GNU ELPA contains packages that fulfill GNU's requirements
;; and NonGNU ELPA contains packages that don't necessarily fulfill GNU's requirement.
;;
;; I add Milkypostman’s Emacs Lisp Package Archive (MELPA) because it contains
;; packages that are not in GNU ELPA or NonGNU ELPA.
;; MELPA contains packages that where built from the latest commits.
;; MELPA contains packages that where built from commits tagged as stable.
;; Some packages only exist in MEPLA because authors don't tag commits as stable.
;;
;; I only add repoistories that I trust because packages run arbitraty code.
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

;; package-initialize loads installed packages and activates them.
(package-initialize)

;; use-package simplifies package installation and configuration.
;; Its documentation is located at https://jwiegley.github.io/use-package/keywords/.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; autorevert.el is a built-in global minor mode that
;; automatically revert buffers
;; whenever the corresponding files have been changed on disk and the
;; buffer contains no unsaved changes.
(use-package autorevert
  :config
  (global-auto-revert-mode 1))

;; simple is a grab-bag of basic Emacs commands not specifically related to some
;; major mode or to file-handling.
;; It is a built-in package.
(use-package simple
  :config
  ;; column-number-mode 1 enables Column Number mode, which
  ;; displays the column number in the mode line.
  (column-number-mode 1))


;; display-line-numbers is a minor mode interface for displaying line numbers.
;; It is a built-in package since Emacs 26.
(use-package display-line-numbers
  ;; The :hook keyword adds functions onto hooks.
  ;; I add hooks to shell-mode-hook and term-mode-hook that disables display-line-numbers-mode.
  :hook ((shell-mode term-mode) . (lambda () (display-line-numbers-mode 0)))
  :demand t
  :config
  ;; global-display-line-numbers-mode t enables display-line-numbers-mode in all buffers.
  (global-display-line-numbers-mode t))

;; dired is a built-in major mode for directory browsing and editing.
(use-package dired
  :commands (dired dired-jump)
  :bind
  ("C-x C-j" . dired-jump)
  :init
  (setq dired-listing-switches "-alh --group-directories-first")
  (setq delete-by-moving-to-trash t))

;; flyspell is built-in package that that performs on-the-fly spell checking.
;; Arch Linux needs aspell and aspell-en for this to work.
(use-package flyspell
  :diminish flyspell-mode
  :hook (((text-mode org-mode) . turn-on-flyspell)
         (prog-mode . flyspell-prog-mode)))

;; org is a built-in package that is for keeping notes,
;; maining ToDo lists, and doing project planning with a fast and effective plain-text system.
(use-package org
  :bind (("C-c a" . org-agenda)
         ("C-c l" . org-store-link)
         ("C-c b" . org-switchb)
         ("C-c c" . org-capture))
  :init
  ;; org-agenda-span controls the number of days to include in the agenda overview display.
  (setq org-agenda-span 'day)
  ;; org-log-time 'time records the time when a task moves to the DONE state.
  (setq org-log-done 'time)
  ;; org-log-into-drawer t inserts state change notes and time stamps into the LOGBOOK drawer.
  (setq org-log-into-drawer t)
  ;; org-hide-emphasis-markers hides the emphasis marker characters.
  (setq org-hide-emphasis-markers t)

  ;; C-c l creates a new PROPERTIES id if it doesn't exist.
  ;; This allows headings to move around without breaking the link.
  (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

  (setq org-plantuml-jar-path "~/.lib/plantuml.jar")
  ;; org-indent-mode indents text according to the outline structure.
  :hook ((org-mode . (lambda () (org-indent-mode 1)))
         (org-agenda-mode . (lambda () (hl-line-mode 1))))
  :config
  ;; This updates org-refile to save all buffers after it runs.
  (advice-add 'org-refile :after 'org-save-all-org-buffers)
  (org-babel-do-load-languages
      'org-babel-load-languages
      '((emacs-lisp . t)
        (plantuml . t)
        (python . t))))

;; org-habit tracks habits in the agenda view.
;; It is built in to emacs but it must be loaded.
(use-package org-habit
  :after org
  :init
  (setq org-habit-graph-column 60)
  :config
  (add-to-list 'org-modules 'org-habit))

;; org-tempo expands the snippet structures and allows < s TAB to create a code block.
;; org-tempo is built into org, but it must be loaded.
(use-package org-tempo
  :after org
  :config
  ;; This adds shell, emacs-lisp, and shell templates with <sh TAB, <el TAB, and <py TAB.
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python")))

;; windmove is a built-in package for selecting windows.
(use-package windmove
  ;; windmove keybindings conflicts with org.
  ;; S-LEFT and S-RIGHT change a task's state.
  ;; S-UP and S-DOWN change a task's priority.
  ;; I prefer windmove keybindings over this functionality.
  ;; I can use C-c t and C-c , for changing a task's state and priority.
  :hook
  ((org-shiftup . windmove-up)
   (org-shiftleft . windmove-left)
   (org-shiftright . windmove-right)
   (org-shiftdown . windmove-down))
  ;; I set demand t so this package is always loaded.
  ;; use-package defers loading when there are hooks.
  :demand t
  :config
  ;; windmove-default-keybindings adds keybindings Shift + Arrows for selecting windows.
  (windmove-default-keybindings))

;; Winner is a built-in global minor mode that records the changes in the
;; window configuration (i.e. how the frames are partitioned into
;; windows) so that the changes can be "undone" using the command
;; `winner-undo'.  By default this one is bound to the key sequence
;; ctrl-c left.  If you change your mind (while undoing), you can
;; press ctrl-c right (calling `winner-redo').
(use-package winner
  :config
  (winner-mode 1))

;; Evil is an extensible vi layer for Emacs.
;; It emulates the main features of Vim, and provides facilities for writing custom extensions.
;; Its official documentation is located at https://evil.readthedocs.io/en/latest/overview.html.
;; Its source code is located at https://github.com/emacs-evil/evil.
(use-package evil
  ;; The :ensure keyword installs the package (if not already).
  :ensure t

  ;; The :init keyword executes code before a package is loaded.
  ;; It accepts one or more forms, up until the next keyword:.
  :init
  ;; evil-want-fine undo uses emacs heustics to determine changes to undo.
  (setq evil-want-fine-undo t)
  ;; evil-want-integration controls the loading of evil-integration.el.
  ;; evil-collection requires evil-integration is t.
  (setq evil-want-integration t)
  ;; evil-want-keybinding controls the evil-keybindings and evil-state that are loaded in other modes.
  ;; evil-want-keybindings must be set before evil is loaded.
  ;; evil-collection requires evil-want-keybinding is nil.
  (setq evil-want-keybinding nil)

  ;; The :config keyword executes code after a package is loaded.
  :config
  ;; evil-mode 1 enables evil-local mode in all buffers.
  (evil-mode 1)
  ;; define-key defines a key in a keymap.
  ;; C-g enters normal mode from insert mode.
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state))

;; evil-collection is a collection of Evil bindings for the parts of Emacs that Evil does not cover properly by default,
;; such as help-mode, M-x calendar, Eshell and more.
(use-package evil-collection
  :after evil
  :diminish evil-collection-unimpaired-mode
  :ensure t
  :config
  ;; evil-collection-init registers the evil bindings for all modes in evil-collection-mode-list.
  (evil-collection-init))

(use-package evil-nerd-commenter
  :after evil
  :ensure t
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

;; Magit is a complete text-based user interface to Git.
;; Magit gives me git superpowers.
;; Magit's website is located at https://magit.vc/.
(use-package magit
  :ensure t
  :commands magit-status
  :config
  ;; magit-display-buffer-same-window-except-diff-v1 displays most magit buffers in
  ;; the currently selected window.
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; Ivy is a generic completion mechanism for Emacs.
;; Ivy has completions for buffers and file names.
;; Its documentation is located at https://oremacs.com/swiper/.
;; Its source code is located at https://github.com/abo-abo/swiper.
(use-package ivy
  :ensure t
  :diminish ivy-mode
  :config
  ;; ivy-mode 1 turns Ivy mode on.
  (ivy-mode 1))

;; Counsel provides versions of common Emacs commands that are customised to make the best use of Ivy.
;; For example, counsel-find-file has some additional keybindings. Pressing DEL will move you to the parent directory.
(use-package counsel
  :ensure t
  :diminish counsel-mode
  :config
  ;; counsel-mode 1 turns Counsel mode on.
  ;; counsel-mode maps keybinds for common emacs functions to their counsel replacement.
  ;; For example, find-file becomes counsel-find-file.
  (counsel-mode 1))

;; ivy-prescient sorts and filters the list of candidates that appears in ivy.
;; For example, ivy-prescient prioritizes foo.org over foo.org~.
(use-package ivy-prescient
  :after (ivy counsel)
  :ensure t
  :init
  ;; ivy-prescient-enable-filtering nil does not change Ivy's filtering behavior.
  (setq ivy-prescient-enable-filtering nil)
  :config
  (ivy-prescient-mode 1))

;; projectile finds project files. A project is one with a .git folder or other project files (e.g., pom.xml).
;; I use C-c p f to find and open files in the project.
(use-package projectile
  :ensure t
  :diminish projectile-mode
  :init
  (setq projectile-completion-system 'ivy)
  ;; Turn off fd because there is an error with it. It says --strip-cwd-prefix cannot be found.
  ;; fd has --strip-cwd-prefix in >= 8.3.0. I disable fd to get around this issue.
  (setq projectile-git-use-fd nil)
  :config
  (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package counsel-projectile
  :ensure t
  :after (projectile counsel)
  :config
  (counsel-projectile-mode))

;; xclip copies and pastes from the GUI clipboard when running in text terminal.
(use-package xclip
  :ensure t
  :init
  ;; x-select-enable-clipboard t means cutting and pasting uses the clipboard.
  (setq x-select-enable-clipboard t)
  ;; x-select-enable-primary t means the selection is used in cutting and pasting.
  (setq x-select-enable-primary t)
  :config
  (xclip-mode 1))

;; which-key is a minor mode for Emacs that displays the key bindings
;; following your currently entered incomplete command (a prefix) in a popup.
;; For example, after enabling the minor mode if you enter C-x and wait for
;; the default of 1 second the minibuffer will expand with all of the available
;; key bindings that follow C-x (or as many as space allows given your settings).
;; Its source code is located at https://github.com/justbur/emacs-which-key.
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :init
  ;; which-key-idle-delay is the delay in seconds before which-key buffers pops up.
  (setq which-key-idle-delay 1)
  :config
  ;; which-key-mode 1 enables which-key minor mode.
  (which-key-mode 1))

;; paredit keeps parentheses balanced while editing.
;; It makes editing lisp code much easier.
(use-package paredit
  :ensure t
  :hook (emacs-lisp-mode . enable-paredit-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; undo-tree replaces the linear Emacs undo system with
;; a tree-based undo system .
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :after evil
  :config
  (global-undo-tree-mode 1)
  (evil-set-undo-system 'undo-tree))

(use-package plantuml-mode
  :ensure t
  :config
  (setq plantuml-jar-path "~/.lib/plantuml.jar")
  (setq plantuml-default-exec-mode 'jar)
  :custom
  (plantuml-indent-level 2)
  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
  (add-to-list 'auto-mode-alist '("\\.pu\\'" . plantuml-mode)))

;; diminish removes a mode's display indicator in the mode line.
;; use-package supports it with the diminish keyword.
(use-package diminish
  :ensure t)

;; custom-file is used for storing customization information.
;; customize-set-variables and customize-set-faces are examples of customizations.
;; By default it is nil, which means it stores the values in the init file.
;; I prefer place customizations in a separate file so the init file is tidy.
;; https://www.reddit.com/r/emacs/comments/67pzh5/using_customsetvariables_programmatically/
(setq custom-file "~/.emacs.d/emacs-custom.el")
(if (file-exists-p custom-file)
    (load custom-file))
