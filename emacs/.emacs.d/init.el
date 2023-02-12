;;; init --- Initialization file for krusimon's emacs -*- mode: elisp -*-

;;; Commentary:
;;; Many thanks to David Wilson of Systems Crafters youtube channel
;;; Many of the contents is derived from https://github.com/daviwil/emacs-from-scratch

;;; Code:

; https://www.gnu.org/software/emacs/manual/html_mono/ccmode.html
; CC Mode is a GNU Emacs mode for editing files containing C, C++, Objective-C, Java, CORBA IDL (and the variants PSDL and CIDL), Pike and AWK code.
; It provides syntax-based indentation, font locking, and has several handy commands and some minor modes to make the editing easier.
; It does not provide tools to look up and navigate between functions, classes, etc.; there are other packages for that.
(require 'cc-mode)

(defvar efs/default-font-size 180)
(defvar efs/default-variable-font-size 180)

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1024 1024))

; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)

(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                     (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)

;; Initialize package sources
;; 'package is the built-in package manager
;; https://wikemacs.org/wiki/Package.el
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; https://github.com/jwiegley/use-package
;; The use-package macro allows you to isolate package configuration in your .emacs file
;; in a way that is both performance-oriented and, well, ti
;; NOTE: use-package is not a package manager!
;; Although use-package does have the useful capability to interface with package managers,
;; its primary purpose is for the configuration and loading of packages.
(eval-when-compile
  (require 'use-package))

(setq use-package-always-ensure t)

;; use-package information
;; :init keyword
;; executes code BEFORE a package is loaded.
;; It accepts one or more forms, up to the next keyword
;;  Since the :init form is always run -- remember to restrict :init code to only what would succeed either way.

;; :config can be used to execute code AFTER a package is loaded.
;; In cases where loading is done lazily, this execution is deferred until after the autoload occurs.

;; :autoload keyword
;; ?

;; :bind keyword

;; :commands keyword
;; creates autoloads for those commands and defers loading of the module until they are used

;; :mode keyword

;; :interpreter keyword

;; :hook keyword
;; allows adding functions onto package hooks

;; :custom keyword
;; allows customization of package custom variables

;; :after keyword
;;


(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))

;; NOTE: If you want to move everything out of the ~/.emacs.d folder
;; reliably, set `user-emacs-directory` before loading no-littering!
;(setq user-emacs-directory "~/.cache/emacs")

(use-package no-littering)

;; no-littering doesn't set this by default so we must place
;; auto save files in the same path as it uses for sessions
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(setq inhibit-startup-message t)

; (scroll-bar-mode -1)        ; Disable visible scrollbar
; (tool-bar-mode -1)          ; Disable the toolbar
; (tooltip-mode -1)           ; Disable tooltips
; (set-fringe-mode 10)        ; Give some breathing room

; (menu-bar-mode -1)            ; Disable the menu bar

;; Set up the visible bell
(setq visible-bell t)

(column-number-mode)
(global-display-line-numbers-mode t)

;; Set frame transparency
; (set-frame-parameter (selected-frame) 'alpha efs/frame-transparency)
; (add-to-list 'default-frame-alist `(alpha . ,efs/frame-transparency))
; (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
; (add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;(set-face-attribute 'default nil :font "Fira Code Retina" :height efs/default-font-size)

;; Set the fixed pitch face
;(set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height efs/default-font-size)

;; Set the variable pitch face
;(set-face-attribute 'variable-pitch nil :font "Bookerly" :height efs/default-variable-font-size :weight 'regular)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package general
  :after evil
  :config
  (general-create-definer efs/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (efs/leader-keys
    "b" '(lambda () (interactive) (ivy-switch-buffer))
    "g" '(lambda () (interactive) (magit))
    "t"  '(:ignore t :which-key "toggles")
    "tt" '(counsel-load-theme :which-key "choose theme")
    "fde" '(lambda () (interactive) (find-file (expand-file-name "~/.emacs.d/Emacs.org")))
    "w" '(lambda () (interactive) (save-buffer))))

(use-package evil
  :init
  (setq evil-want-fine-undo t)
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package command-log-mode
  :commands command-log-mode)

; (use-package twilight-bright-theme)
; (use-package doom-themes
;   :init (load-theme 'doom-one-light t))
; doom-one-light is a light mode
; doom-one is a dark mode

(use-package all-the-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))
(setq find-file-visit-truename t)

(modify-syntax-entry ?_ "w")

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("C-M-j" . 'counsel-switch-buffer)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (counsel-mode 1))

(use-package ivy-prescient
  :after (ivy counsel)
  :custom
  (ivy-prescient-enable-filtering nil)
  :config
  ;; Uncomment the following line to have sorting remembered across sessions!
  ;(prescient-persist-mode 1)
  (ivy-prescient-mode 1))

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package hydra
  :defer t)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(efs/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text"))

(defun efs/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Bookerly" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil    :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
  (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
  (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch))

(defun efs/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1)
  (setq evil-auto-indent nil))

(use-package org
  :pin org
  :bind (("C-c a" . org-agenda)
         ("C-c l" . org-store-link)
         ("C-c b" . org-switchb))
  :commands (org-capture org-agenda)
  :hook (org-mode . efs/org-mode-setup)
  :config
  (add-hook 'org-mode-hook 'turn-on-flyspell)
  (setq org-ellipsis " ▾")
  (setq org-hide-emphasis-markers t)

  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  (setq org-agenda-files
        '("~/org/"))

  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)

  (setq org-todo-keywords
    '((sequence "TODO(t!)" "NEXT(n!)" "|" "DONE(d!)" "CANCELED(c!)")
      (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

  (setq org-refile-targets
    '(("archive.org" :maxlevel . 3)
      ("tasks.org" :maxlevel . 1)))

  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (setq org-tag-alist
    '((:startgroup)
       ; Put mutually exclusive tags here
       (:endgroup)
       ("@errand" . ?e)
       ("@home" . ?h)
       ("@work" . ?w)
       ("1on1" . ?1)
       ("agenda" . ?a)
       ("batch" . ?b)
       ("journal" . ?j)
       ("meetings" . ?m)
       ("note" . ?n)
       ("planning" . ?p)
       ("publish" . ?P)
       ("idea" . ?i)))

  ;; Configure custom agenda views
  (setq org-agenda-custom-commands
   '(("d" "Dashboard"
     ((agenda "" ((org-deadline-warning-days 7)))
      (todo "NEXT"
        ((org-agenda-overriding-header "Next Tasks")))
      (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

    ("n" "Next Tasks"
     ((todo "NEXT"
        ((org-agenda-overriding-header "Next Tasks")))))

    ("W" "Work Tasks" tags-todo "+work-email")

    ;; Low-effort next actions
    ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
     ((org-agenda-overriding-header "Low Effort Tasks")
      (org-agenda-max-todos 20)
      (org-agenda-files org-agenda-files)))

    ("w" "Workflow Status"
     ((todo "WAIT"
            ((org-agenda-overriding-header "Waiting on External")
             (org-agenda-files org-agenda-files)))
      (todo "REVIEW"
            ((org-agenda-overriding-header "In Review")
             (org-agenda-files org-agenda-files)))
      (todo "PLAN"
            ((org-agenda-overriding-header "In Planning")
             (org-agenda-todo-list-sublevels nil)
             (org-agenda-files org-agenda-files)))
      (todo "BACKLOG"
            ((org-agenda-overriding-header "Project Backlog")
             (org-agenda-todo-list-sublevels nil)
             (org-agenda-files org-agenda-files)))
      (todo "READY"
            ((org-agenda-overriding-header "Ready for Work")
             (org-agenda-files org-agenda-files)))
      (todo "ACTIVE"
            ((org-agenda-overriding-header "Active Projects")
             (org-agenda-files org-agenda-files)))
      (todo "COMPLETED"
            ((org-agenda-overriding-header "Completed Projects")
             (org-agenda-files org-agenda-files)))
      (todo "CANC"
            ((org-agenda-overriding-header "Cancelled Projects")
             (org-agenda-files org-agenda-files)))))))

  (setq org-capture-templates
    `(("t" "Tasks / Projects")
      ("tt" "Task" entry (file+olp "~/org/tasks.org" "Inbox")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

      ("j" "Journal Entries")
      ("jj" "Journal" entry
           (file+olp+datetree "~/org/journal.org")
           "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
           ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
           :clock-in :clock-resume
           :empty-lines 1)
      ("jm" "Meeting" entry
           (file+olp+datetree "~/org/journal.org")
           "* %<%I:%M %p> - MEETING_TITLE :meetings:\n\n%?\n\n%a\n\n"
           :clock-in :clock-resume
           :empty-lines 1)

      ("w" "Workflows")
      ("we" "Checking Email" entry (file+olp+datetree "~/org/journal.org")
           "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)

      ("m" "Metrics Capture")
      ("mw" "Weight" table-line (file+headline "~/org/metrics.org" "Weight")
       "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)))

  (define-key global-map (kbd "C-c j")
    (lambda () (interactive) (org-capture nil "jj")))

  (efs/org-font-setup)
  ; https://emacs.stackexchange.com/a/41314
  (defun my/org-read-datetree-date (d)
    "Parse a time string D and return a date to pass to the datetree functions."
    (let ((dtmp (nthcdr 3 (parse-time-string d))))
       (list (cadr dtmp) (car dtmp) (caddr dtmp))))
  (defun my/org-refile-to-archive-datetree (&optional bfn)
    "Refile an entry to a datetree under an archive."
    (interactive)
    (require 'org-datetree)
    (let* ((bfn (or bfn (find-file-noselect (expand-file-name "~/org/archive.org"))))
            (datetree-date (my/org-read-datetree-date (org-read-date t nil))))
        (org-refile nil nil (list nil (buffer-file-name bfn) nil
                                (with-current-buffer bfn
                                    (save-excursion
                                    (org-datetree-find-date-create datetree-date)
                                    (point))))))
    (setq this-command 'my/org-refile-to-journal)))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))

(with-eval-after-load 'org
  (org-babel-do-load-languages
      'org-babel-load-languages
      '((emacs-lisp . t)
      (python . t)))

  (push '("conf-unix" . conf-unix) org-src-lang-modes))

(with-eval-after-load 'org
  ;; org-tempo is needed as of Org 9.2
  (require 'org-tempo)

  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python")))

;; Automatically tangle our Emacs.org config file when we save it
(defun efs/org-babel-tangle-config ()
  (when (string-equal (file-name-directory (buffer-file-name))
                      (expand-file-name user-emacs-directory))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))

;(setq org-roam-v2-ack t)
(use-package org-roam
      :ensure t
      :hook ((after-init . org-roam-setup)
             (org-roam-backlinks-mode . visual-line-mode))
      :custom
      (org-roam-directory (file-truename "~/org"))
      :bind
              (("C-c n c" . org-roam-capture)
               ("C-c n f" . org-roam-node-find)
               ("C-c n g" . org-roam-graph)
               ("C-c n d" . org-id-)
               )
      (:map org-roam-mode-map
              (("C-c n c" . org-roam-capture) ; creates a node if it does not exist, and restores the current window configuration upon completion.
               ("C-c n f" . org-roam-node-find) ; creates a node if it does not exist, and visits the node.
               ("C-c n g" . org-roam-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-node-insert)) ; creates a node if it does not exist, and inserts a link to the node at point.
              (("C-c n I" . org-roam-node-insert-immediate))))

; (defun efs/lsp-mode-setup ()
;   (setq lsp-headerline-breadcrumb-segments '(project path-up-to-project file symbols))
;   (lsp-headerline-breadcrumb-mode))
;
; (use-package lsp-mode
;   :commands (lsp lsp-deferred)
;   :hook (lsp-mode . efs/lsp-mode-setup)
;   :init
;   (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
;   :config
;   (lsp-enable-which-key-integration t))
;
; (use-package lsp-ui
;   :hook (lsp-mode . lsp-ui-mode)
;   :custom
;   (lsp-ui-doc-position 'bottom))
;
; (use-package lsp-treemacs
;   :after lsp)
;
; (use-package lsp-ivy
;   :after lsp)
;
; (use-package dap-mode
;   ;; Uncomment the config below if you want all UI panes to be hidden by default!
;   ;; :custom
;   ;; (lsp-enable-dap-auto-configure nil)
;   ;; :config
;   ;; (dap-ui-mode 1)
;   :commands dap-debug
;   :config
;   ; https://emacs-lsp.github.io/dap-mode/page/configuration/#native-debug-gdblldb
;   (require 'dap-gdb-lldb)
;   (dap-register-debug-template "Rust::GDB Run Configuration"
;                              (list :type "gdb"
;                                    :request "launch"
;                                    :name "GDB::Run"
;                            :gdbpath "rust-gdb"
;                                    :target nil
;                                    :cwd nil))
;   ;; Set up Node debugging
;   (require 'dap-node)
;   (dap-node-setup) ;; Automatically installs Node debug adapter if needed
;
;   ;; Bind `C-c l d` to `dap-hydra` for easy access
;   (general-define-key
;     :keymaps 'lsp-mode-map
;     :prefix lsp-keymap-prefix
;     "d" '(dap-hydra t :wk "debugger")))
;
; (use-package typescript-mode
;   :mode "\\.ts\\'"
;   :hook (typescript-mode . lsp-deferred)
;   :config
;   (setq typescript-indent-level 2))
;
; (use-package python-mode
;   :ensure t
;   :hook (python-mode . lsp-deferred)
;   :custom
;   ;; NOTE: Set these if Python 3 is called "python3" on your system!
;   ;; (python-shell-interpreter "python3")
;   ;; (dap-python-executable "python3")
;   (dap-python-debugger 'debugpy)
;   :config
;   (require 'dap-python))
;
; (use-package pyvenv
;   :after python-mode
;   :config
;   (pyvenv-mode 1))

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (setq projectile-project-search-path
        '(
          "~/repos"
          "/workplace/"
          ))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :after projectile
  :config
  (counsel-projectile-mode)
  ;; Fix counsel-projectile-switch-project to use projectile-dired
  ;; https://github.com/ericdanan/counsel-projectile/issues/98
  (counsel-projectile-modify-action
   'counsel-projectile-switch-project-action
   '((move counsel-projectile-switch-project-action-dired 1)
     (setkey counsel-projectile-switch-project-action-dired "D")
     (setkey counsel-projectile-switch-project-action " "))))

(use-package magit
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; NOTE: Make sure to configure a GitHub token before using this package!
;; - https://magit.vc/manual/forge/Token-Creation.html#Token-Creation
;; - https://magit.vc/manual/ghub/Getting-Started.html#Getting-Started
; (use-package forge
;  :after magit)

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

; (use-package term
;   :commands term
;   :config
;   (setq explicit-shell-file-name "zsh") ;; Change this to zsh, etc
;   ;;(setq explicit-zsh-args '())         ;; Use 'explicit-<shell>-args for shell-specific args
;
;   ;; Match the default Bash shell prompt.  Update this if you have a custom prompt
;   (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))
;
; (use-package eterm-256color
;   :hook (term-mode . eterm-256color-mode))

; (use-package vterm
;   :commands vterm
;   :config
;   (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")  ;; Set this to match your custom shell prompt
;   ;;(setq vterm-shell "zsh")                       ;; Set this to customize the shell to launch
;   (setq vterm-max-scrollback 10000))

; (when (eq system-type 'windows-nt)
;   (setq explicit-shell-file-name "powershell.exe")
;   (setq explicit-powershell.exe-args '()))

; (defun efs/configure-eshell ()
;   ;; Save command history when commands are entered
;   (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)
;
;   ;; Truncate buffer for performance
;   (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)
;
;   ;; Bind some useful keys for evil-mode
;   (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'counsel-esh-history)
;   (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)
;   (evil-normalize-keymaps)
;
;   (setq eshell-history-size         10000
;         eshell-buffer-maximum-lines 10000
;         eshell-hist-ignoredups t
;         eshell-scroll-to-bottom-on-input t))

; (use-package eshell-git-prompt
;   :after eshell)
;
; (use-package eshell
;   :hook (eshell-first-time-mode . efs/configure-eshell)
;   :config
;
;   (with-eval-after-load 'esh-opt
;     (setq eshell-destroy-buffer-when-process-dies t)
;     (setq eshell-visual-commands '("htop" "zsh" "vim")))
;
;   (eshell-git-prompt-use-theme 'powerline))

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-alh --group-directories-first"))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer)
  (setq delete-by-moving-to-trash t))

(use-package dired-single
  :commands (dired dired-jump))

; 2021-06-03 commented out because projectile was getting an error: (wrong-type-argument char-or-string-p nil)
; Setting (setq debug-on-error t) showed a stacktrace with all-the-icons-for-file in it
; (use-package all-the-icons-dired
;   :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-open
  :ensure t
  :commands (dired dired-jump)
  :config
  ;; Doesn't work as expected!
  ;;(add-to-list 'dired-open-functions #'dired-open-xdg t)
  (setq dired-open-extensions '(("png" . "feh")
                                ("mkv" . "mpv")
                                ("jpeg" . "feh")
                                ("jpg" . "feh"))))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

(use-package paredit)

(use-package plantuml-mode
  :config
  (setq plantuml-jar-path "/home/ANT.AMAZON.COM/krusimon/lib/java/plantuml.jar")
  (setq plantuml-default-exec-mode 'jar)
  :custom
  (plantuml-indent-level 2))
(add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
(add-to-list 'auto-mode-alist '("\\.pu\\'" . plantuml-mode))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package flycheck-plantuml)

(with-eval-after-load 'flycheck
  (require 'flycheck-plantuml)
  (flycheck-plantuml-setup))

;; apt install isync for mbsync
; (use-package mu4e
;   :ensure nil
;   :load-path "/usr/share/emacs/site-lisp/mu4e/"
;   :config
;   (setq mu4e-change-filenames-when-moving t)
;   ;(setq mu4e-update-interval (* 10 60))
;   (setq mu4e-get-mail-command "mbsync -a")
;   (setq mu4e-maildir "~/mail/krusimon@amazon.com")
;   (setq mu4e-refile-folder "/Archive")
;   (setq mu4e-sent-folder "/Sent Items")
;   (setq mu4e-maildir-shortcuts
;         '(("/Inbox" . ?i)
;           ("/Sent Items" . ?s)
;           ("/Archive" . ?a))))

; (use-package rustic
;   :ensure
;   :bind (:map rustic-mode-map
;               ("M-j" . lsp-ui-imenu)
;               ("M-?" . lsp-find-references)
;               ("C-c C-c l" . flycheck-list-errors)
;               ("C-c C-c a" . lsp-execute-code-action)
;               ("C-c C-c r" . lsp-rename)
;               ("C-c C-c q" . lsp-workspace-restart)
;               ("C-c C-c Q" . lsp-workspace-shutdown)
;               ("C-c C-c s" . lsp-rust-analyzer-status))
;   :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; uncomment to enable rustfmt on save
  ;(setq rustic-format-on-save t)
;   (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))
;
; (defun rk/rustic-mode-hook ()
;   ;; so that run C-c C-c C-r works without having to confirm
;   (setq-local buffer-save-without-query t))

; (use-package lsp-mode
;   :ensure
;   :commands lsp
;   :custom
;   ;; what to use when checking on-save. "check" is default, I prefer clippy
;   (lsp-rust-analyzer-cargo-watch-command "clippy")
;   (lsp-eldoc-render-all t)
;   (lsp-idle-delay 0.6)
;   :config
;   (add-hook 'lsp-mode-hook 'lsp-ui-mode))
;
; (use-package lsp-ui
;   :ensure
;   :commands lsp-ui-mode
;   :custom
;   (lsp-ui-peek-always-show t)
;   (lsp-ui-sideline-show-hover t)
;   (lsp-ui-doc-enable nil))

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))

(use-package undo-tree)
(global-undo-tree-mode)
(evil-set-undo-system 'undo-tree)

(use-package xclip
  :config
  (xclip-mode 1))
(setq x-select-enable-clipboard t)
(setq x-select-enable-primary t)

(setq path-to-ctags "/usr/bin/ctags")
(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command
   (format "%s -f TAGS -e -R %s" path-to-ctags (directory-file-name dir-name)))
)

;; I first used htmlize exporting org files to html documents.
(use-package htmlize)

;; https://github.com/abo-abo/avy
(use-package avy)
(global-set-key (kbd "C-c SPC") 'avy-goto-char)
(global-set-key (kbd "C-'") 'avy-goto-char-2)

;; https://github.com/abo-abo/ace-window
(use-package ace-window)
(global-set-key (kbd "M-o") 'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

(use-package transpose-frame)

;; https://www.reddit.com/r/emacs/comments/67pzh5/using_customsetvariables_programmatically/
(setq custom-file "~/.emacs.d/emacs-custom.el")
(if (file-exists-p custom-file)
    (load custom-file))
