;;; package --- Summary
;;; Commentary:
;;; .emacs file for Jesse Millwood
;;; Notes:
;;;  This config uses use-package to configure most installed packages
;;;  One confusing part is using hooks, these are all equivalent
;;;    (use-package ace-jump-mode
;;;      :hook prog-mode)
;;;    (use-package ace-jump-mode
;;;      :hook (prog-mode . ace-jump-mode))
;;;    (use-package ace-jump-mode
;;;      :commands ace-jump-mode
;;;      :init
;;;      (add-hook 'prog-mode-hook #'ace-jump-mode))



;;; Code:
;; Package Handling ______________________________________________________________________

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
;; Org-mode specific repo
(add-to-list 'package-archives
             '("org" . "https://orgmode.org/elpa/") t)
(package-initialize)

(unless (package-installed-p 'benchmark-init)
  (package-refresh-contents)
  (package-install 'benchmark-init))
(require 'benchmark-init)
;; To disable collection of benchmark data after init is done.
(add-hook 'after-init-hook 'benchmark-init/deactivate)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)
(setq use-package-always-defer t)
(use-package use-package-ensure-system-package
  :demand
  )

;; Build in Packages

(setq delete-trailing-whitespace-filter-list (list 'markdown-mode))
(defun filtered-delete-trailing-whitespace-hook ()
  (when (eq (member major-mode delete-trailing-whitespace-filter-list ) nil)
    (delete-trailing-whitespace)))

(use-package files
  :ensure nil
  :demand
  :hook
  (before-save . filtered-delete-trailing-whitespace-hook)
  :custom
  (backup-by-copying t               "don't clobber symlinks")
  (version-control t                 "version numbers for backup files")
  (delete-old-versions t             "delete excess backup files silently")
  (delete-by-moving-to-trash t)
  (kept-old-versions 6               "oldest versions to keep when a new numbered backup is made (default: 2)")
  (kept-new-versions 9               "newest versions to keep when a new numbered backup is made (default: 2)")
  (auto-save-timeout 300             "number of seconds idle time before auto-save (default: 30)")
  (auto-save-interval 200            "number of keystrokes between auto-saves (default: 300))" )
  (vc-make-version-backup t          "Make backup even if under version control")

  (custom-file (expand-file-name "emacs-custom.el" user-emacs-directory)
                                     "Where all set-custom variables are stored")
  :config
  (if (file-exists-p custom-file)
      (load custom-file)
    (with-temp-buffer (write-file custom-file)))

  (let ((backup-directory (file-name-as-directory
                           (expand-file-name "emacs-backups" user-emacs-directory))))
    (if (not (file-exists-p backup-directory))
        (make-directory backup-directory))
    (setq backup-directory-alist
          `(("." . ,backup-directory)))
    (setq auto-save-file-name-transforms `((".*" ,backup-directory t)))
    )
  )

(use-package system-packages
  :demand
  )

(use-package bind-key
  :demand
  )

(use-package emacs
  :demand
  :hook
  (
   (text-mode . (lambda () (set-fill-column 80)))
   ;;   (focus-in . set-font-preference)
;;   (after-make-frame . set-font-preference)
   (server-after-make-frame . set-font-preference)
   )
  :bind
  (:map global-map
        ("C-d" . delete-forward-char)
        ("C-x K" . kill-buffer-and-window)
        ("C-x C-b" . ibuffer-other-window))
  :custom
  (scroll-step 1)
  (scroll-conservatively 1000)
  (scroll-margin 7)
  (inhibit-splash-screen t)
  (visible-bell t "No Audible Bell")
  (indent-tabs-mode nil)
  (word-wrap t)
  :config
  (winner-mode)
  (global-display-fill-column-indicator-mode)
  (global-hl-line-mode 1)
  (set-face-attribute hl-line-face nil :background "#254175")
  (tool-bar-mode -1)
  (global-auto-revert-mode)
  (global-display-line-numbers-mode)
  (load (expand-file-name "extra-emacs-functions.el" user-emacs-directory ))
  (load (expand-file-name "fontsetup.el" user-emacs-directory ))
  )

(use-package exec-path-from-shell
  :config
  (when (daemonp)
    (exec-path-from-shell-initialize))
    )

(use-package dashboard
  :ensure t
  :demand t
  :hook
  (dashboard-mode . (lambda () (display-fill-column-indicator-mode 0)))
  :custom
  (dashboard-center-content t)
  (dashboard-startup-banner 'logo)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-set-init-info t)
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((recents . 5)
                          (projects . 5)
                          )))

(use-package frame
  :ensure nil
  :hook (after-make-frame-functions . on-frame-open)
  :custom
  (frame-title-format
   '("" invocation-name ": "(:eval
                             (if (buffer-file-name)
                                 (abbreviate-file-name (buffer-file-name))
                               "%b")
                             )))
  :if (< (length command-line-args) 2)
  :config (dashboard-setup-startup-hook)

  :config
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  )

(use-package ediff
  :init
  (add-hook 'ediff-after-quit-hook-internal 'winner-undo)
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-diff-options "-w")
  :config
  (set-face-attribute ediff-even-diff-face-A nil :background "khaki4")
  (set-face-attribute ediff-even-diff-face-B nil :background "khaki4")
  (set-face-attribute ediff-even-diff-face-C nil :background "khaki4")
  (set-face-attribute ediff-odd-diff-face-A  nil :background "khaki4")
  (set-face-attribute ediff-odd-diff-face-B  nil :background "khaki4")
  (set-face-attribute ediff-odd-diff-face-C  nil :background "khaki4")
  )

(use-package mwheel
  :ensure nil
  :demand
  :custom
  (mouse-wheel-scroll-amount '(1 ((shift) . 1)))
  (mouse-wheel-progressive-speed nil)
  (mouse-wheel-follow-mouse t)
  )

(use-package window
  :ensure nil
  :defer nil
  :bind
  (("<C-up>" . shrink-window)
    ("<C-down>" . enlarge-window)
    ("<C-left>" . shrink-window-horizontally)
    ("<C-right>" . enlarge-window-horizontally)
   )
  )

;; Visual Packages
(use-package doom-themes
  :defer nil
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :config
  (load-theme 'doom-gruvbox t)
  )

(defun enable-doom-modeline-icons (_frame)
  (when (display-graphic-p _frame)
    (setq doom-modeline-icon t)
    (setq doom-modeline-major-mode-icon t)
  ))

(use-package doom-modeline
  :hook (after-init . doom-modeline-init)
  :custom
  (doom-modeline-buffer-file-name-stle 'truncate-upto-project)
  (doom-modeline-height 30)
  (x-underline-at-descent-line nil)
  (doom-modeline-project-detection 'projectile)
  :config
  (column-number-mode)
  (add-hook 'after-make-frame-functions #'enable-doom-modeline-icons)
  )

;; UI Packages

(use-package all-the-icons
  :defer t
  :if window-system
  :init
  ;; Only call if not installed yet:
  (unless (equal (font-family-list) nil )
    (unless (member "all-the-icons" (font-family-list))
      (all-the-icons-install-fonts t)))
)

(use-package all-the-icons-ivy
  :after ivy
  :if window-system
  :config
  (all-the-icons-ivy-setup))

(defun enable-centaur-icons (_frame)
    (when (display-graphic-p _frame)
      (setq centaur-tabs-set-icons t)
    )
)

(use-package centaur-tabs
  :demand
  :custom
  (centaur-tabs-style "rounded")
  (centaur-tabs-set-bar 'over)
  (centaur-tabs-set-modified-marker t)
  (centaur-tabs-modified-marker "●")
  (centaur-tabs-group-by-projectile-project)
  (centaur-tabs-enable-key-bindings t)
  :config
  (centaur-tabs-mode t)
  (add-hook 'after-make-frame-functions #'enable-centaur-icons)
  (setq centaur-tabs-set-icons (display-graphic-p))
  )

(use-package counsel
  :bind
  (:map global-map
        ("M-y"     . counsel-yank-pop )
        ("M-x"     . counsel-M-x)
        ("C-x C-f" . counsel-find-file)
        ("C-h f"   . counsel-describe-function)
        ("C-h v"   . counsel-describe-variable)
        ("C-x l"   . counsel-locate))
  )

(use-package swiper
  :bind
  (:map global-map
        ("C-s"     . swiper))
  )

(use-package ivy
  :custom
  (ivy-use-virtual-buffers nil)
  (ivy-count-format "(%d/%d) ")
  :config
  (ivy-mode 1)
  (if (equal (face-background 'ivy-current-match) (face-foreground 'ivy-modified-buffer))
      (set-face-foreground 'ivy-modified-buffer "#fb4934")
      )
  )

;;(use-package windmove
;;  :bind (("C-S-j" . windmove-left)
;;         ("C-S-l" . windmove-right)
;;;;         ("H-S-i" . windmove-up)
;;         ("C-S-k" . windmove-down))
;;  :config
;;  (global-set-key) )

(use-package ace-window
  :custom
  (aw-dispatch-always t)
  :bind
  (("M-o" . ace-window))
  )

(use-package winum
  )

(use-package multiple-cursors
  :custom
  (mc/always-run-for-all t)
  :bind (( "C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)))

(use-package mc-extras
  :after (multiple-cursors)
  )

(use-package electric-pair-mode
  :ensure nil
  :hook (prog-mode)
  :config
  (electric-pair-mode)
  )

(use-package highlight-parentheses
  :demand
  :commands 'highlight-parentheses-mode
  :config
    (define-globalized-minor-mode global-highlight-parentheses-mode
      highlight-parentheses-mode
      (lambda ()
        (highlight-parentheses-mode t)))
    (global-highlight-parentheses-mode t)
    )


(use-package dired-hacks-utils
  )

(use-package dired-subtree
  :demand
  :bind (:map dired-mode-map
              ("i" . #'dired-subtree-toggle)))

(use-package whitespace-mode
  :ensure nil
  :hook (prog-mode LaTeX-mode markdown-mode)
  :custom
  (whitespace-line-column 100)
  :config
  (whitespace-toggle-options '(face
                               tabs
                               spaces
                               newline
                               empty
                               indentation::space
                               space-after-tab
                               space-before-tab
                               tab-mark
                               space-mark
                               newline-mark))
  )

(use-package rg
  :ensure-system-package (rg . ripgrep)
  :demand
  )

;; Writing/Prose Packages
(use-package auctex
  :hook
  (
   (LaTeX-mode . (lambda () (set-fill-column 80)))
   (LaTeX-mode . LaTeX-math-mode)
   (LaTeX-mode . turn-on-reftex)
   (LaTeX-mode . turn-on-auto-fill)
   )
  :config
  (setq-default TeX-master t)
  )

(use-package tex
  :ensure auctex
  :custom
  (TeX-master nil)
  (TeX-auto-save t)
  (TeX-parse-self t)
  (reftex-plug-into-AUCTeX t)
  (TeX-PDF-mode t)
  )

(use-package academic-phrases
  :after (LaTeX auctex)
  )

(use-package langtool
  ;; Note: I was having trouble getting these variables to be set
  ;;  - Try setq'ing them in scratch or something to see if that works
  ;;  - I was getting an issue where it said no valid setting
  ;;    - This means that the variables aren't set in a way that it can figure out what mode to be in
  :after (LaTeX auctex)
  :config
  (setq langtool-java-bin "/usr/bin/java")
  (setq langtool-java-classpath nil)
  (setq langtool-language-tool-jar "/opt/LanguageTool-5.1/languagetool-commandline.jar")
  (setq langtool-default-language "en-US")
  )

(defvar snippets-dir (file-name-as-directory
                      (expand-file-name "snippets" user-emacs-directory)))

(use-package yasnippet
  :hook ((prog-mode . yas-minor-mode)
         (org-mode . yas-minor-mode))
  :custom
  (yas-snippet-dirs '(snippets-dir))
  :config
  (yas-reload-all)
  )

(use-package yasnippet-snippets
  :after (yasnippet)
  )

(use-package ivy-yasnippet
  :after (ivy yasnippet)
  )

(use-package diminish)

(use-package flyspell
  :custom
  (ispell-dictionary "american")
  :hook ((org-mode . flyspell-mode)
         (latex-mode . flyspell-mode)
         (LaTeX-mode . flyspell-mode)
         (markdown-mode . flyspell-mode)
         (text-mode . flyspell-mode))
  )

(use-package flyspell-correct-ivy
  :after (flyspell)
  :commands (flyspell-correct-ivy flyspell-correct-wrapper)
  :bind
  (:map flyspell-mode-map
        ("C-;" . flyspell-correct-wrapper)
   :map flyspell-mouse-map
   ("<C-down-mouse-3>" . flyspell-correct-word))
  :init
  (setq flyspell-correct-interface #'flyspell-correct-ivy)
  )

;; General Project/tools

(defun projectile-vc-root-dir (dir)
  "Retrieve the root directory of the project at DIR using `vc-root-dir'."
  (let ((default-directory dir))
    (vc-root-dir)))

(use-package projectile
  :hook (prog-mode . projectile-mode)
  :custom
  (projectile-enable-caching t)
  (projectile-completion-system 'ivy)
  (projectile-require-project-root t)
  (projectile-switch-project-action 'projectile-vc)
  ;; Built universal ctags from https://github.com/universal-ctags/ctags
  ;;  ./autogen.sh
  ;;  ./configure --program-prefix=universal-
  ;;  make
  ;;  sudo make install
  ;; (projectile-tags-command "ctags -Re -f \"%s\" %s \"%s\"")
  ;; A list of functions for finding project roots
  (projectile-project-root-functions '(projectile-vc-root-dir
                                       projectile-root-top-down
                                       ;; projectile-root-local
                                       projectile-root-bottom-up
                                       projectile-root-top-down-recurring))
  :config
  (counsel-projectile-mode)
  :bind
  (:map projectile-mode-map
        ("C-c p" . projectile-command-map))
  )

(use-package counsel-projectile
  :after projectile
  :custom
  (counsel-projectile-switch-project-action 'counsel-projectile-switch-project-action-dired)
  )

(use-package diff-hl
  :config
  (global-diff-hl-mode))

;; Magit-mode-bury-buffer passed true kills the buffer
;; - gotten from: https://emacs.stackexchange.com/questions/35775/how-to-kill-magit-diffs-buffers-on-quit
(use-package magit
;;  :hook
;;   (magit-pre-refresh . diff-hl-magit-pre-refresh)
;;   (magit-post-refresh . diff-hl-magit-post-refresh)
  :config
  ;; (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  ;; (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (set-face-foreground 'magit-branch-current "green")
  (load (expand-file-name "gerrit-utils.el" user-emacs-directory))
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch-popup)
         (:map magit-mode-map
               ("q". (lambda() (interactive (magit-mode-bury-buffer t)))))))

(use-package transient
  :custom
  (transient-enable-popup-navigation 't)
  )

(use-package flycheck
  :config (global-flycheck-mode))

(use-package company
;;  :hook (prog-mode . company-mode)
  :custom
;;  (company-flx-mode +1)
  (company-show-numbers t)
  (company-tooltip-align-annotations 't)
  (company-idle-delay .1)
  )

(use-package realgud
  :defer t
  )

(use-package treemacs
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs              (if (executable-find "python") 3 0)
          treemacs-deferred-git-apply-delay   0.5
          treemacs-display-in-side-window     t
          treemacs-file-event-delay           5000
          treemacs-file-follow-delay          0.2
          treemacs-follow-after-init          t
          treemacs-follow-recenter-distance   0.1
          treemacs-goto-tag-strategy          'refetch-index
          treemacs-indentation                2
          treemacs-indentation-string         " "
          treemacs-is-never-other-window      nil
          treemacs-no-png-images              nil
          treemacs-project-follow-cleanup     nil
          treemacs-persist-file               (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-recenter-after-file-follow nil
          treemacs-recenter-after-tag-follow  nil
          treemacs-show-hidden-files          t
          treemacs-silent-filewatch           nil
          treemacs-silent-refresh             nil
          treemacs-sorting                    'alphabetic-desc
          treemacs-space-between-root-nodes   t
          treemacs-tag-follow-cleanup         t
          treemacs-tag-follow-delay           1.5
          treemacs-width                      35)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null (executable-find "python3"))))
      (`(t . t)
       (treemacs-git-mode 'extended))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after treemacs projectile
  )

;; Programming Languages Packages
(use-package gnuplot)
(use-package org
  :defer t
;;  :ensure org-plus-contrib
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c !" . org-time-stamp-inactive)
         )
  :hook (
         (org-mode . company-mode)
         ;;	 (org-finalize-agenda . place-agenda-tags)
         (org-babel-after-execute . org-redisplay-inline-images)
         )
  :custom
  (org-duration-format 'h:mm)
  (org-export-latex-listings t)
  (org-image-actual-width nil)
  (org-agenda-sticky t)
  (org-src-fontify-natively t)
  (org-startup-truncated nil)
  ;;  (org-log-done 'time) ;; This just became too anoying
  (org-ditaa-jar-path "/usr/bin/ditaa")
  (org-plantuml-jar-path "/opt/plantuml.jar")
  ;; Also made it so that the todo state changes are not tracked anymore
  (org-todo-keywords '((sequence "☛ TODO(t)" "Started(s)" "☀ Current(c)" "|" "✔ DONE(d)")
                       (sequence "⚑ WAITING(w)" "|")
                       (sequence "|" "✘ CANCELED(x)")))
  (org-default-notes-file "~/Notes/Notes.org")
  (org-default-agenda-file "~/Notes/Agenda/Default.org")
  (org-agenda-files '("~/Notes/Agenda/"))
  (org-outline-path-complete-in-steps nil)
  (org-refile-allow-creating-parent-nodes 'confirm)
  (org-refile-use-outline-path 'file)
  (org-refile-targets '((org-agenda-files :maxlevel . 3)))
  :config
;;  (require 'ox-confluence)
;;  (require 'ol-man)
   (org-babel-do-load-languages
    'org-babel-load-languages
    '((org . t)
      (calc . t)
      (gnuplot . t)
      (python . t)))

  (defvar org-capture-templates
    '(
      ("t" "todo" entry (file+headline org-default-notes-file "Tasks")
       "* TODO %?\n%u\n%a\n")
      ("n" "Notes" entry (file+headline org-default-notes-file "Notes")
       "* %? :NOTE: \n%iNoted:%u")
      )
    )
  (setq org-agenda-custom-commands
      '(("c" "Simple agenda view"
         (
          (agenda "")
          (alltodo "")))))
  )

(use-package htmlize
  :defer t
  )

(use-package org-bullets
  :defer t
  :init (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  :config (setq org-bullets-bullet-list
                '("◉" "◎" "⚫" "○" "►" "◇")))

(use-package bitbake
  :mode ("\\.bb\\'" . bitbake-mode)
  )

(use-package dts-mode
  :mode ("\\.dts\\'" . dts-mode)
  )

(use-package cmake-mode
  :defer t
  :custom
  (cmake-tab-width 4)
  )

(use-package cc-mode
   :config
   (add-hook 'c-mode-common-hook
    (lambda ()
       (c-set-style "stroustrup")))
  )

(use-package rtags
  :ensure-system-package (rc rdm)
  )
(use-package ivy-rtags
  :after rtags
  )
(use-package xcscope
  :ensure-system-package cscope
  )

(use-package elisp-mode
  :ensure nil
  :hook
  (
   (emacs-lisp-mode . company-mode)
   (inferior-emacs-lisp-mode . company-mode)
   (ielm-mode . company-mode)
   )
  )

(use-package rust-mode
  :mode "\\.rs\\'"
  :init (add-hook 'rust-mode-hook 'lsp)
  )

(use-package cargo
  :disabled
  )

(use-package flycheck-rust
  :hook (flycheck-mode . flycheck-rust-setup))

(use-package json-mode
  :mode "\\.json\\'"
  )

(use-package lsp-mode
  :hook (python-mode rust-mode ocaml-mode)
  :commands lsp
  )

(use-package lsp-ui
  :commands lsp-ui-mode)

(use-package python-mode
  :ensure nil
  :custom
  (python-indent-offset 4)
  (python-shell-interpreter "python3")
  )

(use-package csharp-mode
    :config
    (add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-tree-sitter-mode))
  )

(use-package vterm
  )

(use-package super-save
  :custom
  (super-save-auto-save-when-idle t)
  (super-save-remote-files nil)
  (super-save-exclude '(".gpg"))
  :config
  (super-save-mode +1)
  )

(use-package ibuffer-projectile
  :config
  (add-hook 'ibuffer-hook
    (lambda ()
      (ibuffer-projectile-set-filter-groups)
      (unless (eq ibuffer-sorting-mode 'alphabetic)
        (ibuffer-do-sort-by-alphabetic)))))

(use-package ibuffer-vc
  )

(use-package diredfl
  :hook ((dired-mode . diredfl-mode))
  )

(use-package tree-sitter-langs)
(use-package tree-sitter
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode))

(use-package meson-mode
  :mode "\\meson.build\\'"
  )

(use-package kconfig-mode
  :mode "\\Kconfig\\'")

(use-package git-modes)

;; (load (expand-file-name "gnus.el" user-emacs-directory))
(load (expand-file-name "chezmoi.el" user-emacs-directory))
(load (expand-file-name "newsticker-config.el" user-emacs-directory))

;; Conditionally Load OCaml files
(add-to-list 'auto-mode-alist '("\.\(ml\|mli\)\\'" .  (lambda ()
                                              (if (file-exists-p "~/.opam")
                                                  (progn
                                                    (load "~/.emacs.d/ocaml-setup.el")
                                                    (tuareg-mode))
                                                ))))

;; Machine Specific files
(if (string-equal system-name "masten")
    (load (expand-file-name "masten.el" user-emacs-directory)))

(use-package calc
  :hook (calc-mode . (lambda ()
                         (load (expand-file-name "calc-prog.el" user-emacs-directory))))
  )


;; Reduce gc threshold
(setq gc-cons-threshold (* 2 1000 1000))

(provide 'init)
;;; init.el ends here
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(message "*** Emacs loaded in %s with %d garbage collections."
     (format "%.2f seconds"
             (float-time
              (time-subtract after-init-time before-init-time))) gcs-done)
