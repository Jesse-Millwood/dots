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

;; Set GC threshold high for initializing, reduce later
(setq gc-cons-threshold (* 50 1000 1000))

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.milkbox.net/packages/") t)
;;; Package Repo with GPL compatible licensed packages
(add-to-list 'package-archives
             '("marmalade" . "https://marmalade-repo.org/packages/")t)
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

(use-package files
  :ensure nil
  :demand
  :hook
  (before-save . delete-trailing-whitespace)
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
  (custom-file "~/.emacs.d/emacs-custom.el" "Where all set-custom variables are stored")
  :config
  (if (file-exists-p custom-file)
      (load custom-file)
    (with-temp-buffer (write-file custom-file)))

  (let ((backup-directory "~/.emacs.d/emacs-backups/"))
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
   (text-mode . flyspell-mode)
   (text-mode . (lambda () (set-fill-column 100)))
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
  (garbage-collection-messages t)
  :config
  (winner-mode)
  (global-hl-line-mode 1)
  (set-face-attribute hl-line-face nil :background "#254175")
  (tool-bar-mode -1)
  (global-auto-revert-mode)
  (global-display-line-numbers-mode)
  (load "~/.emacs.d/extra-emacs-functions.el")
  (load "~/.emacs.d/fontsetup.el")
  )

(use-package dashboard
  :ensure t
  :demand t
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
  :config
  (set-font-preference)
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

(use-package doom-modeline
  :hook (after-init . doom-modeline-init)
  :custom
  (doom-modeline-buffer-file-name-stle 'truncate-upto-project)
  (doom-modeline-height 30)
  (x-underline-at-descent-line nil)
  :config
  (column-number-mode)
  (setq doom-modeline-icon (display-graphic-p))
  (setq doom-modeline-major-mode-icon (display-graphic-p))
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

(use-package centaur-tabs
  :demand
  :custom
  (centaur-tabs-style "chamfer")
  (centaur-tabs-set-bar 'over)
  (centaur-tabs-set-modified-marker t)
  (centaur-tabs-modified-marker "●")
  (centaur-tabs-group-by-projectile-project)
  :config
  (centaur-tabs-mode t)
  (setq centaur-tabs-set-icons (display-graphic-p))
  :bind
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward)
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
  )

(use-package windmove
  :bind (("C-S-j" . windmove-left)
         ("C-S-l" . windmove-right)
         ("C-S-i" . windmove-up)
         ("C-S-k" . windmove-down)))

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
  :hook (prog-mode LaTeX-mode)
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
  :ensure-system-package rg
  :demand
  )

;; Writing/Prose Packages
(use-package auctex
  :hook
  (
   (LaTeX-mode . (lambda () (set-fill-column 100)))
   (LaTeX-mode . LaTeX-math-mode)
   (LaTeX-mode . turn-on-reftex)
   (LaTeX-mode . turn-on-auto-fill)
   (LaTeX-mode . flyspell-mode)
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
  :defer t
  :after (LaTeX auctex)
  :custom
  (langtool-java-bin "/usr/bin/java")
  (langtool-language-tool-jar "~/opt/LanguageTool-3.9/languagetool-commandline.jar")
  (langtool-default-language "en-US")
  )

(use-package yasnippet
  :disabled
  :hook ((prog-mode . yas-minor-mode)
         (org-mode . yas-minor-mode))
  :custom
  (yas-snippet-dirs '("~/.emacs.d/snippets"))
  )

(use-package yasnippet-snippets
  :disabled
  :after (yasnippet)
  )

(use-package ivy-yasnippet
  :disabled
  :after (ivy yasnippet)
  )

(use-package flyspell
  :hook
  (latex-mode LaTeX-mode org-mode)
  :custom
  (ispell-dictionary "american")
  :bind
  (:map flyspell-mouse-map
        ("<C-down-mouse-3>" . flyspell-correct-word))
  )

(use-package flyspell-correct-ivy
  :after (flyspell)
  :demand
  :init
  (setq flyspell-correct-interface #'flyspell-correct-ivy))

;; General Project/tools

(use-package projectile
  :hook (prog-mode . projectile-mode)
  :custom
  (projectile-enable-caching t)
  (projectile-completion-system 'ivy)
  (projectile-switch-project-action 'projectile-dired)
  (projectile-tags-command "universalctags -Re -f \"%s\" --exclude=%s \"%s\"") ;; tags-file tags-exclude default-directory
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

;; Magit-mode-bury-buffer passed true kills the buffer
;; - gotten from: https://emacs.stackexchange.com/questions/35775/how-to-kill-magit-diffs-buffers-on-quit
(use-package magit
  :config
  (set-face-foreground 'magit-branch-current "green")
  (load "~/.emacs.d/gerrit-utils.el")
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch-popup)
         (:map magit-mode-map
               ("q". (lambda() (interactive (magit-mode-bury-buffer t)))))))

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

(use-package org
  :defer t
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c !" . org-time-stamp-inactive)
         )
  :hook ((org-mode . turn-on-flyspell)
         (org-mode . company-mode)
         ;;	 (org-finalize-agenda . place-agenda-tags)
         )
  :custom
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
  (org-default-notes-file "~/Notes/CatchAll.org")
  (org-refile-targets '((org-agenda-files :maxlevel . 2)))
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((org . t)
     (ditaa . t)
     (python . t)
     (plantuml . t)
     (dot . t)))
  ;; System Orgmode Files
  (defvar jesse-AgendaDirs '()
    "The list of directories to scan for orgmode agenda files.")
  (defun add-to-AgendaDirs (d)
    "Check if D is a directory and add to list."
    (if (file-directory-p d)
        (push d jesse-AgendaDirs)))
  (mapc 'add-to-AgendaDirs '("~/Notes/DornerWorks"
                             "~/Agenda/"
                             "~/Notes/Personal"))
  (mapc 'load-org-agenda-files-recursively jesse-AgendaDirs)
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
  :custom
  (c-default-style "stroustrup")
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

(use-package company-lsp
  :commands company-lsp)

(use-package python-mode
  :ensure nil
  :custom
  (python-indent-offset 4)
  )

;; Conditionally Load OCaml files
(add-to-list 'auto-mode-alist '("\.\(ml\|mli\)\\'" .  (lambda ()
                                              (if (file-exists-p "~/.opam")
                                                  (progn
                                                    (load "~/.emacs.d/ocaml-setup.el")
                                                    (tuareg-mode))
                                                ))))


;; Reduce gc threshold
(setq gc-cons-threshold (* 2 1000 1000))

(provide 'init)
;;; init.el ends here
