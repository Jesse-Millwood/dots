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
(add-to-list 'package-archives
               '("elpa" .  "https://elpa.gnu.org/packages/" ) t)
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
   (after-init . set-font-preference)
   (server-after-make-frame . set-font-preference)
   (text-mode . (lambda () (display-line-numbers-mode 1)))
   (prog-mode . (lambda () (display-line-numbers-mode 1)))
   )
  :bind
  (:map global-map
        ("C-d" . delete-forward-char)
        ("C-x K" . kill-buffer-and-window)
        ("C-x C-b" . ibuffer-other-window)
        ("RET" . newline-and-indent))
  :custom
  (scroll-step 1)
  (scroll-conservatively 1000)
  (scroll-margin 7)
  (inhibit-splash-screen t)
  (visible-bell t "No Audible Bell")
  (indent-tabs-mode nil)
  (word-wrap t)
  (debug-on-error nil)
  :config
  (winner-mode)
  (winum-mode)
  (column-number-mode)
  (global-display-fill-column-indicator-mode)
  (global-hl-line-mode 1)
  (set-face-attribute hl-line-face nil :background "#254175")
  (tool-bar-mode -1)
  (global-auto-revert-mode)
  (load (expand-file-name "extra-emacs-functions.el"
                          (file-name-concat user-emacs-directory "lisp")))
  (load (expand-file-name "fontsetup.el"
                          (file-name-concat user-emacs-directory "lisp")))
  )

(use-package desktop
  :config
  (desktop-save-mode 1))

(use-package eldoc
  :custom
  (eldoc-echo-area-prefer-doc-buffer t)
)

(use-package dired
  :ensure nil
  :demand
  :hook (dired-mode . (lambda () (setq truncate-lines t)))
  :custom
  (dired-listing-switches "-alFh")
  (dired-dwim-target t)
  )

(use-package tramp
  :custom (tramp-terminal-type "tramp"))

(use-package auth-source
  :config
  (let ((potential-auth-sources '("~/.config/auth/authinfo.gpg"
                                  "~/.authinfo.gpg")))
    (dolist (auth-source-file potential-auth-sources)
      (if (file-exists-p auth-source-file)
          (add-to-list 'auth-sources auth-source-file))
      )
    )
  )
(use-package exec-path-from-shell
  :config
  (when (daemonp)
    (exec-path-from-shell-initialize))
    )

;; (use-package dashboard
;;   :demand t
;;   :hook
;;   (dashboard-mode . (lambda () (display-fill-column-indicator-mode 0)))
;;   :custom
;;   (dashboard-center-content t)
;;   (dashboard-startup-banner 'logo)
;;   (dashboard-set-heading-icons t)
;;   (dashboard-set-file-icons t)
;;   (dashboard-set-init-info t)
;;   (dashboard-icon-type 'all-the-icons)
;;   (dashboard-filter-agenda-entry 'dashboard-no-filter-agenda)
;; ;;  (dashboard-projects-backend 'projectile)
;;   :config
;;   (dashboard-setup-startup-hook)
;;   (setq dashboard-items '((bookmarks . 5)
;;                           (recents . 5)
;; ;;                          (projects . 5)
;; ;;                          (agenda . 5)
;;                           )))

(use-package frame
  :ensure nil
  :hook (after-make-frame-functions . on-frame-open)
  :custom
  (frame-inhibit-implied-resize t)
  (frame-title-format
   '("" invocation-name ": "(:eval
                             (if (buffer-file-name)
                                 (abbreviate-file-name (buffer-file-name))
                               "%b")
                             )))
;  :if (< (length command-line-args) 2)
;  :config (dashboard-setup-startup-hook)

  :config
;;  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  )

(use-package ztree)

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

(use-package gdb-mi
  :custom
  (gdb-restore-window-configuration-after-quit t)
  (gdb-many-windows t)
  (gud-gdb-command-name "gdb -nh -i=mi")
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

;; (use-package simple-modeline
;;   :hook (after-init . simple-modeline-mode)
;;   :custom
;;   (simple-modeline-segments
;;    '((simple-modeline-segment-modified
;;      simple-modeline-segment-buffer-name
;;      simple-modeline-segment-position)
;;     (;;simple-modeline-segment-minor-modes
;;      simple-modeline-segment-input-method
;;      simple-modeline-segment-eol
;;      simple-modeline-segment-encoding
;;      simple-modeline-segment-vc
;;      simple-modeline-segment-misc-info
;;      simple-modeline-segment-process
;;      simple-modeline-segment-major-mode)))
;;   )

;; (use-package telephone-line
;;   :hook (after-init . telephone-line-mode)
;;   :config
;;   (setq telephone-line-primary-left-separator 'telephone-line-cubed-left
;;         telephone-line-secondary-left-separator 'telephone-line-cubed-hollow-left
;;         telephone-line-primary-right-separator 'telephone-line-cubed-right
;;         telephone-line-secondary-right-separator 'telephone-line-cubed-hollow-right)
;;   (setq telephone-line-height 24
;;         telephone-line-evil-use-short-tag nil)
;;   )

(use-package spaceline
  :hook (after-init . spaceline-spacemacs-theme)
  :config
  (spaceline-define-segment
      ati-modified "An `all-the-icons' modified segment"
      (let* ((config-alist
              '(("*" all-the-icons-faicon-family all-the-icons-faicon "chain-broken" :height 1.2 :v-adjust -0.0)
                ("-" all-the-icons-faicon-family all-the-icons-faicon "link" :height 1.2 :v-adjust -0.0)
                ("%" all-the-icons-octicon-family all-the-icons-octicon "lock" :height 1.2 :v-adjust 0.1)))
             (result (cdr (assoc (format-mode-line "%*") config-alist))))

        (propertize (format "%s" (apply (cadr result) (cddr result))) 'face `(:family ,(funcall (car result)) :inherit )))
      :tight t)

  ;; (spaceline-compile
  ;;   ;; left
  ;;   '()
  ;;   ;;right
  ;;   '()
  ;;   )


  )

;(use-package doom-modeline
;  :hook
;  (after-init . doom-modeline-mode)
;  :custom
;  (doom-modeline-buffer-file-name-stle 'truncate-upto-project)
;  (doom-modeline-height 30)
;  (x-underline-at-descent-line nil)
;  (doom-modeline-project-detection 'projectile)
;  (doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode))
;  :config
;  (winum-mode)
;  (column-number-mode)
;  (add-hook 'after-make-frame-functions #'enable-doom-modeline-icons)
;  )

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

(defun enable-centaur-icons (_frame)
    (when (display-graphic-p _frame)
      (setq centaur-tabs-set-icons t)
    )
)

;; (use-package centaur-tabs
;;   :demand
;;   :custom
;;   (centaur-tabs-style "rounded")
;;   (centaur-tabs-set-bar 'over)
;;   (centaur-tabs-set-modified-marker t)
;;   (centaur-tabs-modified-marker "●")
;;   (centaur-tabs-enable-key-bindings t)
;;   :config
;;   (centaur-tabs-mode t)
;;   (centaur-tabs-group-by-projectile-project)
;;   (add-hook 'after-make-frame-functions #'enable-centaur-icons)
;;   (setq centaur-tabs-set-icons (display-graphic-p))
;;   )
;; Completing Read Stack ---------------------------------------------
(use-package vertico
  :bind (:map vertico-map
    ;; Use page-up/down to scroll vertico buffer, like ivy does by default.
    ("<prior>" . 'vertico-scroll-down)
    ("<next>"  . 'vertico-scroll-up))
  :init
  ;; Activate vertico
  (vertico-mode))

(use-package vertico-directory
  :after vertico
  :ensure nil  ;; no need to install, it comes with vertico
  :bind (:map vertico-map
    ("DEL" . vertico-directory-delete-char)))

(use-package orderless
  :custom
  ;; Activate orderless completion
  (completion-styles '(orderless basic))
  ;; Enable partial completion for file wildcard support
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  :custom
  ;; Disable preview
    (consult-preview-key 'any)
  :bind
  (("C-x b" . 'consult-buffer)    ;; Switch buffer, including recentf and bookmarks
   ("M-l"   . 'consult-git-grep)  ;; Search inside a project
   ("M-y"   . 'consult-yank-pop)  ;; Paste by selecting the kill-ring
   ("C-s"   . 'consult-line)      ;; Search current buffer, like swiper
   ))

(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

(use-package all-the-icons-completion
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :config
  (all-the-icons-completion-mode))

;; ___________________________________________________________________

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

(use-package indent-bars
  :hook (prog-mode)
  :custom
  (indent-bars-no-descend-lists t) ; no extra bars in continued func arg lists
  (indent-bars-treesit-support t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  ;; Add other languages as needed
  (indent-bars-treesit-scope '((python function_definition class_definition for_statement
                                       if_statement with_statement while_statement)
                               (rust trait_item impl_item
                                     macro_definition macro_invocation
                                     struct_item enum_item mod_item
                                     const_item let_declaration
                                     function_item for_expression
                                     if_expression loop_expression
                                     while_expression match_expression
                                     match_arm call_expression
                                     token_tree token_tree_pattern
                                     token_repetition)))
  (indent-bars-treesit-wrap '((c argument_list parameter_list init_declarator
                                 parenthesized_expression)
                              (python argument_list parameters
                                      list list_comprehension
                                      dictionary dictionary_comprehension
                                      parenthesized_expression subscript)
                              (rust arguments parameters)
                              (toml
                               table array comment)
                              (yaml
                               block_mapping_pair comment)
                              ))
  )


(use-package dtrt-indent
  :hook (prog-mode)
  :commands (dtrt-indent-global-mode
             dtrt-indent-mode
             dtrt-indent-adapt
             dtrt-indent-undo
             dtrt-indent-diagnosis
             dtrt-indent-highlight)
  :config
  (dtrt-indent-global-mode))

(use-package electric
  :custom
  (electric-indent-inhibit t)
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
  :hook (prog-mode LaTeX-mode markdown-mode conf-mode)
  :custom
  (whitespace-line-column 100)
  (whitespace-style '(face
                               tabs
                               spaces
                               newline
                               empty
                               indentation::space
                               space-after-tab
                               space-before-tab
                               tab-mark
                               space-mark
                      lines-tail
                               newline-mark))
  )


(use-package rg
  ;; Use "C-c s" for rg-menu to customize the search
  :commands 'rg
  :ensure-system-package (rg . ripgrep)
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

(use-package diminish)

(use-package jinx
  ; :hook (emacs-startup . global-jinx-mode)
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages))
  )

(use-package powerthesaurus)

(use-package projectile
  :hook (prog-mode . projectile-mode)
  :custom
  (projectile-enable-caching t)
  (projectile-completion-system 'auto)
  (projectile-require-project-root t)
  (projectile-switch-project-action 'projectile-vc)
  (projectile-auto-discover nil)

  :bind
  (:map projectile-mode-map
        ("C-c p" . projectile-command-map))
  )

(use-package diff-hl
  :hook ((prog-mode . diff-hl-mode)
         (dired-mode . diff-hl-dired-mode)
         (magit-pre-refresh-hook . diff-hl-magit-prerefresh)
         (magit-post-refresh-hook . diff-hl-magit-postrefresh)
         )
  :config
  (setq vc-git-diff-switches '("--ignore-trailing-space"
                               "--ignore-all-space"))
  )

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
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch-popup)
         (:map magit-mode-map
               ("q". (lambda() (interactive (magit-mode-bury-buffer t)))))))

(use-package gerrit-utils
  :after magit
  :load-path (lambda ()
                (file-name-concat user-emacs-directory "lisp")))

(use-package transient
  :custom
  (transient-enable-popup-navigation 't)
  )

(use-package magit-todos
  :after magit
  :config
  (magit-todos-mode))

(use-package titlecase)
(use-package helpful
  :bind
  (:map global-map
        ("C-h f"   . helpful-callable)
        ("C-h v"   . helpful-variable)
        ("C-h k"   . helpful-key)
        ("C-h x"   . helpful-command)
        ("C-c C-d" . helpful-at-point)
        ("C-h F"   . helpful-function)))

(use-package flycheck
  :config (global-flycheck-mode))

(use-package company
  :custom
;;  (company-flx-mode +1)
  (company-show-numbers t)
  (company-tooltip-align-annotations 't)
  (company-idle-delay .1)
  )

(use-package company-box
  :hook (company-mode . company-box-mode)
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
  :pin elpa
  :defer t
;;  :ensure org-contrib
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
  (org-hide-emphasis-markers t)
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
  (org-outline-path-complete-in-steps nil)
  (org-refile-allow-creating-parent-nodes 'confirm)
  (org-refile-use-outline-path 'file)
  (org-refile-targets '((org-agenda-files :maxlevel . 3)))
  :config
;;  (require 'ox-confluence)
;;  (require 'ol-man)
  (if (file-exists-p "~/Notes/Notes.org")
      (setq org-default-notes-file "~/Notes/Notes.org")
    )
  (if (file-exists-p "~/Notes/Agenda/Default.org")
      (setq org-default-agenda-file "~/Notes/Agenda/Default.org")
    )
  (if (file-exists-p "~/Notes/Agenda/")
      (setq org-agenda-files '("~/Notes/Agenda/"))
    )

  (add-to-list 'org-log-note-headings '(state . "State %-12s from %-12S %d"))
   (org-babel-do-load-languages
    'org-babel-load-languages
    '((org . t)
      (ditaa . t)
      (bitfield . t)
      (calc . t)
      (gnuplot . t)
      (python . t)
      (C . t)
      (dot . t)
      (shell . t)
      (java . t)
      (mermaid . t)
      ))

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

(use-package ob-bitfield
  :after org)

(use-package ob-mermaid
  :custom
  ;; (ob-mermaid-cli-path (shell-command-to-string
  ;;                       "npx -p @mermaid-js/mermaid-cli@latest command -v mmdc"))
  (ob-mermaid-cli-path "/home/hfcs/.npm/_npx/23232c69e5d221f3/node_modules/.bin/mmdc")
  )

(use-package ox-asciidoc)

(use-package ox-hugo
  :after ox)

(use-package org-download
  :after org
  :commands (org-download-screenshot
             org-download-clipboard
             org-download-image
             org-download-yank
             org-download-rename-last-file
             org-download-delete)
  :custom
  (org-download-image-dir "./imgs")
  :config
  (add-hook 'dired-mode-hook 'org-download-enable)
  )

(use-package org-appear
  :after org
  :hook (org-mode . org-appear-mode))

(use-package htmlize
  :defer t
  )

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :config (setq org-bullets-bullet-list
                '("◉" "◎" "⚫" "○" "►" "◇")))

(use-package adoc-mode)

(use-package bitbake
  :mode ("\\.bb\\'" . bitbake-mode)
  :mode ("\\.bbappend\\'" . bitbake-mode)
  :mode ("\\.bbclass\\'" . bitbake-mode)
  :mode ("\\.conf\\'" . bitbake-mode)
  :mode ("\\.inc\\'" . bitbake-mode)
  )

(use-package dts-mode
  :mode ("\\.dts\\'" . dts-mode)
  )

(use-package cmake-mode
  :defer t
  :custom
  (cmake-tab-width 4)
  )

(use-package eldoc-cmake
  :hook (cmake-mode . eldoc-cmake-enable))

(use-package cc-mode
  :hook (c-common-mode . (lambda () (c-set-style "stroustrup")))
  )

(use-package rtags
  :ensure-system-package (rc rdm)
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
  )

(use-package cargo
  :disabled
  )

(use-package flycheck-rust
  :hook (flycheck-mode . flycheck-rust-setup))

(use-package json-mode
  :mode "\\.json\\'"
  )

(use-package yaml-mode)

(use-package ada-ref-man
  :pin elpa
  )

(use-package ada-mode
  :pin elpa
  )

(use-package python-mode
  :mode "\\.py\\'"
  :custom
  (python-indent-offset 4)
  (python-shell-interpreter "python3")
  )

(use-package pyvenv
  :after python
  :custom
  (pdb-command-name "python3 -m pdb")
  :init
  (let ((pyenv-global-dir "~/.pyenv/versions"))
    (if (not (file-directory-p pyenv-global-dir))
        (make-directory pyenv-global-dir t))
    (setenv "WORKON_HOME" pyenv-global-dir)
    )
)

(use-package csharp-mode
  :mode "\\.cs\\'"
    :config
    (add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-tree-sitter-mode))
  )

(use-package vterm
  :custom
  (vterm-max-scrollback 5000)
  )

(use-package super-save
  :custom
  (super-save-auto-save-when-idle t)
  (super-save-remote-files nil)
  (super-save-exclude '(".gpg"))
  :config
  (super-save-mode +1)
  )

;; (use-package ibuffer-projectile
;;   :hook (ibuffer . (lambda ()
;;       (ibuffer-projectile-set-filter-groups)
;;       (unless (eq ibuffer-sorting-mode 'alphabetic)
;;                        (ibuffer-do-sort-by-alphabetic))))
;; )
;;
;; (use-package ibuffer-vc
;;   )

(use-package diredfl
  :hook ((dired-mode . diredfl-mode))
  )

(use-package treesit-auto
  :config
  (global-treesit-auto-mode))

(use-package meson-mode
  :mode "\\meson.build\\'"
  )

(use-package lua-mode)

(use-package kconfig-mode
  :mode "\\Kconfig\\'")

(use-package dockerfile-mode
  :mode "\\Dockerfile\\'")

(use-package git-modes)

(use-package undo-tree
  :config
  (global-undo-tree-mode))

(use-package csv-mode
  :custom
  (csv-field-quotes nil)
  :config
  (require 'cl)
  (require 'color)
  (defun csv-highlight (&optional separator)
    (interactive (list (when current-prefix-arg (read-char "Separator: "))))
    (font-lock-mode 1)
    (let* ((separator (or separator ?\,))
           (n (count-matches (string separator) (point-at-bol) (point-at-eol)))
           (colors (loop for i from 0 to 1.0 by (/ 1.0 n)
                         collect (apply #'color-rgb-to-hex
                                        (color-hsl-to-rgb i 0.7 0.5)))))
      ;;(font-lock-remove-keywords 'csv-mode )
      (loop for i from 1 to n by 1
            for c in colors
            for r = (format "^\\([^%c\n]+%c\\)\\{%d\\}" separator separator i)
            do (font-lock-add-keywords nil `((,r (1 '(face (:foreground ,c)))))))))
  ;; provide CSV mode setup
  (defun my-csv-mode-hook ()
    (csv-highlight 15))
  (add-hook 'csv-mode-hook 'my-csv-mode-hook)
  )

;; (use-package bufler)

(use-package license-templates)

(use-package dired-rsync)

(use-package multi-vterm
  :autoload (multi-vterm
             multi-vterm-project
             multi-vterm-dedicated-open
             multi-vterm-dedicated-close
             multi-vterm-dedicated-toggle
             multi-vterm-dedicated-select
             multi-vterm-get-buffer))

(use-package discover)

(use-package mastodon
  :pin  melpa
  :custom
  (mastodon-instance-url "https://fosstodon.org")
  (mastodon-active-user "@jesse_m")
  (mastodon-tl--show-avatars t)
  :config
  (require 'mastodon-async)
  (require 'discover)
  (mastodon-discover)
  )

(use-package pdf-tools
  ;; Un-pin this when first installing
  :disabled
  :pin manual
  :custom
  (pdf-view-display-size 'fit-page)
  (pdf-annot-activate-created-annotations t)
  :bind (:map pdf-view-mode-map  ("C-s" . isearch-forward)
         ("h" . pdf-annot-add-highlight-markup-annotation)
         ("t" . pdf-annot-add-text-annotation)
         ("D" . pdf-annot-delete))

  :config
  (pdf-tools-install))

(use-package which-func
  :hook (prog)
  :commands (which-function-mode)
  :custom
  (which-func-unknown "n/a")
  :config
  (which-function-mode)
  (setq-default header-line-format
                '((which-func-mode ("" which-func-format " "))))
  (setq mode-line-misc-info
        ;; Remove which function mode from the mode line, because it's
        ;; kind of useless
        (assq-delete-all 'which-func-mode mode-line-misc-info))
  )

(use-package ocaml-setup
  :mode "\.\(ml\|mli\)\\'"
  :load-path (lambda () (file-name-concat user-emacs-directory "lisp"))
  :config
  (when (file-exists-p "~/.opam")
    (tuareg-mode))
  )

(use-package org-noter)
(use-package org-pdftools
  :hook (org-mode . org-pdftools-setup-link))
(use-package org-noter-pdftools
  :after org-noter
  :config
  ;; Add a function to ensure precise note is inserted
  (defun org-noter-pdftools-insert-precise-note (&optional toggle-no-questions)
    (interactive "P")
    (org-noter--with-valid-session
     (let ((org-noter-insert-note-no-questions (if toggle-no-questions
                                                   (not org-noter-insert-note-no-questions)
                                                 org-noter-insert-note-no-questions))
           (org-pdftools-use-isearch-link t)
           (org-pdftools-use-freepointer-annot t))
       (org-noter-insert-note (org-noter--get-precise-info)))))

  ;; fix https://github.com/weirdNox/org-noter/pull/93/commits/f8349ae7575e599f375de1be6be2d0d5de4e6cbf
  (defun org-noter-set-start-location (&optional arg)
    "When opening a session with this document, go to the current location.
With a prefix ARG, remove start location."
    (interactive "P")
    (org-noter--with-valid-session
     (let ((inhibit-read-only t)
           (ast (org-noter--parse-root))
           (location (org-noter--doc-approx-location (when (called-interactively-p 'any) 'interactive))))
       (with-current-buffer (org-noter--session-notes-buffer session)
         (org-with-wide-buffer
          (goto-char (org-element-property :begin ast))
          (if arg
              (org-entry-delete nil org-noter-property-note-location)
            (org-entry-put nil org-noter-property-note-location
                           (org-noter--pretty-print-location location))))))))
  (with-eval-after-load 'pdf-annot
    (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))


;; Contitonally Load Custom Packages _________________________________
(use-package chezmoi
  :load-path (lambda () (file-name-concat user-emacs-directory "lisp"))
  :commands (chezmoi|magit-status chezmoi|ediff))

(use-package eglot-setup
  :load-path (lambda () (file-name-concat user-emacs-directory "lisp"))
  :demand t
  )

;; (use-package modeline-region
;;   :load-path (lambda () (file-name-concat user-emacs-directory "lisp"))
;;   :demand t
;;   )
(when (file-exists-p "~/Code/hfcs-emacs")
(use-package hfcs-emacs
  :load-path "~/Code/hfcs-emacs"
    :commands (hfcs-gdb-ut hfcs-compile-make
                           hfcs-compile-clean
                           hfcs-compile-test
                           hfcs-recompile)
    :autoload (hfcs-c-add-styles)
    :bind (:map compilation-mode-map ("g" . hfcs-recompile))
  :custom
  ;; (host-gdb-bin "/home/hfcs/Toolchains/x86_64-unknown-linux-gnu/bin/gdb")
  (hfcs-windriver-dir "~/WindRiver")
  (compilation-scroll-output t)
    :init
    (add-hook 'c-mode-common-hook #'hfcs-c-add-styles)
  :config
  ;; Catch the UtAssert test failures
  (add-to-list 'compilation-error-regexp-alist
               `(,hfcs-utassert-error-regex 1 2))
  ;; (setq compilation-error-regexp-alist (cdr compilation-error-regexp-alist))
  )

(use-package hfcs-gitlab
  :load-path "~/Code/hfcs-emacs"
  :after org
  :commands (hfcs-gl-insert-issue-org))

(use-package hfcs-magit
  :load-path "~/Code/hfcs-emacs"
  :after magit
  :commands (magit-hfcs-check-enable)
  :init
  (add-hook 'magit-mode-hook #'magit-hfcs-check-enable 100))
(use-package hfcs-org
  :load-path "~/Code/hfcs-emacs"
  :after org
  :commands (hfcs-org-insert-issue-note-template
             hfcs-org-insert-troubleshooting-section
             hfcs-org-make-issue-note-from-todo))
(use-package hfcs-project
;;  :demand t
  :load-path "~/Code/hfcs-emacs"
  :after (:all project projectile)
  :autoload project-try-hfcs
  :config
  (add-hook 'project-find-functions #'project-try-hfcs)
  )
(use-package hfcs-projectile
  ;;  :demand t
  :load-path "~/Code/hfcs-emacs"
  :after projectile
  :autoload projectile-vc-root-dir
  :init
  (setq  projectile-project-root-functions
       '((lambda (d) (projectile-root-top-down d '("repo.hfcs")))
         projectile-root-top-down
         ;;projectile-top-down-projectile-file
         projectile-vc-root-dir
         ;; projectile-root-local
         projectile-root-bottom-up
         projectile-root-top-down-recurring)))
  (use-package org-timesheet
    :load-path "~/Code/hfcs-emacs"
    :after org
    :autoload org-dblock-write:timesheet)
  )

(use-package breadcrumb
  :hook (prog-mode . breadcrumb-mode)
  )

(use-package imenu-list
  ;; :hook ((prog-mode . imenu-list-minor-mode)
  ;;        (org-mode . imenu-list-minor-mode))
  )

(use-package string-inflection)

(use-package geiser-guile)
(use-package geiser
  :custom
  (geiser-active-implementations '(guile))
  (geiser-scheme-implementation '(guile))
  (geiser-guile-binary "/usr/bin/guile")
  )

(add-to-list 'Info-default-directory-list
             (append '("/usr/local/share/info") Info-default-directory-list))

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
(put 'erase-buffer 'disabled nil)
