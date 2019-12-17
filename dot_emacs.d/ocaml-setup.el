;;; ocaml-setup.el --- Summary
;;; Commentary:
;;;  This is to be loaded if the OCAML_TOPLEVEL_PATH environment variable is set
;;; Code:

(use-package tuareg
  :ensure t
  :custom
  (tuareg-match-patterns-aligned t)
  :config
  ;; Setup environment variables using opam
  (dolist
      (var (car (read-from-string
                 (shell-command-to-string "opam config env --sexp"))))
    (setenv (car var) (cadr var)))
  ;; Update the emacs path
  (setq exec-path (split-string (getenv "PATH") path-separator))
  ;; Update the emacs load path
  (push (concat (getenv "OCAML_TOPLEVEL_PATH")
                "/../../share/emacs/site-lisp") load-path)
  )

(use-package merlin
  :ensure t
  :defer t
  :config
  (add-hook 'tuareg-mode-hook 'merlin-mode)
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'merlin-company-backend))
  (add-hook 'merlin-mode-hook 'company-mode)
  )

(use-package dune
  :ensure nil
  :mode "\dune\\'"
  :load-path (lambda () (concat (getenv "OCAML_TOPLEVEL_PATH")
                                "../../share/emacs/site-lisp"))
  )

(use-package dune-flymake
  :ensure nil
  :mode "\dune\\'"
  :load-path (lambda () (concat (getenv "OCAML_TOPLEVEL_PATH")
                                "../../share/emacs/site-lisp"))
  )

(use-package utop
  :ensure nil
  :load-path (lambda () (concat (getenv "OCAML_TOPLEVEL_PATH")
                                "../../share/emacs/site-lisp"))
  )

(provide 'ocaml-setup)
;;; ocaml-setup.el ends here
