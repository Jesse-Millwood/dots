;;  (if (not (string-match-p (regexp-quote "nightly") (shell-command-to-string "rustc --version")))
;;      (add-hook 'rust-mode-hook 'lsp)
;;    )

;;(defun disable-lsp-for-current-mode ()
;;  (interactive)
  ;; [x] Get current mode
  ;; Get hook for current mode
;;  (let ((current-mode (with-current-buffer (current-buffer) major-mode))
;;        )
    ;; [x] remove lsp from list
;;    (setq themode-hook (remove 'lsp (intern (format "%s-hook" current-mode))))
    ;; kill lsp, needs a workspace argument
    ;; (lsp-workspace-shutdown)
;;    ))

(use-package lsp-mode
  :hook ((python-mode . lsp)
         (rust-mode . lsp)
         (ocaml-mode . lsp)
         (c-mode . lsp)
         (c++-mode . lsp)
         (ada-mode . lsp))
  :commands lsp
  :bind ("C-c l" . transient-lsp-dispatch)
  :config
  (load (expand-file-name "transient-lsp.el" user-emacs-directory))
  (lsp-treemacs-sync-mode 1)
  )

(use-package lsp-treemacs
  :after lsp-mode)

(use-package lsp-ui
  :commands lsp-ui-mode)
