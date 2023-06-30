(use-package eglot
  :hook ((eglot-managed-mode . company-mode)
;;         (eglot-managed-mode . eglot-inlay-hints-mode)
         (c-mode . eglot-ensure)
         (c++-mode . eglot-ensure)
         (csharp-mode . eglot-ensure)
         (python-mode . eglot-ensure))
  )

(use-package flycheck-eglot
  :after (flycheck eglot)
  :config
  (global-flycheck-eglot-mode 1))

(provide 'eglot-setup)
