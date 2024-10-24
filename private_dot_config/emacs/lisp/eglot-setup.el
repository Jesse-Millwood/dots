(use-package eglot
  :hook ((eglot-managed-mode . company-mode)
;;         (eglot-managed-mode . eglot-inlay-hints-mode)
         (c-mode . eglot-ensure)
         (c++-mode . eglot-ensure)
         (csharp-mode . eglot-ensure)
         (python-mode . eglot-ensure))
  :config
  (add-to-list 'eglot-server-programs
                 '((c-ts-mode c++-ts-mode c-mode c++-mode)
                   . ("clangd"
                      "-j=4"
                      "--log=error"
                      "--background-index"
                      "--clang-tidy"
                      "--completion-style=detailed"
                      "--pch-storage=memory"
                      "--header-insertion=header"
                      "--never-insertion-decorators=0")))
  )

(use-package flycheck-eglot
  :after (flycheck eglot)
  :config
  (global-flycheck-eglot-mode 1))

(provide 'eglot-setup)
