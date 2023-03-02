;; Eglot Setup

(use-package eglot
  :hook ((eglot-managed-mode . company-mode)
         (c-mode . eglot-ensure)
         (c++-mode . eglot-ensure)
         (csharp-mode . eglot-ensure))
  )

(use-package flycheck-eglot
  :after (flycheck eglot)
  :config
  (global-flycheck-eglot-mode 1))
