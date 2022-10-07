;; Eglot Setup

(use-package eglot
  :hook ((eglot-managed-mode . company-mode)
         (c-mode . eglot-ensure)
         (c++-mode . eglot-ensure))
  :config
  (which-function-mode)
  (setq-default header-line-format
                '((which-func-mode ("" which-func-format " "))))
  (setq mode-line-misc-info
        ;; We remove Which Function Mode from the mode line, because it's mostly
        ;; invisible here anyway.
        (assq-delete-all 'which-func-mode mode-line-misc-info))
  )
