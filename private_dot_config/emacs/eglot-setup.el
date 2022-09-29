;; Eglot Setup

(use-package eglot
  :hook ((c-mode . eglot-ensure)
         (c++mode . eglot-ensure))
  :config

  (cl-defmethod project-root ((project (head hfcs)))
    "Return the project root for an HFCS Project"
    (car (cdr project)))

  (defun project-try-hfcs (dir)
    (let (( dominating (concat (locate-dominating-file dir "repo.hfcs") "repos")))
      (list 'hfcs dominating nil nil)
      )
    )

  (add-hook 'project-find-functions #'project-try-hfcs)
  )
