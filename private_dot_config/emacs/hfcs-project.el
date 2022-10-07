;; Project ___________________________________________________________
(cl-defmethod project-root ((project (head hfcs)))
  "Return the project root for an HFCS Project"
  (car (cdr project)))

(defun project-try-hfcs (dir)
  (let (( dominating-dir (locate-dominating-file dir "repo.hfcs")))
    (if dominating-dir
        (list 'hfcs (concat dominating-dir "repos"))
      nil)))

(add-hook 'project-find-functions #'project-try-hfcs)
