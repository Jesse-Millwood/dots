;;;·package·---·Summary$
;;;·Commentary:$
;;;· gerrit utility functions

(defvar gu-gerrit-server "http://gerritcodereview:8080")

(defun gu-get-commit-msg ()
  "Gerrit Utility function to retreive and install the commit-msg hook"
  (interactive)
  (let ((gerrit-commit-msg-path "tools/hooks/commit-msg")
        (git-home-path (magit-rev-parse "--show-toplevel"))
        (git-commit-msg-path ".git/hooks/commit-msg"))
    (url-copy-file (concat gu-gerrit-server "/" gerrit-commit-msg-path)
                   (concat git-home-path "/" git-commit-msg-path))
    (chmod (concat git-home-path "/" git-commit-msg-path) #o775)
    ))

(provide 'gerrit-utils)
;;; gerrit-utils.el ends here
