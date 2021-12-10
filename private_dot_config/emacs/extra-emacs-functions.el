;;; package --- Summary
;;; Commentary:
;;; Extra Emacs functions.
;;; Code:

(defun jcm-yank-single-line ()
  "Useful for yanking from a pdf that inserts unnecessary newlines."
  (interactive)
  (insert (subst-char-in-string ?\n ? (substring-no-properties (current-kill 0))))
  )

;; Grabed from http://emacsredux.com/blog/2013/03/27/copy-filename-to-the-clipboard/
(defun jcm-copy-file-name-to-clipboard (relative-to-root-p)
  "Copy the current buffer file name to the clipboard."
  (interactive (list (y-or-n-p "Relative to project? ")))

  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (abbreviate-file-name (buffer-file-name))))
        (project-root (cdr (project-current)))
        (project-root-dir

         (if (project-current)
         (file-name-nondirectory
          (directory-file-name
               (file-name-directory (cdr (project-current)))))
           nil)
         ))

    (when (and filename relative-to-root-p)
      (message "Relative Path: %s" (file-relative-name filename project-root))
      (message "With project prefix: %s"
               (file-name-nondirectory
                (directory-file-name
                 (file-name-directory project-root)))))

    (when filename
      (if relative-to-root-p
          (progn
            (kill-new (concat
                       (file-name-as-directory project-root-dir)
                       (file-relative-name filename project-root)))
            (message "Copied '%s' to the clipboard"
                     (concat (file-name-as-directory project-root-dir)
                             (file-relative-name filename project-root)))
            )
        (progn
      (kill-new filename)
          (message "Copied '%s' to the clipboard" filename)
          ))
      )
 ))


(defun jcm-sudo-find-file (file-name)
  "Like find file, but opens FILE-NAME as root."
  (interactive "FSudo Find File: ")
  (let ((tramp-file-name (concat "/sudo::" (expand-file-name file-name))))
    (find-file tramp-file-name)))

  (defun wordp (c) (= ?w (char-syntax c)))
  (defun lowercasep (c) (and (wordp c) (= c (downcase c))))
  (defun uppercasep (c) (and (wordp c) (= c (upcase c))))
  (defun split-string-at-caps (inString)
    "Split the INSTRING at capital letters, insert a space, and trim the leading space."
    (let ((outString ""))
      (replace-regexp-in-string "^ +" ""
                                (dolist (currentChar (string-to-list inString) outString)
                                  (setq outString (concat outString
                                                          (if (uppercasep currentChar)
                                                              (concat " " (char-to-string currentChar))
                                                            (char-to-string currentChar)
                                                            )))))
      ))

;; https://stackoverflow.com/questions/17215868/recursively-adding-org-files-in-a-top-level-directory-for-org-agenda-files-take
(defun load-org-agenda-files-recursively (dir)
  "Recursively add agenda files under DIR to the ORG-AGENDA-FILES variable."
  (unless (file-directory-p dir)
    (error "Not a directory '%s'" dir))
  (unless (equal (directory-files dir nil org-agenda-file-regexp t) nil)
    (add-to-list 'org-agenda-files dir))
  (dolist (file (directory-files dir nil nil t))
    (unless (member file '("." ".."))
      (let ((file (concat dir file "/")))
        (when (file-directory-p file)
          (load-org-agenda-files-recursively file))))))
(defun place-agenda-tags ()
  "Put the agenda tags by the right border of the agenda window."
  (interactive)
  (setq org-agenda-tags-column (- 4 (window-width)))
  (org-agenda-align-tags))


(setq rss-config-path (concat user-emacs-directory "rss-config.el"))
(defun jcm-start-elfeed ()
  "Start and load Elfeed."
  (interactive)
  (if (file-exists-p rss-config-path)
      (progn
        (load rss-config-path)
        (elfeed)
        (elfeed-goodies/setup)
        )
    )
  )

(defun jcm-insert-uline-to-fc ()
  "Insert an underline to the fill collumn"
  (interactive)
  ;; Excursion not saved because it is easier to be put at the end
  ;; and not have to worry about accidentally moving the underline
  ;; to the next line
  (insert-char ?_ (- fill-column (current-column)))
  )

(provide 'extra-emacs-functions)
;;; extra-emacs-functions.el ends here
