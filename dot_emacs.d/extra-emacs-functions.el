;;; package --- Summary
;;; Commentary:
;;; Extra Emacs functions.
;;; Code:

(defun yank-single-line ()
  "Useful for yanking from a pdf that inserts unnecessary newlines."
  (interactive)
  (insert (subst-char-in-string ?\n ? (substring-no-properties (current-kill 0))))
  )
;; kill the name of the buffer to the kill ring
(defun copy-full-path-to-kill-ring ()
  "copy buffer's full path to kill ring"
  (interactive)
  (when buffer-file-name
    (kill-new (file-truename buffer-file-name))))
;; Grabed from http://emacsredux.com/blog/2013/03/27/copy-filename-to-the-clipboard/
(defun copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(defun sudo-find-file (file-name)
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

(setq rss-config-path "~/.emacs.d/rss-config.el")
(defun start-elfeed ()
  "Start and load Elfeed."
  (interactive)
  (if (file-exists-p rss-config-path)
      (progn
        (load rss-config-path)
        (elfeed)
        )
    )
  )

(provide 'extra-emacs-functions)
;;; extra-emacs-functions.el ends here
