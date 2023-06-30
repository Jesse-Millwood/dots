;;; masten.el --- Masten Specific Emacs Configs -*- lexical-binding: t -*-

;; Author: Jesse Millwood
;; Maintainer: Jesse Millwood
;; Version: 0.1
;; Keywords: masten, config


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; This is extra configurations and files to support Jesse Millwood
;; working on Masten projects.

;;; Todo:

;;   - Masten Dashboard
;;     - Show todo items
;;     - Show scheduled items

;;   - Masten Project Handling
;;     - Easier way to navigate quickly to masten projects

;;; Code:

(setq org-default-notes-file "~/Masten/Notes/Notes.org")
(setq org-default-agenda-file "~/Masten/Notes/Agenda.org")
(setq org-agenda-files '("~/Masten/Notes/Agenda.org" "~/Masten/Notes/Terms.org"))
(setq masten-terms-file "~/Masten/Notes/Terms.org")

(defun masten-telnet (gpc-num)
  (interactive "nWhat GPC would you like to connect to: ")
  (telnet "console.lab.masten" (+ 4200 gpc-num))
  )

(defun masten-add-org-capture-templates ()
  (add-to-list 'org-capture-templates
               '("m" "Masten Related Templates"))
  (add-to-list 'org-capture-templates
               '("mt"
                 "Masten TODO"
                 entry
                 (file+headline
                  org-default-agenda-file
                  "Tasks Not Started")
                 "** [☛]TODO %?"))
  (add-to-list 'org-capture-templates
               '("md"
                 "Masten Term Definition"
                 item
                 (file+headline
                  masten-terms-file
                  "Unorganized Terms")
                 "- %? :: "))
  )

(add-hook 'org-mode-hook 'masten-add-org-capture-templates)

(provide 'masten)

;;; masten.el ends here
