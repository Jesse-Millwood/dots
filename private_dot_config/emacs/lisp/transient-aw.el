;;; transient-aw.el --- Transient Menu for Ace-Window -*- lexical-binding: t -*-

;; Author: Jesse Millwood
;; Maintainer: Jesse Millwood
;; Version: 0.1
;; Package-Requires: (transient ace-window)
;; Homepage: NA
;; Keywords: transient ace-window


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

;; Transient menu for ace-window

;;; Code:

(require 'transient)
(require 'ace-window)

(transient-define-prefix transient-aw-dispatch ()
  "Invoke an ace-window command from a list of available commands"
  ["Transient Ace-Window Commands"
   [ ("x" "Delete Window"
      (lambda () (interactive) (aw-delete-window))  :transient nil)
     ("m" "Swap Windows"
      (lambda () (interactive ) (aw-swap-window))  :transient nil)
     ("M" "Move Window"
      (lambda () (interactive) (aw-move-window))  :transient nil)
     ("c" "Copy Window"
      (lambda () (interactive) (aw-copy-window))  :transient nil)
     ("j" "Select Buffer"
      (lambda () (interactive) (aw-switch-buffer-in-window))  :transient nil)
     ("n" "Flip Window" aw-flip-window :transient nil)
     ("u" "Switch Buffer Other Window"
      (lambda () (interactive) (aw-switch-buffer-other-window))  :transient nil)
     ("e" "Execute Command Other Window"
      (lambda () (interactive) (aw-execute-command-other-window))
      :transient nil)
     ("F" "Split Fair Window"
      (lambda () (interactive) (aw-split-window-fair))  :transient nil)
     ("v" "Split Vert Window"
      (lambda () (interactive) (aw-split-window-vert))  :transient nil)
     ("b" "Split Horz Window"
      (lambda () (interactive) (aw-split-window-horz))  :transient nil)
     ("o" "Delete Other Windows"
      (lambda () (interactive) (delete-other-windows))  :transient nil)
     ("T" "Transpose Frame"
      (lambda () (interactive) (aw-transpose-frame))  :transient nil)
     ]]
  )

(defun aw-transient ()
  (interactive)
  (transient-aw-dispatch))

(provide 'transient-aw)

;;; transient-aw.el ends here
