;;; splash.el --- Splash package
;; Author: Jesse Millwood

;;; Commentary:

;;; Code:


(defgroup splash
  nil
  "Customization group for the SPLASH package")

(defcustom splash-image
  "~/.emacs.d/splash/img/emacslogo.svg"
  "Path to the image file to be use for the SPLASH-IMAGE"
  :type 'string
  :group 'splash)

(defcustom splash-title
  "Welcome"
  "Title to be displayed in the SPLASH buffer"
  :type 'string
  :group 'splash)

(defun input-centered-string (string)
  "Input the STRING with space offsets to be centered"
  (let* ((indent-columns (floor (- screen-width (length splash-title)) 2)))
    (insert (format "%s%s" (make-string indent-columns ? ) string))
    ))

(defun get-splash-buffer ()
  (let* ( (splash-buf (generate-new-buffer "*Splash*")) )
    (with-current-buffer splash-buf
      (let* (
             (image-spec (create-image splash-image
                                       'imagemagick nil
                                       :scale 0.25
                                       :background nil))
             (screen-width (window-width))
             (image_size (image-size image-spec nil))
             (image_width (car image_size))
             (frame-type (framep (window-frame)))
             (image-indent-columns (floor (- screen-width image_width) 2) )
;;             (left-margin-width image-indent-columns)
             ;;             (left-margin image-indent-columns)
             (left-margin-width 0)
             (right-margin-width 0)
             (display-line-numbers nil)
             )
        (fundamental-mode)
        (input-centered-string splash-title)
        (insert "\n\n")
        (cond ((string= frame-type "t") (insert "image here")
               )
              ((string= frame-type "x")
               (progn
                 (insert (make-string image-indent-columns ? ))
                 (insert-image image-spec)
                 (insert "\n")
                 )
               )
              )
        (insert "\n\n")
        ;; (read-only-mode)
        )
      )
    (get-buffer splash-buf)
    ))

(defun splash-buffer-or-nil ()
  "If a file was passed in the COMMAND-LINE-ARGS return GET-SPLASH-BUFFER, else NIL"
  (if (< 1 (safe-length command-line-args))
      nil
    'get-splash-buffer
    ))

(provide 'splash)
;;; splash.el ends here
