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

(defun get-splash-buffer ()
  (let* ( (splash-buf (generate-new-buffer "*Splash*"))
          (image-spec (create-image splash-image
                                       'imagemagick nil
                                       :scale 0.25
                                       :background nil))
          (screen-width (window-width))
          (image_size (image-size image-spec))
          (image_width (car image_size))
          )
    (with-current-buffer splash-buf
      (let* (
             (left-margin-width (floor (- screen-width image_width) 2))
             (display-numbers nil)
             )
        (insert (format "%s\n\n" splash-title))
        (insert-image image-spec)
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
