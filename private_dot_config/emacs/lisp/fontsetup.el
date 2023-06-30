;;; fontsetup --- Summary
;;; Commentary:
;;; Font Setup Functions.
;;;  - Set Preferred font from list
;;;  - Turn ligatures on and off (interactively)
;;;  - Provide extra font metadata
;;; Code:

;; (cond
;;  ((find-font (font-spec :name "Cascadia Code"))
;;   (set-frame-font "Cascadia Code-12"))
;;  ((find-font (font-spec :name "Menlo"))
;;   (set-frame-font "Menlo-12"))
;;  ((find-font (font-spec :name "DejaVu Sans Mono"))
;;   (set-frame-font "DejaVu Sans Mono-12"))
;;  ((find-font (font-spec :name "Inconsolata"))
;;   (set-frame-font "Inconsolata-12")))

(defun set-font-preference2 ()
  (let ((font-list '((font-spec :name "JetBrains" :family "Monospace" :size 12)
                     (font-spec :name "Noto" :family "Monospace" :size 12)
                     (font-spec :name "Fira Code" :family "Monospace" :size 10)
                     (font-spec :name "Ubuntu Mono" :family "Monospace" :size 10)
                     (font-spec :name "DejaVu Sans Mono" :family "Monospace" :size 10)))))
  ;; iterate through font-list and use (find-font ) to determine correct font
  )

(defun set-font-preference ()
  "Set the preference of fonts from a list of alists"
  (interactive)
  (let ((font-list '(("JetBrains Mono" . 12)
                     ("Noto Mono" . 12)
                     ("Fira Code" . 10)
                     ("Ubuntu Mono" . 10)
                     ("DejaVu Sans Mono" . 10)))
        (font-set-p nil)
        (font-not-found-p nil)
        (font-list-index 0))

    (while (and (not font-set-p) (not font-not-found-p))
      (when (member (car (nth font-list-index font-list)) (font-family-list))
        (let ( (font-family (car (nth font-list-index font-list)))
               (font-size (cdr (nth font-list-index font-list))))
          (set-frame-font (format "%s-%i"
                                  font-family
                                  font-size)
                          nil ;; Keep Size
                          t   ;; Apply to child frames
                          )
        (add-to-list 'default-frame-alist
                     `(font . ,(format "%s-%i"
                                       font-family
                                       font-size)))
        (setq font-set-p t))
        )
      (if (and (not font-set-p)
               (< font-list-index (length font-list)))
          (progn
            (setq font-not-found-p t)
            (message "Could not find your fonts in the FONT-LIST")
            )
        (if (not font-set-p)
            (setq font-list-index (1+ font-list-index)))
        )
      )
    ;; (if font-set-p
    ;;     (message "Font Set: %s-%i"
    ;;              (car (nth font-list-index font-list))
    ;;              (cdr (nth font-list-index font-list)))
    ;;   (message "Font not set by preference list")
    ;;   )
    )
  )

(provide 'fontsetup)
;;; fontsetup.el ends here
