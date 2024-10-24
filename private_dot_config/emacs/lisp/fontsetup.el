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
  (cond
   ((find-font (font-spec :name "JetBrainsMono"))
    (set-frame-font "JetBrainsMono-12"))
   ((find-font (font-spec :name "NotoMono"))
    (set-frame-font "NotoMono-12"))
   ((find-font (font-spec :name "FiraCode"))
    (set-frame-font "FiraCode-12"))
   ((find-font (font-spec :name "UbuntuMono"))
    (set-frame-font "UbuntuMono-12"))
   ((find-font (font-spec :name "UbuntuMono"))
    (set-frame-font "UbuntuMono-12"))
    )
  )


(provide 'fontsetup)
;;; fontsetup.el ends here
