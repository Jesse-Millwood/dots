;; Projectile ________________________________________________________

(defun projectile-vc-root-dir (dir)
  "Retrieve the root directory of the project at DIR using `vc-root-dir'."
  (let ((default-directory dir))
    (vc-root-dir)))
