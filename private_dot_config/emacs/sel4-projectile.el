;;; sel4-projectile.el --- Projectile seL4 Configuration
;;; Commentary:

;;

;;; Code:

(defun sel4-compile-cmd ()
  "Return a string representing the compile command for an seL4 CMake project."
  "mkdir -p build && cd build && ../init-build.sh && ninja"
  )

(projectile-register-project-type 'sel4 '("init-build.sh" ".repo" "kernel" "projects")
                                  :compile 'sel4-compile-cmd
                                  )
(provide 'sel4-projectile)

;;; sel4-projectile.el ends here
