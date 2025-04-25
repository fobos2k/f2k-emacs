;; gen-pkg.el — генерация f2k-config-pkg.el и archive-contents

(defun f2k-generate-pkg-files ()
  (require 'package)
  ;; Установим рабочую директорию программно
  (setq default-directory (or (getenv "F2K_DIST_DIR") default-directory))

  (let* ((pkg-file "f2k-config.el")
         (pkg-desc-file "f2k-config-pkg.el")
         (archive-file "archive-contents")
         (desc (with-temp-buffer
                 (insert-file-contents pkg-file)
                 (package-buffer-info))))
    (unless (package-desc-p desc)
      (error "Invalid package description"))

    ;; Write f2k-config-pkg.el
    (with-temp-file pkg-desc-file
      (let ((print-length nil)
            (print-level nil)
            (print-circle t)
            (print-quoted t)
            (print-pretty t))
        (prin1 desc (current-buffer))))

    ;; Write archive-contents
    (with-temp-file archive-file
      (let ((print-length nil)
            (print-level nil)
            (print-circle t)
            (print-quoted t)
            (print-pretty t)))
      (prin1 `(1 (, (package-desc-name desc) . ,desc))
             (current-buffer)))))
