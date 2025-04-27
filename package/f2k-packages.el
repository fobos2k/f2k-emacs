;;; f2k-packages.el --- Automatic package handling -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(require 'package)

;; MELPA Stable
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;; Shadow the built-in `require` with auto-installing version
(defalias 'f2k/original-require (symbol-function 'require))

(defun f2k/auto-install-require (feature &optional filename noerror)
  "Like `require`, but automatically installs FEATURE if missing."
  (condition-case err
      (f2k/original-require feature filename noerror)
    (file-error
     (let ((pkg (intern (symbol-name feature))))
       (when (and (not (package-installed-p pkg))
                  (assoc pkg package-archive-contents))
         (message "[f2k] Installing missing package: %s" pkg)
         (package-install pkg))
       (f2k/original-require feature filename noerror)))))

;; Override `require` globally
(fset 'require #'f2k/auto-install-require)

(provide 'f2k-packages)
;;; f2k-packages.el ends here
