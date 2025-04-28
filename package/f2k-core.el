;;; f2k-core.el --- Core config for f2k configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Disable backup and autosave files
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Packages
(let ((package-dir (expand-file-name "elpa/" user-emacs-directory)))
  (when (file-directory-p package-dir)
    (dolist (entry (directory-files package-dir t "^[^.]+"))
      (when (and (file-directory-p entry)
                 (not (string-match-p "-theme-" (file-name-nondirectory entry))))
        (add-to-list 'load-path entry)))))

;; Enable yasnippet if available
(when (require 'yasnippet nil 'noerror)
  (yas-global-mode 1))

(require 'f2k-appearance)
(require 'f2k-project)
(require 'f2k-prog)
 
(provide 'f2k-core)

;;; f2k-core.el ends here
