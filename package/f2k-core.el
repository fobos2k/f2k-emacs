;;; f2k-core.el --- Core config for f2k configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(require 'f2k-package)


(defun f2k--init-backup ()
  ;; Disable backup and autosave files
  )

(defun f2k-config-init (profile)
  "Initialize f2k Emacs configuration for PROFILE symbol."
  (f2k--init-common)

  (message "[f2k] Initializing profile: %s" profile)
  (pcase profile
    ('default
     (f2k--init-profile-default))
    ('standalone
     (f2k--init-profile-standalone))
    (_
     (message "[f2k] Unknown profile: %s. Falling back to 'default." profile)
     (f2k--init-profile-default))))


(defun f2k--init-common()
  "Common section of init. Should be nefore any profile."
  (message "[f2k] Loading common config...")
  (setq custom-file (make-temp-file "emacs-custom-"))
  (setq make-backup-files nil)
  (setq auto-save-default nil)

  (f2k--ensure-package-system-ready)
  (f2k--init-paths)
  (f2k-require :require 'f2k-prog)
  (f2k-require :require 'f2k-ui)
  (f2k-require :require 'f2k-project)

  )

(defun f2k--init-profile-default ()
  "Load modules specific to the default profile."
  (message "[f2k] Loading default profile..."))

(defun f2k--init-profile-standalone ()
  "Load minimal offline-compatible configuration."
  (message "[f2k] Loading standalone profile...")
  )

;; Packages
(defun f2k--init-paths()
  "Init paths"
  (let ((package-dir (expand-file-name "elpa/" user-emacs-directory)))
    (when (file-directory-p package-dir)
      (dolist (entry (directory-files package-dir t "^[^.]+"))
        (when (and (file-directory-p entry)
                   (not (string-match-p "-theme-" (file-name-nondirectory entry))))
          (add-to-list 'load-path entry)))))

  ;; Theme setup (offline-safe
  (let ((theme-dir (expand-file-name "elpa/" user-emacs-directory)))
    (when (file-directory-p theme-dir)
      (dolist (entry (directory-files theme-dir t "-theme-"))
        (when (and (file-directory-p entry)
                   (string-match-p "-theme-" (file-name-nondirectory entry)))
          (message "Found theme: `%s'..." entry)
          (add-to-list 'custom-theme-load-path entry))))))

(provide 'f2k-core)

;;; f2k-core.el ends here
