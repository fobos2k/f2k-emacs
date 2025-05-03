;;; f2k-core.el --- Core config for f2k configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun f2k--init-backup ()
  ;; Disable backup and autosave files
  (setq make-backup-files nil)
  (setq auto-save-default nil))

(defun f2k-config-init (profile)
  "Initialize f2k Emacs configuration for PROFILE symbol."
  (message "[f2k] Initializing profile: %s" profile)
  (pcase profile
    ('default
     (f2k--load-core-modules)
     (f2k--load-default-profile))
    ('standalone
     (f2k--load-core-modules)
     (f2k--load-standalone-profile))
    (_
     (message "[f2k] Unknown profile: %s. Falling back to 'default." profile)
     (f2k-config-init 'default))))

(defun f2k--load-core-modules ()
  "Load core modules shared across all profiles."
  (message "[f2k] Loading core modules...")
  ;; Example core modules:
  ;; (f2k--require 'f2k-core)
  (f2k--require 'f2k-ui)
  ;; (f2k--require 'f2k-editing)
  ;; etc.
  )

(defun f2k--load-default-profile ()
  "Load modules specific to the default profile."
  (message "[f2k] Loading default profile modules...")
  ;; (f2k--require 'f2k-completion)
  ;; (f2k--require 'f2k-lsp)
  ;; (f2k--require 'f2k-devtools)
  ;; etc.
  )

(defun f2k--load-standalone-profile ()
  "Load minimal offline-compatible configuration."
  (message "[f2k] Loading standalone profile...")
  ;; Подгружаем только то, что доступно локально
  (f2k--require 'f2k-minimal-ui)
  (f2k--require 'f2k-builtin-enhancements)
  ;; etc.
  )

(defun f2k--require (feature)
  "Wrapper for `require' that logs loading of FEATURE."
  (message "[f2k] Requiring module: %s" feature)
  (require feature))


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

(provide 'f2k-core)

;;; f2k-core.el ends here
