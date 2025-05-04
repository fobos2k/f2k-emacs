;;; f2k-package.el --- Package loading helpers -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defvar f2k--package-system-ready nil
  "Internal flag to ensure package system is initialized only once.")

(defun f2k--ensure-package-system-ready ()
  "Initialize package.el and required archives if not already done."
  (unless f2k--package-system-ready
    (require 'package)
    (unless package--initialized
      (package-initialize))

    (dolist (entry '(("gnu"    . "https://elpa.gnu.org/packages/")
                     ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                     ("melpa"  . "https://melpa.org/packages/")))
      (unless (assoc (car entry) package-archives)
        (add-to-list 'package-archives entry t)))

    (setq f2k--package-system-ready t)))


(defvar f2k--package-last-refresh-file
  (expand-file-name "last-package-refresh"
                    (or (bound-and-true-p user-emacs-directory)
                        user-init-directory))
  "File storing last refresh date for package archives.")

(defvar f2k--package-archives-refreshed-p nil
  "Whether `package-archive-contents` has been refreshed in this session.")

(defun f2k--refresh-archive-contents-daily ()
  "Refresh `package-archive-contents` at most once per day."
  (unless f2k--package-archives-refreshed-p
    (let* ((today (format-time-string "%Y-%m-%d"))
           (last-refresh
            (when (file-exists-p f2k--package-last-refresh-file)
              (with-temp-buffer
                (insert-file-contents f2k--package-last-refresh-file)
                (buffer-string)))))
      (unless (string= today last-refresh)
        (message "[f2k] Refreshing package archive contents...")
        (package-refresh-contents)
        (with-temp-file f2k--package-last-refresh-file
          (insert today))))
    (setq f2k--package-archives-refreshed-p t)))



(defun f2k--require (feature)
  "Load FEATURE with logging. Wrapper for `f2k--require-and-run'."
  (f2k--require-and-run feature))


(defun f2k--require-and-run (package &optional on-success on-failure message)
  "Try to `require' PACKAGE.
If successful, call ON-SUCCESS. If failed, print MESSAGE and call ON-FAILURE.

Behavior depends on `user-init-profile`:
- 'standalone → only warn, never install
- nil or 'default → ensure package system and try installing PACKAGE via `package-install`."
  (if (require package nil 'noerror)
      (when on-success (funcall on-success))
    (let ((profile (or user-init-profile 'default)))
      (cond
       ((eq profile 'standalone)
        (message "[f2k] MISSING: %s — standalone mode, skipping install." package)
        (when on-failure (funcall on-failure)))

       ((memq profile '(default nil))
        (f2k--ensure-package-system-ready)
        (f2k--refresh-archive-contents-daily)

        (let ((pkg-name (intern (symbol-name package))))
          (ignore-errors (package-install pkg-name)))

        (if (require package nil 'noerror)
            (when on-success (funcall on-success))
          (message "[f2k] ERROR: failed to install %s" package)
          (when on-failure (funcall on-failure))))

       (t
        (message "[f2k] Unknown profile: %s" profile)
        (when on-failure (funcall on-failure)))))))


(defun f2k-require (&rest args)
  "Declarative package use interface.
Keywords:
  :require   (symbol)   — name of the package (mandatory)
  :then      (lambda)   — action if package loads
  :else      (lambda)   — fallback action
  :message   (string)   — custom warning if load fails"
  (let ((pkg     (plist-get args :require))
        (then    (plist-get args :then))
        (else    (plist-get args :else))
        (msg     (plist-get args :message)))
    (unless pkg
      (error "[f2k] :require keyword is mandatory"))
    (f2k--require-and-run pkg then else msg)))

(provide 'f2k-package)
;;; f2k-package.el ends here
