;;; init.el --- Bootstrap Emacs configuration -*- lexical-binding: t -*-

(defconst f2k-local-repo "~/_develop/emacs/f2k-config"
  "Path to the development version.")

(defconst f2k-config-dev-mode
  (string= (getenv "F2K_DEV") "1")
  "If non-nil, load f2k-config from local path instead of installing from package archive.")

(when f2k-config-dev-mode
  (setq debug-on-error t)
  (message "[f2k] Development mode: debug-on-error is enabled."))

(if f2k-config-dev-mode
    ;; 🛠 Development mode: Load config from local source
    (progn
      (message "[f2k] Development mode: loading from %s..." f2k-local-repo)
      (add-to-list 'load-path (expand-file-name f2k-local-repo)))

  ;; 🚀 Production mode: always use latest version from GitLab
  (progn
    (require 'package)
    (add-to-list 'package-archives
                 '("f2k" . "https://gitlab.frenkel.org.il/emacs/f2k-config/-/releases/permalink/latest") t)
    (package-initialize)
    (message "[f2k] Production mode: refreshing and loading f2k-config...")
    (package-refresh-contents)
    (unless (package-installed-p 'f2k-config)
      (package-install 'f2k-config))))

(require 'f2k-config)
(setq custom-file (make-temp-file "emacs-custom-"))
