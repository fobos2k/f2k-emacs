;;; f2k-config.el --- My Emacs config -*- lexical-binding: t -*-
;; Copyright (C) 2016-2025 Free Software Foundation, Inc.
;; Autors: Boris Frenkel <developer@frenkel.org.il>
;; Maintainer: Boris Frenkel <developer@frenkel.org.il>
;; Package-Version: %%VERSION%%
;; Package-Revision: %%REVISION%%
;; Package-Requires: ()
;; Keywords: config
;; X-URL: https://gitlab.frenkel.org.il/emacs/f2k-config

;;; Commentary:
;; F2k config set for Emacs

;;; Code:
(require 'f2k-packages)

(require 'darkokai-theme)
(require 'monokai-theme)

(require 'f2k-common-langs)

;; Appearance
;; (load-theme 'monokai t)
(load-theme 'darkokai t)

(set-face-attribute 'default nil
                    :family "FiraCode Nerd Font Mono"
                    :foundry "CTDB"
                    :height 120)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default display-time-24hr-format t)

(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)
(display-time-mode t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(semantic-mode t)
(ido-mode t)

(when (and (fboundp 'display-battery-p)
           (display-battery-p))
  (display-battery-mode t))


(when (fboundp 'yes-or-no-p)
  (fset 'yes-or-no-p 'y-or-n-p))

(provide 'f2k-config)
;;; f2k-config.el ends here
