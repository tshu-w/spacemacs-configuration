;;; config.el --- Org Layer packages File for Spacemacs
;;
;; Copyright (c) 2017-2018 Tshu Wang
;;
;; Author: Tshu Wang <volekingsg@gmail.com>
;; URL: https://github.com/Voleking/spacemacs-configuration
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(when (configuration-layer/layer-usedp 'markdown)
  (setq auto-mode-alist (cons '("\\.mdown$" . markdown-mode) auto-mode-alist))
  (setq auto-mode-alist (cons '("\\.markdown$" . markdown-mode) auto-mode-alist))
  (setq auto-mode-alist (cons '("README\\.md$" . gfm-mode) auto-mode-alist)))

;; Keeping files in sync
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)
(setq revert-without-query '(".*")) ;; disable revert query
