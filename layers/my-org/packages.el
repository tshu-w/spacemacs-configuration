;;; packages.el --- Org Layer packages File for Spacemacs
;;
;; Copyright (c) 2017-2020 Tianshu Wang
;;
;; Author: Tianshu Wang <volekingsg@gmail.com>
;; URL: https://github.com/tshu-w/spacemacs-configuration
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq my-org-packages '(zotxt))

(defun my-org/init-zotxt ()
  (spacemacs|diminish org-zotxt-mode " Ⓩ" " z")
  (spacemacs/declare-prefix-for-mode 'org-mode "mz"  "zotero")
  (spacemacs/set-leader-keys-for-major-mode 'org-mode
    "zi"    'org-zotxt-insert-reference-link
    "iz"    'org-zotxt-insert-reference-link
    "zo"    'org-zotxt-open-attachment)
  (add-hook 'org-mode-hook 'org-zotxt-mode))
