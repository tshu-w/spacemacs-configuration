;;; packages.el --- Org Layer packages File for Spacemacs
;;
;; Copyright (c) 2017-2018 Tshu Wang
;;
;; Author: Tshu Wang <volekingsg@gmail.com>
;; URL: https://github.com/Voleking/spacemacs-configuration
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq my-org-packages '(org-alert org-gcal))

(defun my-org/init-org-alert ()
  (use-package org-alert
    :defer t))

(defun my-org/init-org-gcal ()
  (use-package org-alert
    :defer t
    :init
    (add-hook 'org-agenda-mode-hook (lambda () (org-gcal-sync)))
    ))
