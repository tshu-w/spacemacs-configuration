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

(setq openwith-packages '(openwith))

(defun openwith/init-openwith ()
  (use-package openwith
    :defer t
    :config
    (openwith-mode t)
    (setq openwith-associations
          (list
           (list (openwith-make-extension-regexp
                  '("doc" "docx" "ppt" "pptx" "xls" "xlsx"))
                 "open" '(file))
           '("\\.pdf\\'" "open" (file))))))
