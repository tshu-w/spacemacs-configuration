;;; packages.el --- Org Layer packages File for Spacemacs
;;
;; Copyright (c) 2017-2019 Tshu Wang
;;
;; Author: Tshu Wang <volekingsg@gmail.com>
;; URL: https://github.com/Voleking/spacemacs-configuration
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
