;;; packages.el --- Org Layer packages File for Spacemacs
;;
;; Copyright (c) 2017 Voleking
;;
;; Author: Sylvain Benner <volekingsg@gmail.com>
;; URL: https://github.com/Voleking/spacemacs-configuration
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq openwith-packages '(openwith))

(defun openwith/init-openwith ()
  (use-package openwith
    :defer t
    :init
    (openwith-mode t)
    :config
    (setq openwith-associations '(("\\.pdf\\'" "open" (file))
                                  ("\\.\\(?:docx?\\|pptx?|xlsx?\\)" "open" (file))
                                  ))
    ))
