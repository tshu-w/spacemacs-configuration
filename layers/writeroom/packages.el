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

(setq writeroom-packages '(writeroom-mode))

(defun writeroom/init-writeroom-mode ()
  "Initialize writeroom-mode"

  (use-package writeroom-mode
    :commands (writeroom-mode)
    :init (spacemacs/set-leader-keys "Tw" 'writeroom-mode)
    (global-writeroom-mode 1)
    (add-hook 'find-file-hook #'writeroom-mode)
    ))
