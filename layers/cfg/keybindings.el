;;; keybindings.el --- Org Layer packages File for Spacemacs
;;
;; Copyright (c) 2017 Voleking
;;
;; Author: Sylvain Benner <volekingsg@gmail.com>
;; URL: https://github.com/Voleking/spacemacs-configuration
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Edit Style & Keybinding
;;
(spacemacs/set-leader-keys "o" (lookup-key global-map (kbd "C-c")))
(spacemacs/set-leader-keys "o g" 'org-mac-grab-link)
(global-set-key (kbd "H-b") 'compile)
(global-set-key (kbd "H-r") 'run-current-file)
(global-set-key (kbd "H-/") (lookup-key global-map (kbd "M-;")))
(global-set-key (kbd "<H-backspace>") 'clean-aindent--bsunindent)
