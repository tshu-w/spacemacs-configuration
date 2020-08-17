;;; layers.el --- spacemacs-cfg layer layers file for Spacemacs.
;;
;; Copyright (c) 2020 Tianshu Wang
;;
;; Author: Tianshu Wang <volekingsg@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(configuration-layer/declare-layers
 '(
   helm
   (spacemacs-completion :packages (not ido flx-ido ido-vertical-mode))
   (spacemacs-layouts :variables spacemacs-layouts-restrict-spc-tab t)
   (spacemacs-editing :packages (not clean-aindent-mode hexl editorconfig hungry-delete smartparens move-text uuidgen password-generator lorem-ipsum))
   (spacemacs-editing-visual :packages (not column-enforce-mode))
   (spacemacs-evil :packages (evil-args evil-ediff evil-escape evil-iedit-state evil-indent-plus evil-lion evil-matchit evil-surround evil-textobj-line evil-visual-mark-mode evil-visualstar))
   evil-commentary
   spacemacs-language
   spacemacs-misc
   (spacemacs-modeline :packages (spaceline doom-modeline))
   (spacemacs-navigation :packages (not ace-window open-junk-file symbol-overlay))
   spacemacs-org
   spacemacs-project
   ;; spacemacs-purpose
   (spacemacs-visual :packages (not popwin zoom-frm))
   )
 )
