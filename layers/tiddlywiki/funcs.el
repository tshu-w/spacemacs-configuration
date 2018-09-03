;;; funcs.el --- Org Layer packages File for Spacemacs
;;
;; Copyright (c) 2017-2018 Tshu Wang
;;
;; Author: Tshu Wang <volekingsg@gmail.com>
;; URL: https://github.com/Voleking/spacemacs-configuration
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun open-wiki ()
  "Opens a TiddlyWiki directory in Dired."
  (interactive)
  (dired tiddlywiki-directory))

(defun browse-wiki ()
  "Opens TiddlyWiki in the browser."
  (interactive)
  (browse-url "http://127.0.0.1:8080/"))
