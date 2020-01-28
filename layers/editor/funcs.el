;;; funcs.el --- editor layer functions file for Spacemacs.
;;
;; Copyright (c) 2020 Tianshu Wang
;;
;; Author: Tianshu Wang <volekingsg@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun ivy-tab ()
  (interactive)
  (let ((dir ivy--directory))
    (ivy-partial-or-done)
    (when (string= dir ivy--directory)
      (ivy-insert-current)
      (when (and (eq (ivy-state-collection ivy-last) #'read-file-name-internal)
                 (setq dir (ivy-expand-file-if-directory (ivy-state-current ivy-last))))
        (ivy--cd dir)
        (setq this-command 'ivy-cd)))))

(defun ivy-c-h ()
  (interactive)
  (if (eq (ivy-state-collection ivy-last) #'read-file-name-internal)
      (if (string-equal (ivy--input) "")
          (counsel-up-directory)
        (delete-minibuffer-contents))
    (ivy-backward-delete-char)))

(defun ivy-edit ()
  "Edit the current search results in a buffer using wgrep."
  (interactive)
  (run-with-idle-timer 0 nil 'ivy-wgrep-change-to-wgrep-mode)
  (ivy-occur))

(defun make-directory-maybe ()
  "Create parent directory if not exists while visiting file."
  (let ((dir (file-name-directory buffer-file-name)))
    (unless (file-exists-p dir)
      (if (y-or-n-p (format "Directory %s does not exist,do you want you create it? " dir))
          (make-directory dir t)
        (keyboard-quit)))))

(defun parse-url (url)
  "convert a git remote location as a HTTP URL"
  (if (string-match "^http" url) url
    (replace-regexp-in-string "\\(.*\\)@\\(.*\\):\\(.*\\)\\(\\.git?\\)"
                              "https://\\2/\\3" url)))
(defun magit-open-repo ()
  "open remote repo URL"
  (interactive)
  (let ((url (magit-get "remote" "origin" "url")))
    (progn
      (browse-url (parse-url url))
      (message "opening repo %s" url))))

(defun osx-switch-back-to-previous-application ()
  (interactive)
  (do-applescript
   (mapconcat
    #'identity
    '("tell application \"System Events\""
      "  tell process \"Finder\""
      "    activate"
      "    keystroke tab using {command down}"
      "  end tell"
      "end tell")
    "\n")))

(defun set-file-executable ()
  "Add executable permissions on current file."
  (interactive)
  (when (buffer-file-name)
    (set-file-modes buffer-file-name
                    (logior (file-modes buffer-file-name) #o100))
    (message (concat "Made " buffer-file-name " executable"))))
