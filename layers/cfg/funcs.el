;;; funcs.el --- Org Layer packages File for Spacemacs
;;
;; Copyright (c) 2017 Voleking
;;
;; Author: Sylvain Benner <volekingsg@gmail.com>
;; URL: https://github.com/Voleking/spacemacs-configuration
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; (defun org-capture-template-goto-link ())

(defun notify-osx (title message)
  (call-process "terminal-notifier"
                nil 0 nil
                "-sender" "org.gnu.Emacs"
                "-group" "Emacs"
                "-title" title
                "-message" message
                "-activate" "org.gnu.Emacs"))

(defun backward-kill-line (arg)
  "Kill ARG lines backward."
  (interactive "p")
  (kill-line (- 1 arg)))

;; Iterm2 Intergration
(defun iterm-focus ()
  (interactive)
  (do-applescript
   " do shell script \"open -a iTerm\"\n"))

(defun iterm-command (cmd)
  "Go to present working dir and focus iterm"
  (interactive)
  (do-applescript
   (concat
    " do shell script \"open -a iTerm\"\n"
    " tell application \"iTerm2\"\n"
    "   tell the current session of current window\n"
    (format "     write text \"%s\" \n" cmd)
    "   end tell\n"
    " end tell\n")))

(defun iterm-goto-filedir-or-home ()
  "Go to present working dir and focus iterm"
  (interactive)
  (iterm-command (concat (replace-regexp-in-string "\\\\" "\\\\\\\\"
                                                   (shell-quote-argument (or default-directory "~")))
                         "; clear")))

;; Add executable attribute to file
(defun set-file-executable()
  "Add executable permissions on current file."
  (interactive)
  (when (buffer-file-name)
    (set-file-modes buffer-file-name
                    (logior (file-modes buffer-file-name) #o100))
    (message (concat "Made " buffer-file-name " executable"))))

(defun safari-refresh ()
  "Refresh the current tab of Safari."
  (do-applescript
   "tell application \"Safari\"
      set sameURL to URL of current tab of front window
      set URL of current tab of front window to sameURL
    end tell"))

(defun safari-get-frontmost-url ()
  (interactive)
  "Return the URL of the current tab of Chrome"
  (replace-regexp-in-string
   (rx (or (and string-start ?\")
           (and ?\" string-end)))
   ""
   (do-applescript
    "tell application \"Safari\" to return URL of current tab of front window")))

(defun org-preview-in-safari ()
  "Export OrgMode and preview the result in Safari.
This function reuses the current tab of Safari."
  (interactive)
  (let ((output (org-html-export-to-html)))
    (if (string-match-p (regexp-quote output) (safari-get-frontmost-url))
        (safari-refresh)
      (browse-url output))))
