;;; funcs.el --- my-org layer functions file for Spacemacs.
;;
;; Copyright (c) 2020 Tianshu Wang
;;
;; Author: Tianshu Wang <volekingsg@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Org Capture
(defun org-capture-goto-link ()
  (org-capture-put :target (list 'file+headline
                                 (nth 1 (org-capture-get :target))
                                 (org-capture-get :annotation)))
  (org-capture-put-target-region-and-position)
  (widen)
  (let ((hd (org-capture-get :annotation)))
    (goto-char (point-min))
    (if (re-search-forward
         (format org-complex-heading-regexp-format (regexp-quote hd)) nil t)
        (org-end-of-subtree)
      (goto-char (point-max))
      (or (bolp) (insert "\n"))
      (insert "* " (nth 2 (org-capture-get :target)) "\n"))))

;; Org Agenda
(defun append-org-agenda-files (file)
  "append to org-agenda-files if file exists"
  (when (file-exists-p file)
    (push file org-agenda-files)))

;; Org Clock
(defun my/org-clock-select-task ()
  (interactive)
  (org-clock-in '(4)))

;; Org-journal
(defun org-journal-find-location (&optional days)
  (let ((date (time-add (current-time) (days-to-time days))))
    (org-journal-new-entry t date)
    (goto-char (point-max))))

(defun org-journal-date-format-func (time)
  "Custom function to insert journal date header,
and some custom text on a newly created journal file."
  (when (= (buffer-size) 0)
    (insert
     (pcase org-journal-file-type
       (`daily   "#+TITLE: Daily Journal\n\n")
       (`weekly  "#+TITLE: Weekly Journal\n\n")
       (`monthly "#+TITLE: Monthly Journal\n\n")
       (`yearly  "#+TITLE: Yearly Journal\n\n"))))
  (concat org-journal-date-prefix (format-time-string "%A, %B %d %Y" time)))

;; System interaction function
(defun supress-frame-splitting (&rest r)
  (let ((frame-name (frame-parameter nil 'name)))
    (when (or (equal "capture" frame-name)
              (equal "agenda" frame-name))
      (delete-other-windows))))

(defun org-capture-finalize@after (&rest r)
  (when (equal "l" (plist-get org-capture-plist :key))
    (run-at-time 0 nil #'osx-switch-back-to-previous-application))
  (when (equal "capture" (frame-parameter nil 'name))
    (spacemacs/frame-killer)))

(defun org-agenda-finalize@after (&rest r)
  (when (equal "agenda" (frame-parameter nil 'name))
    (spacemacs/frame-killer)))

(defun org-capture-select-template@around (org-capture-select-template &optional keys)
  (let ((res (ignore-errors (funcall org-capture-select-template keys))))
    (unless res (setq res "q"))
    (when (and (equal "capture" (frame-parameter nil 'name))
                (equal "q" res))
      (spacemacs/frame-killer))
    res))

(defun org-agenda-get-restriction-and-command@around (org-agenda-get-restriction-and-command prefix-descriptions)
  (let ((res (ignore-errors (funcall org-agenda-get-restriction-and-command prefix-descriptions))))
    (when (and (not res)
                (equal "agenda" (frame-parameter nil 'name)))
      (spacemacs/frame-killer))
    res))

;; Export Org to Apple Note
;; https://emacs-china.org/t/org-apple-note/10706
;; https://vxlabs.com/2018/10/29/importing-orgmode-notes-into-apple-notes/
(defun string-utils-escape-double-quotes (str-val)
  "Return STR-VAL with every double-quote escaped with backslash."
  (save-match-data
    (replace-regexp-in-string "\"" "\\\\\"" str-val)))
(defun string-utils-escape-backslash (str-val)
  "Return STR-VAL with every backslash escaped with an additional backslash."
  (save-match-data
    (replace-regexp-in-string "\\\\" "\\\\\\\\" str-val)))

(defun oan-export ()
  (interactive)
  (let ((title (file-name-base (buffer-file-name)))
        (as-tmpl "set TITLE to \"%s\"
  set NBODY to \"%s\"
  tell application \"Notes\"
          tell folder \"Org-mode\"
                  if not (note named TITLE exists) then
                          make new note with properties {name:TITLE}
                  end if
                  set body of note TITLE to NBODY
          end tell
  end tell"))
    (with-current-buffer (org-export-to-buffer 'html "*orgmode-to-apple-notes*")
      (let ((body (string-utils-escape-double-quotes
                   (string-utils-escape-backslash (buffer-string)))))
        ;; install title + body into template above and send to notes
        (do-applescript (format as-tmpl title body))
        ;; get rid of temp orgmode-to-apple-notes buffer
        (kill-buffer)
        (delete-window)
        (message "export successfully")))))

;; ox-html
(defun filter-local-links (link backend info)
  "Filter that converts all the /index.html links to /"
  (if (org-export-derived-backend-p backend 'html)
      (replace-regexp-in-string "/index.html" "/" link)))
