;;; config.el --- Org Layer packages File for Spacemacs
;;
;; Copyright (c) 2017 Voleking
;;
;; Author: Sylvain Benner <volekingsg@gmail.com>
;; URL: https://github.com/Voleking/spacemacs-configuration
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(with-eval-after-load 'org
  (setq evil-org-key-theme '(textobjects navigation additional insert todo))
  ;; (setq org-startup-indented t)
  (setq org-agenda-span 'day)
  (setq org-agenda-custom-commands
        '(("c" "Simple agenda view"
           ((agenda "")
            (alltodo "")))))
  (setq org-refile-targets
        '((nil :maxlevel . 1)
          (org-agenda-files :maxlevel . 2)))

  (setq org-agenda-file-gtd (expand-file-name "gtd.org" org-directory))
  (setq org-agenda-file-gcal (expand-file-name "gcal.org" org-directory))
  (setq org-agenda-file-note (expand-file-name "notes.org" org-directory))
  (setq org-agenda-file-journal (expand-file-name "journal.org" org-directory))
  (setq org-default-notes-file (expand-file-name "gtd.org" org-directory))

  (setq org-capture-templates
       '(("t" "Todo" entry (file+headline org-agenda-file-gtd "Tasks")
            "* TODO %?\n  %i\n")
          ("a" "Appointment" entry (file org-agenda-file-gcal)
           "* %?\n  %^T\n\n")
          ("A" "Assignments" entry (file+headline org-agenda-file-gtd "Assignments")
           "* TODO [#A] %?\n  %i\n")
          ("i" "Inbox" entry (file+headline org-agenda-file-gtd "Inbox")
           "* %?\n  %i\n")
          ("l" "Link" entry (file+headline org-agenda-file-gtd "Inbox")
           "* [[%l][%:description]]\n %i\n")
          ("n" "Notes" entry (file+headline org-agenda-file-note "Quick notes")
            "* %?\n  %i\n %T\n"
            :empty-lines 1)
          ("j" "Journal" entry (file+datetree org-agenda-file-journal)
            "* %?\n"
            :empty-lines 1)
          ))

  (with-eval-after-load 'org-agenda
    (require 'org-projectile)
    (setq org-agenda-files (append org-agenda-files (org-projectile:todo-files))))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (emacs-lisp . t)
     (C . t)
     (latex . t)
     ))

  ;; default options for all output formats
  (setq org-pandoc-options '((standalone . t)))
  ;; cancel above settings only for 'docx' format
  (setq org-pandoc-options-for-docx '((standalone . nil)))
  ;; special settings for beamer-pdf and latex-pdf exporters
  (setq org-pandoc-options-for-beamer-pdf '((latex-engine . "xelatex")))
  (setq org-pandoc-options-for-latex-pdf '((latex-engine . "xelatex")
                                           (template . "eisvogel.latex")))

  ;; Org Agent alert
  ;; https://emacs-china.org/t/org-agenda/232
  (require 'appt)
  (setq appt-time-msg-list nil)      ;; clear existing appt list
  (setq appt-display-interval '10)   ;; warn every 10 minutes from t - appt-message-warning-time
  (setq
    appt-message-warning-time '15    ;; send first warning 15 minutes before appointment
    appt-display-mode-line nil       ;; don't show in the modeline
    appt-display-format 'window)     ;; pass warnings to the designated window function
    (appt-activate 1)                ;; activate appointment notification
    (display-time)                   ;; activate time display
  (with-eval-after-load 'org-agenda
    (org-agenda-to-appt))            ;; generate the appt list from org agenda files on emacs launch
  (run-at-time "24:01" 3600 'org-agenda-to-appt)           ;; update appt list hourly
  (add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt) ;; update appt list on agenda view
  (defun my-appt-display (min-to-app new-time msg)
    (notify-osx
    (format "Appointment in %s minutes" min-to-app)    ;; passed to -title in terminal-notifier call
    (format "%s" msg)))                                ;; passed to -message in terminal-notifier call
  (setq appt-disp-window-function (function my-appt-display))

  ;; (require 'org-gcal)
  ;; (setq org-gcal-client-id "oauth 2.0 client ID"
  ;;       org-gcal-client-secret "client secret"
  ;;       org-gcal-file-alist '(("volekingsg@gmail.com" . "~/Documents/Org/gcal.org")))

  )
