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
  (require 'org-protocol-capture-html)

  (setq evil-org-key-theme '(textobjects navigation additional insert todo))
  (add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))
  (setq org-startup-indented t)
  (setq org-agenda-span 'day)

  (setq org-agenda-custom-commands
        '(("c" "Daily agenda and all TODOs"
           ((tags "PRIORITY=\"A\""
                  ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '(done "CANCELED")))
                   (org-agenda-overriding-header "High-priority unfinished tasks:")))
            (agenda "" ((org-agenda-overriding-header "Today tasks:")))
            (todo "TODO|STARTED" ((org-agenda-skip-function '(or (org-agenda-skip-entry-if 'regexp "\\[#A\\]")
                                                                 (org-agenda-skip-subtree-if 'regexp ":LAST_REPEAT:")
                                                                 (org-agenda-skip-entry-if 'timestamp)))
                      (org-agenda-overriding-header "All TODO Items except High-priority:")))
            (todo "WAITING" )
            (todo "SOMEDAY" )
            )
           ((org-agenda-compact-blocks t)
            (org-agenda-priority-up t)
            (org-agenda-repeating-timestamp-show-all nil)))
         ))

  (setq org-refile-targets
        '((nil :maxlevel . 1)
          (org-agenda-files :maxlevel . 2)))

  (setq org-todo-keywords
        '((sequence "TODO(t)" "STARTED(s!)" "|" "DONE(d)")
          (sequence "WAITING(w@/!)" "SOMEDAY(f)" "|" "CANCELED(c!)"))
        )

  (setq org-todo-keyword-faces
        '(("WAITING" . (:foreground "IndianRed1" :weight bold))
          ("STARTED" . (:foreground "plum" :weight bold))
          ("SOMEDAY" . (:foreground "thistle" :weight bold))))

  (setq org-agenda-file-gtd (expand-file-name "gtd.org" org-directory))
  (setq org-agenda-file-gcal (expand-file-name "gcal.org" org-directory))
  (setq org-agenda-file-note (expand-file-name "notes.org" org-directory))
  (setq org-agenda-file-journal (expand-file-name "journal.org" org-directory))
  (setq org-file-archive (expand-file-name "archive.org" org-directory))
  (setq org-default-notes-file (expand-file-name "gtd.org" org-directory))

  (setq org-capture-templates
       '(("t" "Todo" entry (file+headline org-agenda-file-gtd "Tasks")
          "* TODO %?\nSCHEDULED: %t\n")
          ("a" "Appointment" entry (file org-agenda-file-gcal)
           "* %?\n\n  %^T\n\n")
          ("A" "Assignments" entry (file+headline org-agenda-file-gtd "Assignments")
           "* TODO [#A] %?\n  %i\n")
          ("i" "Inbox" entry (file+headline org-agenda-file-gtd "Inbox")
           "* %?\n  %i\n")
          ("l" "Link" entry (file+headline org-agenda-file-gtd "Inbox")
           "* [[%l][%:description]]\n %i\n")
          ("n" "Notes" entry (file+headline org-agenda-file-note "Quick notes")
            "* %?\n\t%U\n"
            :empty-lines 1)
          ("j" "Journal" entry (file+datetree org-agenda-file-journal)
            "* %?\n"
            :empty-lines 1)
          ("w" "Web site" entry (file org-file-archive)
           "* %a :website:\n\n%U %?\n\n%:initial")
          ))

  (with-eval-after-load 'org-agenda
    (require 'org-projectile)
    ;; (setq org-agenda-files (append org-agenda-files (org-projectile:todo-files)))
    (mapcar '(lambda (file)
               (when (file-exists-p file)
                 (push file org-agenda-files)))
            (org-projectile-todo-files))
    )

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (emacs-lisp . t)
     (C . t)
     (latex . t)
     (org . t)
     ))

  (use-package pyim
    :config
    (defun eh-company-dabbrev--prefix (orig-fun)
      "取消中文补全"
      (let ((string (pyim-char-before-to-string 0)))
        (if (pyim-string-match-p "\\cc" string)
            nil
          (funcall orig-fun))))
    (advice-add 'company-dabbrev--prefix
                :around #'eh-company-dabbrev--prefix))

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
