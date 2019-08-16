;;; config.el --- Org Layer packages File for Spacemacs
;;
;; Copyright (c) 2017-2019 Tshu Wang
;;
;; Author: Tshu Wang <volekingsg@gmail.com>
;; URL: https://github.com/Voleking/spacemacs-configuration
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(with-eval-after-load 'org
  ;; Org basic
  (require 'org-tempo)
  (require 'org-projectile)

  (setq org-columns-default-format "%50ITEM %2PRIORITY %10Effort(Effort){:} %10CLOCKSUM"
        org-image-actual-width 500
        org-global-properties '(("STYLE_ALL" . "habit"))
        org-log-into-drawer t
        org-startup-indented t
        org-tags-match-list-sublevels 'intented
        org-track-ordered-property-with-tag t)

  (setq org-todo-keywords
        '((sequence "TODO(t)" "|" "DONE(d)")
          (sequence "PROJ(p)" "WAITING(w@/!)" "|" "SOMEDAY(s)" "CANCELED(c)"))
        org-todo-keyword-faces
        '(("TODO" . org-upcoming-deadline)
          ("DONE" . org-upcoming-distant-deadline)
          ("CANCELED" . org-upcoming-distant-deadline)
          ("PROJ" . (:foreground "RosyBrown4" :weight bold))
          ("WAITING" . (:foreground "light coral" :weight bold))
          ("SOMEDAY" . (:foreground "plum" :weight bold))))

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

  (setq org-gtd-file (expand-file-name "gtd.org" org-directory)
        org-default-notes-file (expand-file-name "gtd.org" org-directory))

  (setq org-capture-templates
        '(("i" "Inbox" entry (file org-gtd-file)
           "* %?\n  %i\n")
          ("j" "Journal" plain (function org-journal-find-location)
           "** %(format-time-string org-journal-time-format)%?")
          ("t" "Todo" entry (file org-gtd-file)
           "* TODO %?\n  %i\n")
          ("l" "Link" plain (file+function org-gtd-file org-capture-goto-link)
           "%i\n" :empty-lines 1 :immediate-finish t)
          ("a" "Attach" entry (file org-gtd-file)
           "* %?\n %(call-interactively #'org-attach-attach)"
           :immediate-finish t)
          ("R" "Review")
          ("Rd" "Daily Review" plain (function org-journal-find-location)
           "** Daily Review\n%?\n%i")
          ("Rw" "Weekly Review" plain (function org-journal-find-location)
           "* Weekly Review\n%?\n%i")))

  ;; Org Agenda
  (add-to-list 'org-modules 'org-habit t)

  (defun append-org-agenda-files (file)
    "append to org-agenda-files if file exists"
    (when (file-exists-p file)
      (push file org-agenda-files)))
	(mapcar 'append-org-agenda-files
          (org-projectile-todo-files))

  (setq org-agenda-clockreport-parameter-plist
        '(:maxlevel 3 :scope agenda-with-archives :fileskip0 t :stepskip0 t
                    :emphasize t :link t :narrow 80! :tcolumns 1 :formula %)
        org-agenda-columns-add-appointments-to-effort-sum t
        org-agenda-dim-blocked-tasks t
        org-agenda-persistent-filter t
        org-agenda-skip-additional-timestamps-same-entry t
        org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled
        org-agenda-todo-ignore-scheduled t
        org-agenda-todo-ignore-deadlines 'far
        org-agenda-time-grid nil
        org-enforce-todo-dependencies t
        org-enforce-todo-checkbox-dependencies t
        org-habit-graph-column 75
        org-stuck-projects '("/PROJ" ("TODO") nil ""))

  (setq org-agenda-custom-commands
        '(("c" "Daily agenda and all TODOs"
           ((agenda "" ((org-agenda-overriding-header "Today's tasks:")
                        (org-agenda-skip-scheduled-if-done t)
                        (org-agenda-skip-deadline-if-done t)
                        (org-agenda-skip-timestamp-if-done t)
                        (org-agenda-skip-scheduled-if-deadline-is-shown t)
                        (org-agenda-span 1)))
            (todo "TODO" ((org-agenda-overriding-header "All TODO items without scheduled or deadline")
                          (org-agenda-skip-function '(or (org-agenda-skip-entry-if 'timestamp)
                                                         (org-agenda-skip-subtree-if 'regexp "habit"))))))
           ((org-agenda-compact-blocks t)
            (org-agenda-dim-blocked-tasks 'invisible)
            (org-agenda-repeating-timestamp-show-all nil)))
          ("O" . "Overview")
          ("Od" "Daily Review"
           ((agenda "" ((org-agenda-span 3))))
           ((org-agenda-compact-blocks t)
            (org-agenda-start-with-log-mode '(closed clock state))
            (org-agenda-start-with-clockreport-mode t)
            (org-agenda-archives-mode)))
          ("Ow" "Weekly Review"
           ((agenda "" ((org-agenda-span 7)
                        (org-agenda-start-on-weekday 1)))
            (todo "PROJECT")
            (todo "WAITING")
            (todo "SOMEDAY")
            (stuck ""))
           ((org-agenda-compact-blocks t)
            (org-agenda-start-with-clockreport-mode t)
            (org-agenda-archives-mode t)))
          ("d" "Upcoming deadlines" agenda ""
           ((org-agenda-entry-types '(:deadline))
            (org-agenda-span 1)
            (org-deadline-warning-days 30)))))

  ;; Org Clock
  (defun my-org-clock-select-task ()
    (interactive)
    (org-clock-in '(4)))

  (org-clock-persistence-insinuate)
  (setq org-clock-auto-clock-resolution 'when-no-clock-is-running
        org-clock-history-length 10
        org-clock-idle-time 10
        org-clock-in-resume t
        org-clock-persist t
        org-clock-persist-query-resume nil
        org-clock-out-remove-zero-time-clocks t
        org-clock-out-when-done t
        org-clock-report-include-clocking-task t)

  ;; Org Refile
  (setq org-outline-path-complete-in-steps nil
        org-refile-allow-creating-parent-nodes 'confirm
        org-refile-use-outline-path 'file
        org-refile-targets
        '((org-agenda-files :maxlevel . 2)
          (deft-files :maxlevel . 1)))

  ;; Org Babel
  (setq org-confirm-babel-evaluate nil
        org-edit-src-content-indentation 0
        org-src-preserve-indentation t
        org-src-tab-acts-natively t)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (emacs-lisp . t)
     (C . t)
     (latex . t)
     (org . t)
     (latex . t)))

  ;; Org Exporter
  (require 'ox-html)
  (require 'ox-latex)
  (setf org-html-mathjax-options
        '((path " https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.3/MathJax.js?config=TeX-AMS_HTML")
          (scale "100")
          (align "center")
          (font "TeX")
          (linebreaks "false")
          (autonumber "AMS")
          (indent "0em")
          (multlinewidth "85%")
          (tagindent ".8em")
          (tagside "right")))

  ; Tex
  (add-hook 'org-mode-hook 'org-cdlatex-mode)
  (add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))

  (setq org-latex-compiler "xelatex"
        org-latex-packages-alist '(("fontset=macnew,UTF8" "ctex" t))
        org-preview-latex-default-process 'dvisvgm
        org-preview-latex-process-alist
        '((dvisvgm :programs ("xelatex" "dvisvgm")
                   :description "xdv > svg" :use-xcolor t
                   :message "you need to install the programs: xelatex and dvisvgm."
                   :image-input-type "xdv" :image-output-type "svg" :image-size-adjust (1.7 . 1.5)
                   :latex-compiler ("xelatex -no-pdf -interaction nonstopmode -output-directory %o %f")
                   :image-converter ("dvisvgm %f -n -b min -c %S -o %O"))
          (imagemagick :programs ("xelatex" "convert")
                       :description "pdf > png" :use-xcolor t
                       :message "you need to install the programs: xelatex and imagemagick."
                       :image-input-type "pdf" :image-output-type "png" :image-size-adjust (1.0 . 1.0)
                       :latex-compiler ("xelatex -interaction nonstopmode -output-directory %o %f")
                       :image-converter ("convert -density %D -trim -antialias %f -quality 100 %O"))))

  ;; org attach
  (setq org-attach-auto-tag "ATTACH"
        org-attach-archive-delete 'query
        org-attach-directory "attach/"
        org-attach-method 'mv)

  ;; org tag
  (setq org-fast-tag-selection-single-key 'expert
        org-tags-column 0
        org-tag-alist (quote ((:startgroup)
                              ("@office" . ?o)
                              ("@home" . ?h)
                              ("@computer" .?c)
                              ("@phone" . ?p)
                              (:endgroup)
                              ("PERSONAL" . ?p)
                              ("errants" . ?e)
                              ("WORK" . ?w)
                              ("NOTE" . ?n))))

  ;; Deft
  (setq deft-files (directory-files-recursively deft-directory ""))

  ;; org-journal
  (defun org-journal-find-location ()
    (org-journal-new-entry t)
    (goto-char (point-max)))

  (defun org-journal-date-format-func (time)
    "Custom function to insert journal date header,
and some custom text on a newly created journal file."
    (when (= (buffer-size) 0)
      (insert
       (pcase org-journal-file-type
         (`daily "#+TITLE: Daily Journal\n\n")
         (`weekly "#+TITLE: Weekly Journal\n\n")
         (`monthly "#+TITLE: Monthly Journal\n\n")
         (`yearly "#+TITLE: Yearly Journal\n\n"))))
    (concat org-journal-date-prefix (format-time-string "%A, %B %d %Y" time)))

  (setq org-journal-file-type 'weekly
        org-journal-date-format 'org-journal-date-format-func)

  ;; miscellaneous
  (defun org-search ()
    "use org-refile to search org-mode headings"
    (interactive)
    (org-refile '(4)))

  (spacemacs/set-leader-keys-for-major-mode 'org-mode
    "sf" 'org-search)

  ;; Interaction with system
  (defun supress-frame-splitting (&rest r)
    (let ((frame-name (frame-parameter nil 'name)))
      (when (or (equal "capture" frame-name)
                (equal "agenda" frame-name))
        (delete-other-windows))))

  (defun org-capture-finalize@after (&rest r)
    (when (equal "l" (plist-get org-capture-plist :key))
      (run-at-time 0 nil #'osx-switch-back-to-previous-application))
    (when (equal "capture" (frame-parameter nil 'name))
      (spacemacs/frame-killer)
      (run-at-time 0 nil #'osx-switch-back-to-previous-application)))

  (defun org-agenda-finalize@after (&rest r)
    (when (equal "agenda" (frame-parameter nil 'name))
      (run-at-time 0 nil #'osx-switch-back-to-previous-application)
      (spacemacs/frame-killer)))

  (defun org-capture-select-template@around (org-capture-select-template &optional keys)
    (let ((res (ignore-errors (funcall org-capture-select-template keys))))
      (unless res (setq res "q"))
      (when (and (equal "capture" (frame-parameter nil 'name))
                 (equal "q" res))
        (run-at-time 0 nil #'osx-switch-back-to-previous-application)
        (spacemacs/frame-killer))
      res))

  (defun org-agenda-get-restriction-and-command@around (org-agenda-get-restriction-and-command prefix-descriptions)
    (let ((res (ignore-errors (funcall org-agenda-get-restriction-and-command prefix-descriptions))))
      (when (and (equal "agenda" (frame-parameter nil 'name))
                 (not res))
        (run-at-time 0 nil #'osx-switch-back-to-previous-application)
        (spacemacs/frame-killer))
      res))

  (advice-add 'org-switch-to-buffer-other-window :after 'supress-frame-splitting)
  (advice-add 'org-capture-finalize :after 'org-capture-finalize@after)
  (advice-add 'org-capture-select-template :around 'org-capture-select-template@around)
  (advice-add 'org-agenda-quit :after 'org-agenda-finalize@after)
  (advice-add 'org-agenda-exit :after 'org-agenda-finalize@after)
  (advice-add 'org-agenda-get-restriction-and-command :around 'org-agenda-get-restriction-and-command@around)

  )
