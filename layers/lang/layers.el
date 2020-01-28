;;; layers.el --- lang layer layers file for Spacemacs.
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
   emacs-lisp
   (c-c++ :packages (cc-mode ccls company-c-headers)
          :variables
          c-c++-default-mode-for-headers 'c++-mode
          c++-enable-organize-includes-on-save t
          ;; c-c++-backend t
          c-basic-offset 4
          c-c++-adopt-subprojects t)
   (python :packages (not yapfify live-py-mode)
           :variables
           python-backend 'anaconda
           python-sort-imports-on-save t
           python-test-runner '(pytest nose)
           python-formatter 'black
           python-format-on-save nil
           blacken-fast-unsafe t
           python-fill-column 88)
   (conda :variables
          conda-anaconda-home "/usr/local/anaconda3")

   (lsp :packages (not lsp-treemacs))
   dap

   (org :packages (not org-brain org-download org-mime org-pomodoro org-present)
        :variables
        org-directory '"~/Documents/Org"
        org-agenda-files '("~/Documents/Org")
        org-projectile-file "TODOs.org"
        org-want-todo-bindings t
        org-enable-org-journal-support t
        org-journal-dir "~/Documents/Org/Journals/")
   my-org
   (deft :variables
     deft-directory "~/Documents/Org/Notes"
     deft-recursive t
     deft-extensions '("org" "md" "txt")
     deft-org-mode-title-prefix t
     deft-use-filename-as-title nil
     deft-use-filter-string-for-filename t
     deft-auto-save-interval 0
     deft-file-naming-rules '((noslash . "-") (nospace . "-") (case-fn . downcase))
     deft-strip-summary-regexp (concat "\\("
                                       "[\n\t]" ;; blank
                                       "\\|^#\\+[[:upper:]_]+:.*$" ;; org-mode metadata
                                       "\\|^#\\+[[:alnum:]_]+:.*$" ;; org-mode metadata
                                       "\\)"))

   (markdown :packages (not gh-md)
             :variables
             markdown-command "pandoc -f markdown+smart -t html5 --mathjax --highlight-style=pygments --toc --toc-depth 3 --template github.html5 --shift-heading-level-by=-1 --quiet"
             markdown-live-preview-engine 'pandoc)

   (latex :variables
          latex-enable-auto-fill nil
          latex-enable-folding t
          latex-enable-magic nil)
   (bibtex :variables
           org-ref-default-bibliography '("~/Dropbox/Documents/Zotero/refs.bib")
           org-ref-pdf-directory "~/Dropbox/Documents/Zotero/"
           org-ref-bibliography-notes "~/Dropbox/Documents/Zotero/notes.org")
   )
 )
