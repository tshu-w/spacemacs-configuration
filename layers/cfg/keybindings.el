;;; keybindings.el --- Org Layer packages File for Spacemacs
;;
;; Copyright (c) 2017-2020 2017-2020Tianshu Wang
;;
;; Author: Tianshu Wang <volekingsg@gmail.com>
;; URL: https://github.com/tshu-w/spacemacs-configuration
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(spacemacs/set-leader-keys "o" (lookup-key global-map (kbd "C-c")))
(spacemacs/set-leader-keys "o i" 'my-org-clock-select-task)
(spacemacs/set-leader-keys "o I" 'org-clock-in)
(spacemacs/set-leader-keys "o o" 'org-clock-out)
(spacemacs/set-leader-keys "o g" 'org-mac-grab-link)
(spacemacs/set-leader-keys "o s" 'org-save-all-org-buffers)
(spacemacs/set-leader-keys "o h" 'helm-org-agenda-files-headings)
(spacemacs/set-leader-keys "a o g" 'org-mac-grab-link)

(global-set-key (kbd "H-'") 'iterm-goto-filedir-or-home)
(global-set-key (kbd "H-.") 'iterm-focus)
(global-set-key (kbd "H-b") 'compile)
(global-set-key (kbd "H-r") 'run-current-file)
(global-set-key (kbd "H-/") 'comment-dwim)
(global-set-key (kbd "<H-backspace>") 'backward-kill-line)

(with-eval-after-load 'helm-files
	(define-key helm-find-files-map (kbd "M-o") (lambda () (interactive)
                                                (setq current-prefix-arg '(4))
                                                (helm-ff-run-switch-other-window))))

(with-eval-after-load 'evil-maps
  (define-key evil-normal-state-map (kbd "DEL") 'evil-repeat-find-char-reverse)
  (define-key evil-insert-state-map (kbd "C-c u") 'backward-kill-line)
  (define-key evil-insert-state-map (kbd "C-c w") 'backward-kill-word)
  (evil-define-key 'insert org-mode-map (kbd "<tab>") #'org-cycle)
  (evil-ex-define-cmd "q" 'kill-buffer-and-window)
  (evil-ex-define-cmd "quit" 'evil-quit)
  )

(with-eval-after-load 'transient
  (transient-bind-q-to-quit))

(with-eval-after-load 'magit
  (transient-append-suffix 'magit-remote "C"
    '("o" "Open remote" magit-open-repo)))
