;;; layers.el --- editor layer layers file for Spacemacs.
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
 '(spacemacs-cfg

   (ivy :variables
        ivy-re-builders-alist '((spacemacs/counsel-search . spacemacs/ivy--regex-plus)
                                (counsel-M-x . ivy--regex-fuzzy)
                                (t . ivy--regex-ignore-order))
        ivy-initial-inputs-alist nil
        ivy-enable-advanced-buffer-information t)
   (shell :packages (comint shell eshell terminal-here shell-pop vterm)
          :variables
          shell-default-shell 'vterm
          shell-default-height 50
          close-window-with-terminal t
          terminal-here-terminal-command '("open" "-a" "iterm.app" "."))

   (version-control :packages (not browse-at-remote)
                    :variables
                    version-control-diff-tool 'diff-hl)
   (git :packages (not smeargle)
        :variables
        magit-wip-mode t
        git-magit-status-fullscreen t)
   (github :packages (not github-clone github-search))

   (auto-completion :packages (not auto-yasnippet auto-complete ac-ispell)
                    :variables
                    auto-completion-enable-help-tooltip 'manual
                    auto-completion-use-company-box t
                    auto-completion-enable-snippets-in-popup t
                    auto-completion-enable-sort-by-usage nil)
   (templates :variables
              templates-private-directory "~/.spacemacs.d/templates")
   (syntax-checking :variables
                    syntax-checking-enable-by-default nil)
   )
 )
