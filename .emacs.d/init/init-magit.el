;;; init-magit.el --- Magit initialization -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024  donniebreve

;; Author: donniebreve <donniebreve@protonmail.com>
;; URL: https://github.com/donniebreve/dotfiles/.emacs.d
;; Version: 0
;; Package-Requires: ((emacs "29") (elpaca) (setup) (general))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Magit is an interface to the Git version control system, implemented as a GNU Emacs package written in Elisp.
;; https://magit.vc/
;;
;; magit-todos: displays keyword entries from source code comments and Org files in the Magit status buffer.
;; https://github.com/alphapapa/magit-todos
;;
;; transient: implements a similar abstraction involving a prefix command, infix arguments and suffix commands.
;; https://www.gnu.org/software/emacs/manual/html_mono/transient.html

;;; Code:

(require 'elpaca)
(require 'setup)
(require 'general)

(elpaca (magit :tag "v3.3.0")
  (setup magit
    (:general
     (:states '(normal)
              :keymaps '(override)
              :prefix "SPC"
              "g"   '(:ignore t :which-key "Magit")
              "g/"  '(magit-dispatch :which-key "Magit dispatch")
              "g."  '(magit-file-dispatch :which-key "Magit file dispatch")
              "gb"  '(magit-branch-checkout :which-key "Magit checkout branch")
              "gB"  '(magit-blame-addition :which-key "Magit blame addition")
              "gc"  '(:ignore t :which-key "Magit create")
              "gcc" '(magit-commit-create :which-key "Magit create commit")
              "gcf" '(magit-commit-fixup :which-key "Magit create fixup")
              "gC"  '(magit-clone :which-key "Magit clone")
              "gD"  '(magit-file-delete :which-key "Magit file delete")
              "gf"  '(:ignore t :which-key "Magit find")
              "gfc" '(magit-show-commit :which-key "Magit find commit")
              "gff" '(magit-find-file :which-key "Magit find file")
              "gfg" '(magit-find-git-config-file :which-key "Magit find gitconfig file")
              "gF"  '(magit-fetch :which-key "Magit fetch")
              "gg"  '(magit-status :which-key "Magit status")
              "gG"  '(magit-status-here :which-key "Magit status here")
              "gI"  '(magit-init :which-key "Magit initialize repo")
              "gl"  '(:ignore t :which-key "Magit list")
              "glr" '(magit-list-repositories :which-key "Magit list repositories")
              "gls" '(magit-list-submodules :which-key "Magit list submodules")
              "gL"  '(magit-log-buffer-file :which-key "Magit buffer log")
              "gS"  '(magit-stage-file :which-key "Magit stage file")
              "gU"  '(magit-unstage-file :which-key "Magit unstage file")))))

(setup transient
  (:load-after magit)
  (:general
   (:keymaps '(transient-map)
             "<escape>" 'transient-quit-one)))

(elpaca magit-todos
  (setup magit-todos
    (:load-after magit)
    ;; (if (eq system-type 'windows-nt)
    ;;     (setq magit-todos-nice nil)) ;; don't use `nice` on windows, it doesn't exist T_T
    ;;;; https://github.com/alphapapa/magit-todos/issues/156
    ;; (if (eq system-type 'windows-nt)
    ;;     (setq magit-todos-scanners nil)
    ;;   (magit-todos-defscanner "rg"
    ;;                           :test (executable-find "rg")
    ;;                           :directory-form (f-relative directory default-directory) ;; revert
    ;;                           :allow-exit-codes (0 1)
    ;;                           :command (list "rg" "--no-heading" "--line-number"
    ;;                                          (when depth
    ;;                                            (list "--maxdepth" (1+ depth)))
    ;;                                          (when magit-todos-ignore-case
    ;;                                            "--ignore-case")
    ;;                                          (when magit-todos-exclude-globs
    ;;                                            (--map (list "--glob" (concat "!" it))
    ;;                                                   magit-todos-exclude-globs))
    ;;                                          (unless magit-todos-submodule-list
    ;;                                            (--map (list "--glob" (concat "!" it))
    ;;                                                   (magit-list-module-paths)))
    ;;                                          extra-args search-regexp-pcre directory)))
    (:general
     (:states '(normal)
              :keymaps '(override)
              :prefix "SPC"
              "pt" '(magit-todos-list :which-key "List project todos")))))

(provide 'init-magit)
;; init-magit.el ends here
