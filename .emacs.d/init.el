;;; init.el --- Emacs configuration -*- lexical-binding: t -*-

;; Copyright (C) 2024  donniebreve

;; Author: donniebreve <donniebreve@protonmail.com>
;; URL: https://github.com/donniebreve/dotfiles/.emacs.d
;; Version: 0
;; Package-Requires: ((emacs "29"))

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
;; My main Emacs configuration file. Heavily relies on
;; elpaca, setup, general and evil.

;;; References:
;;
;; https://github.com/d12frosted/environment/tree/master/emacs
;; https://github.com/daviwil/emacs-from-scratch/blob/master/init.el
;; https://whhone.com/emacs-config/#startup

;;; Informational links:
;;
;; https://endlessparentheses.com/get-in-the-habit-of-using-sharp-quote.html

;;; Code:

;; Load private files
(load-file (expand-file-name "private/feeds.el" user-emacs-directory))

;; Add init files to load path
(add-to-list 'load-path (expand-file-name "init" user-emacs-directory))

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Use ctrl+shift+p to get to the "actions menu"
(global-set-key (kbd "C-S-p") 'execute-extended-command)

;; Core
(require 'init-env)
(require 'init-elpaca)
(require 'init-setup)
(require 'init-general)
(require 'init-evil)

;; User interface
(require 'init-ui)
(require 'init-window)
(require 'init-font)
(require 'init-theme)
(require 'init-vertico)
(require 'init-consult)
(require 'init-orderless)
(require 'init-marginalia)
(require 'init-dashboard)
(require 'init-which-key)
(require 'init-helpful)
(require 'init-mood-line)
(require 'init-confirmation)

;; Files
(require 'init-dired)
(require 'init-project)

;; Editing
(require 'init-text)
(require 'init-company)
(require 'init-org)
(require 'init-markdown-mode)
(require 'init-visual-fill-column)

;; Programming
(require 'init-prog)
(require 'init-smartparens)
(require 'init-avy)
(require 'init-eglot)
(require 'init-eldoc)
(require 'init-magit)
(require 'init-web-mode)
(require 'init-editorconfig)
(require 'init-pcre2el)
(require 'init-hl-todo)

;; Terminal
(require 'init-terminal)

;; Leisure
(require 'init-elfeed)
(require 'init-eww)

;; Global Emacs keybindings These functions are default Emacs
;; functions only. Package initialization may define global
;; keybindings as well.
(general-define-key
 :states '(normal)
 :keymaps '(override)
 :prefix "SPC"
 ;; Global
 ";"  '(pp-eval-expression :which-key "Eval expression")
 ":"  '(execute-extended-command :which-key "M-x")
 "x"  '(scratch-buffer :which-key "Switch to scratch buffer")
 "X"  '(org-capture :which-key "Org capture")
 "u"  '(universal-argument :which-key "Universal argument")
 ;; Buffer
 "b"  '(:ignore t :which-key "buffer")
 "b[" '(previous-buffer :which-key "Previous buffer")
 "b]" '(next-buffer :which-key "Next buffer")
 "bd" '(kill-current-buffer :which-key "Kill buffer")
 ;; File
 "f"  '(:ignore t :which-key "file")
 "fs" '(save-buffer :which-key "Save file")
 "fS" '(write-file :which-key "Save file as...")
 ;; Help
 "h"  '(:ignore t :which-key "help")
 "h'" '(describe-char :which-key "Describe char")
 "hF" '(describe-face :which-key "Describe face")
 "hm" '(describe-mode :which-key "Describe mode")
 ;; Insert
 "i"  '(:ignore t :which-key "insert")
 ;; Open
 "o"  '(:ignore t :which-key "open")
 ;; Project
 "p"  '(:ignore t :which-key "project")
 ;; Package
 "P"  '(:ignore t :which-key "package")
 "Pl" '(elpaca-log :which-key "List packages")
 ;; Quit
 "q"  '(:ignore t :which-key "quit")
 "qq" '(save-buffers-kill-emacs :which-key "Quit Emacs"))
