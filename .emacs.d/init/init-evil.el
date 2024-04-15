;;; init-evil.el --- Evil mode initialization -*- lexical-binding: t; -*-

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
;; Evil mode set up and configuration.
;; Provides vi/vim key bindings for Emacs.
;; https://github.com/emacs-evil/evil

;;; Code:

(require 'elpaca)
(require 'setup)
(require 'general)

;; Move line up
(defun +move-line-up ()
  (interactive)
  (transpose-lines 1)
  (previous-line 2))

;; Move line down
(defun +move-line-down ()
  (interactive)
  (next-line 1)
  (transpose-lines 1)
  (previous-line 1))

(defun +evil-shift-right ()
  (interactive)
  (evil-shift-right evil-visual-beginning evil-visual-end)
  (evil-normal-state)
  (evil-visual-restore))

(defun +evil-shift-left ()
  (interactive)
  (evil-shift-left evil-visual-beginning evil-visual-end)
  (evil-normal-state)
  (evil-visual-restore))

(elpaca evil
  (setup evil
    (:option
     evil-want-C-u-delete t
     evil-want-C-u-scroll t
     evil-disable-insert-state-bindings t
     evil-move-cursor-back nil
     evil-move-beyond-eol t
     evil-cross-lines t
     evil-ex-substitute-global t
     evil-split-window-below t
     evil-vsplit-window-right t
     evil-want-fine-undo t
     evil-kill-on-visual-paste nil
     evil-respect-visual-line-mode t
     evil-want-integration t
     evil-want-keybinding nil
     evil-search-module 'evil-search
     evil-undo-system 'undo-redo)
    (:general
     (:states '(normal)
              "<escape>" 'evil-ex-nohighlight
              "C-'" 'evil-normal-state)
     (:states '(insert)
              "TAB" 'tab-to-tab-stop
              "C-'" 'evil-normal-state)
     (:states '(visual)
              ">" '+evil-shift-right
              "<" '+evil-shift-left)
     (:states '(normal visual)
              "H" 'evil-beginning-of-line
              "L" 'evil-end-of-line
              "U" 'evil-redo
              "M-j" '+move-line-down
              "M-k" '+move-line-up)
     (:states '(normal)
              :keymaps '(override)
              :prefix "SPC"
              "qQ" '(evil-quit-all-with-error-code :which-key "Quit Emacs")
              "w"  '(evil-window-map :which-key "window")
              "wd" '(evil-window-delete :which-key "Kill window")))
    (evil-mode 1)))
(elpaca-wait) ;; Wait for completion

(elpaca evil-collection
  (setup evil-collection
    (:load-after evil-mode)
    (evil-collection-init)))

(elpaca evil-commentary
  (setup evil-commentary
    (:load-after evil-mode)
    (evil-commentary-mode)))

(elpaca evil-surround
  (setup evil-surround
    (:load-after evil-mode)
    (global-evil-surround-mode 1)))

(elpaca evil-textobj-anyblock
  (setup evil-textobj-anyblock
    (:load-after evil-mode)
    ;; Remove quotes from blocks
    (setq evil-textobj-anyblock-blocks '(("(" . ")") ("{" . "}") ("\\[" . "\\]") ("<" . ">"))) 
    ;; Set up quotes separately
    (setq evil-textobj-anyblock-quotes '(("'" . "'") ("\"" . "\"") ("`" . "`") ("“" . "”")))
    (evil-define-text-object evil-textobj-anyblock-inner-quote
      (count &optional beg end type)
      "Select the closest outer quote."
      (let ((evil-textobj-anyblock-blocks evil-textobj-anyblock-quotes))
	(evil-textobj-anyblock--make-textobj beg end type count nil)))
    (evil-define-text-object evil-textobj-anyblock-a-quote
      (count &optional beg end type)
      "Select the closest outer quote."
      (let ((evil-textobj-anyblock-blocks evil-textobj-anyblock-quotes))
	(evil-textobj-anyblock--make-textobj beg end type count t)))
    (:general
     (:keymaps '(evil-inner-text-objects-map)
	       "b" 'evil-textobj-anyblock-inner-block
	       "q" 'evil-textobj-anyblock-inner-quote)
     (:keymaps '(evil-outer-text-objects-map)
	       "b" 'evil-textobj-anyblock-a-block
	       "q" 'evil-textobj-anyblock-a-quote))))


(provide 'init-evil)
;; init-evil.el ends here
