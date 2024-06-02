;;; init-ui.el --- User Inerface -*- lexical-binding: t; -*-

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
;; User inteface customizations.

;;; Code:

(menu-bar-mode -1)                          ; Disable the menu bar
(tool-bar-mode -1)                          ; Disable the toolbar
(scroll-bar-mode -1)                        ; Disable visible scrollbar
(tooltip-mode -1)                           ; Disable tooltips
(savehist-mode 1)                           ; Save minibuffer history
(save-place-mode 1)                         ; Save cursor location in buffers
(column-number-mode)                        ; Show column number in modeline
(display-time-mode t)                       ; Displays the time in the modeline
(setq frame-resize-pixelwise t)             ; Don't restrict frame size to char size
(setq frame-inhibit-implied-resize t)       ; Don't change frame size with font size
(setq use-dialog-box nil)                   ; Don't pop up UI dialogs when prompting
(setq desktop-save-mode nil)                ; Don't restore desktop
(setq inhibit-startup-message t)            ; Don't show start up screen
(setq visible-bell nil)                     ; Don't flash
(setq ring-bell-function 'ignore)           ; Don't ring (https://www.emacswiki.org/emacs/AlarmBell)
(setopt use-short-answers t)                ; Use y/n instead of yes/no
(setq calendar-date-style 'iso)             ; Use yyyy/mm/dd
(setq display-line-numbers-grow-only t)     ; Don't shrink the number gutter
(setq-default display-line-numbers-width 3) ; Use at least 3 columns for line numbers
;; Sets the vertical margin while scrolling (vim=scrolloff)
(setq scroll-margin 16)
;; (Turns this off for specific modes)
(add-hook 'term-mode-hook
          (lambda ()
            (make-local-variable 'scroll-margin)
            (setq scroll-margin 0)))
(add-hook 'comint-mode-hook
          (lambda ()
            (make-local-variable 'scroll-margin)
            (setq scroll-margin 0)))
;; Don't jump (https://www.reddit.com/r/emacs/comments/fwmqc8/how_to_stop_emacs_from_half_scrolling_from_bottom/)
(setq scroll-conservatively 101)

(provide 'init-ui)
;; init-ui.el ends here
