;;; init-window.el --- Window customizations -*- lexical-binding: t; -*-

;; Copyright (C) 2024  donniebreve

;; Author: donniebreve <donniebreve@protonmail.com>
;; URL: https://github.com/donniebreve/dotfiles/.emacs.d
;; Version: 0
;; Package-Requires: ((emacs "29") (evil))

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
;; Window split customizations.
;;
;; I've never been able to get the splits to work how I want.
;; So far this seems to be working well.

;;; Code:

(require 'evil)

(setq split-height-threshold 120
      split-width-threshold 200)

;; Focus the help window when it is shown
(setq help-window-select t)

;; (defun +split-window-sensibly (&optional window)
;;   "Split WINDOW in a way suitable for display-buffer.
;; WINDOW defaults to the currently selected window.
;; If window count is one, split WINDOW horizontally.
;; If window count is not one and WINDOW is not the first
;; window, split WINDOW vertically. Otherwise don't split."
;;   (interactive)
;;   (let ((window (or window (selected-window))))
;;     (if (one-window-p)
;;         (with-selected-window window
;;           (split-window-right))
;;       (if (eq window (frame-first-window))
;;           (with-selected-window window
;;             (split-window-right))
;;         (with-selected-window window
;;           (split-window-below))))))

;; (setq split-window-preferred-function '+split-windows-right-and-down)

(defun +display-buffer-in-pop-up-or-side-window (buffer alist)
  "Display buffer in a pop-up if the window is splittable, otherwise
in a side window."
  (if (window-splittable-p (selected-window) t)
      (display-buffer-pop-up-window buffer alist)
    (if (eq (selected-window) (frame-first-window))
        (display-buffer-use-least-recent-window buffer alist)
      (display-buffer-same-window buffer alist))))

(setq display-buffer-alist
      '(("\\magit" (display-buffer-reuse-mode-window
                    display-buffer-reuse-window
                    +display-buffer-in-pop-up-or-side-window))
        ("\\*" (display-buffer-reuse-mode-window
                display-buffer-reuse-window
                +display-buffer-in-pop-up-or-side-window))))
;;(setq display-buffer-alist nil)

(evil-define-command +evil-window-decrease-width (count)
  "Decrease current window width by COUNT. Repeatable."
  :repeat t
  (interactive "p")
  (when (eq count 1)
    (setq count 5))
  (enlarge-window (- count) t))

(evil-define-command +evil-window-increase-width (count)
  "Increase current window width by COUNT. Repeatable."
  :repeat t
  (interactive "p")
  (when (eq count 1)
    (setq count 5))
  (enlarge-window count t))

(setup evil
  (:general
   (:states '(normal)
            :keymaps '(override)
            :prefix "SPC"
            "w<" '(+evil-window-decrease-width :which-key "Decrease window width")
            "w>" '(+evil-window-increase-width :which-key "Decrease window width"))))

(provide 'init-window)
;; init-window.el ends here
