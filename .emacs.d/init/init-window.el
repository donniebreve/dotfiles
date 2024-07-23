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

;;; Code:

(require 'general)
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

;; (setq display-buffer-alist
;;       '(("\\magit" (display-buffer-reuse-mode-window
;;                     display-buffer-reuse-window
;;                     +display-buffer-in-pop-up-or-side-window))
;;         ("\\*" (display-buffer-reuse-mode-window
;;                 display-buffer-reuse-window
;;                 +display-buffer-in-pop-up-or-side-window))))
;;(setq display-buffer-alist nil)

(defun +buffer-move (direction)
  "Attempt to move the current buffer to the window in `direction'."
  (interactive)
  (let ((target-window (windmove-find-other-window direction)))
    (if (not target-window)
        (message "No window to the %s" direction)
      (let ((buffer (current-buffer))
            (previous-buffer (other-buffer)))
        (set-window-buffer (selected-window) previous-buffer)
        (select-window target-window)
        (set-window-buffer (selected-window) buffer)))))

(defun +buffer-move-up ()
  "Attempt to move the current buffer to the window above."
  (interactive)
  (+buffer-move 'up))

(defun +buffer-move-down ()
  "Attempt to move the current buffer to the window below."
  (interactive)
  (+buffer-move 'down))

(defun +buffer-move-left ()
  "Attempt to move the current buffer to the window to the left."
  (interactive)
  (+buffer-move 'left))

(defun +buffer-move-right ()
  "Attempt to move the current buffer to the window to the right."
  (interactive)
  (+buffer-move 'right))

(general-define-key
 :states '(normal)
 :keymaps '(override)
 :prefix "SPC"
 "bK" '(+buffer-move-up :which-key "Move buffer up")
 "bJ" '(+buffer-move-down :which-key "Move buffer down")
 "bH" '(+buffer-move-left :which-key "Move buffer left")
 "bL" '(+buffer-move-right :which-key "Move buffer right"))

(provide 'init-window)
;; init-window.el ends here
