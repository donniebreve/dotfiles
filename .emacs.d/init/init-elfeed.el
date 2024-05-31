;;; init-elfeed.el --- Magit initialization -*- lexical-binding: t; -*-
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
;; Elfeed is an extensible web feed reader for Emacs, supporting both Atom and RSS. It requires Emacs 24.3 and is available for download from MELPA or el-get. Elfeed was inspired by notmuch.
;; https://github.com/skeeto/elfeed

;;; Code:

(require 'elpaca)
(require 'setup)
(require 'general)

(defun +elfeed-search-browse-url-eww ()
  "Visit the current entry in your eww using `eww-browse-url'."
  (interactive)
  (let ((buffer (current-buffer))
        (entries (elfeed-search-selected)))
    (cl-loop for entry in entries
             do (elfeed-untag entry 'unread)
             when (elfeed-entry-link entry)
             do (eww-browse-url it))
    ;; `browse-url' could have switched to another buffer if eww or another
    ;; internal browser is used, but the remainder of the functions needs to
    ;; run in the elfeed buffer.
    (with-current-buffer buffer
      (mapc #'elfeed-search-update-entry entries)
      (unless (or elfeed-search-remain-on-entry (use-region-p))
        (forward-line)))))

(defun +elfeed-show-browse-url-eww ()
  "Visit the current entry in your eww using `eww-browse-url'."
  (interactive)
  (let ((link (elfeed-entry-link elfeed-show-entry)))
    (when link
      (eww-browse-url link))))

(elpaca elfeed
  (setup elfeed
    (:option elfeed-feeds --elfeed-feeds)
    (:face shr-text ((t ('default))))
    (:after-load
     ;; To overwrite existing keybindings, we should call general after the package has been loaded
     (general-define-key
      :states '(normal)
      :keymaps '(elfeed-search-mode-map)
      "gr" '(elfeed-update :which-key "Update")
      "go" '(+elfeed-search-browse-url-eww :which-key "Open in EWW"))
     (general-define-key
      :states '(normal)
      :keymaps '(elfeed-show-mode-map)
      "go" '(+elfeed-show-browse-url-eww :which-key "Open in EWW")))
    (:general
     (:states '(normal)
              :keymaps '(override)
              :prefix "SPC"
              "on" '(elfeed :which-key "Open News")))))

(provide 'init-elfeed)
;; init-elfeed.el ends here
