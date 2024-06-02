;;; init-eww.el --- EWW initialization -*- lexical-binding: t; -*-
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
;; EWW, the Emacs Web Wowser, is a web browser for GNU Emacs that
;; provides a simple, no-frills experience that focuses on
;; readability. It loads, parses, and displays web pages using
;; shr.el. It can display images inline, if Emacs was built with image
;; support, but there is no support for CSS or JavaScript.
;; https://www.gnu.org/software/emacs/manual/html_mono/eww.html

;;; Code:

(require 'setup)
(require 'general)

(setup eww
  (:option eww-search-prefix "https://search.brave.com/search?q="
           eww-header-line-format nil
           eww-download-directory (expand-file-name "~/Downloads")
           eww-browse-url-new-window-is-tab nil
           eww-form-checkbox-selected-symbol "[X]"
           eww-form-checkbox-symbol "[ ]"
           shr-use-fonts  nil
           shr-use-colors nil
           shr-indentation 2
           shr-width 120)
  (:general
   (:states '(normal)
            :keymaps '(eww-mode-map)
            "B" '(eww-list-bookmarks :which-key "Bookmarks"))))

(provide 'init-eww)
;; init-eww.el ends here
