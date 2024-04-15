;;; init-markdown-mode.el --- Markdown mode initialization -*- lexical-binding: t; -*-

;; Copyright (C) 2024  donniebreve

;; Author: donniebreve <donniebreve@protonmail.com>
;; URL: https://github.com/donniebreve/dotfiles/.emacs.d
;; Version: 0
;; Package-Requires: ((emacs "29") (elpaca) (setup))

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
;; markdown-mode is a major mode for editing Markdown-formatted text.
;; https://jblevins.org/projects/markdown-mode

;;; Code:

(elpaca markdown-mode
  (setup markdown-mode
    (:option markdown-command "multimarkdown")
    (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))))

(provide 'init-markdown-mode)
;; init-markdown-mode.el ends here
