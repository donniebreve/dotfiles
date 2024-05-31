;;; init-dashboard.el --- setup.el -*- lexical-binding: t; -*-

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
;; Dashboard set up and configuration.
;; https://github.com/emacs-dashboard/emacs-dashboard

;;; Code:

(require 'elpaca)
(require 'setup)

(elpaca dashboard
  (setup dashboard
    (:option
     dashboard-startup-banner (expand-file-name "banners/sub-zero.txt" user-emacs-directory)
     dashboard-center-content t
     dashboard-filter-agenda-entry 'dashboard-no-filter-agenda
     dashboard-items '((recents  . 5)
                       (projects . 5)
                       (agenda . 5)
                       (bookmarks . 5))
     dashboard-agenda-release-buffers t)
    (add-hook 'elpaca-after-init-hook #'dashboard-insert-startupify-lists)
    (add-hook 'elpaca-after-init-hook #'dashboard-initialize)
    (dashboard-setup-startup-hook)))

(provide 'init-dashboard)
;; init-dashboard.el ends here
