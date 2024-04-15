;;; init-confirmation.el --- Confirmation settings -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024  donniebreve

;; Author: donniebreve <donniebreve@protonmail.com>
;; URL: https://github.com/donniebreve/dotfiles/.emacs.d/
;; Version: 0
;; Package-Requires: ((emacs "29") (elpaca) (setup) (general) (evil))

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
;; Sets up kill confirmation messages a la DOOM.
;; Taken from DOOM Emacs (with love). 
;; https://github.com/doomemacs/doomemacs

;;; Code:

(global-set-key [remap save-buffers-kill-terminal] #'save-buffers-kill-emacs)

(defvar doom-quit-messages
  `(;; from Doom 1
    "Please don't leave, there's more demons to toast!"
    "Let's beat it -- This is turning into a bloodbath!"
    ,(format "I wouldn't leave if I were you. %s is much worse."
             (if (member system-type '(ms-dos-windows-nt cygwin))
                 "DOS"
               "UNIX"))
    "Don't leave yet -- There's a demon around that corner!"
    "Ya know, next time you come in here I'm gonna toast ya."
    "Go ahead and leave. See if I care."
    ;;"Are you sure you want to quit this great editor?" ; meh
    ;; from Portal
    "Thank you for participating in this Aperture Science computer-aided enrichment activity."
    "You can't fire me, I quit!"
    "I don't know what you think you are doing, but I don't like it. I want you to stop."
    "This isn't brave. It's murder. What did I ever do to you?"
    "I'm the man who's going to burn your house down! With the lemons!"
    "Okay, look. We've both said a lot of things you're going to regret..."
    ;; Custom
    "(setq nothing t everything 'permitted)"
    ;;"Emacs will remember that." ; meh
    "Emacs, Emacs never changes."
    "Hey! Hey, M-x listen!"
    "It's not like I'll miss you or anything, b-baka!"
    "Wake up, Mr. Stallman. Wake up and smell the ashes."
    "You are *not* prepared!"
    "Please don't go. The drones need you. They look up to you."
    ;; Extra
    "And by your standards, every crime committed by a Christian is a stain on Christ. - And so it is."
    "We choose [to leave Emacs], not because [it is] easy, but because [it is] hard.")
  "A list of quit messages, picked randomly by `+quit'. Taken from
http://doom.wikia.com/wiki/Quit_messages and elsewhere.")

(defun doom-quit-p (&optional prompt)
  "Prompt the user for confirmation when killing Emacs. Returns t if it is safe to 
kill this session."
  (or ;;(not (ignore-errors (doom-real-buffer-list))) ;; we don't have this function
      (yes-or-no-p (format "%s" (or prompt "Really quit Emacs?")))
      (ignore (message "Aborted"))))

(defun +quit (&rest _)
  (doom-quit-p
   (format "%s"
           (propertize (nth (random (length doom-quit-messages))
                            doom-quit-messages)
                       'face '(italic default)))))

(setq confirm-kill-emacs '+quit)

(provide 'init-confirmation)
;; init-confirmation.el ends here
