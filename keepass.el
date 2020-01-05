;;; keepass.el --- Access functions for KeePass mode  -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2019  Ignasi Fosch

;; Author: Ignasi Fosch <natx@y10k.ws>
;; Keywords: keepass password tools
;; Version: 0.0.1
;; Homepage: https://github.com/ifosch/keepass-mode
;; Package-Requires: ((emacs "27"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses>.

;;; Commentary:

;; KeePass mode provides a major mode to work with KeePass DB files.
;; So far it provides with simple navigation through folders and entries,
;; and copying passwords to Emacs clipboard.

;;; Code:

(require 'keepass-utils)
(defvar keepass-group-path "")

(defun keepass-get-password (entry)
  "Retrieve password for ENTRY."
  (keepass-utils-get-field "Password" (shell-command-to-string (keepass-utils-command (keepass-utils-quote-unless-empty entry) "show"))))

(defun keepass-get-entries (group)
  "Get entry list for GROUP."
  (nbutlast (split-string (shell-command-to-string (keepass-utils-command (keepass-utils-quote-unless-empty group) "ls")) "\n") 1))

(defun keepass-concat-group-path (group)
  "Concat GROUP and group path."
  (format "%s%s" keepass-group-path (or group "")))

(defun keepass-update-group-path (group)
  "Update group-path with GROUP."
  (setq keepass-group-path group))

(defun keepass-get-entry (entry)
  "Get ENTRY details."
  (shell-command-to-string (keepass-utils-command (keepass-utils-quote-unless-empty entry) "show")))

(provide 'keepass)

;;; keepass.el ends here
