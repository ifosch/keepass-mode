;;; keepass.el --- Access functions for KeePass mode  -*- lexical-binding: t; coding: utf-8 -*-
;;
;; Copyright (C) 2019  Ignasi Fosch
;;
;; Author: Ignasi Fosch <natx@y10k.ws>
;; Keywords: keepass password tools
;; Version: 0.0.1
;; Homepage: https://github.com/ifosch/keepass-mode
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses>.

;;; Commentary:
;; KeePass mode provides a major mode to work with KeePass DB files.
;; So far it provides with simple navigation through folders and entries,
;; and copying passwords to Emacs clipboard.

;;; Code:

(require 'keepass-utils)
(defvar keepass-group-path "")

(defun keepass-select (entry)
  "Select ENTRY in current KeePass DB."
  (if (string-suffix-p "/" entry)
      (setq keepass-group-path (keepass-open entry))
      (keepass-show entry)))

(defun keepass-back ()
  "Navigate back in group tree."
  (setq keepass-group-path (mapconcat 'identity (butlast (split-string keepass-group-path "/" t) 1) "/"))
  (keepass-open nil))

(defun keepass-copy-password (entry)
  "Copy ENTRY password to clipboard."
  (if (string-suffix-p "/" entry)
      (message (format "%s is a group, not an entry" entry))
      (progn (kill-new (kpu--get-field "Password" (shell-command-to-string (kpu--command (format "'%s'" entry) "show"))))
             (message (format "Password for '%s%s' copied to kill-ring" keepass-group-path entry)))))

(defun keepass-ask-password ()
  "Ask the user for the password."
  (read-passwd (format "Password for %s: " keepass-db)))

(defun keepass-open (group)
  "Open a Keepass file at GROUP."
  (let ((columns [("Key" 100)])
        (rows (mapcar (lambda (x) `(nil [,x]))
              (split-string (shell-command-to-string (kpu--command
                (format "'%s%s'" keepass-group-path (or group "")) "ls")) "\n"))))
    (setq tabulated-list-format columns)
    (setq tabulated-list-entries rows)
    (insert (format "%s%s" keepass-group-path (or group "")))
    (tabulated-list-init-header)
    (tabulated-list-print)
    (format "%s%s" keepass-group-path (or group ""))))

(defun keepass-show (group)
  "Show a Keepass entry at GROUP."
  (let* ((entry (format "%s%s" (if (boundp 'keepass-group-path) keepass-group-path "") (or group "")))
        (output (shell-command-to-string (kpu--command (format "'%s'" entry) "show"))))
    (switch-to-buffer (format "*keepass %s %s*" keepass-db entry))
    (insert (replace-regexp-in-string "Password: .+" "Password: ************" output))
    (read-only-mode)))

(provide 'keepass)

;;; keepass.el ends here
