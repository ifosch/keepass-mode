;;; keepass-utils.el --- Helper and utility functions for KeePass mode  -*- lexical-binding: t; coding: utf-8 -*-
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

(defvar keepass-db "")
(defvar keepass-password "")

(defun kpu--read-data-from-string (input)
  "Read data from INPUT string into an alist."
  (mapcar
    (lambda (arg) (split-string arg ":" nil " "))
    (split-string input "\n")))

(defun kpu--get-value-from-alist (key alist)
  "Get the value for KEY from the ALIST."
  (car (cdr (assoc key alist))))

(defun kpu--get-field (field entry)
  "Get FIELD from an ENTRY."
  (kpu--get-value-from-alist field (kpu--read-data-from-string entry)))

(defun kpu--command (group command)
  "Generate KeePass COMMAND to run, on GROUP."
  (format "echo %s | \
           keepassxc-cli %s %s %s | \
           grep -v 'Insert password to unlock'"
          keepass-password
          command
          keepass-db
          group))

(defun kpu--quote-unless-empty (text)
  "Quote TEXT unless it's empty."
  (if (= (length text) 0) text (format "'%s'" text)))

(provide 'keepass-utils)

;;; keepass-utils.el ends here
