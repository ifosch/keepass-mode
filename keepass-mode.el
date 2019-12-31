;;; keepass-mode.el --- Mode for KeePass DB  -*- lexical-binding: t; coding: utf-8 -*-
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

(require 'keepass)

(defun kpm-select ()
  "Select an entry in current Keepass key."
  (interactive)
  (let ((entry (aref (tabulated-list-get-entry) 0)))
    (if (kpu--is-group entry)
        (progn
          (keepass-update-group-path (keepass-concat-group-path entry))
          (kpm-open))
      (kpm-show entry))))

(defun kpm-back ()
  "Navigate back in group tree."
  (interactive)
  (keepass-update-group-path (mapconcat 'identity (butlast (split-string keepass-group-path "/" t) 1) "/"))
  (kpm-open))

(defun kpm-copy-password ()
  "Copy current entry password to clipboard."
  (interactive)
  (let ((entry (aref (tabulated-list-get-entry) 0)))
    (if (kpu--is-group entry)
        (message (format "%s is a group, not an entry" entry))
      (progn (kill-new (keepass-get-password entry))
             (message (format "Password for '%s%s' copied to kill-ring" keepass-group-path entry))))))

(defun kpm-open ()
  "Open a Keepass file at GROUP."
  (let ((columns [("Key" 100)])
        (rows (mapcar (lambda (x) `(nil [,x]))
                      (keepass-get-entries keepass-group-path))))
    (setq tabulated-list-format columns)
    (setq tabulated-list-entries rows)
    (tabulated-list-init-header)
    (tabulated-list-print)))

(defun kpm-ask-password ()
  "Ask the user for the password."
  (read-passwd (format "Password for %s: " keepass-db)))

(defun kpm-show (group)
  "Show a Keepass entry at GROUP."
  (let* ((entry (keepass-concat-group-path group))
        (output (replace-regexp-in-string "Password: .+" "Password: *************" (keepass-get-entry entry))))
    (switch-to-buffer (format "*keepass %s %s*" keepass-db entry))
    (insert output)
    (read-only-mode)))

(defvar keepass-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'kpm-select)
    (define-key map (kbd "<backspace>") 'kpm-back)
    (define-key map (kbd "c") 'kpm-copy-password)
   map))

(define-derived-mode keepass-mode tabulated-list-mode "KeePass mode"
  "KeePass mode."
  (setq keepass-db buffer-file-truename)
  (setq keepass-password (kpm-ask-password))
  (setq keepass-group-path "")
  (kill-buffer)
  (switch-to-buffer (format "*KeePass %s*" keepass-db))
  (use-local-map keepass-mode-map)
  (make-local-variable 'keepass-db)
  (make-local-variable 'keepass-password)
  (make-local-variable 'keepass-group-path)
  (kpm-open))

(add-to-list 'auto-mode-alist '("\\.kdbx\\'" . keepass-mode))
(add-to-list 'auto-mode-alist '("\\.kdb\\'" . keepass-mode))

(provide 'keepass-mode)

;;; keepass-mode.el ends here
