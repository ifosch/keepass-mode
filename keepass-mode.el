;;; keepass-mode.el --- Mode to open Keepass DB  -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2019  Ignasi Fosch

;; Author: Ignasi Fosch <natx@y10k.ws>
;; Keywords: data files tools
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
;; along with this program.  If not, see <https://www.gnu.org/licenses>.

;;; Commentary:

;; KeePass mode provides a major mode to work with KeePass DB files.
;; So far it provides with simple navigation through folders and entries,
;; and copying passwords to Emacs clipboard.

;;; Code:

(defvar keepass-mode-db "")
(defvar keepass-mode-password "")
(defvar keepass-mode-group-path "")

(defun keepass-mode-select ()
  "Select an entry in current Keepass key."
  (interactive)
  (let ((entry (aref (tabulated-list-get-entry) 0)))
    (if (keepass-mode-is-group-p entry)
        (progn
          (keepass-mode-update-group-path (keepass-mode-concat-group-path entry))
          (keepass-mode-open))
      (keepass-mode-show entry))))

(defun keepass-mode-back ()
  "Navigate back in group tree."
  (interactive)
  (keepass-mode-update-group-path (mapconcat #'identity (butlast (split-string keepass-mode-group-path "/" t) 1) "/"))
  (keepass-mode-open))

(defun keepass-mode-copy-password ()
  "Copy current entry password to clipboard."
  (interactive)
  (let ((entry (aref (tabulated-list-get-entry) 0)))
    (if (keepass-mode-is-group-p entry)
        (message "%s is a group, not an entry" entry)
      (progn (kill-new (keepass-mode-get-password entry))
             (message "Password for '%s%s' copied to kill-ring" keepass-mode-group-path entry)))))

(defun keepass-mode-open ()
  "Open a Keepass file at GROUP."
  (let ((columns [("Key" 100)])
        (rows (mapcar (lambda (x) `(nil [,x]))
                      (keepass-mode-get-entries keepass-mode-group-path))))
    (setq tabulated-list-format columns)
    (setq tabulated-list-entries rows)
    (tabulated-list-init-header)
    (tabulated-list-print)))

(defun keepass-mode-ask-password ()
  "Ask the user for the password."
  (read-passwd (format "Password for %s: " keepass-mode-db)))

(defun keepass-mode-show (group)
  "Show a Keepass entry at GROUP."
  (let* ((entry (keepass-mode-concat-group-path group))
        (output (replace-regexp-in-string "Password: .+" "Password: *************" (keepass-mode-get-entry entry))))
    (switch-to-buffer (format "*keepass %s %s*" keepass-mode-db entry))
    (insert output)
    (read-only-mode)))

(defvar keepass-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'keepass-mode-select)
    (define-key map (kbd "<backspace>") 'keepass-mode-back)
    (define-key map (kbd "c") 'keepass-mode-copy-password)
   map))

(define-derived-mode keepass-mode tabulated-list-mode "KeePass mode"
  "KeePass mode."
  (setq keepass-mode-db buffer-file-truename)
  (setq keepass-mode-password (keepass-mode-ask-password))
  (setq keepass-mode-group-path "")
  (kill-buffer)
  (switch-to-buffer (format "*KeePass %s*" keepass-mode-db))
  (use-local-map keepass-mode-map)
  (make-local-variable 'keepass-mode-db)
  (make-local-variable 'keepass-mode-password)
  (make-local-variable 'keepass-mode-group-path)
  (keepass-mode-open))

(add-to-list 'auto-mode-alist '("\\.kdbx\\'" . keepass-mode))
(add-to-list 'auto-mode-alist '("\\.kdb\\'" . keepass-mode))

(defun keepass-mode-get-password (entry)
  "Retrieve password for ENTRY."
  (keepass-mode-get-field "Password" (shell-command-to-string (keepass-mode-command (keepass-mode-quote-unless-empty entry) "show"))))

(defun keepass-mode-get-entries (group)
  "Get entry list for GROUP."
  (nbutlast (split-string (shell-command-to-string (keepass-mode-command (keepass-mode-quote-unless-empty group) "ls")) "\n") 1))

(defun keepass-mode-concat-group-path (group)
  "Concat GROUP and group path."
  (format "%s%s" keepass-mode-group-path (or group "")))

(defun keepass-mode-update-group-path (group)
  "Update group-path with GROUP."
  (setq keepass-mode-group-path group))

(defun keepass-mode-get-entry (entry)
  "Get ENTRY details."
  (shell-command-to-string (keepass-mode-command (keepass-mode-quote-unless-empty entry) "show")))

(defun keepass-mode-get-field (field entry)
  "Get FIELD from an ENTRY."
  (keepass-mode-get-value-from-alist field (keepass-mode-read-data-from-string entry)))

(defun keepass-mode-command (group command)
  "Generate KeePass COMMAND to run, on GROUP."
  (format "echo %s | \
           keepassxc-cli %s %s %s | \
           grep -v 'Insert password to unlock'"
          keepass-mode-password
          command
          keepass-mode-db
          group))

(defun keepass-mode-quote-unless-empty (text)
  "Quote TEXT unless it's empty."
  (if (= (length text) 0) text (format "'%s'" text)))

(defun keepass-mode-get-value-from-alist (key alist)
  "Get the value for KEY from the ALIST."
  (car (cdr (assoc key alist))))

(defun keepass-mode-read-data-from-string (input)
  "Read data from INPUT string into an alist."
  (mapcar
    (lambda (arg) (split-string arg ":" nil " "))
    (split-string input "\n")))

(defun keepass-mode-is-group-p (entry)
  "Return if ENTRY is a group."
  (string-suffix-p "/" entry))

(provide 'keepass-mode)

;;; keepass-mode.el ends here
