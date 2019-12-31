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

(defvar keepass-db "")
(defvar keepass-password "")
(defvar keepass-group-path "")

(defvar keepass-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'keepass-select)
    (define-key map (kbd "<backspace>") 'keepass-group-back)
    (define-key map (kbd "c") 'keepass-copy-password)
   map))

(defun keepass-group-back ()
  "Navigate back in group tree."
  (interactive)
  (setq keepass-group-path (mapconcat 'identity (butlast (split-string keepass-group-path "/" t) 1) "/"))
  (keepass-open nil))

(defun keepass-select ()
  "Select an entry in current Keepass key."
  (interactive)
  (let ((entry (aref (tabulated-list-get-entry) 0)))
    (if (string-suffix-p "/" entry)
      (setq keepass-group-path (keepass-open entry))
      (keepass-show entry))))

(defun keepass-copy-password ()
  "Copy current entry password to clipboard."
  (interactive)
  (let ((entry (aref (tabulated-list-get-entry) 0)))
    (if (string-suffix-p "/" entry)
      (message (format "%s is a group, not an entry" entry))
      (progn (kill-new (keepass-get-field "Password" (shell-command-to-string (keepass-command (format "'%s'" entry) "show"))))
             (message (format "Password for '%s%s' copied to kill-ring" keepass-group-path entry))))))

(defun keepass-ask-password ()
  "Ask the user for the password."
  (read-passwd (format "Password for %s: " keepass-db)))

(defun keepass-read-data-from-string (input)
  "Read data from INPUT string into an alist."
  (mapcar
    (lambda (arg) (split-string arg ":" nil " "))
    (split-string input "\n")))

(defun keepass-get-value-from-alist (key alist)
  "Get the value for KEY from the ALIST."
  (car (cdr (assoc key alist))))

(defun keepass-get-field (field entry)
  "Get FIELD from an ENTRY."
  (keepass-get-value-from-alist field (keepass-read-data-from-string entry)))

(defun keepass-command (group command)
  "Generate keepass COMMAND to run, on GROUP."
  (format "echo %s | \
           keepassxc-cli %s %s %s | \
           grep -v 'Insert password to unlock'"
          keepass-password
          command
          keepass-db
          group))

(defun keepass-open (group)
  "Open a Keepass file at GROUP."
  (let ((columns [("Key" 100)])
        (rows (mapcar (lambda (x) `(nil [,x]))
              (split-string (shell-command-to-string (keepass-command
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
        (output (shell-command-to-string (keepass-command (format "'%s'" entry) "show"))))
    (switch-to-buffer (format "*keepass %s %s*" keepass-db entry))
    (insert (replace-regexp-in-string "Password: .+" "Password: ************" output))
    (read-only-mode)))

;;;###autoload
(define-derived-mode keepass-mode tabulated-list-mode "KeePass mode"
  "KeePass mode."
  (setq keepass-db buffer-file-truename)
  (setq keepass-password (keepass-ask-password))
  (setq keepass-group-path "")
  (kill-buffer)
  (switch-to-buffer (format "*KeePass %s*" keepass-db))
  (use-local-map keepass-mode-map)
  (make-local-variable 'keepass-db)
  (make-local-variable 'keepass-password)
  (make-local-variable 'keepass-group-path)
  (setq keepass-group-path (keepass-open nil)))

(add-to-list 'auto-mode-alist '("\\.kdbx\\'" . keepass-mode))
(add-to-list 'auto-mode-alist '("\\.kdb\\'" . keepass-mode))

(provide 'keepass-mode)

;;; keepass-mode.el ends here
