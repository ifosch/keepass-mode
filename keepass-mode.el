;;; keepass-mode.el --- Mode to open Keepass DB  -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2020  Ignasi Fosch

;; Author: Ignasi Fosch <natx@y10k.ws>
;; Keywords: data files tools
;; Version: 0.0.4
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

(defgroup keepass nil
  "KeePass/KeePassXC integration with Emacs."
  :group 'convenience
  :tag "keepass-mode"
  :prefix "keepass-mode-")

(defcustom keepass-mode-ls-recursive t
  "Should list entries recursively?
If nil, it will only show the first level of entries and folders.

Tip: Recursive list may be useful when searching for a key in a buffer with
\\[isearch-forward] (command `isearch-forward').  However, it may be too slow with a
large KeePass database file."
  :type 'boolean
  :group 'keepass)

(defvar-local keepass-mode-db "")
(defvar-local keepass-mode-password "")
(defvar-local keepass-mode-group-path "")

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
  (keepass-mode-update-group-path (replace-regexp-in-string "[^/]*/?$" "" keepass-mode-group-path))
  (keepass-mode-open))

(defun keepass-mode-copy (field)
  "Copy current entry FIELD to clipboard."
  (let ((entry (aref (tabulated-list-get-entry) 0)))
    (if (keepass-mode-is-group-p entry)
        (message "%s is a group, not an entry" entry)
      (progn (kill-new (keepass-mode-get field entry))
             (message "%s for '%s%s' copied to kill-ring" field keepass-mode-group-path entry)))))

(defun keepass-mode-copy-url ()
  "Copy current entry URL to clipboard."
  (interactive)
  (keepass-mode-copy "URL"))

(defun keepass-mode-copy-username ()
  "Copy current entry username to clipboard."
  (interactive)
  (keepass-mode-copy "UserName"))

(defun keepass-mode-copy-password ()
  "Copy current entry password to clipboard."
  (interactive)
  (keepass-mode-copy "Password"))

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
    (define-key map (kbd "u") 'keepass-mode-copy-url)
    (define-key map (kbd "b") 'keepass-mode-copy-username)
    (define-key map (kbd "c") 'keepass-mode-copy-password)
   map))

(defface keepass-mode-font-lock-directory
  '((t (:inherit dired-directory)))
  "Face used for directory entries."
  :group 'keepass)

(defconst keepass-mode-font-lock-keywords
  '(("^.*/" (0 'keepass-mode-font-lock-directory t)))
  "Font-lock keywords for `keepass-mode'.")

;;;###autoload
(define-derived-mode keepass-mode tabulated-list-mode "KeePass"
  "KeePass mode for interacting with the KeePass DB.
\\{keepass-mode-map}."
  (setq-local font-lock-defaults '(keepass-mode-font-lock-keywords nil t))
  (setq-local keepass-mode-db buffer-file-truename)
  (when (zerop (length keepass-mode-password))
    (setq-local keepass-mode-password (keepass-mode-ask-password)))
  (setq-local keepass-mode-group-path "")
  (keepass-mode-open))

(add-to-list 'auto-mode-alist '("\\.kdbx\\'" . keepass-mode))
(add-to-list 'auto-mode-alist '("\\.kdb\\'" . keepass-mode))

(defconst keepass-mode-output-buffer "*keepass-mode-command-output*"
  "Buffer name used for the keepassxc-cli command output.")

(defun keepass-mode-call-command (group command)
  "Call the keepassxc-cli command and return its output.
GROUP and COMMAND are passed to `keepass-mode-command'.  They are strings with
the group to process (the directory) and the keepass command (for example:
\"ls\", \"show\")."
  (let ((password (shell-quote-argument keepass-mode-password))
        (filepath keepass-mode-db))
    (with-current-buffer (get-buffer-create keepass-mode-output-buffer)
      (delete-region (point-min) (point-max)))
    (with-temp-buffer
      (insert password)
      (call-shell-region (point-min) (point-max)
                         (keepass-mode-command group command filepath)
                         t keepass-mode-output-buffer))
    (with-current-buffer keepass-mode-output-buffer
      (buffer-string))))

(defun keepass-mode-get (field entry)
  "Retrieve FIELD from ENTRY."
  (keepass-mode-get-field field (keepass-mode-call-command (keepass-mode-quote-unless-empty entry) "show -s")))

(defun keepass-mode-get-entries (group)
  "Get entry list for GROUP."
  (nbutlast (split-string (keepass-mode-call-command (keepass-mode-quote-unless-empty group)
                                                     (if keepass-mode-ls-recursive
                                                         "ls -R -f"
                                                       "ls")) "\n") 1))

(defun keepass-mode-concat-group-path (group)
  "Concat GROUP and group path."
  (format "%s%s" keepass-mode-group-path (or group "")))

(defun keepass-mode-update-group-path (group)
  "Update group-path with GROUP."
  (setq keepass-mode-group-path group))

(defun keepass-mode-get-entry (entry)
  "Get ENTRY details."
  (keepass-mode-call-command (keepass-mode-quote-unless-empty entry) "show"))

(defun keepass-mode-get-field (field entry)
  "Get FIELD from an ENTRY."
  (keepass-mode-get-value-from-alist field (keepass-mode-read-data-from-string entry)))

(defun keepass-mode-command (group command &optional db)
  "Generate KeePass COMMAND to run, on GROUP."
  (format "keepassxc-cli %s %s %s 2>&1 | \
           grep -E -v '[Insert|Enter] password to unlock %s'"
          command
          (or db keepass-mode-db)
          group
          (or db keepass-mode-db)))

(defun keepass-mode-quote-unless-empty (text)
  "Quote TEXT unless it's empty."
  (if (= (length text) 0) text (format "'%s'" text)))

(defun keepass-mode-get-value-from-alist (key alist)
  "Get the value for KEY from the ALIST."
  (mapconcat 'identity (cdr (assoc key alist)) ":"))

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
