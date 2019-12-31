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
    (keepass-select entry)))

(defun kpm-back ()
  "Navigate back in group tree."
  (interactive)
  (keepass-back))

(defun kpm-copy-password ()
  "Copy current entry password to clipboard."
  (interactive)
  (let ((entry (aref (tabulated-list-get-entry) 0)))
    (keepass-copy-password entry)))

(defvar keepass-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'kpm-select)
    (define-key map (kbd "<backspace>") 'kpm-back)
    (define-key map (kbd "c") 'kpm-copy-password)
   map))

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
