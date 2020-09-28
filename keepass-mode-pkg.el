;;; keepass-mode-pkg.el --- Mode for KeePass DB  -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2020  Ignasi Fosch

;; Author: Ignasi Fosch <natx@y10k.ws>
;; Keywords: keepass password tools
;; Version: 0.0.3
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

(define-package "keepass-mode" "0.0.3"
  "Mode for KeePass DB."
  '((emacs "27"))
  :keywords
  '("keepass" "password" "tools"))

(provide 'keepass-mode-pkg)

;;; keepass-mode-pkg.el ends here
