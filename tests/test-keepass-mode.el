;;; test-keepass-mode.el -- Tests for keepass-mode.el -*-lexical-binding:t; coding: utf-8 -*-

;; Copyright (C) 2019  Ignasi Fosch

;; Author: Ignasi Fosch <natx@y10k.ws>
;; Keywords: data files tools
;; Version: 0.0.1
;; Homepage: https://github.com/ifosch/keepass-mode
;; Package-Requires: ((emacs "27"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; These are the tests for the keepass.el functions.

;;; Code:

(require 'keepass-mode)

(setq keepass-mode-password "test")
(setq keepass-mode-db "tests/fixtures/test.kdbx")

(describe "keepass-mode-get-password"
          (it "returns the password for an entry"
              (expect (keepass-mode-get-password "Internet/Some site")
                      :to-equal
                      "s0m3s1t3")))

(describe "keepass-mode-get-entries"
          (it "returns the entries for the root group"
              (expect (keepass-mode-get-entries "")
                      :to-equal
                      '("Internet/" "SSH Keys/")))
          (it "returns the entries for a group with space in name"
              (expect (keepass-mode-get-entries "SSH Keys/")
                      :to-equal
                      '("username@github.com" "Servers/")))
          (it "returns the entries for a second-level group"
              (expect (keepass-mode-get-entries "SSH Keys/Servers/")
                      :to-equal
                      '("root@home"))))

(describe "keepass-mode-get-entry"
          (it "returns an entry details"
              (expect (keepass-mode-get-entry "Internet/Some site")
                      :to-equal
                      "Title: Some site\nUserName: username\nPassword: PROTECTED\nURL: https://somesite.com\nNotes: \n")))

;;; test-keepass-mode.el ends here
