;;; test-keepass-mode.el -- Tests for keepass-mode.el -*-lexical-binding:t; coding: utf-8 -*-

;; Copyright (C) 2019  Ignasi Fosch

;; Author: Ignasi Fosch <natx@y10k.ws>
;; Keywords: data files tools
;; Version: 0.0.4
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

(setq-default keepass-mode-password "test")
(setq-default keepass-mode-db "tests/fixtures/test.kdbx")

(describe "keepass-mode-get-URL"
          (it "returns the URL for an entry"
              (expect (keepass-mode-get "URL" "Internet/Some site")
                      :to-equal
                      "https://somesite.com")))

(describe "keepass-mode-get-username"
          (it "returns the URL for an entry"
              (expect (keepass-mode-get "UserName" "Internet/Some site")
                      :to-equal
                      "username")))

(describe "keepass-mode-get-password"
          (it "returns the password for an entry"
              (expect (keepass-mode-get "Password" "Internet/Some site")
                      :to-equal
                      "s0m3s1t3"))
          (it "returns the complete password with :"
              (expect (keepass-mode-get "Password" "Internet/Password with :")
                      :to-equal
                      "part1:part2")))

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

(describe "keepass-mode-back in a sub-group"
          (it "returns the parent-group"
              (find-file keepass-mode-db)
              (search-forward-regexp "^SSH Keys")
              (keepass-mode-select)
              (search-forward-regexp "^Servers")
              (keepass-mode-select)
              (keepass-mode-back)
              (expect keepass-mode-group-path
                      :to-equal
                      "SSH Keys/")))

;;; test-keepass-mode.el ends here
