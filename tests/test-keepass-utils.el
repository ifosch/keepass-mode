;;; test-keepass-utils.el -- Tests for keepass-utils.el -*-lexical-binding:t; coding: utf-8 -*-

;; Copyright (C) 2019  Ignasi Fosch

;; Author: Ignasi Fosch <natx@y10k.ws>
;; Keywords: keepass passwod tools
;; Version: 0.0.1
;; Homepage: https://github.com/ifosch/keepass-mode

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

;;; Code:

(require 'keepass-utils)

(setq keepass-password "test")
(setq keepass-db "tests/fixtures/test.kdbx")

(describe "kpu--read-data-from-string"
          (xit "requires basic functionality testing")
          )

(describe "kpu--get-value-from-a-list"
          (xit "requires basic functionality testing")
          )

(describe "kpu--get-field"
          (xit "requires basic functionality testing")
          )

(describe "kpu--command"
          (xit "requires basic functionality testing")
          )

(describe "kpu--quote-unless-empty"
          (xit "requires basic functionality testing")
          )

(describe "kpu--is-group"
          (xit "requires basic functionality testing")
          )

;;; test-keepass-utils.el ends here
