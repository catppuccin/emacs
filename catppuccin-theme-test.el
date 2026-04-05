;;; catppuccin-theme.test.el --- tests for the Catppuccin theme -*- lexical-binding: t; no-byte-compile: t; -*-

;; Copyright 2022-present Catppuccin, All rights reserved
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be included
;; in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;; Maintainer: Jeremy Baxter <jeremy@baxters.nz>
;; Author: nyxkrage
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.1"))
;; URL: https://github.com/catppuccin/emacs

;;; Commentary:

;; 🍄 Soothing pastel tests for The Catppuccin Emacs Theme

;;; Code:
(defmacro assert (form message)
  `(unless ,form
     (error "Assertion failed: %s\n%s" (format "%s" ',form) ,message)))
(advice-add 'display-color-cells :override (lambda (&rest r) 16777216))

(add-to-list 'custom-theme-load-path (file-name-directory load-file-name))
(load-theme 'catppuccin t) ; load theme functions
(setq catppuccin-flavor 'mocha)
(catppuccin-set-color 'base "#000000")
(catppuccin-reload)

(assert (string-equal "#000000" (catppuccin-color 'base))
  "Base does not match custom specified")
(setq catppuccin-flavor 'frappe)
(assert (string-equal "#000000" (catppuccin-color 'base 'mocha))
  "Base does not match custom specified for other flavor")
(setq catppuccin-flavor 'mocha)
(assert (string-equal "#000000" (catppuccin-color 'base))
  "Base does not match custom specified after switching back")

;;; catppuccin-theme.test.el ends here
