;;; catppuccin-theme.test.el --- Tests for The Catppuccin Emacs Theme -*- lexical-binding: t; no-byte-compile: t; -*-

;; Copyright 2022-present, All rights reserved
;;
;; Code licensed under the MIT license

;; Maintainer: Carsten Kragelund <carsten@kragelund.me>
;; Author: nyxkrage
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.1"))
;; URL: https://github.com/catppuccin/emacs

;;; Commentary:

;; 🍄 Soothing pastel tests for The Catppuccin Emacs Theme

;;; Code:
(defmacro assert (test-form message)
  `(when (not ,test-form)
     (error "Assertion failed: %s\n%s" (format "%s" ',test-form) ,message)))
(advice-add 'display-color-cells :override (lambda (&rest r) 16777216))

(add-to-list 'custom-theme-load-path (file-name-directory load-file-name))
(load-theme 'catppuccin t) ;; Load theme functions
(setq catppuccin-flavor 'mocha)
(catppuccin-set-color 'base "#000000")
(catppuccin-reload)

(assert (string-equal "#000000" (catppuccin-get-color 'base)) "Base does not match custom specified")
(setq catppuccin-flavor 'frappe)
(assert (string-equal "#000000" (catppuccin-get-color 'base 'mocha)) "Base does not match custom specified for other flavor")
(setq catppuccin-flavor 'mocha)
(assert (string-equal "#000000" (catppuccin-get-color 'base)) "Base does not match custom specified after switching back")

;;; catppuccin-tests.test..el ends here
