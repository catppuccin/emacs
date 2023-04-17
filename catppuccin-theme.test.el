;;; catppuccin-theme.test.el --- Tests for the catppuccin-theme -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Carsten Kragelund
;;
;; Author: Carsten Kragelund <carsten@kragelund.me>
;; Maintainer: Carsten Kragelund <carsten@kragelund.me>
;; Created: February 09, 2023
;; Modified: February 09, 2023
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/carsten/catppuccin-tests
;; Package-Requires: ((emacs "25.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
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

;;; catppuccin-tests.el ends here
