;;; catppuccin-modus.el --- Catppuccin for Emacs - 🍄 Soothing pastel theme for Emacs -*- lexical-binding: t; -*-

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
;; Original-Author: film42
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1"))
;; URL: https://github.com/catppuccin/emacs

;;; Commentary:

;; 🍄 Soothing pastel theme for Emacs
;;
;; catppuccin-modus.el provides the theme `catppuccin', a port of the
;; Catppuccin colors to Emacs.
;;
;; For more information visit <https://catppuccin.com>.

;;; Code:

(eval-when-compile (require 'subr-x))

(eval-and-compile
  (unless (require 'modus-themes nil :noerror)
    ;; Fall back to the copy bundled with Emacs (etc/themes is not in
    ;; `load-path', so a plain `require' cannot find it).
    (require-theme 'modus-themes)))

(require 'catppuccin-latte-theme)
(require 'catppuccin-frappe-theme)
(require 'catppuccin-macchiato-theme)
(require 'catppuccin-mocha-theme)

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

;; Local Variables:
;; indent-tabs-mode: nil
;; lisp-indent-offset: 2
;; End:

(provide 'catppuccin-modus)
;;; catppuccin-modus.el ends here
