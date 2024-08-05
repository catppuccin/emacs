;;; format-definitions.el --- format catppuccin-definitions.el -*- lexical-binding: t; -*-

(when (version< emacs-version "29.1")
  (require 'compat)) ; file-name-parent-directory

(defvar script-path (file-name-parent-directory load-file-name))

(let*
  ((definitions-file (concat script-path "../catppuccin-definitions.el"))
    (buffer (find-file-noselect definitions-file)))
  (with-current-buffer buffer
    (setq indent-tabs-mode nil)
    (beginning-of-buffer)
    (replace-string ")(" ")\n(")
    (indent-region (point-min) (point-max))
    (replace-string "\t" "  ")
    (indent-region (point-min) (point-max))
    (save-buffer)))
