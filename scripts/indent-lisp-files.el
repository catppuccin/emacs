;;; indent-lisp-files.el --- call `indent-region' over all Lisp files -*- lexical-binding: t; -*-

(require 'seq)
(when (version< emacs-version "29.1")
  (require 'compat)) ; file-name-parent-directory

(defun .el-files-in (directory)
  (seq-filter #'(lambda (path)
                  (and (not (string-suffix-p "-definitions.el" path))
                    (string-match "^.+\\.el$" path)))
    (directory-files directory)))

(let*
  ((root-path (file-name-parent-directory
                (file-name-parent-directory load-file-name)))
    (files (.el-files-in root-path))
    (scripts (.el-files-in (concat root-path "/scripts"))))
  (dolist (file (append files scripts))
    (with-current-buffer (find-file-noselect file)
      (setq indent-tabs-mode nil
        lisp-indent-offset 2)
      (indent-region (point-min) (point-max))
      (save-buffer)
      (kill-buffer (current-buffer)))))
