;;; indent-lisp-files.el --- call `indent-region' over all Lisp files -*- lexical-binding: t; -*-

(require 'bytecomp)
(require 'seq)
(when (version< emacs-version "29.1")
  (require 'compat)) ; file-name-parent-directory

(defun .el-files-in (directory)
  (mapcar #'(lambda (item) (concat directory item))
    (seq-filter #'(lambda (path)
                    (and (not (string-suffix-p "-definitions.el" path))
                      (string-match "^.+\\.el$" path)))
      (directory-files directory))))

(let*
  ((root-path (file-name-parent-directory
                (file-name-parent-directory load-file-name)))
    (files (.el-files-in root-path))
    (scripts (.el-files-in (concat root-path "/scripts/"))))
  (dolist (file (append files scripts))
    (setq
      byte-compile-error-on-warn t
      lisp-indent-offset 2)
    (unless (byte-compile-file file)
      (error "byte-compile-file failed (%s)" file))
    (with-current-buffer (find-file-noselect file)
      (indent-region (point-min) (point-max))
      (save-buffer)
      (kill-buffer (current-buffer)))))
