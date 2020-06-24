;;; comby.el --- Emacs comby integration -*- lexical-binding: t -*-

;; Copyright (C) 2020 Sergey Kostyaev, all rights reserved.

;; Author: Sergey Kostyaev <feo.me@ya.ru>
;; Keywords: languages
;; Package-Version: 20171023.358
;; Url: https://github.com/s-kostyaev/comby.el
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))

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

;; This package is simple wrapper for https://comby.dev/
;; You should install `comby' for use it.

;;; Code:
(require 'cl-lib)
(require 'ansi-color)
(require 'project)

(defgroup comby nil
  "Comby is a tool for searching and changing code structure."
  :group 'tools)

(defcustom comby-binary "comby"
  "Path to comby binary."
  :type 'string
  :group 'comby)

(defcustom comby-args nil
  "Additional arguments for `comby'."
  :type '(repeat (choice string))
  :group 'comby)

(defcustom comby-show-changes t
  "Show changes before write."
  :type 'boolean
  :group 'comby)

(defvar comby-header
  "Press `C-c C-c' to apply changes\nPress `q' to discard changes.\n\n")

(defun comby-create-command (match-template rewrite-template &optional full-file-paths-or-file-suffixes &rest flags)
  "Create comby command for rewriting MATCH-TEMPLATE by REWRITE-TEMPLATE inside FULL-FILE-PATHS-OR-FILE-SUFFIXES with additional FLAGS."
  (comby--flatten-tree
   (cl-remove-if
    (lambda (el) (not el))
    (list comby-binary match-template rewrite-template comby-args flags full-file-paths-or-file-suffixes))))

(defun comby-run (match-template rewrite-template &optional full-file-paths-or-file-suffixes changed-file-func &rest flags)
  "Run comby command for rewriting MATCH-TEMPLATE by REWRITE-TEMPLATE.
Inside FULL-FILE-PATHS-OR-FILE-SUFFIXES with additional FLAGS.  On every changed
file will be executed CHANGED-FILE-FUNC.  It is function with single argument -
changed file path."
  (let* ((cur-buf (buffer-name))
	 (buf (generate-new-buffer "*comby*"))
	 (cmd (comby--flatten-tree (list "comby" buf (comby-create-command match-template rewrite-template full-file-paths-or-file-suffixes flags))))
	 (apply-cmd (append cmd '("-in-place")))
	 (diff-buf (generate-new-buffer "*comby-diff*"))
	 (diff-cmd (comby--flatten-tree (list "comby" diff-buf (comby-create-command match-template rewrite-template full-file-paths-or-file-suffixes flags) "-diff"))))
    (if (not comby-show-changes)
	(if changed-file-func
	    (set-process-sentinel
	     (apply #'start-process diff-cmd)
	     (lambda (_1 _2)
	       (let ((changed-files (with-current-buffer diff-buf
				      (comby--extract-changed-files-list))))
		 (set-process-sentinel
		  (apply #'start-process apply-cmd)
		  (lambda (_1 _2)
		    (dolist (file changed-files)
		      (funcall changed-file-func file))
		    (kill-buffer buf)
		    (kill-buffer diff-buf)
		    (with-current-buffer cur-buf
		      (revert-buffer nil t)))))))
	  (set-process-sentinel
	   (apply #'start-process apply-cmd)
	   (lambda (_1 _2)
	     (kill-buffer buf)
	     (with-current-buffer cur-buf
	       (revert-buffer nil t)))))
      (kill-buffer diff-buf)
      (let* ((proc (apply #'start-process cmd)))
	(set-process-sentinel proc
			      (lambda (_arg1 _arg2)
				(with-current-buffer buf
				  (ansi-color-apply-on-region (point-min) (point-max))
				  (goto-char (point-min))
				  (insert comby-header)
				  (read-only-mode 1)
				  (local-set-key (kbd "q") (lambda ()
							     (interactive) (kill-buffer buf)))
				  (local-set-key (kbd "C-c C-c")
						 (lambda ()
						   (interactive)
						   (set-process-sentinel
						    (apply #'start-process apply-cmd)
						    (lambda (_1 _2)
						      (if changed-file-func
							  (dolist (file (comby--extract-changed-files-list))
							    (funcall changed-file-func file)))
						      (kill-buffer buf)
						      (with-current-buffer cur-buf
							(revert-buffer nil t)))))))
				(switch-to-buffer buf)))))))


(defun comby--extract-changed-files-list ()
  "Extract changed file list from result buffer.
For internal usage."
  (save-excursion
    (save-match-data
      (let ((res nil))
	(goto-char (point-min))
	(while
	    (re-search-forward
	     (rx line-start (or (+ "-") (+ "+")) " " (group (+ not-newline)))
	     nil t)
	  (push (match-string-no-properties 1) res))
	(cl-remove-duplicates res :test 'string-equal)))))

(defun comby--flatten-tree (tree)
  "Return a \"flattened\" copy of TREE.
In other words, return a list of the non-nil terminal nodes, or
leaves, of the tree of cons cells rooted at TREE.  Leaves in the
returned list are in the same order as in TREE.  This code is
is backported `flatten-tree' from GNU Emacs 27.1.

\(comby--flatten-tree \\='(1 (2 . 3) nil (4 5 (6)) 7))
=> (1 2 3 4 5 6 7)"
  (let (elems)
    (while (consp tree)
      (let ((elem (pop tree)))
        (while (consp elem)
          (push (cdr elem) tree)
          (setq elem (car elem)))
        (if elem (push elem elems))))
    (if tree (push tree elems))
    (nreverse elems)))

;;;###autoload
(defun comby (&optional beg end)
  "Refactor your code by `comby'.
If you have active selection between BEG & END, it will be initial input for match template."
  (interactive "r")
  (let* ((project (project-current))
	 (project-root (if project (car (project-roots project))))
	 (default-directory (if project-root
				project-root
			      default-directory))
	 (sel (if (region-active-p)
		  (buffer-substring-no-properties beg end)))
	 (match-template (read-string "match template: "
				      sel))
	 (rewrite-template (read-string "rewrite template: "
					match-template))
	 (file-suffixes (concat "." (file-name-extension (buffer-file-name)))))
    (save-buffer)
    (comby-run match-template rewrite-template file-suffixes)))

(provide 'comby)
;;; comby.el ends here
