;;; company-async-files.el --- company backend for files  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 by Troy Hinckley

;; Author: Troy Hinckley <troy.hinckley@gmail.com>
;; URL: https://github.com/CeleritasCelery/company-async-files
;; Version: 0.1.0
;; Package-Requires: ((company "0.9.3") (cl-lib "0.5.0") (f "0.18.2") (dash "2.12.0") (s "1.12") (emacs "25"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; async Company backend for files.
;; =company-async-files= provides the same completion as =company-files=,
;; but asynchronously uses find in the background to get the candidates.
;; This ensures that your user thread is never blocked by your completion
;; backend, which is the way it should be.

;;; Code:

(require 'company)
(require 'dash)
(require 's)
(require 'f)
(require 'cl-lib)

;;; Customizable variables
(defgroup company-async-files nil
  "company back-end for async file completion"
  :prefix "company-async-files-"
  :group 'programming
  :link '(url-link :tag "Github" "https://github.com/CeleritasCelery/company-async-files"))

(defcustom company-async-files-depth-search-timeout 0.5
  "Amount of time in seconds to wait before cancelling the depth search."
  :type 'number)

(defvar company-async-files--cand-dir nil)

(defun company-async-files--get-path ()
  "Get the current path at point.
Returns a cons cell with directory in `car'
and prefix in `cdr'"
  (--when-let (-some->> (point)
                        (buffer-substring-no-properties (line-beginning-position))
                        (s-match (rx (+ (any alnum "~/${}._-" "'\"")) eos))
                        (car)
                        (s-replace "~" "$HOME")
                        (s-replace "$ENV" "$") ;; perl form
                        substitute-env-vars
                        (replace-regexp-in-string (rx (any "'\"")) "")
                        (s-split (f-path-separator))
                        (-rotate 1))
    (unless (equal it '(""))
      (-let* (((prefix . dirs) it)
              ;; when we are at the root need to
              ;; include the root
              (dir-name (if (equal '("") dirs)
                            (f-root)
                          (s-join (f-path-separator) dirs))))
        (cons dir-name prefix)))))

(defun company-async-files--prefix ()
  "Get the uncompleted part of the path at point."
  (-let [(dir . prefix) (company-async-files--get-path)]
    (when (and dir
               (f-directory? dir)
               (looking-back (rx (or symbol-end punctuation)) (1- (point)))
               (looking-back (regexp-quote prefix)
                             (- (point) (length prefix)))
               (->> (format "find %s -maxdepth 1 -name '%s*' 2>/dev/null | wc -l" (f-full dir) prefix)
                    shell-command-to-string
                    string-to-number
                    zerop
                    not))
      (when (and company-async-files--cand-dir
                 (f-dirname dir)
                 (f-same? company-async-files--cand-dir (f-dirname dir)))
        (setq prefix (concat (f-filename dir) (f-path-separator) prefix)))
      (cons prefix (+ (length dir) (length prefix))))))

(defun company-async-files--candidates (callback)
  "Get all files and directories at point and invoke CALLBACK.
By deafult `company-async-files--candidates' get all candidates in the current
directory and all subdirectories. If this takes longer then
`company-async-files-depth-search-timeout' it will only supply candiates in the
current directory."
  (-let (((dir . prefix) (company-async-files--get-path))
         (buffer-1 (get-buffer-create "file-candiates-1"))
         (buffer-2 (get-buffer-create "file-candiates-2"))
         ((timeout? respond)))
    (cl-loop for buffer in (list buffer-1 buffer-2)
             do (when-let ((proc (process-live-p (get-buffer-process buffer))))
                  (kill-process proc))
             (with-current-buffer buffer
               (erase-buffer)))
    (setq company-async-files--cand-dir dir)
    (setq dir (f-full dir))
    (setq respond (lambda (buf)
                    (funcall callback (company-async-files--parse buf))))
    (set-process-sentinel (start-process-shell-command
                           "file-candiates-1"
                           buffer-1
                           (s-lex-format "cd ${dir} && find -L ${prefix}* -maxdepth 0 -printf '%p\t%y\n' 2>/dev/null" ))
                          (lambda (_ event)
                            (when (string-equal event "finished\n")
                              (if timeout?
                                  (funcall respond buffer-1)
                                (setq timeout? t)))))
    (set-process-sentinel (start-process-shell-command
                           "file-candiates-2"
                           buffer-2
                           (s-lex-format "cd ${dir} && find -L ${prefix}* -maxdepth 1 -printf '%p\t%y\n' 2>/dev/null" ))
                          (lambda (_ event)
                            (when (string-equal event "finished\n")
                              (funcall respond buffer-2))))
    (run-at-time company-async-files-depth-search-timeout nil
                 (lambda ()
                   (if timeout?
                       (funcall respond buffer-1)
                     (setq timeout? t))))))

(defun company-async-files--parse (buffer)
  "Read the result of GNU find from BUFFER.
The results are of the form
candidate type"
  (--map (-let [(file type) (s-split "\t" it)]
           (if (string-equal type "d")
               (concat file (f-path-separator))
             file))
         (s-lines
          (s-trim (with-current-buffer buffer
                    (buffer-string))))))

(defun company-async-files--post (cand)
  "Remove the trailing `f-path-separator' from CAND."
  (when (s-suffix? (f-path-separator) cand)
    (delete-char -1))
  (setq company-async-files--cand-dir nil))

(defun company-async-files--meta (cand)
  "Show the system info for CAND."
  (->> (expand-file-name cand company-async-files--cand-dir)
       (format "ls --directory --human-readable -l %s")
       (shell-command-to-string)
       (replace-regexp-in-string (rx (group-n 1
                                              (repeat 8
                                                      (and (1+ (not space))
                                                           (1+ space))))
                                     (1+ (not space)))
                                 "\\1")))

;;;###autoload
(defun company-async-files (command &optional arg &rest ignored)
  "Complete file paths using find. See `company's COMMAND ARG and IGNORED for details."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-async-files))
    (prefix (company-async-files--prefix))
    (candidates (cons :async (lambda (callback) (company-async-files--candidates callback))))
    (meta (company-async-files--meta arg))
    (post-completion (company-async-files--post arg))))

(defun company-async-files--clear-dir (_)
  "Clear async files directory."
  (setq company-async-files--cand-dir nil))

(add-hook 'company-completion-cancelled-hook 'company-async-files--clear-dir)

(provide 'company-async-files)
;;; company-async-files.el ends here
