;;; company-async-files.el --- company backend for files  -*- lexical-binding: t; -*-

(require 'company)
(require 'dash)
(require 's)
(require 'f)
(require 'cl-lib)

(defvar company-async-files--cand-dir nil)

(defun company-async-files--get-path ()
  "Get the current path at point.
Returns a cons cell with directory in `car'
and prefix in `cdr'"
  (--when-let (-some->> (point)
                        (buffer-substring (line-beginning-position))
                        (s-match (rx (+ (any alnum "~/${}._-" "'\"")) eos))
                        (car)
                        (s-replace "~" "$HOME")
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
               (looking-back (rx symbol-end) (1- (point)))
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

(defvar company-async-files-depth-search-timeout 0.5
  "amount of time in seconds to wait before cancelling the depth search")

(defun company-async-files--candidates (callback)
  "Get all files and directories at point.
By deafult `company-async-files--candidates' get all candidates in the current
directory and all subdirectories. If this takes longer then
`company-async-files-depth-search-timeout' it will only supply candiates in the
current directory."
  (-let (((dir . prefix) (company-async-files--get-path))
         (buffer-1 (generate-new-buffer "file-candiates-1"))
         (buffer-2 (generate-new-buffer "file-candiates-2"))
         ((finished? timeout? respond) (-repeat 3 nil)))
    (setq company-async-files--cand-dir dir)
    (setq dir (f-full dir))
    (setq respond (lambda (buf)
                    (if finished?
                        (kill-buffer buf)
                      (funcall callback (company-async-files--parse buf))
                      (setq finished? t))))
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
  "read the result of GNU find.
The results are of the form
candiate type"
  (prog1
      (--map (-let [(file type) (s-split "\t" it)]
               (if (string-equal type "d")
                   (concat file (f-path-separator))
                 file))
             (s-lines
              (s-trim (with-current-buffer buffer
                        (buffer-string)))))
    (kill-buffer buffer)))

(defun company-async-files--post (cand)
  "remove the trailing `f-path-separator'"
  (when (s-suffix? (f-path-separator) cand)
    (delete-char -1))
  (setq company-async-files--cand-dir nil))

;;;###autoload
(defun company-async-files (command &optional arg &rest ignored)
  "Complete shell commands and options using Fish shell. See `company's COMMAND ARG and IGNORED for details."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-async-files))
    (prefix (company-async-files--prefix))
    (candidates
     (cons :async (lambda (callback) (company-async-files--candidates callback)))
     )
    (post-completion (company-async-files--post arg))))

(defun company-async-files--clear-dir (_)
  (setq company-async-files--cand-dir nil))

(add-hook 'company-completion-cancelled-hook 'company-async-files--clear-dir)

(provide 'company-async-files)
