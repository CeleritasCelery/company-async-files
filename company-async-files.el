;;; company-async-files.el --- company backend for files  -*- lexical-binding: t; -*-

(require 'company)
(require 'dash)
(require 's)
(require 'cl-lib)

(defvar company-async-files-enabled-modes '(shell-mode eshell-mode) "enabled modes.")
(defvar company-async-files--cand-dir nil)

(defun company-async-files--get-path ()
  (--when-let (-some->> (point)
                        (buffer-substring-no-properties (line-beginning-position))
                        (s-match (rx (+ (any alnum "~/${}._-")) eos))
                        (car)
                        (s-replace "~" "$HOME")
                        (substitute-env-vars)
                        (s-split "/")
                        (-rotate 1))
    (-let* (((prefix . dirs) it)
            (dir-name (s-join "/" dirs)))
      (cons dir-name prefix))))

(defun company-async-files--prefix ()
  (when (-contains? company-async-files-enabled-modes major-mode)
    (-let [(dir . prefix) (company-async-files--get-path)]
      (when (and dir
                 (f-directory? dir)
                 (looking-back prefix (length prefix))
                 (< 0 (->> (format "find %s -maxdepth 1 -name '%s*' 2>/dev/null | wc -l" (f-full dir) prefix)
                           (shell-command-to-string)
                           (string-to-number))))
        (when (and company-async-files--cand-dir
                   (f-same? company-async-files--cand-dir (f-dirname dir)))
          (setq prefix (concat (f-filename dir) "/" prefix)))
        (cons prefix (+ (length dir) (length prefix)))))))

(defun company-async-files--candidates (callback)
  (-let (((dir . prefix) (company-async-files--get-path))
         (buffer (generate-new-buffer "file-candiates")))
    (setq company-async-files--cand-dir dir)
    (setq dir (f-full dir))
    (set-process-sentinel (start-process-shell-command
                           "file-candiates"
                           buffer
                           (s-lex-format "cd ${dir} && find -L ${prefix}* -maxdepth 1 -printf '%p\t%y\n' 2>/dev/null" ))
                          (lambda (_ event)
                            (when (s-equals? event "finished\n")
                              (funcall callback (company-async-files--parse buffer)))))))

(defun company-async-files--parse (buffer)
  (prog1
      (--map (-let [(file type) (s-split "\t" it)]
               (if (s-equals? type "d")
                   (concat file "/")
                 file))
             (s-lines
              (s-trim (with-current-buffer buffer
                        (buffer-string)))))
    (kill-buffer buffer)))

(defun company-async-files--post (cand)
  (when (s-suffix? "/" cand)
    (delete-char -1))
  (setq company-async-files--cand-dir nil))

;;;###autoload
(defun company-async-files (command &optional arg &rest ignored)
  "Complete shell commands and options using Fish shell. See `company's COMMAND ARG and IGNORED for details."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-async-files))
    (prefix (company-async-files--prefix))
    (candidates (cons :async (lambda (callback) (company-async-files--candidates callback))))
    (post-completion (company-async-files--post arg))))

(defun company-async-files--clear-dir (_)
  (setq company-async-files--cand-dir nil))

(add-hook 'company-completion-cancelled-hook 'company-async-files--clear-dir)

(provide 'company-async-files)
