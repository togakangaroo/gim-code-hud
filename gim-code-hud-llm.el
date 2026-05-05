;;; -*- lexical-binding: t -*-
;;; gim-code-hud-llm.el --- Claude CLI integration and caching for gim-code-hud

(require 'cl-lib)
(require 'async)
(require 'gim-code-hud-git)

;;; Configuration

(defcustom gim-code-hud-cli "claude"
  "Path to the Claude CLI executable."
  :type 'string
  :group 'gim-code-hud)

(defcustom gim-code-hud-model nil
  "Claude model to pass via --model, or nil to use the CLI default."
  :type '(choice (const nil) string)
  :group 'gim-code-hud)

;;; Cache

(defvar gim-code-hud--cache (make-hash-table :test #'equal)
  "In-memory cache: key -> (value . timestamp).")

(defun gim-code-hud--cache-get (key max-age)
  "Return cached value for KEY if younger than MAX-AGE seconds, else nil."
  (when-let ((entry (gethash key gim-code-hud--cache)))
    (when (< (- (float-time) (cdr entry)) max-age)
      (car entry))))

(defun gim-code-hud--cache-put (key value)
  "Store VALUE under KEY with the current timestamp."
  (puthash key (cons value (float-time)) gim-code-hud--cache))

;;; Cache key helpers

(defun gim-code-hud--purpose-key (file)
  "Cache key for purpose summary: file path + mtime hour."
  (format "purpose:%s:%s"
          file
          (format-time-string "%Y-%m-%dT%H" (nth 5 (file-attributes file)))))

(defun gim-code-hud--history-key (file)
  "Cache key for history summary: file path + mtime date."
  (format "history:%s:%s"
          file
          (format-time-string "%Y-%m-%d" (nth 5 (file-attributes file)))))

;;; Claude CLI call

(defun gim-code-hud--call-claude (prompt callback)
  "Send PROMPT to the Claude CLI and call CALLBACK with the response text.
Uses `gim-code-hud-cli' (`claude -p') as a subprocess; non-blocking."
  (let ((args (append
               (when gim-code-hud-model (list "--model" gim-code-hud-model))
               (list "-p" prompt))))
    (apply #'async-start-process
           "gim-code-hud-claude" gim-code-hud-cli
           (lambda (proc)
             (let ((text (with-current-buffer (process-buffer proc)
                           (string-trim (buffer-string)))))
               (kill-buffer (process-buffer proc))
               (funcall callback text)))
           args)))

;;; Public API

;;;###autoload
(defun gim-code-hud/get-purpose (file callback)
  "Call CALLBACK with a ≤3-sentence purpose summary for FILE (cached hourly)."
  (let ((key (gim-code-hud--purpose-key file)))
    (if-let ((cached (gim-code-hud--cache-get key 3600)))
        (funcall callback cached)
      (let ((prompt (format "In 3 sentences or fewer, describe what this file does.\n\n%s"
                            (with-temp-buffer
                              (insert-file-contents file)
                              (buffer-string)))))
        (gim-code-hud--call-claude
         prompt
         (lambda (text)
           (gim-code-hud--cache-put key text)
           (funcall callback text)))))))

;;;###autoload
(defun gim-code-hud/get-history (file callback)
  "Call CALLBACK with a ≤5-sentence history narrative for FILE (cached daily)."
  (let ((key (gim-code-hud--history-key file)))
    (if-let ((cached (gim-code-hud--cache-get key 86400)))
        (funcall callback cached)
      (gim-code-hud--git-async
       (file-name-directory file)
       (list "log" "--follow" "--oneline" (file-name-nondirectory file))
       (lambda (log)
         (let ((prompt (format "In 5 sentences or fewer, narrate how this file evolved.\n\nGit log:\n%s" log)))
           (gim-code-hud--call-claude
            prompt
            (lambda (text)
              (gim-code-hud--cache-put key text)
              (funcall callback text)))))))))

;;;###autoload
(defun gim-code-hud/clear-cache ()
  "Clear the in-memory LLM summary cache."
  (interactive)
  (clrhash gim-code-hud--cache)
  (message "gim-code-hud: cache cleared"))

(provide 'gim-code-hud-llm)
;;; gim-code-hud-llm.el ends here
