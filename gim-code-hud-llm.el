;;; -*- lexical-binding: t -*-
;;; gim-code-hud-llm.el --- Claude API integration and caching for gim-code-hud

(require 'cl-lib)
(require 'url)
(require 'json)
(require 'gim-code-hud-git)

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
  "Cache key for purpose summary: file + mtime hour."
  (format "purpose:%s:%s"
          file
          (format-time-string "%Y-%m-%dT%H" (nth 5 (file-attributes file)))))

(defun gim-code-hud--history-key (file)
  "Cache key for history summary: file + mtime date."
  (format "history:%s:%s"
          file
          (format-time-string "%Y-%m-%d" (nth 5 (file-attributes file)))))

;;; Claude API

(defcustom gim-code-hud-api-key nil
  "Anthropic API key.  Falls back to the ANTHROPIC_API_KEY environment variable."
  :type '(choice (const nil) string)
  :group 'gim-code-hud)

(defcustom gim-code-hud-model "claude-haiku-4-5-20251001"
  "Claude model to use for summaries."
  :type 'string
  :group 'gim-code-hud)

(defun gim-code-hud--api-key ()
  "Return the API key or signal an error if absent."
  (or gim-code-hud-api-key
      (getenv "ANTHROPIC_API_KEY")
      (error "gim-code-hud: set `gim-code-hud-api-key' or ANTHROPIC_API_KEY")))

(defun gim-code-hud--call-claude (prompt callback)
  "Send PROMPT to the Claude API and call CALLBACK with the response text."
  (let* ((api-key (gim-code-hud--api-key))
         (body (json-encode
                `((model . ,gim-code-hud-model)
                  (max_tokens . 512)
                  (messages . [((role . "user") (content . ,prompt))]))))
         (url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type"      . "application/json")
            ("x-api-key"         . ,api-key)
            ("anthropic-version" . "2023-06-01")))
         (url-request-data (encode-coding-string body 'utf-8)))
    (url-retrieve
     "https://api.anthropic.com/v1/messages"
     (lambda (status)
       (if (plist-get status :error)
           (message "gim-code-hud: API error %S" (plist-get status :error))
         (goto-char (point-min))
         (re-search-forward "\r?\n\r?\n")
         (let* ((json-object-type 'alist)
                (data (json-read))
                (text (alist-get 'text (aref (alist-get 'content data) 0))))
           (funcall callback text))))
     nil t)))

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
