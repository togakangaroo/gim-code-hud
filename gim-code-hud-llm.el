;;; -*- lexical-binding: t -*-
;;; gim-code-hud-llm.el --- Claude CLI integration for gim-code-hud

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

;;; Claude CLI call

(defun gim-code-hud--call-claude (prompt callback)
  "Send PROMPT to the Claude CLI and call CALLBACK with the response text."
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
  "Call CALLBACK with a ≤3-sentence purpose summary for FILE."
  (let ((prompt (format "In 3 sentences or fewer, describe what this file does.\n\n%s"
                        (with-temp-buffer
                          (insert-file-contents file)
                          (buffer-string)))))
    (gim-code-hud--call-claude prompt callback)))

;;;###autoload
(defun gim-code-hud/get-history (file callback)
  "Call CALLBACK with a ≤5-sentence history narrative for FILE."
  (gim-code-hud--git-async
   (file-name-directory file)
   (list "log" "--follow" "--oneline" (file-name-nondirectory file))
   (lambda (log)
     (let ((prompt (format "In 5 sentences or fewer, narrate how this file evolved.\n\nGit log:\n%s" log)))
       (gim-code-hud--call-claude prompt callback)))))

(provide 'gim-code-hud-llm)
;;; gim-code-hud-llm.el ends here
