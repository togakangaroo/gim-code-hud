;;; -*- lexical-binding: t -*-
;;; gim-code-hud.el --- Heads-up display buffer for the current file

;; Author: George Mauer
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (dash "2.19") (async "1.9"))
;; Keywords: tools, vc

;;; Commentary:
;; A global minor mode that keeps a *gim-code-hud* buffer updated with git
;; status, contributors, co-change partners, and LLM-generated summaries for
;; the file you are currently visiting.  Uses an idle timer (à la Flycheck)
;; so that rapid buffer switches don't trigger redundant work.

;;; Code:

(require 'gim-code-hud-git)
(require 'gim-code-hud-llm)
(require 'gim-code-hud-render)

;;; Internal state

(defvar gim-code-hud--idle-timer nil
  "Pending idle timer for the next HUD refresh, or nil.")

(defvar gim-code-hud--last-buffer nil
  "Buffer that last triggered a HUD update, used to debounce rapid switches.")

(defvar gim-code-hud--current-file nil
  "Absolute path of the file currently reflected in the HUD buffer.")

;;; Update pipeline

(defun gim-code-hud--make-guard (file data key)
  "Return a callback that sets KEY in DATA and re-renders if FILE is still current."
  (lambda (value)
    (when (equal file gim-code-hud--current-file)
      (plist-put data key value)
      (gim-code-hud/render data))))

(defun gim-code-hud--do-update (file)
  "Populate the HUD for FILE, rendering a skeleton immediately then filling async."
  (setq gim-code-hud--idle-timer  nil
        gim-code-hud--current-file file)
  (let* ((root (or (ignore-errors
                     (let ((default-directory (file-name-directory file)))
                       (vc-root-dir)))
                   (file-name-directory file)))
         (data (list :file         (abbreviate-file-name file)
                     :root         root
                     :status       "(loading…)"
                     :contributors nil
                     :co-changes   nil
                     :purpose      nil
                     :history      nil)))
    (gim-code-hud/render data)
    (gim-code-hud--git-status-async   file (gim-code-hud--make-guard file data :status))
    (gim-code-hud--contributors-async file (gim-code-hud--make-guard file data :contributors))
    (gim-code-hud--co-changes-async   file (gim-code-hud--make-guard file data :co-changes))
    (gim-code-hud/get-purpose         file (gim-code-hud--make-guard file data :purpose))
    (gim-code-hud/get-history         file (gim-code-hud--make-guard file data :history))))

(defun gim-code-hud--schedule-update ()
  "Hook handler: debounce buffer switches and schedule a HUD refresh via idle timer."
  (let ((buf (current-buffer)))
    (unless (or (eq buf gim-code-hud--last-buffer)
                (null (buffer-file-name buf))
                (equal (buffer-name buf) gim-code-hud--buffer-name))
      (setq gim-code-hud--last-buffer buf)
      (when gim-code-hud--idle-timer
        (cancel-timer gim-code-hud--idle-timer))
      (setq gim-code-hud--idle-timer
            (run-with-idle-timer 0.3 nil #'gim-code-hud--do-update
                                 (buffer-file-name buf))))))

;;; Global minor mode

;;;###autoload
(define-minor-mode gim-code-hud-mode
  "Toggle the gim-code-hud heads-up display."
  :global t
  :lighter " HUD"
  (if gim-code-hud-mode
      (add-hook 'buffer-list-update-hook #'gim-code-hud--schedule-update)
    (remove-hook 'buffer-list-update-hook #'gim-code-hud--schedule-update)
    (when gim-code-hud--idle-timer
      (cancel-timer gim-code-hud--idle-timer)
      (setq gim-code-hud--idle-timer nil))
    (setq gim-code-hud--last-buffer  nil
          gim-code-hud--current-file nil)))

;;; Interactive commands

;; Bind `g' in the HUD buffer to refresh (gim-code-hud/refresh is defined below,
;; after gim-code-hud-display-mode-map is created by the require above).
(define-key gim-code-hud-display-mode-map "g" #'gim-code-hud/refresh)

;;;###autoload
(defun gim-code-hud/show ()
  "Open or switch to the *gim-code-hud* buffer."
  (interactive)
  (pop-to-buffer gim-code-hud--buffer-name))

;;;###autoload
(defun gim-code-hud/refresh ()
  "Force a full HUD refresh for the current file."
  (interactive)
  (when-let ((file (or (buffer-file-name) gim-code-hud--current-file)))
    (setq gim-code-hud--last-buffer nil)
    (gim-code-hud--do-update file)))

(provide 'gim-code-hud)
;;; gim-code-hud.el ends here
