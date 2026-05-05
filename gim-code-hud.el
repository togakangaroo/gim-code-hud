;;; -*- lexical-binding: t -*-
;;; gim-code-hud.el --- Heads-up display buffer for the current file

;; Author: George Mauer
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (dash "2.19") (async "1.9"))
;; Keywords: tools, vc

;;; Commentary:
;; A minor mode that keeps a *gim-code-hud* buffer updated with git status,
;; contributors, co-change partners, and LLM-generated summaries for the
;; file you are currently visiting.

;;; Code:

(require 'gim-code-hud-git)
(require 'gim-code-hud-llm)
(require 'gim-code-hud-render)

;;; Internal state

(defvar gim-code-hud--current-file nil
  "Absolute path of the file currently reflected in the HUD buffer.")

;;; Update logic

(defun gim-code-hud--make-guard (file data key)
  "Return a callback that updates DATA at KEY and re-renders if FILE is current."
  (lambda (value)
    (when (equal file gim-code-hud--current-file)
      (plist-put data key value)
      (gim-code-hud/render data))))

(defun gim-code-hud--update ()
  "Refresh the HUD for the current buffer's file."
  (let ((file (buffer-file-name)))
    (unless (or (null file)
                (equal file gim-code-hud--current-file)
                (equal (buffer-name) gim-code-hud--buffer-name))
      (setq gim-code-hud--current-file file)
      (let ((data (list :file         (abbreviate-file-name file)
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
        (gim-code-hud/get-history         file (gim-code-hud--make-guard file data :history))))))

;;; Minor mode

;;;###autoload
(define-minor-mode gim-code-hud-mode
  "Toggle the gim-code-hud heads-up display."
  :global t
  :lighter " HUD"
  (if gim-code-hud-mode
      (add-hook 'buffer-list-update-hook #'gim-code-hud--update)
    (remove-hook 'buffer-list-update-hook #'gim-code-hud--update)
    (setq gim-code-hud--current-file nil)))

;;; Interactive commands

;;;###autoload
(defun gim-code-hud/show ()
  "Open or switch to the *gim-code-hud* buffer."
  (interactive)
  (pop-to-buffer gim-code-hud--buffer-name))

;;;###autoload
(defun gim-code-hud/refresh ()
  "Force a full refresh of the HUD for the current buffer."
  (interactive)
  (setq gim-code-hud--current-file nil)
  (gim-code-hud--update))

(provide 'gim-code-hud)
;;; gim-code-hud.el ends here
