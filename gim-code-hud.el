;;; -*- lexical-binding: t -*-
;;; gim-code-hud.el --- Heads-up display buffer for the current file

;; Author: George Mauer
;; Version: 0.2.0
;; Package-Requires: ((emacs "30.1") (dash "2.19") (async "1.9"))
;; Keywords: tools, vc

;;; Commentary:
;; A global minor mode keeping a *gim-code-hud* org buffer updated with git
;; status, contributors, co-change partners, and LLM-generated summaries for
;; the file you are currently visiting.
;;
;; Two repeating timers drive the update cycle, both only running while the
;; HUD buffer is visible in any window:
;;
;;   Staleness timer (5 s) — for each section of the current file, fires an
;;   async fetch if the cached value has expired.  Results are pushed onto the
;;   pending-updates map.
;;
;;   Flush timer (0.5 s) — drains pending-updates into the org buffer.

;;; Code:

(require 'gim-code-hud-db)
(require 'gim-code-hud-git)
(require 'gim-code-hud-llm)
(require 'gim-code-hud-render)

;;; Internal state

(defvar gim-code-hud--db nil
  "Open SQLite handle for the current project's .gim-code-hud.db, or nil.")

(defvar gim-code-hud--current-file nil
  "Absolute path of the file currently reflected in the HUD.")

(defvar gim-code-hud--current-root nil
  "Project root of gim-code-hud--current-file, for resolving co-change paths.")

(defvar gim-code-hud--next-update (make-hash-table :test #'equal)
  "Map of (file . section-id) -> float-time of next required fetch.")

(defvar gim-code-hud--pending-updates (make-hash-table :test #'equal)
  "Map of section-id -> (value . retrieved-at), drained by the flush timer.")

(defvar gim-code-hud--staleness-timer nil
  "Repeating timer that checks for expired sections every 5 s, or nil.")

(defvar gim-code-hud--flush-timer nil
  "Repeating timer that flushes pending-updates to the HUD buffer every 0.5 s, or nil.")

(defvar gim-code-hud--switch-timer nil
  "One-shot idle timer (0.3 s) debouncing buffer switches before HUD render.")

;;; DB helpers

(defun gim-code-hud--ensure-db (file)
  "Open (or reuse) the SQLite DB rooted at FILE's project root."
  (let ((root (or (ignore-errors
                    (let ((default-directory (file-name-directory file)))
                      (vc-root-dir)))
                  (file-name-directory file))))
    (unless (and gim-code-hud--db (sqlitep gim-code-hud--db))
      (setq gim-code-hud--db (gim-code-hud--db-open root)))
    gim-code-hud--db))

;;; Staleness table helpers

(defun gim-code-hud--next-update-key (file section-id)
  (cons file section-id))

(defun gim-code-hud--seed-staleness (file)
  "Populate next-update table for FILE from SQLite valid_until values."
  (let ((db (gim-code-hud--ensure-db file)))
    (dolist (section-id '("git-status" "contributors" "co-changes" "purpose" "history"))
      (let* ((key      (gim-code-hud--db-key section-id file))
             (vu       (gim-code-hud--db-valid-until db key))
             (nk       (gim-code-hud--next-update-key file section-id)))
        (puthash nk (or vu 0.0) gim-code-hud--next-update)))))

(defun gim-code-hud--mark-updated (file section-id)
  "Record that SECTION-ID for FILE was just fetched; set next update from TTL."
  (let* ((ttl (gim-code-hud--db-ttl section-id))
         (nk  (gim-code-hud--next-update-key file section-id)))
    (puthash nk (+ (float-time) ttl) gim-code-hud--next-update)))

(defun gim-code-hud--section-expired-p (file section-id)
  "Return t if the cached value for SECTION-ID / FILE needs refreshing."
  (let ((nk (gim-code-hud--next-update-key file section-id)))
    (> (float-time) (or (gethash nk gim-code-hud--next-update) 0.0))))

;;; Async fetch dispatch

(defun gim-code-hud--value-to-string (section-id value)
  "Convert VALUE for SECTION-ID to a display string."
  (pcase section-id
    ("contributors" (gim-code-hud--format-contributors value))
    ("co-changes"   (gim-code-hud--format-co-changes value
                      (or gim-code-hud--current-root default-directory)))
    (_              (or value "(loading…)"))))

(defun gim-code-hud--push-result (file section-id value)
  "Format VALUE, store in SQLite and pending-updates map for SECTION-ID of FILE."
  (when (equal file gim-code-hud--current-file)
    (let* ((text (gim-code-hud--value-to-string section-id value))
           (db   (gim-code-hud--ensure-db file)))
      (gim-code-hud--db-put db (gim-code-hud--db-key section-id file)
                            text (gim-code-hud--db-ttl section-id))
      (gim-code-hud--mark-updated file section-id)
      (puthash section-id (cons text (float-time)) gim-code-hud--pending-updates))))

(defun gim-code-hud--fetch-section (file section-id)
  "Asynchronously fetch SECTION-ID for FILE and push result when done."
  (let ((push (lambda (v) (gim-code-hud--push-result file section-id v))))
    (pcase section-id
      ("git-status"   (gim-code-hud--git-status-async   file push))
      ("contributors" (gim-code-hud--contributors-async file push))
      ("co-changes"   (gim-code-hud--co-changes-async   file push))
      ("purpose"      (gim-code-hud/get-purpose          file push))
      ("history"      (gim-code-hud/get-history          file push)))))

;;; Staleness timer body

(defun gim-code-hud--staleness-tick ()
  "Check all sections for the current file; fetch any that have expired."
  (when gim-code-hud--current-file
    (dolist (section-id '("git-status" "contributors" "co-changes" "purpose" "history"))
      (when (gim-code-hud--section-expired-p gim-code-hud--current-file section-id)
        (gim-code-hud--fetch-section gim-code-hud--current-file section-id)))))

;;; Flush timer body

(defun gim-code-hud--flush-tick ()
  "Drain pending-updates into the HUD org buffer."
  (gim-code-hud/flush-pending gim-code-hud--pending-updates))

;;; Visibility gating

(defun gim-code-hud--hud-visible-p ()
  "Return non-nil if the HUD buffer is visible in any live window."
  (get-buffer-window gim-code-hud--buffer-name t))

(defun gim-code-hud--start-timers ()
  "Start both timers if not already running."
  (unless gim-code-hud--staleness-timer
    (setq gim-code-hud--staleness-timer
          (run-with-timer 0 5 #'gim-code-hud--staleness-tick)))
  (unless gim-code-hud--flush-timer
    (setq gim-code-hud--flush-timer
          (run-with-timer 0 0.5 #'gim-code-hud--flush-tick))))

(defun gim-code-hud--stop-timers ()
  "Cancel both timers."
  (when gim-code-hud--staleness-timer
    (cancel-timer gim-code-hud--staleness-timer)
    (setq gim-code-hud--staleness-timer nil))
  (when gim-code-hud--flush-timer
    (cancel-timer gim-code-hud--flush-timer)
    (setq gim-code-hud--flush-timer nil)))

(defun gim-code-hud--visibility-update ()
  "Start or stop timers based on whether the HUD buffer is currently visible."
  (if (gim-code-hud--hud-visible-p)
      (gim-code-hud--start-timers)
    (gim-code-hud--stop-timers)))

;;; File-switch handler (debounced)

(defun gim-code-hud--do-switch (file)
  "Perform the HUD render for FILE after the debounce idle delay."
  (setq gim-code-hud--switch-timer nil)
  (let ((root (or (ignore-errors
                    (let ((default-directory (file-name-directory file)))
                      (vc-root-dir)))
                  (file-name-directory file))))
    (setq gim-code-hud--current-file file
          gim-code-hud--current-root root)
    (gim-code-hud--ensure-db file)
    (gim-code-hud--seed-staleness file)
    (let ((db gim-code-hud--db))
      (dolist (sid '("git-status" "contributors" "co-changes" "purpose" "history"))
        (when-let ((cached (gim-code-hud--db-get db (gim-code-hud--db-key sid file))))
          (puthash sid (cons cached (float-time)) gim-code-hud--pending-updates))))
    (gim-code-hud/render-init file)
    ;; Flush cached values immediately so "(loading…)" is never visibly shown.
    (gim-code-hud/flush-pending gim-code-hud--pending-updates)))

(defun gim-code-hud--on-buffer-switch ()
  "Debounce buffer switches; schedule HUD render after 0.3 s of idle."
  (let ((buf (current-buffer)))
    (when (and (buffer-file-name buf)
               (not (equal (buffer-name buf) gim-code-hud--buffer-name)))
      (let ((file (buffer-file-name buf)))
        (unless (equal file gim-code-hud--current-file)
          (when gim-code-hud--switch-timer
            (cancel-timer gim-code-hud--switch-timer))
          (setq gim-code-hud--switch-timer
                (run-with-idle-timer 0.3 nil #'gim-code-hud--do-switch file)))))))



;;; Global minor mode

;;;###autoload
(define-minor-mode gim-code-hud-mode
  "Toggle the gim-code-hud heads-up display."
  :global t
  :lighter " HUD"
  (if gim-code-hud-mode
      (progn
        (define-key gim-code-hud-display-mode-map "g" #'gim-code-hud/refresh)
        (add-hook 'buffer-list-update-hook   #'gim-code-hud--on-buffer-switch)
        (add-hook 'window-configuration-change-hook #'gim-code-hud--visibility-update))
    (define-key gim-code-hud-display-mode-map "g" nil)
    (remove-hook 'buffer-list-update-hook   #'gim-code-hud--on-buffer-switch)
    (remove-hook 'window-configuration-change-hook #'gim-code-hud--visibility-update)
    (when gim-code-hud--switch-timer
      (cancel-timer gim-code-hud--switch-timer)
      (setq gim-code-hud--switch-timer nil))
    (gim-code-hud--stop-timers)
    (when (and gim-code-hud--db (sqlitep gim-code-hud--db))
      (sqlite-close gim-code-hud--db)
      (setq gim-code-hud--db nil))
    (setq gim-code-hud--current-file nil
          gim-code-hud--current-root nil)
    (clrhash gim-code-hud--next-update)
    (clrhash gim-code-hud--pending-updates)))

;;; Interactive commands

;;;###autoload
(defun gim-code-hud/show ()
  "Open or switch to the *gim-code-hud* buffer."
  (interactive)
  (pop-to-buffer gim-code-hud--buffer-name))

;;;###autoload
(defun gim-code-hud/refresh ()
  "Force a full HUD refresh for the current file by expiring all sections."
  (interactive)
  (when gim-code-hud--current-file
    (dolist (section-id '("git-status" "contributors" "co-changes" "purpose" "history"))
      (puthash (gim-code-hud--next-update-key gim-code-hud--current-file section-id)
               0.0 gim-code-hud--next-update))
    (gim-code-hud--staleness-tick)))

;;;###autoload
(defun gim-code-hud/toggle-staleness-timer ()
  "Toggle the staleness timer on or off; return the new state (t = running)."
  (interactive)
  (if gim-code-hud--staleness-timer
      (progn
        (cancel-timer gim-code-hud--staleness-timer)
        (setq gim-code-hud--staleness-timer nil)
        (message "gim-code-hud: staleness timer OFF")
        nil)
    (setq gim-code-hud--staleness-timer
          (run-with-timer 0 5 #'gim-code-hud--staleness-tick))
    (message "gim-code-hud: staleness timer ON")
    t))

;;;###autoload
(defun gim-code-hud/toggle-flush-timer ()
  "Toggle the flush timer on or off; return the new state (t = running)."
  (interactive)
  (if gim-code-hud--flush-timer
      (progn
        (cancel-timer gim-code-hud--flush-timer)
        (setq gim-code-hud--flush-timer nil)
        (message "gim-code-hud: flush timer OFF")
        nil)
    (setq gim-code-hud--flush-timer
          (run-with-timer 0 0.5 #'gim-code-hud--flush-tick))
    (message "gim-code-hud: flush timer ON")
    t))

(provide 'gim-code-hud)
;;; gim-code-hud.el ends here
