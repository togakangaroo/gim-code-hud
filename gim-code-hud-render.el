;;; -*- lexical-binding: t -*-
;;; gim-code-hud-render.el --- Org-mode HUD buffer rendering for gim-code-hud

(require 'org)
(require 'cl-lib)
(require 's)

(defconst gim-code-hud--buffer-name "*gim-code-hud*")

;;; Template

(defcustom gim-code-hud-org-template-suffix
  ""
  "Org-mode text appended to the default value of `gim-code-hud-org-template'.
Set this to add extra headings without replacing the entire template."
  :type 'string
  :group 'gim-code-hud)

(defcustom gim-code-hud-org-template
  "#+STARTUP: hidedrawers
* HUD: {file}

** Git Status
:PROPERTIES:
:GIM_CODE_HUD_ANALYSIS_ID: git-status
:END:

(loading…)

** Contributors
:PROPERTIES:
:GIM_CODE_HUD_ANALYSIS_ID: contributors
:END:

(loading…)

** Code that changes whenver this file changes
:PROPERTIES:
:GIM_CODE_HUD_ANALYSIS_ID: co-changes
:END:

(loading…)

** Purpose
:PROPERTIES:
:GIM_CODE_HUD_ANALYSIS_ID: purpose
:END:

(loading…)

** History
:PROPERTIES:
:GIM_CODE_HUD_ANALYSIS_ID: history
:END:

(loading…)
${suffix}"
  "Org-mode template for the *gim-code-hud* buffer.
{file} is replaced with the abbreviated file path on init.
${suffix} is replaced with `gim-code-hud-org-template-suffix' at render time.
Headings are located by the GIM_CODE_HUD_ANALYSIS_ID property; heading
text may be freely edited."
  :type 'string
  :group 'gim-code-hud)

;;; Display mode (org-mode derived)

(define-derived-mode gim-code-hud-display-mode org-mode "HUD"
  "Org-mode derived display mode for the *gim-code-hud* buffer.
`q' buries the buffer; `g' (bound in gim-code-hud.el) forces a refresh."
  :interactive nil
  (setq-local org-startup-folded nil)
  (setq-local truncate-lines t))

(define-key gim-code-hud-display-mode-map "q" #'quit-window)

;;; Value formatters (structured data → display string)

(defun gim-code-hud--format-contributors (pairs)
  "Format contributor PAIRS ((AUTHOR . COUNT) ...) as a plain-text string."
  (if (null pairs)
      "(none)"
    (mapconcat (lambda (pair)
                 (format "%3d  %s" (cdr pair) (car pair)))
               pairs "\n")))

(defun gim-code-hud--format-co-changes (value root)
  "Format co-change VALUE (TOTAL . PAIRS) as percentage + org-link lines under ROOT.
Each line shows the co-change rate as a ceiling percentage before the file link."
  (if (null value)
      "(none)"
    (let* ((total (car value))
           (pairs (-take 10 (cdr value))))
      (if (null pairs)
          "(none)"
        (mapconcat
         (lambda (pair)
           (let* ((file  (car pair))
                  (count (cdr pair))
                  (pct   (ceiling (* 100.0 (/ (float count) total))))
                  (path  (expand-file-name file root)))
             (format "%3d%% [[file:%s][%s]]" pct path file)))
         pairs "\n")))))

;;; Init: write template

(defun gim-code-hud/render-init (file)
  "Erase the HUD buffer and insert the template for FILE."
  (with-current-buffer (get-buffer-create gim-code-hud--buffer-name)
    (unless (derived-mode-p 'gim-code-hud-display-mode)
      (gim-code-hud-display-mode))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (string-replace
               "{file}" (abbreviate-file-name file)
               (s-format gim-code-hud-org-template 'aget
                         `(("suffix" . ,gim-code-hud-org-template-suffix))))))
    (goto-char (point-min))
    (org-set-startup-visibility)
    (set-buffer-modified-p nil)))

;;; Section body update

(defun gim-code-hud--section-replace-body (section-id text)
  "Replace body of the section with GIM_CODE_HUD_ANALYSIS_ID = SECTION-ID with TEXT."
  (save-excursion
    (goto-char (or (org-find-property "GIM_CODE_HUD_ANALYSIS_ID" section-id)
                   (error "HUD section not found: %s" section-id)))
    (org-end-of-meta-data t)
    (let ((body-start (point)))
      (outline-next-heading)
      (let ((body-end (point)))
        (delete-region body-start body-end)
        (goto-char body-start)
        (insert text "\n\n")))))

;;; Per-section property readers

(defun gim-code-hud--section-ttl-override (section-id)
  "Return the GIM_CODE_HUD_TTL_SECONDS value for SECTION-ID, or nil if absent."
  (when-let* ((buf (get-buffer gim-code-hud--buffer-name))
              (pos (with-current-buffer buf
                     (org-find-property "GIM_CODE_HUD_ANALYSIS_ID" section-id))))
    (with-current-buffer buf
      (save-excursion
        (goto-char pos)
        (when-let ((val (org-entry-get (point) "GIM_CODE_HUD_TTL_SECONDS")))
          (string-to-number val))))))

;;; Ad-hoc section discovery

(defun gim-code-hud--ad-hoc-sections ()
  "Return alist of (section-id . command) for ad-hoc sections in the HUD buffer.
Ad-hoc sections carry both GIM_CODE_HUD_ANALYSIS_ID and GIM_CODE_HUD_CLI_COMMAND."
  (when-let ((buf (get-buffer gim-code-hud--buffer-name)))
    (with-current-buffer buf
      (let (result)
        (org-map-entries
         (lambda ()
           (let ((id  (org-entry-get (point) "GIM_CODE_HUD_ANALYSIS_ID"))
                 (cmd (org-entry-get (point) "GIM_CODE_HUD_CLI_COMMAND")))
             (when (and id cmd)
               (push (cons id cmd) result))))
         nil nil)
        (nreverse result)))))

;;; Flush: drain pending-updates map into buffer

(defun gim-code-hud/flush-pending (pending-map)
  "Drain PENDING-MAP (section-id → (value . retrieved-at)) into the HUD buffer.
Clears each entry after writing. No-ops if the buffer does not exist."
  (when-let ((buf (get-buffer gim-code-hud--buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (maphash
         (lambda (section-id entry)
           (condition-case err
               (gim-code-hud--section-replace-body section-id (car entry))
             (error (message "gim-code-hud flush error for %s: %s"
                             section-id (error-message-string err))))
           (remhash section-id pending-map))
         pending-map))
      (set-buffer-modified-p nil))))

(provide 'gim-code-hud-render)
;;; gim-code-hud-render.el ends here
