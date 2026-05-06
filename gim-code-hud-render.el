;;; -*- lexical-binding: t -*-
;;; gim-code-hud-render.el --- Org-mode HUD buffer rendering for gim-code-hud

(require 'org)
(require 'cl-lib)

(defconst gim-code-hud--buffer-name "*gim-code-hud*")

;;; Template

(defcustom gim-code-hud-org-template
  "* HUD: {file}

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

** Co-change Partners
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
"
  "Org-mode template for the *gim-code-hud* buffer.
{file} is replaced with the abbreviated file path on init.
Headings are located by the GIM_CODE_HUD_ANALYSIS_ID property; heading
text may be freely edited."
  :type 'string
  :group 'gim-code-hud)

;;; Display mode (org-mode derived)

(define-derived-mode gim-code-hud-display-mode org-mode "HUD"
  "Org-mode derived display mode for the *gim-code-hud* buffer.
`q' buries the buffer; `g' (bound in gim-code-hud.el) forces a refresh."
  :interactive nil
  (setq-local org-startup-folded nil))

(define-key gim-code-hud-display-mode-map "q" #'quit-window)

;;; Init: write template

(defun gim-code-hud/render-init (file)
  "Erase the HUD buffer and insert the template for FILE."
  (with-current-buffer (get-buffer-create gim-code-hud--buffer-name)
    (unless (derived-mode-p 'gim-code-hud-display-mode)
      (gim-code-hud-display-mode))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (string-replace "{file}" (abbreviate-file-name file)
                              gim-code-hud-org-template)))
    (goto-char (point-min))
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
        (insert "\n" text "\n\n")))))

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
