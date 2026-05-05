;;; -*- lexical-binding: t -*-
;;; gim-code-hud-render.el --- HUD buffer rendering for gim-code-hud

(require 'cl-lib)
(require 'dash)

(defconst gim-code-hud--buffer-name "*gim-code-hud*")

;;; Display mode

(define-derived-mode gim-code-hud-display-mode special-mode "HUD"
  "Read-only display mode for the *gim-code-hud* buffer.
Inherits `q' (bury) from `special-mode'; `g' is rebound to refresh."
  :interactive nil)

;;; Status face

(defun gim-code-hud--status-face (status)
  (pcase status
    ("clean"     'success)
    ("staged"    'diff-added)
    ("dirty"     'warning)
    ("untracked" 'shadow)
    (_           'default)))

;;; Section helpers

(defun gim-code-hud--insert-header (title)
  "Insert a bold section TITLE."
  (insert (propertize (concat "▸ " title "\n") 'face 'bold)))

(defun gim-code-hud--insert-contributors (pairs)
  "Insert formatted contributor PAIRS (AUTHOR . COUNT)."
  (if (null pairs)
      (insert "  (none)\n")
    (cl-loop for (author . count) in pairs
             do (insert (format "  %-30s %3d\n" author count)))))

(defun gim-code-hud--insert-co-changes (pairs root)
  "Insert co-change PAIRS (FILE . COUNT) as clickable buttons resolved under ROOT."
  (if (null pairs)
      (insert "  (none)\n")
    (cl-loop for (file . count) in (-take 10 pairs)
             do (insert "  ")
                (insert-text-button
                 file
                 'action (let ((target (expand-file-name file root)))
                           (lambda (_btn) (find-file target)))
                 'follow-link t
                 'help-echo (format "Visit %s" file))
                (insert (format "%s %3d\n"
                                (make-string (max 1 (- 40 (length file))) ?\s)
                                count)))))

;;; Public render entry point

(defun gim-code-hud/render (data)
  "Render DATA plist into the *gim-code-hud* buffer.
DATA keys: :file :root :status :contributors :co-changes :purpose :history."
  (let ((buf  (get-buffer-create gim-code-hud--buffer-name))
        (root (or (plist-get data :root) default-directory)))
    (with-current-buffer buf
      (unless (derived-mode-p 'gim-code-hud-display-mode)
        (gim-code-hud-display-mode))
      (let ((inhibit-read-only t)
            (saved-pos (point)))
        (erase-buffer)
        (insert (propertize (format "HUD  %s\n\n" (plist-get data :file))
                            'face '(bold underline)))
        (gim-code-hud--insert-header "Git status")
        (let ((status (or (plist-get data :status) "…")))
          (insert (propertize (format "  %s\n\n" status)
                              'face (gim-code-hud--status-face status))))
        (gim-code-hud--insert-header "Contributors")
        (gim-code-hud--insert-contributors (plist-get data :contributors))
        (insert "\n")
        (gim-code-hud--insert-header "Co-change partners")
        (gim-code-hud--insert-co-changes (plist-get data :co-changes) root)
        (insert "\n")
        (gim-code-hud--insert-header "Purpose")
        (insert (format "  %s\n\n" (or (plist-get data :purpose) "(loading…)")))
        (gim-code-hud--insert-header "History")
        (insert (format "  %s\n\n" (or (plist-get data :history) "(loading…)")))
        (goto-char (min saved-pos (point-max)))
        (set-buffer-modified-p nil)))))

(provide 'gim-code-hud-render)
;;; gim-code-hud-render.el ends here
