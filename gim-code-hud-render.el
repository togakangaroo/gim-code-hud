;;; -*- lexical-binding: t -*-
;;; gim-code-hud-render.el --- HUD buffer rendering for gim-code-hud

(require 'cl-lib)
(require 'dash)

(defconst gim-code-hud--buffer-name "*gim-code-hud*")

(defun gim-code-hud--section (title body)
  "Insert a TITLE header followed by BODY text."
  (insert (propertize (concat "## " title "\n") 'face 'bold))
  (insert body "\n\n"))

(defun gim-code-hud--render-contributors (pairs)
  "Format contributor PAIRS as a string."
  (if (null pairs)
      "(none)"
    (->> pairs
         (--map (format "  %-30s %d" (car it) (cdr it)))
         (string-join "\n"))))

(defun gim-code-hud--render-co-changes (pairs &optional limit)
  "Format co-change PAIRS as a string, up to LIMIT entries (default 10)."
  (if (null pairs)
      "(none)"
    (->> (-take (or limit 10) pairs)
         (--map (format "  %-40s %d" (car it) (cdr it)))
         (string-join "\n"))))

(defun gim-code-hud/render (data)
  "Render DATA plist into the HUD buffer.
DATA keys: :file :status :contributors :co-changes :purpose :history"
  (let ((buf (get-buffer-create gim-code-hud--buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize
                 (format "# HUD: %s\n\n" (plist-get data :file))
                 'face '(bold underline)))
        (gim-code-hud--section
         "Git status"
         (format "  %s" (or (plist-get data :status) "unknown")))
        (gim-code-hud--section
         "Contributors"
         (gim-code-hud--render-contributors (plist-get data :contributors)))
        (gim-code-hud--section
         "Co-change partners"
         (gim-code-hud--render-co-changes (plist-get data :co-changes)))
        (gim-code-hud--section
         "Purpose"
         (or (plist-get data :purpose) "(loading…)"))
        (gim-code-hud--section
         "History"
         (or (plist-get data :history) "(loading…)"))
        (goto-char (point-min))
        (set-buffer-modified-p nil)
        (read-only-mode 1)))))

(provide 'gim-code-hud-render)
;;; gim-code-hud-render.el ends here
