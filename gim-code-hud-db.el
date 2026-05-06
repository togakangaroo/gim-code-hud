;;; -*- lexical-binding: t -*-
;;; gim-code-hud-db.el --- SQLite cache layer for gim-code-hud

(unless (sqlite-available-p)
  (error "gim-code-hud requires Emacs built with SQLite support (Emacs 29+)"))

;;; TTL configuration

(defcustom gim-code-hud-ttls
  '(("git-status"   . 60)
    ("contributors" . 86400)
    ("co-changes"   . 86400)
    ("purpose"      . 3600)
    ("history"      . 86400))
  "Alist mapping section IDs to cache TTL in seconds."
  :type '(alist :key-type string :value-type integer)
  :group 'gim-code-hud)

;;; Schema

(defconst gim-code-hud--db-schema
  "CREATE TABLE IF NOT EXISTS cache (
     key         TEXT PRIMARY KEY,
     value       TEXT NOT NULL,
     valid_until REAL NOT NULL
   )")

;;; Connection

(defun gim-code-hud--db-open (project-root)
  "Open (or create) .gim-code-hud.db under PROJECT-ROOT and return the handle."
  (let* ((path (expand-file-name ".gim-code-hud.db" project-root))
         (db   (sqlite-open path)))
    (sqlite-execute db gim-code-hud--db-schema)
    db))

;;; Key helper

(defun gim-code-hud--db-key (section-id file)
  "Return the cache key string for SECTION-ID and absolute FILE path."
  (format "%s:%s" section-id (expand-file-name file)))

;;; TTL helper

(defun gim-code-hud--db-ttl (section-id)
  "Return the configured TTL in seconds for SECTION-ID."
  (or (cdr (assoc section-id gim-code-hud-ttls)) 3600))

;;; Public API

(defun gim-code-hud--db-get (db key)
  "Return cached value for KEY if valid_until > now, else nil."
  (when-let* ((rows (sqlite-select db
                      "SELECT value FROM cache WHERE key = ? AND valid_until > ?"
                      (list key (float-time))))
              (row (car rows)))
    (car row)))

(defun gim-code-hud--db-put (db key value ttl)
  "Store VALUE under KEY expiring TTL seconds from now."
  (sqlite-execute db
    "INSERT OR REPLACE INTO cache (key, value, valid_until) VALUES (?, ?, ?)"
    (list key value (+ (float-time) ttl))))

(defun gim-code-hud--db-valid-until (db key)
  "Return the valid_until float timestamp for KEY, or nil if not present."
  (when-let* ((rows (sqlite-select db
                      "SELECT valid_until FROM cache WHERE key = ?"
                      (list key)))
              (row (car rows)))
    (car row)))

(provide 'gim-code-hud-db)
;;; gim-code-hud-db.el ends here
