;;; -*- lexical-binding: t -*-
;;; gim-code-hud-tests.el --- ERT tests for gim-code-hud

(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(require 'gim-code-hud-db)
(require 'gim-code-hud-git)
(require 'gim-code-hud-llm)
(require 'gim-code-hud-render)

;;;; ─── Test infrastructure ────────────────────────────────────────────────────

;;; Scratch git repo

(defun gim-code-hud-test/make-repo ()
  "Create a fresh temporary git repo and return its absolute path."
  (let ((dir (file-name-as-directory (make-temp-file "gim-code-hud-test-" t))))
    (let ((default-directory dir))
      (call-process "git" nil nil nil "init")
      (call-process "git" nil nil nil "config" "user.name"  "Test User")
      (call-process "git" nil nil nil "config" "user.email" "test@example.com"))
    dir))

(defun gim-code-hud-test/cleanup-repo (dir)
  "Delete the temporary git repo at DIR."
  (delete-directory dir t))

(defmacro gim-code-hud-test/with-repo (var &rest body)
  "Bind VAR to a fresh git repo, execute BODY, then delete the repo."
  (declare (indent 1))
  `(let ((,var (gim-code-hud-test/make-repo)))
     (unwind-protect
         (progn ,@body)
       (gim-code-hud-test/cleanup-repo ,var))))

(defun gim-code-hud-test/commit (repo filename content message
                                  &optional author-name author-email)
  "In REPO write CONTENT to FILENAME, stage it, and commit with MESSAGE.
AUTHOR-NAME and AUTHOR-EMAIL default to \"Test User\" / \"test@example.com\"."
  (let* ((default-directory repo)
         (path (expand-file-name filename repo)))
    (make-directory (file-name-directory path) t)
    (with-temp-file path (insert content))
    (call-process "git" nil nil nil "add" filename)
    (let ((process-environment
           (append
            (list (format "GIT_AUTHOR_NAME=%s"    (or author-name  "Test User"))
                  (format "GIT_AUTHOR_EMAIL=%s"   (or author-email "test@example.com"))
                  (format "GIT_COMMITTER_NAME=%s"  (or author-name  "Test User"))
                  (format "GIT_COMMITTER_EMAIL=%s" (or author-email "test@example.com")))
            process-environment)))
      (call-process "git" nil nil nil "commit" "-m" message))))

;;; Synchronous wrapper for async callbacks

(defun gim-code-hud-test/call-sync (fn &rest args)
  "Call async FN with ARGS, appending a capturing callback.
Blocks until the callback fires (up to 5 s) and returns its argument."
  (let ((result nil) (done nil))
    (apply fn (append args (list (lambda (v) (setq result v done t)))))
    (let ((deadline (+ (float-time) 5.0)))
      (while (and (not done) (< (float-time) deadline))
        (sit-for 0.05)))
    (unless done (error "gim-code-hud-test/call-sync: timed out after 5 s"))
    result))

;;; Claude CLI stub

(defmacro gim-code-hud-test/with-claude-stub (response &rest body)
  "Execute BODY with `gim-code-hud-cli' pointing to a script that prints RESPONSE."
  (declare (indent 1))
  `(let* ((script (make-temp-file "gim-code-hud-claude-stub" nil ".sh"))
          (gim-code-hud-cli script))
     (with-temp-file script
       (insert "#!/bin/sh\n")
       (insert (format "printf '%%s' %s\n" (shell-quote-argument ,response))))
     (set-file-modes script #o755)
     (unwind-protect
         (progn ,@body)
       (ignore-errors (delete-file script)))))

;;;; ─── Harness smoke tests ────────────────────────────────────────────────────

(ert-deftest gim-code-hud-test/harness-repo-lifecycle ()
  "make-repo creates a git repo; cleanup-repo removes it."
  (let ((dir (gim-code-hud-test/make-repo)))
    (should (file-directory-p dir))
    (should (file-directory-p (expand-file-name ".git" dir)))
    (gim-code-hud-test/cleanup-repo dir)
    (should-not (file-exists-p dir))))

(ert-deftest gim-code-hud-test/harness-commit ()
  "commit creates a file and a git commit."
  (gim-code-hud-test/with-repo repo
    (gim-code-hud-test/commit repo "foo.el" "(message \"hi\")" "Initial commit")
    (let ((default-directory repo))
      (let ((log (string-trim (shell-command-to-string "git log --oneline"))))
        (should (string-match-p "Initial commit" log))))))

(ert-deftest gim-code-hud-test/harness-call-sync ()
  "call-sync blocks until an async git function completes."
  (gim-code-hud-test/with-repo repo
    (gim-code-hud-test/commit repo "foo.el" ";; test" "First")
    (let ((file (expand-file-name "foo.el" repo)))
      (let ((status (gim-code-hud-test/call-sync
                     #'gim-code-hud--git-status-async file)))
        (should (stringp status))))))

(ert-deftest gim-code-hud-test/harness-claude-stub ()
  "with-claude-stub routes through the real async process path and returns canned text."
  (gim-code-hud-test/with-repo repo
    (gim-code-hud-test/commit repo "foo.el" ";; stub test" "Init")
    (let ((file (expand-file-name "foo.el" repo)))
      (gim-code-hud-test/with-claude-stub "This is a stub response."
        (let ((got (gim-code-hud-test/call-sync #'gim-code-hud/get-purpose file)))
          (should (equal got "This is a stub response.")))))))

;;;; ─── Git status ────────────────────────────────────────────────────────────

;;; Parser unit tests (no subprocess, no git repo)

(ert-deftest gim-code-hud-test/git-status-parse-clean ()
  (should (equal "clean" (gim-code-hud--parse-git-status "")))
  (should (equal "clean" (gim-code-hud--parse-git-status "  \n"))))

(ert-deftest gim-code-hud-test/git-status-parse-untracked ()
  (should (equal "untracked" (gim-code-hud--parse-git-status "?? foo.el\n"))))

(ert-deftest gim-code-hud-test/git-status-parse-dirty ()
  ;; space in index column = not staged
  (should (equal "dirty" (gim-code-hud--parse-git-status " M foo.el\n"))))

(ert-deftest gim-code-hud-test/git-status-parse-staged ()
  (should (equal "staged" (gim-code-hud--parse-git-status "M  foo.el\n")))
  (should (equal "staged" (gim-code-hud--parse-git-status "A  foo.el\n")))
  ;; both staged and dirty: index column wins
  (should (equal "staged" (gim-code-hud--parse-git-status "MM foo.el\n"))))

;;; Async integration tests (real scratch repo)

(ert-deftest gim-code-hud-test/git-status-async-clean ()
  "Committed, unmodified file reports clean."
  (gim-code-hud-test/with-repo repo
    (gim-code-hud-test/commit repo "foo.el" ";; v1" "Init")
    (let ((file (expand-file-name "foo.el" repo)))
      (should (equal "clean"
                     (gim-code-hud-test/call-sync
                      #'gim-code-hud--git-status-async file))))))

(ert-deftest gim-code-hud-test/git-status-async-dirty ()
  "Unstaged modification reports dirty."
  (gim-code-hud-test/with-repo repo
    (gim-code-hud-test/commit repo "foo.el" ";; v1" "Init")
    (let ((file (expand-file-name "foo.el" repo)))
      (with-temp-file file (insert ";; v2"))
      (should (equal "dirty"
                     (gim-code-hud-test/call-sync
                      #'gim-code-hud--git-status-async file))))))

(ert-deftest gim-code-hud-test/git-status-async-staged ()
  "Staged modification reports staged."
  (gim-code-hud-test/with-repo repo
    (gim-code-hud-test/commit repo "foo.el" ";; v1" "Init")
    (let ((default-directory repo)
          (file (expand-file-name "foo.el" repo)))
      (with-temp-file file (insert ";; v2"))
      (call-process "git" nil nil nil "add" "foo.el")
      (should (equal "staged"
                     (gim-code-hud-test/call-sync
                      #'gim-code-hud--git-status-async file))))))

(ert-deftest gim-code-hud-test/git-status-async-untracked ()
  "New file not yet added to git reports untracked."
  (gim-code-hud-test/with-repo repo
    ;; need at least one commit so the repo HEAD exists
    (gim-code-hud-test/commit repo "other.el" ";; x" "Init")
    (let ((file (expand-file-name "new.el" repo)))
      (with-temp-file file (insert ";; new"))
      (should (equal "untracked"
                     (gim-code-hud-test/call-sync
                      #'gim-code-hud--git-status-async file))))))

;;;; ─── Co-change partners ────────────────────────────────────────────────────

;; Raw output from `git log --follow --name-only --format=COMMIT <file>'
;; looks like: "COMMIT\n\nfile-a\nfile-b\n\nCOMMIT\n\nfile-a\n\n"
;; (blank line after COMMIT header, blank line as commit separator)

(defun gim-code-hud-test--co-change-output (&rest commit-file-lists)
  "Build fake `git log --pretty=tformat:COMMIT --name-only' output.
Each element of COMMIT-FILE-LISTS is a list of filenames for one commit.
Format: COMMIT<LF><LF>file1<LF>file2<LF>COMMIT<LF>..."
  (mapconcat (lambda (files)
               (concat "COMMIT\n\n" (mapconcat #'identity files "\n") "\n"))
             commit-file-lists ""))

;;; Parser unit tests

(ert-deftest gim-code-hud-test/co-changes-parse-empty ()
  "No commits → empty partner list."
  (should (null (gim-code-hud--parse-co-changes "foo.el" ""))))

(ert-deftest gim-code-hud-test/co-changes-parse-solo-commit ()
  "Commit touching only the target file → no partners."
  (let ((output (gim-code-hud-test--co-change-output '("foo.el"))))
    (should (null (gim-code-hud--parse-co-changes "foo.el" output)))))

(ert-deftest gim-code-hud-test/co-changes-parse-single-partner ()
  "One commit with one partner → count of 1."
  (let ((output (gim-code-hud-test--co-change-output '("foo.el" "bar.el"))))
    (should (equal '(("bar.el" . 1))
                   (gim-code-hud--parse-co-changes "foo.el" output)))))

(ert-deftest gim-code-hud-test/co-changes-parse-excludes-self ()
  "Target file is never in its own partner list."
  (let ((output (gim-code-hud-test--co-change-output '("foo.el" "bar.el" "foo.el"))))
    (should (null (rassoc "foo.el"
                          (gim-code-hud--parse-co-changes "foo.el" output))))))

(ert-deftest gim-code-hud-test/co-changes-parse-counts-and-sort ()
  "Multiple commits accumulate counts; results sorted descending."
  (let* ((output (gim-code-hud-test--co-change-output
                  '("foo.el" "bar.el" "baz.el")   ; bar+1 baz+1
                  '("foo.el" "bar.el")             ; bar+1
                  '("foo.el" "qux.el")))           ; qux+1
         (result (gim-code-hud--parse-co-changes "foo.el" output)))
    (should (equal "bar.el" (car (nth 0 result))))
    (should (= 2            (cdr (nth 0 result))))
    (should (= 1            (cdr (nth 1 result))))
    (should (= 1            (cdr (nth 2 result))))
    ;; bar must be first
    (should (equal "bar.el" (caar result)))))

;;; Async integration tests

(ert-deftest gim-code-hud-test/co-changes-async-no-partners ()
  "File committed alone has total > 0 but empty partner list."
  (gim-code-hud-test/with-repo repo
    (gim-code-hud-test/commit repo "foo.el" "v1" "Commit 1")
    (gim-code-hud-test/commit repo "foo.el" "v2" "Commit 2")
    (let* ((file   (expand-file-name "foo.el" repo))
           (result (gim-code-hud-test/call-sync
                    #'gim-code-hud--co-changes-async file)))
      (should (= 2 (car result)))       ; 2 commits
      (should (null (cdr result))))))   ; no partners

(ert-deftest gim-code-hud-test/co-changes-async-with-partners ()
  "Files committed together return (total . pairs) with correct counts and total."
  (gim-code-hud-test/with-repo repo
    (let ((default-directory repo))
      ;; commit A: foo.el + bar.el + baz.el  → bar×1, baz×1
      (with-temp-file (expand-file-name "foo.el" repo) (insert "f1"))
      (with-temp-file (expand-file-name "bar.el" repo) (insert "b1"))
      (with-temp-file (expand-file-name "baz.el" repo) (insert "z1"))
      (call-process "git" nil nil nil "add" ".")
      (call-process "git" nil nil nil "commit" "-m" "Commit A")
      ;; commit B: foo.el + bar.el  → bar×2
      (with-temp-file (expand-file-name "foo.el" repo) (insert "f2"))
      (with-temp-file (expand-file-name "bar.el" repo) (insert "b2"))
      (call-process "git" nil nil nil "add" ".")
      (call-process "git" nil nil nil "commit" "-m" "Commit B"))
    (let* ((file   (expand-file-name "foo.el" repo))
           (result (gim-code-hud-test/call-sync
                    #'gim-code-hud--co-changes-async file))
           (total  (car result))
           (pairs  (cdr result)))
      (should (= 2 total))                              ; 2 commits touched foo.el
      (should (equal "bar.el" (caar pairs)))            ; bar first
      (should (= 2 (cdar pairs)))                       ; bar in both commits
      (should (= 1 (cdr (assoc "baz.el" pairs))))       ; baz in one commit
      (should (null (assoc "foo.el" pairs))))))         ; self excluded

(ert-deftest gim-code-hud-test/co-changes-async-excludes-self ()
  "Target file never appears in its own partner list."
  (gim-code-hud-test/with-repo repo
    (let ((default-directory repo))
      (with-temp-file (expand-file-name "foo.el" repo) (insert "x"))
      (with-temp-file (expand-file-name "bar.el" repo) (insert "y"))
      (call-process "git" nil nil nil "add" ".")
      (call-process "git" nil nil nil "commit" "-m" "Init"))
    (let* ((file   (expand-file-name "foo.el" repo))
           (result (gim-code-hud-test/call-sync
                    #'gim-code-hud--co-changes-async file)))
      (should (null (assoc "foo.el" (cdr result)))))))

;;;; ─── SQLite cache layer ────────────────────────────────────────────────────

(defmacro gim-code-hud-test/with-db (var &rest body)
  "Bind VAR to a fresh temporary SQLite DB, execute BODY, then close and delete it."
  (declare (indent 1))
  `(let* ((dir  (make-temp-file "gim-code-hud-db-test-" t))
          (,var (gim-code-hud--db-open dir)))
     (unwind-protect
         (progn ,@body)
       (sqlite-close ,var)
       (delete-directory dir t))))

(ert-deftest gim-code-hud-test/db-get-miss ()
  "Missing key returns nil."
  (gim-code-hud-test/with-db db
    (should (null (gim-code-hud--db-get db "no-such-key")))))

(ert-deftest gim-code-hud-test/db-put-get-roundtrip ()
  "Value stored with a long TTL is returned by get."
  (gim-code-hud-test/with-db db
    (gim-code-hud--db-put db "k" "hello" 3600)
    (should (equal "hello" (gim-code-hud--db-get db "k")))))

(ert-deftest gim-code-hud-test/db-get-expired ()
  "Value stored with a negative TTL is not returned (already expired)."
  (gim-code-hud-test/with-db db
    (gim-code-hud--db-put db "k" "stale" -1)
    (should (null (gim-code-hud--db-get db "k")))))

(ert-deftest gim-code-hud-test/db-put-replaces ()
  "Putting a new value under the same key replaces the old one."
  (gim-code-hud-test/with-db db
    (gim-code-hud--db-put db "k" "first"  3600)
    (gim-code-hud--db-put db "k" "second" 3600)
    (should (equal "second" (gim-code-hud--db-get db "k")))))

(ert-deftest gim-code-hud-test/db-valid-until-present ()
  "valid-until returns a float > now for a freshly stored entry."
  (gim-code-hud-test/with-db db
    (gim-code-hud--db-put db "k" "v" 100)
    (let ((vu (gim-code-hud--db-valid-until db "k")))
      (should (floatp vu))
      (should (> vu (float-time))))))

(ert-deftest gim-code-hud-test/db-valid-until-absent ()
  "valid-until returns nil for a key that was never stored."
  (gim-code-hud-test/with-db db
    (should (null (gim-code-hud--db-valid-until db "absent")))))

(ert-deftest gim-code-hud-test/db-key-format ()
  "db-key produces <section-id>:<absolute-path>."
  (let ((key (gim-code-hud--db-key "purpose" "/home/user/foo.el")))
    (should (string-prefix-p "purpose:" key))
    (should (string-suffix-p "foo.el" key))))

(ert-deftest gim-code-hud-test/db-ttl-lookup ()
  "db-ttl returns configured TTL for known section IDs and 3600 for unknown."
  (should (=     60 (gim-code-hud--db-ttl "git-status")))
  (should (= 86400 (gim-code-hud--db-ttl "contributors")))
  (should (=  3600 (gim-code-hud--db-ttl "purpose")))
  (should (=  3600 (gim-code-hud--db-ttl "unknown-section"))))

(ert-deftest gim-code-hud-test/db-persists-across-connections ()
  "A value written in one connection is readable after reopening the DB file."
  (let* ((dir  (make-temp-file "gim-code-hud-db-persist-" t))
         (db1  (gim-code-hud--db-open dir)))
    (unwind-protect
        (progn
          (gim-code-hud--db-put db1 "k" "persistent" 3600)
          (sqlite-close db1)
          (let ((db2 (gim-code-hud--db-open dir)))
            (unwind-protect
                (should (equal "persistent" (gim-code-hud--db-get db2 "k")))
              (sqlite-close db2))))
      (delete-directory dir t))))

;;;; ─── Org render + flush ─────────────────────────────────────────────────────

(defmacro gim-code-hud-test/with-hud-buffer (&rest body)
  "Execute BODY with a fresh *gim-code-hud* buffer, then kill it."
  (declare (indent 0))
  `(unwind-protect
       (progn ,@body)
     (when-let ((buf (get-buffer gim-code-hud--buffer-name)))
       (kill-buffer buf))))

(ert-deftest gim-code-hud-test/render-init-creates-buffer ()
  "render-init creates the HUD buffer in gim-code-hud-display-mode."
  (gim-code-hud-test/with-hud-buffer
    (gim-code-hud/render-init "/some/file.el")
    (let ((buf (get-buffer gim-code-hud--buffer-name)))
      (should buf)
      (with-current-buffer buf
        (should (derived-mode-p 'gim-code-hud-display-mode))))))

(ert-deftest gim-code-hud-test/render-init-inserts-file-name ()
  "render-init inserts the abbreviated file name into the buffer."
  (gim-code-hud-test/with-hud-buffer
    (gim-code-hud/render-init "/some/file.el")
    (with-current-buffer gim-code-hud--buffer-name
      (should (string-match-p "file\\.el" (buffer-string))))))

(ert-deftest gim-code-hud-test/render-init-has-all-sections ()
  "render-init inserts all five GIM_CODE_HUD_ANALYSIS_ID properties."
  (gim-code-hud-test/with-hud-buffer
    (gim-code-hud/render-init "/some/file.el")
    (with-current-buffer gim-code-hud--buffer-name
      (dolist (id '("git-status" "contributors" "co-changes" "purpose" "history"))
        (should (org-find-property "GIM_CODE_HUD_ANALYSIS_ID" id))))))

(ert-deftest gim-code-hud-test/flush-pending-updates-section ()
  "flush-pending replaces the body of the named section."
  (gim-code-hud-test/with-hud-buffer
    (gim-code-hud/render-init "/some/file.el")
    (let ((pending (make-hash-table :test #'equal)))
      (puthash "git-status" (cons "clean" (float-time)) pending)
      (with-current-buffer gim-code-hud--buffer-name
        (gim-code-hud/flush-pending pending))
      (with-current-buffer gim-code-hud--buffer-name
        (goto-char (org-find-property "GIM_CODE_HUD_ANALYSIS_ID" "git-status"))
        (org-end-of-meta-data t)
        (let ((body (buffer-substring-no-properties
                     (point)
                     (progn (outline-next-heading) (point)))))
          (should (string-match-p "clean" body)))))))

(ert-deftest gim-code-hud-test/flush-pending-drains-map ()
  "flush-pending removes processed entries from the map."
  (gim-code-hud-test/with-hud-buffer
    (gim-code-hud/render-init "/some/file.el")
    (let ((pending (make-hash-table :test #'equal)))
      (puthash "purpose" (cons "Does stuff." (float-time)) pending)
      (with-current-buffer gim-code-hud--buffer-name
        (gim-code-hud/flush-pending pending))
      (should (= 0 (hash-table-count pending))))))

(ert-deftest gim-code-hud-test/flush-pending-noop-on-missing-buffer ()
  "flush-pending does not error when the HUD buffer does not exist."
  (when-let ((buf (get-buffer gim-code-hud--buffer-name)))
    (kill-buffer buf))
  (let ((pending (make-hash-table :test #'equal)))
    (puthash "git-status" (cons "clean" (float-time)) pending)
    (should-not (condition-case _ (progn (gim-code-hud/flush-pending pending) nil)
                  (error t)))))

;;;; ─── Two-timer: staleness + push + toggles ────────────────────────────────

;;; section-expired-p

(ert-deftest gim-code-hud-test/staleness-expired-when-no-entry ()
  "Section with no next-update entry is immediately expired."
  (let ((gim-code-hud--next-update (make-hash-table :test #'equal)))
    (should (gim-code-hud--section-expired-p "/some/file.el" "git-status"))))

(ert-deftest gim-code-hud-test/staleness-not-expired-when-future ()
  "Section with next-update in the future is not expired."
  (let ((gim-code-hud--next-update (make-hash-table :test #'equal)))
    (puthash (gim-code-hud--next-update-key "/some/file.el" "git-status")
             (+ (float-time) 3600)
             gim-code-hud--next-update)
    (should-not (gim-code-hud--section-expired-p "/some/file.el" "git-status"))))

(ert-deftest gim-code-hud-test/staleness-expired-when-past ()
  "Section with next-update in the past is expired."
  (let ((gim-code-hud--next-update (make-hash-table :test #'equal)))
    (puthash (gim-code-hud--next-update-key "/some/file.el" "git-status")
             (- (float-time) 1)
             gim-code-hud--next-update)
    (should (gim-code-hud--section-expired-p "/some/file.el" "git-status"))))

;;; mark-updated

(ert-deftest gim-code-hud-test/mark-updated-sets-future-timestamp ()
  "mark-updated sets next-update to now+TTL, making the section non-expired."
  (let ((gim-code-hud--next-update (make-hash-table :test #'equal)))
    (gim-code-hud--mark-updated "/some/file.el" "git-status")
    (should-not (gim-code-hud--section-expired-p "/some/file.el" "git-status"))))

;;; seed-staleness

(ert-deftest gim-code-hud-test/seed-staleness-from-db ()
  "seed-staleness populates next-update from SQLite valid_until values."
  (gim-code-hud-test/with-db db
    (let ((gim-code-hud--db db)
          (gim-code-hud--next-update (make-hash-table :test #'equal))
          (file "/tmp/test-seed.el"))
      (gim-code-hud--db-put db (gim-code-hud--db-key "git-status" file) "clean" 3600)
      (gim-code-hud--seed-staleness file)
      (should-not (gim-code-hud--section-expired-p file "git-status"))
      (should     (gim-code-hud--section-expired-p file "purpose")))))

;;; value-to-string

(ert-deftest gim-code-hud-test/value-to-string-passthrough ()
  "String values for git-status/purpose/history pass through unchanged."
  (should (equal "clean"   (gim-code-hud--value-to-string "git-status"  "clean")))
  (should (equal "Does X." (gim-code-hud--value-to-string "purpose"     "Does X.")))
  (should (equal "Grew."   (gim-code-hud--value-to-string "history"     "Grew."))))

(ert-deftest gim-code-hud-test/value-to-string-contributors ()
  "Contributors alist is formatted as aligned name+count lines."
  (let* ((pairs  '(("Alice Smith" . 12) ("Bob Jones" . 5)))
         (result (gim-code-hud--value-to-string "contributors" pairs)))
    (should (string-match-p "Alice Smith" result))
    (should (string-match-p "12"          result))
    (should (string-match-p "Bob Jones"   result))))

(ert-deftest gim-code-hud-test/value-to-string-co-changes ()
  "Co-changes (total . pairs) is formatted as percentage + org-link lines."
  (let* ((gim-code-hud--current-root "/proj/")
         (value  (cons 10 '(("bar.el" . 5) ("baz.el" . 1))))
         (result (gim-code-hud--value-to-string "co-changes" value)))
    (should (string-match-p "bar.el"      result))
    (should (string-match-p "50%"         result))   ; 5/10 = 50%
    (should (string-match-p "10%"         result))   ; ceil(1/10*100) = 10%
    (should (string-match-p "\\[\\[file:" result))))

(ert-deftest gim-code-hud-test/value-to-string-nil-lists ()
  "nil contributors and co-changes both produce \"(none)\"."
  (should (equal "(none)" (gim-code-hud--value-to-string "contributors" nil)))
  (let ((gim-code-hud--current-root "/proj/"))
    (should (equal "(none)" (gim-code-hud--value-to-string "co-changes" nil)))))

;;; push-result

(ert-deftest gim-code-hud-test/push-result-populates-pending ()
  "push-result adds the formatted value to pending-updates when file matches."
  (gim-code-hud-test/with-db db
    (let ((gim-code-hud--db             db)
          (gim-code-hud--current-file   "/proj/foo.el")
          (gim-code-hud--current-root   "/proj/")
          (gim-code-hud--pending-updates (make-hash-table :test #'equal))
          (gim-code-hud--next-update     (make-hash-table :test #'equal)))
      (gim-code-hud--push-result "/proj/foo.el" "git-status" "clean")
      (should (equal "clean"
                     (car (gethash "git-status" gim-code-hud--pending-updates)))))))

(ert-deftest gim-code-hud-test/push-result-ignores-stale-file ()
  "push-result does nothing when the file no longer matches current-file."
  (gim-code-hud-test/with-db db
    (let ((gim-code-hud--db             db)
          (gim-code-hud--current-file   "/proj/other.el")
          (gim-code-hud--pending-updates (make-hash-table :test #'equal))
          (gim-code-hud--next-update     (make-hash-table :test #'equal)))
      (gim-code-hud--push-result "/proj/foo.el" "git-status" "clean")
      (should (= 0 (hash-table-count gim-code-hud--pending-updates))))))

(ert-deftest gim-code-hud-test/push-result-writes-to-sqlite ()
  "push-result persists the formatted value to the SQLite cache."
  (gim-code-hud-test/with-db db
    (let ((gim-code-hud--db             db)
          (gim-code-hud--current-file   "/proj/foo.el")
          (gim-code-hud--current-root   "/proj/")
          (gim-code-hud--pending-updates (make-hash-table :test #'equal))
          (gim-code-hud--next-update     (make-hash-table :test #'equal)))
      (gim-code-hud--push-result "/proj/foo.el" "git-status" "staged")
      (should (equal "staged"
                     (gim-code-hud--db-get
                      db (gim-code-hud--db-key "git-status" "/proj/foo.el")))))))

;;; in-flight dedup

(ert-deftest gim-code-hud-test/fetch-section-skips-when-in-flight ()
  "fetch-section does not dispatch a second time while one is outstanding."
  (let ((gim-code-hud--in-flight (make-hash-table :test #'equal))
        (calls 0))
    (cl-letf (((symbol-function 'gim-code-hud--do-fetch-section)
               (lambda (_file _section-id) (cl-incf calls))))
      (gim-code-hud--fetch-section "/proj/foo.el" "purpose")
      (gim-code-hud--fetch-section "/proj/foo.el" "purpose")
      (should (= 1 calls)))))

(ert-deftest gim-code-hud-test/fetch-section-marks-in-flight ()
  "fetch-section records the (file . section-id) key as in-flight."
  (let ((gim-code-hud--in-flight (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'gim-code-hud--do-fetch-section)
               (lambda (_file _section-id) nil)))
      (gim-code-hud--fetch-section "/proj/foo.el" "purpose")
      (should (gethash (gim-code-hud--next-update-key "/proj/foo.el" "purpose")
                        gim-code-hud--in-flight)))))

(ert-deftest gim-code-hud-test/push-result-clears-in-flight ()
  "push-result clears the in-flight marker so the next tick can refetch."
  (gim-code-hud-test/with-db db
    (let ((gim-code-hud--db             db)
          (gim-code-hud--current-file   "/proj/foo.el")
          (gim-code-hud--current-root   "/proj/")
          (gim-code-hud--pending-updates (make-hash-table :test #'equal))
          (gim-code-hud--next-update     (make-hash-table :test #'equal))
          (gim-code-hud--in-flight       (make-hash-table :test #'equal)))
      (puthash (gim-code-hud--next-update-key "/proj/foo.el" "git-status")
               t gim-code-hud--in-flight)
      (gim-code-hud--push-result "/proj/foo.el" "git-status" "clean")
      (should-not (gethash (gim-code-hud--next-update-key "/proj/foo.el" "git-status")
                            gim-code-hud--in-flight)))))

(ert-deftest gim-code-hud-test/fetch-section-refetches-after-push-result ()
  "After push-result clears in-flight, fetch-section dispatches again."
  (gim-code-hud-test/with-db db
    (let ((gim-code-hud--db             db)
          (gim-code-hud--current-file   "/proj/foo.el")
          (gim-code-hud--current-root   "/proj/")
          (gim-code-hud--pending-updates (make-hash-table :test #'equal))
          (gim-code-hud--next-update     (make-hash-table :test #'equal))
          (gim-code-hud--in-flight       (make-hash-table :test #'equal))
          (calls 0))
      (cl-letf (((symbol-function 'gim-code-hud--do-fetch-section)
                 (lambda (_file _section-id) (cl-incf calls))))
        (gim-code-hud--fetch-section "/proj/foo.el" "git-status")
        (gim-code-hud--push-result "/proj/foo.el" "git-status" "clean")
        (gim-code-hud--fetch-section "/proj/foo.el" "git-status")
        (should (= 2 calls))))))

;;; HUD visibility

(ert-deftest gim-code-hud-test/hud-visible-false-when-no-buffer ()
  "hud-visible-p returns nil when the HUD buffer does not exist."
  (when-let ((buf (get-buffer gim-code-hud--buffer-name)))
    (kill-buffer buf))
  (should-not (gim-code-hud--hud-visible-p)))

;;; Timer toggles

(ert-deftest gim-code-hud-test/toggle-staleness-timer-on-off ()
  "toggle-staleness-timer returns t when turning on and nil when turning off."
  (let ((gim-code-hud--staleness-timer nil))
    (unwind-protect
        (progn
          (should (eq t   (gim-code-hud/toggle-staleness-timer)))
          (should (eq nil (gim-code-hud/toggle-staleness-timer))))
      (when gim-code-hud--staleness-timer
        (cancel-timer gim-code-hud--staleness-timer)
        (setq gim-code-hud--staleness-timer nil)))))

(ert-deftest gim-code-hud-test/toggle-flush-timer-on-off ()
  "toggle-flush-timer returns t when turning on and nil when turning off."
  (let ((gim-code-hud--flush-timer nil))
    (unwind-protect
        (progn
          (should (eq t   (gim-code-hud/toggle-flush-timer)))
          (should (eq nil (gim-code-hud/toggle-flush-timer))))
      (when gim-code-hud--flush-timer
        (cancel-timer gim-code-hud--flush-timer)
        (setq gim-code-hud--flush-timer nil)))))

;;;; ─── Ad-hoc sections ───────────────────────────────────────────────────────

(ert-deftest gim-code-hud-test/section-ttl-override-absent ()
  "section-ttl-override returns nil when the property is not set."
  (gim-code-hud-test/with-hud-buffer
    (gim-code-hud/render-init "/some/file.el")
    (should (null (gim-code-hud--section-ttl-override "git-status")))))

(ert-deftest gim-code-hud-test/section-ttl-override-present ()
  "section-ttl-override returns the integer value of GIM_CODE_HUD_TTL_SECONDS."
  (gim-code-hud-test/with-hud-buffer
    (let ((gim-code-hud-org-template-suffix
           "\n** Custom\n:PROPERTIES:\n:GIM_CODE_HUD_ANALYSIS_ID: custom\n:GIM_CODE_HUD_CLI_COMMAND: true\n:GIM_CODE_HUD_TTL_SECONDS: 120\n:END:\n\n(loading…)\n"))
      (gim-code-hud/render-init "/some/file.el")
      (should (= 120 (gim-code-hud--section-ttl-override "custom"))))))

(ert-deftest gim-code-hud-test/effective-ttl-uses-override ()
  "effective-ttl returns the property value when GIM_CODE_HUD_TTL_SECONDS is set."
  (gim-code-hud-test/with-hud-buffer
    (let ((gim-code-hud-org-template-suffix
           "\n** Custom\n:PROPERTIES:\n:GIM_CODE_HUD_ANALYSIS_ID: custom\n:GIM_CODE_HUD_CLI_COMMAND: true\n:GIM_CODE_HUD_TTL_SECONDS: 300\n:END:\n\n(loading…)\n"))
      (gim-code-hud/render-init "/some/file.el")
      (should (= 300 (gim-code-hud--effective-ttl "custom"))))))

(ert-deftest gim-code-hud-test/effective-ttl-falls-back-to-default ()
  "effective-ttl falls back to gim-code-hud--db-ttl when no property is set."
  (gim-code-hud-test/with-hud-buffer
    (gim-code-hud/render-init "/some/file.el")
    (should (= (gim-code-hud--db-ttl "git-status")
               (gim-code-hud--effective-ttl "git-status")))))

(ert-deftest gim-code-hud-test/expand-cli-command ()
  "expand-cli-command substitutes {active_file_path} in the template."
  (should (equal "claude -p /proj/foo.el"
                 (gim-code-hud--expand-cli-command
                  "claude -p {active_file_path}" "/proj/foo.el"))))

(ert-deftest gim-code-hud-test/ad-hoc-sections-empty-when-no-buffer ()
  "ad-hoc-sections returns nil when the HUD buffer does not exist."
  (when-let ((buf (get-buffer gim-code-hud--buffer-name)))
    (kill-buffer buf))
  (should (null (gim-code-hud--ad-hoc-sections))))

(ert-deftest gim-code-hud-test/ad-hoc-sections-discovers-cli-command ()
  "ad-hoc-sections returns (id . command) for headings with CLI_COMMAND property."
  (gim-code-hud-test/with-hud-buffer
    (let ((gim-code-hud-org-template-suffix
           "\n** My Analysis\n:PROPERTIES:\n:GIM_CODE_HUD_ANALYSIS_ID: my-analysis\n:GIM_CODE_HUD_CLI_COMMAND: echo {active_file_path}\n:END:\n\n(loading…)\n"))
      (gim-code-hud/render-init "/some/file.el")
      (let ((sections (gim-code-hud--ad-hoc-sections)))
        (should (= 1 (length sections)))
        (should (equal "my-analysis" (caar sections)))
        (should (string-match-p "echo" (cdar sections)))))))

(ert-deftest gim-code-hud-test/ad-hoc-sections-ignores-builtin-sections ()
  "ad-hoc-sections does not return built-in sections (they lack CLI_COMMAND)."
  (gim-code-hud-test/with-hud-buffer
    (gim-code-hud/render-init "/some/file.el")
    (should (null (gim-code-hud--ad-hoc-sections)))))

(ert-deftest gim-code-hud-test/all-section-ids-includes-builtins ()
  "all-section-ids always contains the five built-in section IDs."
  (gim-code-hud-test/with-hud-buffer
    (gim-code-hud/render-init "/some/file.el")
    (let ((ids (gim-code-hud--all-section-ids)))
      (dolist (id '("git-status" "contributors" "co-changes" "purpose" "history"))
        (should (member id ids))))))

(ert-deftest gim-code-hud-test/all-section-ids-includes-ad-hoc ()
  "all-section-ids appends ad-hoc sections found in the HUD buffer."
  (gim-code-hud-test/with-hud-buffer
    (let ((gim-code-hud-org-template-suffix
           "\n** Extra\n:PROPERTIES:\n:GIM_CODE_HUD_ANALYSIS_ID: extra\n:GIM_CODE_HUD_CLI_COMMAND: true\n:END:\n\n(loading…)\n"))
      (gim-code-hud/render-init "/some/file.el")
      (should (member "extra" (gim-code-hud--all-section-ids))))))

(provide 'gim-code-hud-tests)
;;; gim-code-hud-tests.el ends here
