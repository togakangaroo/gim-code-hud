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
    (clrhash gim-code-hud--cache)
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
  "File committed alone every time has no co-change partners."
  (gim-code-hud-test/with-repo repo
    (gim-code-hud-test/commit repo "foo.el" "v1" "Commit 1")
    (gim-code-hud-test/commit repo "foo.el" "v2" "Commit 2")
    (let ((file (expand-file-name "foo.el" repo)))
      (should (null (gim-code-hud-test/call-sync
                     #'gim-code-hud--co-changes-async file))))))

(ert-deftest gim-code-hud-test/co-changes-async-with-partners ()
  "Files committed together are returned with correct counts, sorted descending."
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
                    #'gim-code-hud--co-changes-async file)))
      (should (equal "bar.el" (caar result)))   ; bar first, count 2
      (should (= 2 (cdar result)))
      (should (= 1 (cdr (assoc "baz.el" result))))
      (should (null (assoc "foo.el" result))))))

(ert-deftest gim-code-hud-test/co-changes-async-excludes-self ()
  "Target file never appears in its own co-change list."
  (gim-code-hud-test/with-repo repo
    (let ((default-directory repo))
      (with-temp-file (expand-file-name "foo.el" repo) (insert "x"))
      (with-temp-file (expand-file-name "bar.el" repo) (insert "y"))
      (call-process "git" nil nil nil "add" ".")
      (call-process "git" nil nil nil "commit" "-m" "Init"))
    (let* ((file   (expand-file-name "foo.el" repo))
           (result (gim-code-hud-test/call-sync
                    #'gim-code-hud--co-changes-async file)))
      (should (null (assoc "foo.el" result))))))

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

(provide 'gim-code-hud-tests)
;;; gim-code-hud-tests.el ends here
