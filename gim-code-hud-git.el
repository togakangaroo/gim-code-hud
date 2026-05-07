;;; -*- lexical-binding: t -*-
;;; gim-code-hud-git.el --- Git analysis for gim-code-hud

(require 'cl-lib)
(require 'dash)
(require 'async)

;;; Internal async helper

(defun gim-code-hud--git-async (dir args finish-fn)
  "Run git --no-pager with ARGS in DIR; call FINISH-FN with output string."
  (let ((default-directory (file-name-as-directory dir)))
    (apply #'async-start-process
           "gim-code-hud-git" "git"
           (lambda (proc)
             (let ((output (with-current-buffer (process-buffer proc)
                             (buffer-string))))
               (kill-buffer (process-buffer proc))
               (funcall finish-fn output)))
           "--no-pager" args)))

;;; Git status

(defun gim-code-hud--parse-git-status (output)
  "Parse raw `git status --porcelain` OUTPUT into a status symbol string."
  (if (string-empty-p (string-trim output))
      "clean"
    (let ((code (substring output 0 (min 2 (length output)))))
      (cond
       ((string-match-p "\\?" code) "untracked")
       ((string-match-p "^[MADRC]" code) "staged")
       (t "dirty")))))

(defun gim-code-hud--git-status-async (file callback)
  "Call CALLBACK with the git status string for FILE."
  (gim-code-hud--git-async
   (file-name-directory file)
   (list "status" "--porcelain" (file-name-nondirectory file))
   (lambda (output)
     (funcall callback (gim-code-hud--parse-git-status output)))))

;;; Contributors

(defun gim-code-hud--parse-contributors (output)
  "Parse raw `git log --format=%an` OUTPUT into a sorted (AUTHOR . COUNT) alist."
  (let* ((lines (-remove #'string-empty-p (split-string output "\n")))
         (counts (cl-loop with tbl = (make-hash-table :test #'equal)
                          for author in lines
                          do (puthash author (1+ (gethash author tbl 0)) tbl)
                          finally return tbl))
         (pairs (cl-loop for k being the hash-keys of counts
                         collect (cons k (gethash k counts)))))
    (-sort (lambda (a b) (> (cdr a) (cdr b))) pairs)))

(defun gim-code-hud--contributors-async (file callback)
  "Call CALLBACK with a sorted (AUTHOR . COUNT) alist for FILE."
  (gim-code-hud--git-async
   (file-name-directory file)
   (list "log" "--follow" "--format=%an" (file-name-nondirectory file))
   (lambda (output)
     (funcall callback (gim-code-hud--parse-contributors output)))))

;;; Co-change partners

(defun gim-code-hud--parse-co-changes (rel output)
  "Parse `git log --name-only --format=COMMIT` OUTPUT for file REL.
Returns a sorted (PARTNER . COUNT) alist excluding REL itself."
  (let* ((blocks (split-string output "COMMIT\n" t))
         (counts (make-hash-table :test #'equal)))
    (dolist (block blocks)
      (let ((partners (->> (split-string block "\n" t)
                           (-map #'string-trim)
                           (-remove #'string-empty-p)
                           (-remove (lambda (f) (string= f rel))))))
        (dolist (p partners)
          (puthash p (1+ (gethash p counts 0)) counts))))
    (-sort (lambda (a b) (> (cdr a) (cdr b)))
           (cl-loop for k being the hash-keys of counts
                    collect (cons k (gethash k counts))))))

(defun gim-code-hud--co-changes-async (file callback)
  "Call CALLBACK with (TOTAL . PAIRS) for FILE, or nil if FILE has no commits.
TOTAL is the number of commits touching FILE; PAIRS is a sorted
\((PARTNER-FILE . COUNT)) alist.  COUNT / TOTAL gives the co-change rate."
  (let ((dir (file-name-directory file))
        (rel (file-name-nondirectory file)))
    (gim-code-hud--git-async
     dir
     (list "log" "--follow" "--pretty=tformat:%H" "--" rel)
     (lambda (sha-output)
       (let ((shas (-remove #'string-empty-p (split-string sha-output "\n"))))
         (if (null shas)
             (funcall callback nil)
           (gim-code-hud--git-async
            dir
            (append (list "log" "--no-walk" "--name-only" "--pretty=tformat:COMMIT") shas)
            (lambda (output)
              (funcall callback
                       (cons (length shas)
                             (gim-code-hud--parse-co-changes rel output)))))))))))

(provide 'gim-code-hud-git)
;;; gim-code-hud-git.el ends here
