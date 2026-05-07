#!/bin/bash

# Warm up the *ert* buffer with a single sync test before running the full
# suite.  Without this, ert creates the *ert* buffer mid-run (while the first
# async test's subprocess is in flight), and the display update starves the
# process sentinel for one event-loop tick, causing spurious timeouts.

emacsclient -e '
    (save-window-excursion
      (load "./gim-code-hud-db.el")
      (load "./gim-code-hud-git.el")
      (load "./gim-code-hud-llm.el")
      (load "./gim-code-hud-render.el")
      (load "./gim-code-hud.el")
      (load "./gim-code-hud-tests.el")
      (ert-run-tests-interactively "gim-code-hud-test/harness-commit")
      (ert-run-tests-interactively t)
      (with-current-buffer "*ert*"
        (buffer-substring-no-properties (point-min) (point-max))))
'
