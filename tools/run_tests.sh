#!/bin/bash

emacsclient -e '(progn
  (load "/Users/georgemauer/code/gim-code-hud/gim-code-hud-git.el")
  (load "/Users/georgemauer/code/gim-code-hud/gim-code-hud-llm.el")
  (load "/Users/georgemauer/code/gim-code-hud/gim-code-hud-render.el")
  (load "/Users/georgemauer/code/gim-code-hud/gim-code-hud.el")
  (load "/Users/georgemauer/code/gim-code-hud/gim-code-hud-tests.el")
  (ert-run-tests-interactively t)
  (with-current-buffer "*ert*"
    (buffer-substring-no-properties (point-min) (point-max))))'
