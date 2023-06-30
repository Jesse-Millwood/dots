;;; transient-lsp.el --- Transient Menu for LSP -*- lexical-binding: t -*-

;; Author: Jesse Millwood
;; Maintainer: Jesse Millwood
;; Version: 0.1
;; Package-Requires: (transient lsp)
;; Keywords: transient lsp


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; Provides a transient menu for LSP
;; TODO:
;;  - [ ] Support prefixes to the jump commands
;;    - Allow to specify if the buffer should be opened in other window
;;  - [ ] Support starting lsp in buffer that is jumped to

;;; Code:

(require 'transient)
(require 'lsp-treemacs)

(transient-define-prefix transient-lsp-workspace ()
  ["Workspace"
   [("s" "Start LSP server." lsp)
    ("r" "Restart language server" lsp-workspace-restart)
    ("q" "Shutdown language server" lsp-workspace-shutdown)
    ("d" "Describes current session" lsp-describe-session)
    ("D" "Disconnect the buffer from the server." lsp-disconnect)]
   ])

(transient-define-prefix transient-lsp-format ()
  ["LSP Format"
   [("=" "Ask the server to format this document." lsp-format-buffer)
    ("r" "Ask the server to format the region or line." lsp-format-region)]])

(transient-define-prefix transient-lsp-project-roots ()
  ["LSP Project Roots"
   [("a" "Add new project root to the list of workspace folders." lsp-workspace-folders-add)
    ("r" "Remove project root from the list of workspace folders." lsp-workspace-folders-remove)
    ("b" "Remove project root from the workspace blacklist." lsp-workspace-blacklist-remove)]]
  )

(transient-define-prefix transient-lsp-toggle ()
  ["LSP Toggle Features"
   [("l" "Toggle code-lens overlays." lsp-lens-mode)
    ("L" "Toggle client-server protocol logging." lsp-toggle-trace-io)
    ("h" "Toggle symbol highlighting." lsp-toggle-symbol-highlight)
    ("S" "Toggle show info in sideline" lsp-ui-sideline-mode)
    ("d" "Toggle showing hover information in child frame." lsp-ui-doc-mode)
    ("s" "Toggle signature auto activate." lsp-toggle-signature-auto-activate)
    ("f" "Toggle on type formatting." lsp-toggle-on-type-formatting)
    ("T" "Toggle syncing lsp  workspace and treemacs ." lsp-treemacs-sync-mode)]])

(transient-define-prefix transient-lsp-find ()
  ["LSP Find/Jump"
   [("g" "Find definitions of the symbol under point." lsp-find-definition)
    ("r" "Find references of the symbol under point." lsp-find-references)
    ("i" "Find implementations of the symbol under point." lsp-find-implementation)
    ("t" "Find type definitions of the symbol under point." lsp-find-type-definition)
    ("d" "Find declarations of the symbol under point." lsp-find-declaration)
    ("h" "Show the incoming call hierarchy for the symbol at point" lsp-treemacs-call-hierarchy)
    ("a" "Find all meaningful symbols that match pattern." xref-find-apropos)
    ("," "Return to previous spot before last jump" xref-pop-marker-stack)]])

(transient-define-prefix transient-lsp-doc ()
  ["LSP Help/Docs"
   [("h" "Display the type signature and documentation of the thing at point" lsp-describe-thing-at-point)
    ("s" "Activate signature help." lsp-signature-activate)
    ("g" "Trigger display hover information popup and hide it on next typing." lsp-ui-doc-glance)]])

(transient-define-prefix transient-lsp-refactor ()
  ["LSP Rename/Reorganize"
   [("r" "Rename the symbol (and all references to it)." lsp-rename)
    ("o" "Perform the source.organizeImports code action, if available." lsp-organize-imports)]]
  )

(transient-define-prefix transient-lsp-interact ()
  ["LSP Code Action/Lens/Highlight"
   [("a" "Execute code action." lsp-execute-code-action)
    ("l" "Click lsp lens using ‘avy’ package." lsp-avy-lens)
    ("h" "Highlight symbol at point." lsp-document-highlight)]])

(transient-define-prefix transient-lsp-peek ()
  ["LSP Peek"
   [("g" "Peek definitions to the identifier at point." lsp-ui-peek-find-definitions)
    ("r" "Peek references to the identifier at point." lsp-ui-peek-find-references)
    ("i" "Peek implementation locations of the symbol at point." lsp-ui-peek-find-implementation)
    ("s" "Peek symbols in the workspace" lsp-ui-peek-find-workspace-symbol)]])

(transient-define-prefix transient-lsp-dispatch ()
  "Invoke an LSP command from a list of available commands"
  ["Transient LSP Commands"
   [("w" "Workspace" transient-lsp-workspace)
    ("=" "Format" transient-lsp-format)
    ("F" "Project Roots" transient-lsp-project-roots)
    ("T" "Toggle features" transient-lsp-toggle)
    ("g" "Find elements" transient-lsp-find)
    ("h" "Documentation/Help" transient-lsp-doc)
    ("r" "Rename/Reorganize" transient-lsp-refactor)
    ("a" "Code Action/Lens/Highlight" transient-lsp-interact)
    ("G" "Peek" transient-lsp-peek)]])

(provide 'transient-lsp)

;;; transient-lsp.el ends here
