;;; evil-org-agenda.el --- evil keybindings for org-agenda-mode

;; Copyright (C) 2012-2017 by Somelauw
;; Maintainer: Somelauw
;; Original-author: Edward Tjörnhammar
;; URL: https://github.com/Somelauw/evil-org-mode.git
;; Git-Repository: git://github.com/Somelauw/evil-org-mode.git
;; Created: 2012-06-14
;; Forked-since: 2017-02-12
;; Version: 0.9.6
;; Package-Requires: ((emacs "24.4") (evil "1.0") (org "8.0.0"))
;; Keywords: evil vim-emulation org-mode key-bindings presets

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;; Minimal config
;; (add-hook 'org-mode-hook 'evil-org-mode)

;;; Commentary:
;;
;; Known Bugs:
;; See, https://github.com/Somelauw/evil-org-mode/issues
;;
;;; Code:

(defun evil-org-agenda-set-keys ()
  "Set motion state keys for `org-agenda'."
  (evil-set-initial-state 'org-agenda-mode 'motion)

  ;; Horizontal movements have little use, so we override "f" and "t".
  ;; "w", "b", "e", "ge" and their upcase counterparts are preserved.
  (evil-define-key 'motion org-agenda-mode-map
    ;; TODO: Unused keys: D, X, o, p, P

    (kbd "<tab>") 'org-agenda-goto
    (kbd "<return>") 'org-agenda-switch-to
    (kbd "S-<return>") 'org-agenda-recenter

    (kbd "SPC") 'org-agenda-show-and-scroll-up
    (kbd "<delete>") 'org-agenda-show-scroll-down
    (kbd "<backspace>") 'org-agenda-show-scroll-down

    ;; Motion
    "j" 'org-agenda-next-line
    "k" 'org-agenda-previous-line
    (kbd "C-j") 'org-agenda-next-item
    (kbd "C-k") 'org-agenda-previous-item
    (kbd "[") 'org-agenda-earlier
    (kbd "]") 'org-agenda-later

    ;; Manipulation
    ;; We follow standard org-mode bindings (not org-agenda bindings):
    ;; <HJKL> change todo items and priorities.
    ;; M-<jk> drag lines.
    ;; M-<hl> cannot demote/promote, we use it for "do-date".
    "J" 'org-agenda-priority-down
    "K" 'org-agenda-priority-up
    "H" 'evil-org-agenda-previous-todo-keyword
    "L" 'evil-org-agenda-next-todo-keyword
    "t" 'org-agenda-todo
    (kbd "M-j") 'org-agenda-drag-line-forward
    (kbd "M-k") 'org-agenda-drag-line-backward
    (kbd "M-h") 'org-agenda-do-date-earlier
    (kbd "M-l") 'org-agenda-do-date-later

    ;; Undo
    "u" 'org-agenda-undo
    "U" 'org-agenda-redo-all

    ;; Actions
    "dd" 'org-agenda-kill
    "dA" 'org-agenda-archive
    "da" 'org-agenda-archive-default-with-confirmation
    "st" 'org-agenda-set-tags
    "se" 'org-agenda-set-effort
    "S" 'org-timer-set-timer
    "i" 'org-agenda-diary-entry
    "a" 'org-agenda-add-note
    "A" 'org-agenda-append-agenda
    "C" 'org-agenda-capture

    ;; Marking
    "m" 'org-agenda-bulk-toggle
    "*" 'org-agenda-bulk-toggle-all
    "%" 'org-agenda-bulk-mark-regexp
    "M" 'org-agenda-bulk-remove-all-marks
    "x" 'org-agenda-bulk-action

    ;; Refresh
    (kbd "gr") 'org-agenda-redo

    ;; Quit
    "ZQ" 'org-agenda-exit
    "ZZ" 'org-agenda-quit

    ;; Display
    ;; "Dispatch" can prefix the following:
    ;; 'org-agenda-toggle-deadlines
    ;; 'org-agenda-toggle-diary
    ;; 'org-agenda-follow-mode
    ;; 'org-agenda-log-mode
    ;; 'org-agenda-entry-text-mode
    ;; 'org-agenda-toggle-time-grid
    ;; 'org-agenda-day-view
    ;; 'org-agenda-week-view
    ;; 'org-agenda-year-view
    "z" 'org-agenda-view-mode-dispatch
    "ZD" 'org-agenda-dim-blocked-tasks

    ;; Filter
    ;; TODO: What's the common binding for filtering?
    "fc" 'org-agenda-filter-by-category
    "fr" 'org-agenda-filter-by-regexp
    "fe" 'org-agenda-filter-by-effort
    "ft" 'org-agenda-filter-by-tag
    "fu" 'org-agenda-filter-remove-all
    "f^" 'org-agenda-filter-by-top-headline
    ;; TODO: Better used for toggle states.  What's the common binding for limiting?
    ;; Same as for filtering?
    "~" 'org-agenda-limit-interactively

    ;; Clock
    "I" 'org-agenda-clock-in ; Original binding
    "O" 'org-agenda-clock-out ; Original binding
    "cg" 'org-agenda-clock-goto
    "cc" 'org-agenda-clock-cancel
    "cr" 'org-agenda-clockreport-mode

    ;; Go and show
    "." 'org-agenda-goto-today ; TODO: What about evil-repeat?
    "gc" 'org-agenda-goto-calendar
    "gC" 'org-agenda-convert-date
    "gd" 'org-agenda-goto-date
    "gh" 'org-agenda-holidays
    "gm" 'org-agenda-phases-of-moon
    "gs" 'org-agenda-sunrise-sunset
    "gt" 'org-agenda-show-tags

    ;; TODO: Work out the following.
    ;; 'org-agenda-date-prompt
    ;; 'org-agenda-show-the-flagging-note
    ;; 'org-save-all-org-buffers

    ;; Others
    "+" 'org-agenda-manipulate-query-add
    "-" 'org-agenda-manipulate-query-subtract))

(provide 'evil-org-agenda)
;;; evil-org-agenda.el ends here
