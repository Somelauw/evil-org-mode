;;; evil-org.el --- evil keybindings for org-mode

;; Copyright (C) 2012-2017 by Somelauw
;; Maintainer: Somelauw
;; Original-author: Edward Tjörnhammar
;; URL: https://github.com/Somelauw/evil-org.git
;; Git-Repository: git://github.com/Somelauw/evil-org.git
;; Created: 2012-06-14
;; Forked-since: 2017-02-12
;; Version: 0.6.2
;; Package-Requires: ((emacs "24.4") (evil "0") (org "8.0.0"))
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

;;; Commentary:
;;
;; Known Bugs:
;; See, https://github.com/Somelauw/evil-org/issues
;;
;;; Code:
(eval-when-compile
  (require 'let-alist))
(require 'evil)
(require 'org)
(require 'org-element)
(require 'org-table)
(require 'leader nil 'noerror)

(defgroup evil-org nil
  "Provides integration of org-mode and evil."
  :group 'org
  :prefix "evil-org-")

;;; Customizations
(defcustom evil-org-movement-bindings
  '((up . "k")
    (down . "j")
    (left . "h")
    (right . "l"))
  "AList of normal keys to use for arrows.

   This can be used by non-qwerty users who don't use hjkl."
  :group 'evil-org
  :type '(alist :key-type symbol :value-type string)
  :options '(up down left right))

(defcustom evil-org-use-additional-insert
  nil
  "Whether additional keybindings should also be available in insert mode."
  :group 'evil-org)

(defcustom evil-org-key-theme
  (if (and (boundp 'evil-disable-insert-state-bindings)
           (evil-disable-insert-state-bindings))
      '(textobjects navigation additional)
    '(textobjects navigation insert additional))
  "Which key themes to enable.
If you use this variable, you should call `evil-org-set-key-theme' with zero
arguments."
  :group 'evil-org
  :type '(set (const navigation)
              (const textobjects)
              (const insert)
              (const rsi)
              (const additional)
              (const shift)
              (const todo)
              (const heading)
              (const leader)))

(add-hook 'org-mode-hook 'evil-org-mode) ;; only load with org-mode

;;; Variable declarations
(defvar browse-url-generic-program)
(defvar browse-url-generic-args)
(defvar evil-disable-insert-state-bindings)
(defvar org-capture-mode-map)

(defvar evil-org-mode-map (make-sparse-keymap))

;;;###autoload
(define-minor-mode evil-org-mode
  "Buffer local minor mode for evil-org"
  :init-value nil
  :lighter " EvilOrg"
  :keymap evil-org-mode-map
  :group 'evil-org
)

(defun evil-org-eol-call (fun)
  "Go to end of line and call provided function.
FUN function callback"
  (end-of-visible-line)
  (funcall fun)
  (evil-append nil))

(defun evil-org-bol-call (fun)
  "Go to beginning of line and call provided function.
FUN function callback"
  (beginning-of-visual-line)
  (funcall fun)
  (evil-append nil))

(defun evil-org-open-below (count)
  "Clever insertion of org item.
Argument COUNT number of lines to insert."
  (interactive "p")
  (cond ((org-at-table-p)
         (org-table-insert-row '(4))
         (evil-insert count))
        ((org-in-item-p)
         (end-of-visible-line)
         (org-insert-item)
         (evil-append count))
        (t (evil-open-below count))))

(defun evil-org-open-above (count)
  "Clever insertion of org item.
Argument COUNT number of lines to insert."
  (interactive "p")
  (cond ((org-at-table-p)
         (org-table-insert-row)
         (evil-insert count))
        ((org-in-item-p)
         (beginning-of-visual-line)
         (org-insert-item)
         (evil-append count))
        (t (evil-open-above count))))

(defun evil-org-insert-below (count)
  "Insert heading or item below.
Argument COUNT number of lines to insert."
  (interactive "p")
  (end-of-visible-line)
  (org-meta-return)
  (evil-append count))

(defun evil-org-insert-subheading (count)
  "Insert new subheading."
  (interactive "p")
   (end-of-visible-line)
   (org-insert-heading)
   (org-metaright)
   (evil-append count))

(defun evil-org-insert-subtodo (count)
  "Insert new todo subheading."
  (interactive "p")
   (end-of-visible-line)
   (org-insert-todo-heading nil)
   (org-metaright)
   (evil-append count))

;;; motion declarations
(evil-declare-motion 'org-beginning-of-line)
(evil-declare-motion 'org-end-of-line)
(evil-declare-motion 'org-backward-sentence)
(evil-declare-motion 'org-forward-sentence)
(evil-declare-motion 'org-backward-paragraph)
(evil-declare-motion 'org-forward-paragraph)

;; heading
(evil-declare-motion 'org-backward-heading-same-level)
(evil-declare-motion 'org-forward-heading-same-level)
(evil-declare-motion 'org-previous-visible-heading)
(evil-declare-motion 'org-next-visible-heading)

;; elements
(evil-declare-motion 'org-backward-element)
(evil-declare-motion 'org-forward-element)
(evil-declare-motion 'org-up-element)
(evil-declare-motion 'org-down-element)

;; items
(evil-declare-motion 'org-previous-item)
(evil-declare-motion 'org-next-item)
(evil-declare-motion 'org-beginning-of-item)
(evil-declare-motion 'org-end-of-item)
(evil-declare-motion 'org-beginning-of-item-list)
(evil-declare-motion 'org-end-of-item-list)

;; blocks
(evil-declare-motion 'org-previous-block)
(evil-declare-motion 'org-next-block)

;; table
(evil-declare-motion 'org-table-previous-row)
(evil-declare-motion 'org-table-next-row)
(evil-declare-motion 'org-table-previous-field)
(evil-declare-motion 'org-table-next-field)
(evil-declare-motion 'org-table-beginning-of-field)
(evil-declare-motion 'org-table-end-of-field)

;;; non-repeatible
(evil-declare-change-repeat 'org-cycle)
(evil-declare-change-repeat 'org-shifttab)
(evil-declare-change-repeat 'org-table-end-of-field)

;;; new motions
(evil-define-motion evil-org-forward-sentence (count)
  "In a table go to next cell, otherwise go to next sentence."
  :type exclusive
  :jump t
  (interactive "p")
  (if (org-at-table-p)
      (org-table-end-of-field count)
    (evil-forward-sentence-begin count)))

(evil-define-motion evil-org-backward-sentence (count)
  "In a table go to previous cell, otherwise go to previous sentence."
  :type exclusive
  :jump t
  (interactive "p")
  (if (org-at-table-p)
      (org-table-beginning-of-field count)
    (evil-backward-sentence-begin count)))

(evil-define-motion evil-org-top ()
  "Find the nearest one-star heading."
  :type exclusive
  :jump t
  (while (org-up-heading-safe)))

;;; operators
(evil-define-operator evil-org-demote-or-indent (beg end count)
  "Demote or indent selection (dwim)."
  :move-point nil
  (interactive "<r><vc>")
  (when (null count) (setq count 1))
  (cond
   ;; Work with subtrees and headings
   ((org-with-limited-levels
     (or (org-at-heading-p)
         (save-excursion (goto-char beg) (org-at-heading-p))))
    (if (> count 0)
        (org-map-region 'org-do-demote beg end)
      (org-map-region 'org-do-promote beg end)))
   ;; Work with items
   ((or (org-at-item-p)
        (save-excursion (goto-char beg) (org-at-item-p)))
    (evil-org-indent-items beg end count))
   ;; Default indentation
   (t
    ;; special casing tables
    (when (and (not (region-active-p)) (org-at-table-p))
      (setq beg (min beg (org-table-begin)))
      (setq end (max end (org-table-end))))
    (evil-shift-right beg end count))))

(evil-define-operator evil-org-promote-or-dedent (beg end count)
  "Promote or dedent selection (dwim)."
  (interactive "<r><vc>")
  (evil-org-demote-or-indent beg end (- (or count 1))))

(defun evil-org-indent-items (beg end count)
  "Indent all selected items in itemlist."
  (when (null count) (setq count 1))
  (let* ((struct (save-excursion (goto-char beg) (org-list-struct)))
         (region-p (region-active-p)))
    ;; special case: indenting all items
    (if (and struct org-list-automatic-rules (not region-p)
             (= (point-at-bol) (org-list-get-top-point struct)))
        (org-list-indent-item-generic count nil struct)
      ;; indenting selected items
      (save-excursion
        (when region-p (deactivate-mark))
        (set-mark beg)
        (goto-char end)
        (org-list-indent-item-generic count t struct)))))

(evil-define-operator evil-org-delete-char (count beg end type register)
  "Combine evil-delete-char with org-delete-char"
  :motion evil-forward-char
  (interactive "p<R><x>")
  (if (evil-visual-state-p)             ; No special support for visual state
      (evil-delete-char beg end type register)
    (evil-set-register ?- (filter-buffer-substring beg end))
    (evil-yank beg end type register)
    (org-delete-char count)))

(evil-define-operator evil-org-delete-backward-char (count beg end type register)
  "Combine evil-delete-backward-char with org-delete-backward-char"
  :motion evil-backward-char
  (interactive "p<R><x>")
  (if (evil-visual-state-p)             ; No special support for visual state
      (evil-delete-backward-char beg end type register)
    (evil-set-register ?- (filter-buffer-substring beg end))
    (evil-yank beg end type register)
    (org-delete-backward-char count)))

(evil-define-operator evil-org-recompute-clocks (beg end type register yank-handler)
  "Recompute clocks in visual selection."
  :keep-visual t
  :move-point nil
  (interactive "<r>")
  (progn
    (message "start!")
    (save-excursion
      (while (< (point) end)
        (org-evaluate-time-range)
        (forward-line)
        (message "at position %S" (point))))))

(defun evil-org-generic-open-links (beg end incog)
  "Open org mode links in visual selection.
Argument BEG beginning of region.
Argument END end of region.
Argument INCOG whether to open in incognito mode."
  (progn
    (save-excursion
      (goto-char beg)
      (catch 'break
        (while t
          (org-next-link)
          ;; break from outer loop when there are no more
          ;; org links
          (when (or (not (< (point) end))
                    (not (null org-link-search-failed)))
            (throw 'break 0))
          (if (not (null incog))
              (let* ((new-arg
                      ;; if incog is true, decide which incognito settings to
                      ;; use dependening on the browser
                      (cond ((not (null (string-match "^.*\\(iceweasel\\|firefox\\).*$" browse-url-generic-program))) "--private-window")
                            ((not (null (string-match "^.*\\(chrome\\|chromium\\).*$" browse-url-generic-program))) "--incognito")
                            (t "")))
                     (old-b (list browse-url-generic-args " "))
                     (browse-url-generic-args (add-to-ordered-list 'old-b new-arg 0)))
                (org-open-at-point))
            (let ((browse-url-generic-args '("")))
              (org-open-at-point))))))))

(evil-define-operator evil-org-open-links (beg end &optional count)
  "Open links in visual selection.
If a prefix argument is given, links are opened in incognito mode."
  :keep-visual t
  :move-point nil
  (interactive "<r><vc>")
  (evil-org-generic-open-links beg end (not (null count))))

(evil-define-operator evil-org-open-links-incognito (beg end)
  "Open links in visual selection in incognito mode."
  :keep-visual t
  :move-point nil
  (interactive "<r>")
  (evil-org-generic-open-links beg end t))

;;; text-objects
(evil-define-text-object org-element-textobj (count &optional beg end type)
  "An org element."
  (let ((element (org-element-at-point)))
    (list (org-element-property :begin element)
          (org-element-property :end element))))

(evil-define-text-object org-subtree-textobj (count &optional beg end type)
  "An org subtree."
  (org-with-limited-levels
   (cond ((org-at-heading-p) (beginning-of-line))
         ((org-before-first-heading-p) (user-error "Not in a subtree"))
         (t (outline-previous-visible-heading 1))))
  (when count (while (and (> count 1) (org-up-heading-safe)) (cl-decf count)))
  (let ((element (org-element-at-point)))
    (list (org-element-property :begin element)
          (org-element-property :end element))))

(evil-define-text-object evil-org-table-inner-cell (count &optional beg end type)
  "Inner org table cell."
  (save-excursion
    (when (not (looking-back "|\\s-?")) (org-table-beginning-of-field 1))
    (let* ((b (point))
           (e (progn (when (looking-at "\\s-*|") ; empty cells
                       (right-char)
                       (setq count (1- count)))
                     (when (> count 0) (org-table-end-of-field count))
                     (point))))
      (list b e))))

(evil-define-text-object evil-org-table-a-cell (count &optional beg end type)
  "An org table cell."
  (save-excursion
    (when (not (looking-back "|\\s-?")) (org-table-beginning-of-field 1))
    (list (point)
          (dotimes (_ count (point))
            (org-table-next-field)))))

(evil-define-text-object evil-org-inner-sentence (count &optional beg end type)
  "Inner sentence or table cell when in an org table."
  (if (org-at-table-p)
      (evil-org-table-inner-cell count beg end type)
    (evil-a-sentence count beg end type)))

(evil-define-text-object evil-org-a-sentence (count &optional beg end type)
  "Outer sentence or table cell when in an org table."
  (if (org-at-table-p)
      (evil-org-table-a-cell)
    (evil-a-sentence count beg end type)))

(evil-define-text-object evil-org-inner-paragraph (count &optional beg end type)
  "Inner paragraph or table when in an org table."
  :type line
  (if (org-at-table-p)
      (list (org-table-begin) (org-table-end))
    (evil-select-inner-object 'evil-paragraph beg end type count t)))

(evil-define-text-object evil-org-a-paragraph (count &optional beg end type)
  "A paragraph or table when in an org table."
  :type line
  (if (org-at-table-p)
      (let ((p (point))
            (b (org-table-begin))
            (e (org-table-end)))
        (list (min (or beg p) b)
              (max (or end p) e)))
    (evil-select-an-object 'evil-paragraph beg end type count t)))

;;; Keythemes
(defun evil-org--populate-base-bindings ()
  "Bindings that are always available."
  (let-alist evil-org-movement-bindings
    (dolist (state '(normal visual operator motion))
      (evil-define-key state evil-org-mode-map
        (kbd "$") 'org-end-of-line
        (kbd "^") 'org-beginning-of-line
        (kbd "x") 'evil-org-delete-char
        (kbd "X") 'evil-org-delete-backward-char
        (kbd ")") 'evil-org-forward-sentence
        (kbd "(") 'evil-org-backward-sentence
        (kbd "}") 'org-forward-paragraph
        (kbd "{") 'org-backward-paragraph
        (kbd "<C-return>") (lambda ()
                             (interactive)
                             (evil-org-eol-call
                              'org-insert-heading-respect-content))
        (kbd "<C-S-return>") (lambda ()
                               (interactive)
                               (evil-org-eol-call
                                'org-insert-todo-heading-respect-content))))
    (dolist (state '(normal visual))
      (evil-define-key state evil-org-mode-map
        (kbd "<") 'evil-org-promote-or-dedent
        (kbd ">") 'evil-org-demote-or-indent
        (kbd "<tab>") 'org-cycle
        (kbd "<S-tab>") 'org-shifttab))
    (evil-define-key 'normal evil-org-mode-map
      (kbd "o") 'evil-org-open-below
      (kbd "O") 'evil-org-open-above)))

(defun evil-org--populate-textobjects-bindings ()
  "Text objects."
  (dolist (state '(visual operator))
    (evil-define-key state evil-org-mode-map "ae" 'org-element-textobj)
    (evil-define-key state evil-org-mode-map "ar" 'org-subtree-textobj)
    (evil-define-key state evil-org-mode-map "is" 'evil-org-inner-sentence)
    (evil-define-key state evil-org-mode-map "as" 'evil-org-a-sentence)
    (evil-define-key state evil-org-mode-map "ip" 'evil-org-inner-paragraph)
    (evil-define-key state evil-org-mode-map "ap" 'evil-org-a-paragraph)))

(defun evil-org--populate-insert-bindings ()
  "Define insert mode bindings."
  (evil-define-key 'insert evil-org-mode-map
    (kbd "C-t") 'org-metaright
    (kbd "C-d") 'org-metaleft))

(defun evil-org--populate-rsi-bindings ()
  "Define key bindings to use in hybrid state."
  (define-key org-mode-map (kbd "C-d")
    (lambda (n)
      (interactive "p")
      (if (and (org-at-heading-or-item-p) (eolp))
          (org-metaleft)
        (org-delete-char n))))
  (define-key org-mode-map (kbd "C-f")
    (lambda (n)
      (interactive "p")
      (if (and (org-at-heading-or-item-p) (eolp))
          (org-metaright)
        (forward-char n)))))

(defun evil-org--populate-navigation-bindings ()
  "Configures gj/gk/gh/gl for navigation."
  (let-alist evil-org-movement-bindings
    (evil-define-key 'motion evil-org-mode-map
      (kbd (concat "g" .left)) 'org-up-element
      (kbd (concat "g" .right)) 'org-down-element
      (kbd (concat "g" .up)) 'org-backward-element
      (kbd (concat "g" .down)) 'org-forward-element
      (kbd (concat "g" (capitalize .left))) 'evil-org-top)))

(defun evil-org--populate-additional-bindings ()
  "Bindings with meta and control."
  (let-alist evil-org-movement-bindings
    (dolist (state (if evil-org-use-additional-insert
                       '('normal visual insert)
                     '(normal visual)))
      (evil-define-key state evil-org-mode-map
        (kbd (concat "M-" .left)) 'org-metaleft
        (kbd (concat "M-" .right)) 'org-metaright
        (kbd (concat "M-" .up)) 'org-metaup
        (kbd (concat "M-" .down)) 'org-metadown
        (kbd (concat "M-" (capitalize .left))) 'org-shiftmetaleft
        (kbd (concat "M-" (capitalize .right))) 'org-shiftmetaright
        (kbd (concat "M-" (capitalize .up))) 'org-shiftmetaup
        (kbd (concat "M-" (capitalize .down))) 'org-shiftmetadown
        (kbd (concat "C-S-" .left)) 'org-shiftcontrolleft
        (kbd (concat "C-S-" .right)) 'org-shiftcontrolright
        (kbd (concat "C-S-" .up)) 'org-shiftcontrolup
        (kbd (concat "C-S-" .down)) 'org-shiftcontroldown))))

;; (let ((state (if evil-org-use-additional-insert '(normal insert) 'normal)))
;;   (evil-define-key state evil-org-mode-map
;;     (kbd "M-o") 'evil-org-insert-subheading
;;     (kbd "M-t") 'evil-org-insert-subtodo))

(defun evil-org--populate-shift-bindings ()
  "Shift bindings that conflict with evil bindings."
  (let-alist evil-org-movement-bindings
    (evil-define-key 'normal evil-org-mode-map
      (capitalize .left) 'org-shiftleft
      (capitalize .right) 'org-shiftright
      (capitalize .down) 'org-shiftdown
      (capitalize .up) 'org-shiftup)))

(defun evil-org--populate-todo-bindings ()
  "Bindings for easy todo insertion."
  (evil-define-key 'normal evil-org-mode-map
    "t" 'org-todo
    "T" '(lambda ()
           (interactive)
           (evil-org-eol-call
            (lambda ()
              (org-insert-todo-heading nil))))
    (kbd "M-t") 'evil-org-insert-subtodo))

(defun evil-org--populate-heading-bindings ()
  "Bindings for easy heading insertion."
  (evil-define-key 'normal evil-org-mode-map
    (kbd "O") '(lambda ()
                 (interactive)
                 (evil-org-eol-call
                  (lambda ()
                    (org-insert-heading))))
    (kbd "M-o") 'evil-org-insert-subheading))

;; leader maps
(defun evil-org--populate-leader-bindings ()
  "Leader bindings (deprecated)."
  (make-obsolete 'leader "please bind leader keys in your local config." "0.5.0")
  (evil-leader/set-key-for-mode 'org-mode
    "t" 'org-show-todo-tree
    "a" 'org-agenda
    "c" 'org-archive-subtree
    "l" 'evil-org-open-links
    "o" 'evil-org-recompute-clocks))

;;;###autoload
(defun evil-org-set-key-theme (&optional theme)
  "Select what keythemes to enable.
Optional argument THEME list of themes. See evil-org-keytheme for a list of values."
  (let ((theme (or theme evil-org-key-theme)))
    (setq evil-org-mode-map (make-sparse-keymap))
    (evil-org--populate-base-bindings)
    (when (memq 'navigation theme) (evil-org--populate-navigation-bindings))
    (when (memq 'insert theme) (evil-org--populate-insert-bindings))
    (when (memq 'textobjects theme) (evil-org--populate-textobjects-bindings))
    (when (memq 'rsi theme) (evil-org--populate-rsi-bindings))
    (when (memq 'additional theme) (evil-org--populate-additional-bindings))
    (when (memq 'shift theme) (evil-org--populate-shift-bindings))
    (when (memq 'todo theme) (evil-org--populate-todo-bindings))
    (when (memq 'heading theme) (evil-org--populate-heading-bindings))
    (when (memq 'leader theme) (evil-org--populate-leader-bindings))
    (setcdr
     (assq 'evil-org-mode minor-mode-map-alist)
     evil-org-mode-map)))

;;; vim-like confirm/abort for capture and src
;;; Taken from mwillsey (Max Willsey) on https://github.com/syl20bnr/spacemacs/pull/7400
(with-eval-after-load 'org-capture
  (define-key org-capture-mode-map [remap evil-save-and-close]          'org-capture-finalize)
  (define-key org-capture-mode-map [remap evil-save-modified-and-close] 'org-capture-finalize)
  (define-key org-capture-mode-map [remap evil-quit]                    'org-capture-kill))

(with-eval-after-load 'org-src
  (define-key org-src-mode-map [remap evil-save-and-close]          'org-edit-src-exit)
  (define-key org-src-mode-map [remap evil-save-modified-and-close] 'org-edit-src-exit)
  (define-key org-src-mode-map [remap evil-quit]                    'org-edit-src-abort))

;; Set customizable theme
(evil-org-set-key-theme evil-org-key-theme)

(provide 'evil-org)
;;; evil-org.el ends here
