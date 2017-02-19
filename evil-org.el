;;; evil-org.el --- evil keybindings for org-mode

;; Maintainer: Somelauw
;; Original author: Edward Tjörnhammar
;; URL: https://github.com/Somelauw/evil-org-improved.git
;; Git-Repository; git://github.com/Somelauw/evil-org-improved.git
;; Created: 2012-06-14
;; Forked since 2017-02-12
;; Version: 0.4.1
;; Package-Requires: ((evil "0") (org "0") (evil-leader "0"))
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
;; See, https://github.com/Somelauw/evil-org-mode/issues
;;
;;; Code:
(require 'evil)
(require 'org)

(defgroup evil-org nil
  "Provides integration of org-mode and evil."
  :group 'org
  :prefix "evil-org-")

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

(defvar evil-org-mode-map (make-sparse-keymap))

;;;###autoload
(define-minor-mode evil-org-mode
  "Buffer local minor mode for evil-org"
  :init-value nil
  :lighter " EvilOrg"
  :keymap evil-org-mode-map
  :group 'evil-org)

(add-hook 'org-mode-hook 'evil-org-mode) ;; only load with org-mode

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
  "Clever insertion of org item."
  (interactive "p")
  (cond ((org-in-item-p)
         (evil-open-below count)
         (org-insert-item))
        ((org-at-table-p)
         (org-table-insert-row '(4))
         (evil-insert count))
        (t (evil-open-below count))))

(defun evil-org-open-above (count)
  "Clever insertion of org item."
  (interactive "p")
  (cond ((org-in-item-p)
         (beginning-of-visual-line)
         (org-insert-item)
         (evil-append count))
        ((org-at-table-p)
         (org-table-insert-row)
         (evil-insert count))
        (t (evil-open-above count))))

;;; motions
(evil-declare-motion 'org-forward-sentence)
(evil-declare-motion 'org-backward-sentence)
(evil-declare-motion 'org-forward-paragraph)
(evil-declare-motion 'org-backward-paragraph)
(evil-declare-motion 'org-table-next-row)
(evil-declare-motion 'org-table-previous-row)
(evil-declare-motion 'org-table-previous-field)
(evil-declare-motion 'org-table-next-field)
(evil-declare-motion 'org-table-beginning-of-field)
(evil-declare-motion 'org-table-end-of-field)

;; heading
(evil-declare-motion 'org-forward-heading-same-level)
(evil-declare-motion 'org-backward-heading-same-level)

;; elements
(evil-declare-motion 'org-forward-element)
(evil-declare-motion 'org-backward-element)
(evil-declare-motion 'org-down-element)
(evil-declare-motion 'org-up-element)

;; other
(evil-declare-motion 'org-next-block)
(evil-declare-motion 'org-next-item)
(evil-declare-motion 'org-next-visible-heading)
(evil-declare-motion 'org-previous-block)
(evil-declare-motion 'org-previous-item)
(evil-declare-motion 'org-previous-visible-heading)


;;; non-repeatible
(evil-declare-not-repeat 'org-shifttab)
(evil-declare-not-repeat 'org-cycle)

;;; operators
(evil-define-operator evil-org-shift-left (beg end)
  "Demote all headings in selection."
  :move-point nil
  (org-map-region 'org-metaleft beg end))

(evil-define-operator evil-org-shift-right (beg end)
  "Promote all headings in selection."
  :move-point nil
  (org-map-region 'org-metaright beg end))

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

;; recompute clocks in visual selection
(evil-define-operator evil-org-recompute-clocks (beg end type register yank-handler)
  :keep-visual t
  :move-point nil
  (interactive "<r>")
  (progn
    (message "start!")
    (save-excursion
      (while (< (point) end)
        (org-evaluate-time-range)
        (next-line)
        (message "at position %S" (point))))))

;; open org-mode links in visual selection
(defun evil-org-generic-open-links (beg end type register yank-handler incog)
  (progn
    (save-excursion
      (goto-char beg)
      (catch 'break
        (while t
          (org-next-link)
          ;;; break from outer loop when there are no more
          ;;; org links
          (when (or
                 (not (< (point) end))
                 (not (null org-link-search-failed)))
            (throw 'break 0))

          (if (not (null incog))
              (let* ((new-arg
                      ;;; if incog is true, decide which incognito settings to
                      ;;; use dependening on the browser
                      (cond ((not (null (string-match "^.*\\(iceweasel\\|firefox\\).*$" browse-url-generic-program)))  "--private-window")
                            ((not (null (string-match "^.*\\(chrome\\|chromium\\).*$"  browse-url-generic-program)))   "--incognito"     )
                            (t "")
                            ))
                     (old-b (list browse-url-generic-args " " ))
                     (browse-url-generic-args (add-to-ordered-list 'old-b new-arg 0)))
                (progn
                  (org-open-at-point)))
            (let ((browse-url-generic-args '("")))
              (org-open-at-point)))
          )))))


;;; open links in visual selection
(evil-define-operator evil-org-open-links (beg end type register yank-handler)
  :keep-visual t
  :move-point nil
  (interactive "<r>")
  (evil-org-generic-open-links beg end type register yank-handler nil))

;;; open links in visual selection in incognito mode
(evil-define-operator evil-org-open-links-incognito (beg end type register yank-handler)
  :keep-visual t
  :move-point nil
  (interactive "<r>")
  (evil-org-generic-open-links beg end type register yank-handler t))

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
  (if (org-at-table-p)
      (list (org-table-begin) (org-table-end))
    (evil-inner-paragraph count beg end type)))

(evil-define-text-object evil-org-a-paragraph (count &optional beg end type)
  "Outer paragraph or table when in an org table."
  (if (org-at-table-p)
      (list (org-table-begin) (org-table-end))
    (evil-a-paragraph count beg end type)))

;;; Keythemes
(defun evil-org--populate-base-bindings ()
  (let-alist evil-org-movement-bindings
    (dolist (state '(normal visual operator motion))
      (evil-define-key state evil-org-mode-map
        (kbd "$") 'org-end-of-line
        (kbd "^") 'org-beginning-of-line
        (kbd "x") 'evil-org-delete-char
        (kbd "X") 'evil-org-delete-backward-char
        (kbd ")") 'org-forward-sentence
        (kbd "(") 'org-backward-sentence
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
        (kbd "<") 'evil-org-shift-left
        (kbd ">") 'evil-org-shift-right
        (kbd "<tab>") 'org-cycle
        (kbd "<S-tab>") 'org-shifttab))
    (evil-define-key 'normal evil-org-mode-map
      (kbd "o") 'evil-org-open-below
      (kbd "O") 'evil-org-open-above)))

(defun evil-org--populate-textobjects-bindings ()
  (dolist (state '(visual operator))
    (evil-define-key state evil-org-mode-map "ae" 'org-element-textobj)
    (evil-define-key state evil-org-mode-map "ar" 'org-subtree-textobj)
    (evil-define-key state evil-org-mode-map "is" 'evil-org-inner-sentence)
    (evil-define-key state evil-org-mode-map "as" 'evil-org-a-sentence)
    (evil-define-key state evil-org-mode-map "ip" 'evil-org-inner-paragraph)
    (evil-define-key state evil-org-mode-map "ap" 'evil-org-a-paragraph)))

(defun evil-org--populate-insert-bindings ()
  (evil-define-key 'insert evil-org-mode-map
    (kbd "C-t") 'org-metaright
    (kbd "C-d") 'org-metaleft))

(defun evil-org--populate-additional-bindings ()
  (let-alist evil-org-movement-bindings
    (dolist (state '(motion))
      (evil-define-key state evil-org-mode-map
        (kbd (concat "g" .left)) 'org-up-element
        (kbd (concat "g" .right)) 'org-down-element
        (kbd (concat "g" .up)) (if (fboundp 'org-backward-same-level)
                                  'org-backward-same-level
                                'org-backward-heading-same-level)
        (kbd (concat "g" .down)) (if (fboundp 'org-forward-same-level) ;to be backward compatible with older org version
                                    'org-forward-same-level
                                  'org-forward-heading-same-level)))
    (dolist (state '(normal visual))
      (evil-define-key state evil-org-mode-map
        (kbd (concat "M-" .left)) 'org-metaleft
        (kbd (concat "M-" .right)) 'org-metaright
        (kbd (concat "M-" .up)) 'org-metaup
        (kbd (concat "M-" .down)) 'org-metadown
        (kbd (concat "M-" (capitalize .left))) 'org-shiftmetaleft
        (kbd (concat "M-" (capitalize .right))) 'org-shiftmetaright
        (kbd (concat "M-" (capitalize .up))) 'org-shiftmetaup
        (kbd (concat "M-" (capitalize .down))) 'org-shiftmetadown
        (kbd (concat "C-" (capitalize .left))) 'org-shiftcontrolleft
        (kbd (concat "C-" (capitalize .right))) 'org-shiftcontrolright
        (kbd (concat "C-" (capitalize .up))) 'org-shiftcontrolup
        (kbd (concat "C-" (capitalize .down))) 'org-shiftcontroldown))))

(defun evil-org--populate-shift-bindings ()
  (let-alist evil-org-movement-bindings
    (evil-define-key 'normal evil-org-mode-map
      (capitalize .left) 'org-shiftleft
      (capitalize .right) 'org-shiftright
      (capitalize .down) 'org-shiftdown
      (capitalize .up) 'org-shiftup)))

;; leader maps
(defun evil-org--populate-leader-bindings ()
  (evil-leader/set-key-for-mode 'org-mode
    "t" 'org-show-todo-tree
    "a" 'org-agenda
    "c" 'org-archive-subtree
    "l" 'evil-org-open-links
    "o" 'evil-org-recompute-clocks))

(defun evil-org--populate-todo-bindings ()
  (evil-define-key 'normal evil-org-mode-map
    "t" 'org-todo
    "T" '(lambda ()
           (interactive)
           (evil-org-eol-call
            (lambda ()
              (org-insert-todo-heading nil))))
    (kbd "M-t") '(lambda ()
           (interactive)
           (evil-org-eol-call
            (lambda ()
              (org-insert-todo-heading nil)
              (org-metaright))))))

(defun evil-org--populate-heading-bindings ()
  (evil-define-key 'normal evil-org-mode-map
    (kbd "O") '(lambda ()
                 (interactive)
                 (evil-org-eol-call
                   (lambda ()
                     (org-insert-heading))))
    (kbd "M-o") '(lambda ()
                   (interactive)
                   (evil-org-eol-call
                    '(lambda ()
                       (org-insert-heading)
                       (org-metaright))))))

;;;###autoload
(defun evil-org-set-key-theme (theme)
  (setq evil-org-mode-map (make-sparse-keymap))
  (evil-org--populate-base-bindings)
  (when (memq 'textobjects theme) (evil-org--populate-textobjects-bindings))
  (when (memq 'insert theme) (evil-org--populate-insert-bindings))
  (when (memq 'additional theme) (evil-org--populate-additional-bindings))
  (when (memq 'shift theme) (evil-org--populate-shift-bindings))
  (when (memq 'leader theme) (evil-org--populate-leader-bindings))
  (when (memq 'todo theme) (evil-org--populate-todo-bindings))
  (when (memq 'heading theme) (evil-org--populate-heading-bindings))
  (setcdr
   (assq 'evil-org-mode minor-mode-map-alist)
   evil-org-mode-map))

(if (and (boundp 'evil-disable-insert-state-bindings)
         (evil-disable-insert-state-bindings))
    (evil-org-set-key-theme '(textobjects additional))
    (evil-org-set-key-theme '(textobjects insert additional)))

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

(provide 'evil-org)
;;; evil-org.el ends here
