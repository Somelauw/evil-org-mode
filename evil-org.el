;;; evil-org.el --- evil keybindings for org-mode

;; Maintainer: Somelauw
;; Original author: Edward Tjörnhammar
;; URL: https://github.com/Somelauw/evil-org-improved.git
;; Git-Repository; git://github.com/Somelauw/evil-org-improved.git
;; Created: 2012-06-14
;; Forked since 2017-02-12
;; Version: 0.2.2
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

(defun clever-insert-item ()
  "Clever insertion of org item."
  (if (not (org-in-item-p))
      (insert "\n")
    (org-insert-item)))

(defun evil-org-eol-call (fun)
  "Go to end of line and call provided function.
FUN function callback"
  (end-of-visible-line)
  (funcall fun)
  (evil-append nil))

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



(defun evil-org--populate-base-bindings ()
  (let-alist evil-org-movement-bindings
    (dolist (state '(normal visual operator motion))
      (evil-define-key state evil-org-mode-map
        (kbd "$") 'org-end-of-line
        (kbd "^") 'org-beginning-of-line
        (kbd "x") 'org-delete-char
        (kbd "X") 'org-delete-backward-char
        (kbd ")") 'org-forward-sentence
        (kbd "(") 'org-backward-sentence
        (kbd "}") 'org-forward-paragraph
        (kbd "{") 'org-backward-paragraph))
    (dolist (state '(normal visual))
      (evil-define-key state evil-org-mode-map
        (kbd "<") 'org-metaleft
        (kbd ">") 'org-metaright
        (kbd "<tab>") 'org-cycle
        (kbd "<S-tab>") 'org-shifttab))
    (evil-define-key 'normal evil-org-mode-map
      (kbd "o") '(lambda ()
                   (interactive)
                   (evil-org-eol-call 'clever-insert-item)))))

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
              (org-insert-todo-heading nil))))))

(defun evil-org--populate-heading-bindings ()
  (evil-define-key 'normal evil-org-mode-map
    (kbd "O") '(lambda ()
                 (interactive)
                 (evil-org-eol-call
                  (evil-org-eol-call
                   (lambda ()
                     (org-insert-heading)))))
    (kbd "M-o") '(lambda () (interactive)
                   (evil-org-eol-call
                    '(lambda ()
                       (org-insert-heading)
                       (org-metaright))))))

;;;###autoload
(defun evil-org-set-key-theme (theme)
  (setq evil-org-mode-map (make-sparse-keymap))
  (evil-org--populate-base-bindings)
  (when (memq 'additional theme) (evil-org--populate-additional-bindings))
  (when (memq 'shift theme) (evil-org--populate-shift-bindings))
  (when (memq 'leader theme) (evil-org--populate-leader-bindings))
  (when (memq 'todo theme) (evil-org--populate-todo-bindings))
  (when (memq 'heading theme) (evil-org--populate-heading-bindings))
  (setcdr
   (assq 'evil-org-mode minor-mode-map-alist)
   evil-org-mode-map))

;; This default will soon be changed to '(additional)
(evil-org-set-key-theme '(additional shift leader todo heading))

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
