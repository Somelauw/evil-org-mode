(require 'evil-org)
(require 'ert)

(defmacro evil-org-with (in &rest body)
  ;; TODO use evil-test-buffer instead
  `(with-temp-buffer
     ;; "hello"
     (evil-mode)
     (org-mode)
     (evil-org-mode)
     (insert ,in)
     (goto-char (point-min))
     (search-forward "|")
     (backward-delete-char 1)
     ,@body
     (insert "|")
     (buffer-substring-no-properties (point-min) (point-max))))

(ert-deftest evil-org-test-beginning-of-line ()
  (should (equal "|hello"
                 (evil-org-with "hello|" (evil-org-beginning-of-line))))
  (should (equal "|*hello\nworld"
                 (evil-org-with "|*hello\nworld" (evil-org-beginning-of-line)))))

;; TODO test with org-special-ctrl-a/e

(ert-deftest evil-org-test-end-of-line ()
  (should (equal "hello|"
                 (evil-org-with "|hello" (evil-org-end-of-line))))
  (should (equal "*hello|\nworld"
                 (evil-org-with "|*hello\nworld" (evil-org-end-of-line)))))

;; TODO test with org-special-ctrl-a/e

(ert-deftest evil-org-test-insert-line-special ()
  (should (equal "* TODO |hello"
                 (evil-org-with "* TODO hello|"
                                (let ((org-special-ctrl-a/e t))
                                  (evil-org-insert-line 1))))))

(ert-deftest evil-org-test-insert-line-no-special ()
 (should (equal "|* TODO hello"
                (evil-org-with "* TODO hello|"
                               (let ((org-special-ctrl-a/e nil))
                                 (evil-org-insert-line 1))))))

(ert-deftest evil-org-test-append-line-special ()
  (should (equal "* hello|  :tag:"
                 (evil-org-with "|* hello  :tag:"
                                (let ((org-special-ctrl-a/e t))
                                  (evil-org-append-line 1))))))

(ert-deftest evil-org-test-append-line-no-special ()
  (should (equal "* hello  :tag\:|"
                 (evil-org-with "|* hello  :tag:"
                                (let ((org-special-ctrl-a/e nil))
                                  (evil-org-append-line 1))))))

(ert-deftest evil-org-test-open-below ()
  (should (equal "hello\n|"
                 (evil-org-with "|hello" (evil-org-open-below 1)))))

(ert-deftest evil-org-test-open-below-heading ()
  (dolist (opener '(evil-org-open-below evil-open-below))
   (should (equal "* heading\nfolded\n|"
                  (evil-org-with "|* heading\nfolded"
                                 (org-overview) ; Make folded to be folded
                                 (call-interactively opener))))))

(ert-deftest evil-org-test-open-below-table ()
  (should (equal "| cell 1 | cell 2 |
| |       |        |\n"
                 (evil-org-with "||cell 1| cell 2|"
                                (call-interactively 'evil-org-open-below)))))

(ert-deftest evil-org-test-open-below-itemlist-continue ()
  (should (equal "- hello\n- |"
                 (evil-org-with "- |hello"
                                (call-interactively 'evil-org-open-below)))))

(ert-deftest evil-org-test-open-below-itemlist-no-continue ()
  (should (equal "- hello\n\n  |\n"
                 (evil-org-with "- hello\n|\n"
                                (call-interactively 'evil-org-open-below)))))

(ert-deftest evil-org-test-open-below-itemlist-last-line ()
  :expected-result :failed
  (should (equal "- hello\n\n  |"
                 (evil-org-with "- hello\n|"
                                (call-interactively 'evil-org-open-below)))))

(ert-deftest evil-org-test-open-below-code ()
  (should (equal
           "- code
   #+BEGIN_SRC emacs-lisp
   (message \"press o now\")
   |
   #+END_SRC"
           (evil-org-with
            "- code
   #+BEGIN_SRC emacs-lisp
   (message |\"press o now\")
   #+END_SRC"
            (call-interactively 'evil-org-open-below)))))

(ert-deftest evil-org-test-delete-list-item ()
  (should (equal "
                  1. emacs
|                  2. evil_org"
                 (evil-org-with "
                  4. emacs
                  5. |evil
                  6. evil_org"
                                (evil-org-delete (line-beginning-position)
                                                 (line-beginning-position 2)
                                                 'line)))))

(ert-deftest evil-org-test-delete-tags ()
  (should (equal "* |heading with some text                                           :testcase:"
                 (evil-org-with
                  "* |Funny heading with some text                                     :testcase:"
                  (let ((w (evil-a-word)))
                    (evil-org-delete (cl-first w) (cl-second w)))))))

;; TODO test x and X
;; TODO test < and >
