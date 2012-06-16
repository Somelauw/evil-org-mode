(defun esf/evil-key-bindings-for-org ()
  ;;(message "Defining evil key bindings for org")
  (evil-declare-key 'normal org-mode-map
                    "gh" 'outline-up-heading
                    "gj" 'org-forward-same-level
                    "gk" 'org-backward-same-level
                    "gl" 'outline-next-visible-heading
                    "H" 'org-beginning-of-line ; smarter behaviour on headlines etc.
                    "L" 'org-end-of-line ; smarter behaviour on headlines etc.
                    "t" 'org-todo ; mark a TODO item as DONE
                    ",c" 'org-cycle
                    ",e" 'org-export-dispatch
                    ",n" 'outline-next-visible-heading
                    ",p" 'outline-previous-visible-heading
                    ",t" 'org-set-tags-command
                    ",u" 'outline-up-heading
                    "$" 'org-end-of-line ; smarter behaviour on headlines etc.
                    "^" 'org-beginning-of-line ; ditto
                    "-" 'org-ctrl-c-minus ; change bullet style
                    "<" 'org-metaleft ; out-dent
                    ">" 'org-metaright ; indent
                    )
  (mapcar (lambda (state)
            (evil-declare-key state org-mode-map
                              (kbd "M-l") 'org-metaright
                              (kbd "M-h") 'org-metaleft
                              (kbd "M-k") 'org-metaup
                              (kbd "M-j") 'org-metadown
                              (kbd "M-L") 'org-shiftmetaright
                              (kbd "M-H") 'org-shiftmetaleft
                              (kbd "M-K") 'org-shiftmetaup
                              (kbd "M-J") 'org-shiftmetadown))
          '(normal insert)))
