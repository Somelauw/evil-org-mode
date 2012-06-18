evil-org-mode
=============

Supplemental evil-mode key-bindings to Emacs org-mode. This is a work in progress, expect improvements and don't be afraid to contribute patches.

Requirements
============

* org-mode, git://repo.or.cz/org-mode.git
* evil-mode, git://gitorious.org/evil/evil.git
* evil-leader, git://github.com/cofi/evil-leader.git

Installation
============

    mkdir -p ~/.emacs.d/plugins; git clone git://github.com/edwtjo/evil-org-mode.git ~/.emacs.d/plugins/evil-org-mode

emacs.el
--------

    (add-to-list 'load-path "~/.emacs.d/plugins/evil-org-mode")
    (require 'evil-org)

License
=======

Gnu General Public License v3.0, http://www.gnu.org/copyleft/gpl.html
