;;; gnus.el --- GNUS Configuration

;;; Commentary:
;; GNUS Configuration
;;

(use-package gnus
  :config
  (add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
  (setq gnus-use-trees t)
  (setq gnus-gcc-mark-as-read t)
  (setq gnus-asynchronous t)
  ;; Email ___________________________________________________________
  (setq gnus-posting-styles
        '(("fastmail" ; Matches all groups of messages
           (address "Jesse Millwood <jesse_m@fastmail.com>")
           ("X-Message-SMTP-Method" "smtp smtp.fastmail.com 587 jesse_m@fastmail.com")
           )
          ("gmail" ; Matches all groups of messages
           (address "Jesse Millwood <jesse.c.millwood@gmail.com>")
           ("X-Message-SMTP-Method" "smtp smtp.gmail.com 587 jesse.c.millwood@gmail.com")
           )
          ("DornerWorks" ; Matches all groups of messages
           (address "Jesse Millwood <jesse.millwood@dornerworks.com>")
           ("X-Message-SMTP-Method" "smtp outlook.office365.us 587 jessemillwood@dornerworks.com")
           )
          )
        )
  ;; (setq gnus-permanently-visible-groups ".*")
  ;; Select Methods __________________________________________________
  (setq gnus-select-method '(nnnil ""))
  (setq gnus-secondary-select-methods
        '((nnmaildir  "" (directory "~/Maildir"))
          (nnimap "fastmail"
                  (nnimap-address "imap.fastmail.com")
                  (nnimap-server-port "imaps")
                  (nnimap-stream ssl)
                  (nnir-search-engine imap)
                  (nnmail-expiry-target "nnimap+fastmail:Trash")
                  (nnmail-expiry-wait 'immediate))
          (nnimap "gmail"
                  (nnimap-address "imap.gmail.com")
                  (nnimap-server-port "imaps")
                  (nnimap-stream ssl)
                  (nnir-search-engine imap)
                  (nnmail-expiry-target "nnimap+gmail:[Gmail]/Trash")
                  (nnmail-expiry-wait 'immediate))
          (nnimap "DornerWorks"
                  (nnimap-address "outlook.office365.us")
                  (nnimap-server-port "imaps")
                  (nnimap-stream ssl)
                  (nnir-search-engine imap)
                  (nnmail-expiry-target "nnimap+DornerWorks:Trash")
                  (nnmail-expiry-wait 'immediate))
;;          (nnreddit "")
;;          (nnhackernews "")
          ))
  ;; RSS _____________________________________________________________
  (setq mail-sources '((maildir :path "/home/jesse/Maildir"
                                :subdirs ("cur" "new"))
                       ))
  )

(use-package nnreddit
  :custom
  (nnreddit-python-command "python3"))

(use-package nnhackernews)

;; bxq2dshv9xpsfd38

(provide 'gnus)

;;; gnus.el ends here
