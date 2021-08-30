;;; -*- lexical-binding: t; -*-
;;; rss-config --- Summary

;;; Commentary:
;; RSS Feed Configuration
;; Some of these feeds freeze up on base because every damn port is blocked
;; if that happens and Emacs completely becomes unresponsive, press C-g a few times
;; maybe run M-x elfeed-unjam after you regain control
;; It is also attributed with a bug in a diffrent package that elfeed relies on
;; Resources:
;;  - https://karthinks.com/blog/lazy-elfeed/
;;; Code:

(use-package elfeed
  :ensure t
  :demand
  :custom
  (url-queue-timeout 300)
  (elfeed-search-filter "@1-week-ago +unread ")
  (elfeed-search-title-max-width 120)
  :config

  (setq elfeed-feeds
        '(("https://xkcd.com/rss.xml" Comics Funny)
          ("https://feeds.feedburner.com/oatmealfeed" Comics Funny)
          ("https://jvns.ca/atom.xml" Electronics)
          ("https://bootlin.com/feed/" Electronics Projects)
          ("https://sachachua.com/blog/category/emacs/feed/" Emacs)
          ("https://openstates.org/mi/bills/feed/?status=passed_upper&amp;sort=passed_upper" Politics)
          ("https://bluesabre.org/feed/" Linux)
          ("https://rebble.io/feed.xml" Pebble Projects)
          ("https://this-week-in-rust.org/rss.xml" Rust Programming)
          ("https://wiki.gentoo.org/api.php?hidebots=1&translations=filter&urlversion=1&days=7&limit=50&action=feedrecentchanges&feedformat=atom" Gentoo)
          ("https://packages.gentoo.org/packages/updated.atom" Gentoo Updated)
          ("https://packages.gentoo.org/packages/added.atom" Gentoo Added)
          ("https://planet.gentoo.org/atom.xml" Gentoo)
          ("https://feeds.feedburner.com/d0od" Linux)
;;          ("https://linux.pictures/feed" Linux)
          ("https://puri.sm/feed/" Purism)
          ("https://www.redox-os.org/news/index.xml" Rust Programming Projects)
          ("https://rust-embedded.github.io/blog/rss.xml" Rust Programming)
          ("https://ocaml.org/feed.xml" OCaml)
          ("https://azeria-labs.com/feed/" Hacking)
          ("https://github.com/seL4/seL4/releases.atom" seL4)
          ("https://blog.frama-c.com/index.php?feed/atom" FramaC Formal-Methods)
          ("https://alan.petitepomme.net/cwn/cwn.rss" OCaml)
          ("https://feeds.feedburner.com/EvanSultanik" Hacking)
          ("https://riscv.org/feed" RISCV)
          ("https://blog.system76.com/rss" Linux)
          ("https://arxiv.org/rss/cs.CR" Arxiv-CS Cryptography Security)
          ("https://arxiv.org/rss/cs.GT" Arxiv-CS Game-Theory)
          ("https://arxiv.org/rss/cs.DC" Arxiv-CS Distributed-Computing)
          ("https://arxiv.org/rss/cs.ET" Arxiv-CS Emerging-Technologies)
          ("https://arxiv.org/rss/cs.FL" Arxiv-CS Formal-Methods)
          ("https://arxiv.org/rss/cs.GL" Arxiv-CS General-CS)
          ("https://arxiv.org/rss/cs.AR" Arxiv-CS Computer-Architecture)
          ("https://arxiv.org/rss/cs.MA" Arxiv-CS Multi-Agent)
          ("https://arxiv.org/rss/cs.PL" Arxiv-CS Programming-Languages)
          ("https://arxiv.org/rss/cs.PF" Arxiv-CS Performance)
          ("https://arxiv.org/rss/cs.NI" Arxiv-CS Networking)
          ("https://arxiv.org/rss/cs.SE" Arxiv-CS Software-Engineering)
          ("https://arxiv.org/rss/cs.SC" Arxiv-CS Symbolic-Computation)
          ("https://arxiv.org/rss/cs.SY" Arxiv-CS Control-Systems)
          ("https://arxiv.org/rss/cs.OS" Arxiv-CS Operating-Systems)
          ("https://arxiv.org/rss/econ" Arxiv-Econ)
          ("https://arxiv.org/rss/eess" Arxiv-EE)
          ("https://feeds.feedburner.com/ItsFoss" FOSS)
          ("https://feeds.feedburner.com/ImprobableResearch" Funny)
          ("https://www.livekindly.com/feed/" Vegan)
          ("https://vegnews.com/news/todays-top-stories/feed.rss" Vegan)
          ("https://embeddedartistry.com/feed/" Electronics)
          ("https://kernelnewbies.org/Linux_Kernel_Newbies?action=rss_rc&unique=1&ddiffs=1&show_att=1&diffs=1" Linux)
          ("https://groups.google.com/forum/feed/pagedout-notifications/msgs/rss.xml?num=15" Hacking)
          ("https://www.woodtv.com/news/grand-rapids/feed" Local)
          ("https://revuewm.com/features/news?format=feed" Local)
          ("https://www.schneier.com/blog/atom.xml" Security)
          ("https://www.eff.org/rss/updates.xml" Security)
          ("https://amos.me/index.xml" Programming)
          ("https://web.eecs.utk.edu/~smarz1/osblog/feed.rss" Rust RISCV)
          ("https://blog.japaric.io/index.xml" Rust)
          ("https://www.politico.com/rss/politicopicks.xml" Politics)
          ("https://boingboing.net/author/cory_doctorow_1/feed" FOSS)
          ("https://feeds.feedburner.com/cnx-software/blog?format=xml" Electronics)
          ("https://linuxgizmos.com/feed/" Electronics)
          ("https://toggl.com/blog/track/creative/feed" Funny)
          ("www.righto.com/feeds/posts/default" Electronics)
          ("https://microkerneldude.wordpress.com/feed/" Programming)
          ("https://genode.org/news/aggregator/rss" Programming)
          ("https://genodians.org/rss" Programming)
          ("https://emacsredux.com/atom.xml" Emacs)
          ("https://feeds.urbandictionary.com/UrbanWordOfTheDay" Funny)
          ("https://restoreprivacy.com/feed" Security)
          ("https://www.megabeets.net/feed/" Hacking)
          ("https://rizin.re/posts/index.xml" Projects)
          ("https://api.archives-ouvertes.fr/search/hal/?omitHeader=true&wt=rss&q=%22virtualization%22+OR+%22hypervisor%22&fq=NOT+instance_s%3Asfo&fq=NOT+instance_s%3Adumas&fq=NOT+instance_s%3Amemsic&fq=NOT+instance_s%3Ahceres&fq=NOT+%28docType_s%3AHDR+AND+submitType_s%3A%28notice+OR+annex%29%29&fq=NOT+docType_s%3A%28MEM+OR+PRESCONF+OR+MINUTES+OR+NOTE+OR+SYNTHESE+OR+OTHERREPORT+OR+REPACT+OR+BOOKREPORT+OR+ETABTHESE+OR+MEMLIC+OR+CREPORT%29&fq=NOT+status_i%3A111&fq=%7B%21tag%3Dtag0__docType_s%7DdocType_s%3A%28%22REPORT%22+OR+%22UNDEFINED%22+OR+%22ART%22+OR+%22COMM%22%29&fq=%7B%21tag%3Dtag1__language_s%7Dlanguage_s%3A%28%22en%22%29&defType=edismax&rows=50" HAL-Virtualization)
          ("https://api.archives-ouvertes.fr/search/hal/?omitHeader=true&wt=rss&q=microkernel&sort=producedDate_tdate+desc&fq=NOT+instance_s%3Asfo&fq=NOT+instance_s%3Adumas&fq=NOT+instance_s%3Amemsic&fq=NOT+instance_s%3Ahceres&fq=NOT+%28docType_s%3AHDR+AND+submitType_s%3A%28notice+OR+annex%29%29&fq=NOT+docType_s%3A%28MEM+OR+PRESCONF+OR+MINUTES+OR+NOTE+OR+SYNTHESE+OR+OTHERREPORT+OR+REPACT+OR+BOOKREPORT+OR+ETABTHESE+OR+MEMLIC+OR+CREPORT%29&fq=NOT+status_i%3A111&fq=%7B%21tag%3Dtag0__docType_s%7DdocType_s%3A%28%22REPORT%22+OR+%22UNDEFINED%22+OR+%22ART%22+OR+%22COMM%22%29&fq=%7B%21tag%3Dtag1__language_s%7Dlanguage_s%3A%28%22en%22%29&defType=edismax&rows=50" HAL-microkernels)
          ("https://www.dailywritingtips.com/feed/" Writing)
          ("https://www.beningo.com/feed/" Electronics)
          ("https://www.bridgemi.com/rss.xml" Michigan Local)
          ("https://pluralistic.net/feed/" Society)
          ("https://blog.tecosaur.com/tmio/rss.xml" Emacs)
          ("https://citizen.org/feed" Society)
          ("https://element.io/blog/rss/" Projects)
          ("https://matrix.org/blog/feed" Programming)
          ("https://interrupt.memfault.com/feed.xml" Programming)
          ("https://blog.althea.net/rss" Projects)
          ))
  )


;; Interested Keywords
(setq arxiv-ee-interesting-keywords '("ct" "uav" "power" "ultrasound" "music"))
(setq arxiv-cs-interesting-keywords '("mesh" "xen" "virtual" "net" "rust" "lisp" "federated"
                                      "distributed" "analysis" "grid" "energy" "encrypt"
                                      "concurrenc" "trust" "flow" "information" "dift"
                                      "survey" "battery" "risc-v" "concurren" "verification"
                                      "validation" "proof" "capabilitiy" "sel4" "schedul" "memory"
                                      "kernel" "ocaml" "cap"))
(setq arxiv-interesting-keywords (append arxiv-cs-interesting-keywords
                                         arxiv-ee-interesting-keywords))
(defface arxiv-interesting-face
  '((t :foreground "#00cc00"))
  "Marks a item of interest from the arxiv feed"
  )
;; Associate face with tag and push onto the search face alist
(push '(arxiv-filtered-tag arxiv-interesting-face) elfeed-search-face-alist)

(mapc (lambda (keyword)
        (add-hook 'elfeed-new-entry-hook
                  (elfeed-make-tagger :feed-url "https://arxiv\\.org/rss/.*"
                                      :entry-title keyword
                                      :add 'arxiv-filtered-tag)))
      arxiv-interesting-keywords )

;; Default Entry Hooks:
;; (setq elfeed-new-entry-hook '())
;; Default Search Face AList
;; (setq elfeed-search-face-alist '((unread elfeed-search-unread-title-face)))

(setq arxiv-ignore-keywords '("neural" "learning" "machine" "train" "deep"))
(defface arxiv-ignore-face
  '((t :foreground "#cccc00"))
  "Marks disinterested subjects"
  )
(push '(arxiv-ignore-tag arxiv-ignore-face) elfeed-search-face-alist)

(mapc (lambda (keyword)
        (add-hook 'elfeed-new-entry-hook
          (elfeed-make-tagger :feed-url "https://arxiv\\.org/rss/.*"
                              :entry-title keyword
                              :add 'arxiv-ignore-tag
                              :remove 'arxiv-filtered-tag)))
      arxiv-ignore-keywords)

(use-package elfeed-goodies
  :ensure t
  :after (elfeed)
  :custom
  (elfeed-goodies/entry-pane-position 'bottom)
  :config
  (elfeed-goodies/setup)
  )

(defun elfeed-ivy-filter ()
  (interactive)
  (let
      (
       (filtered-tag (ivy-completing-read "Choose Tags: " (elfeed-db-get-all-tags)))
       )
    (progn
      (setq elfeed-search-filter (concat elfeed-search-filter " +" filtered-tag))
      (elfeed-search-update--force)
      )
    )
  )


(provide 'rss-config)
;;; rss-config.el ends here
