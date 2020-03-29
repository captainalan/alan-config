;; -*-coding: utf-8; lexical-binding: t; -*-
;; first item in lists expands to second item

;; Note: Use C-q to prevent the expansion of an abbreviation.

(clear-abbrev-table global-abbrev-table)
(define-abbrev-table 'global-abbrev-table
  '(
    ;; Diacritic chaos
    ("espanol" "español")
    ("naive" "naïve")
    ("Quebec" "Québec")
    ("Quebecois" "Québécois")

    ;; Editorial board
    ("programming" "programing")

    ))

(set-default 'abbrev-mode t)
(setq save-abbrevs nil)
