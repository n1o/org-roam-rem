;;; org-roam-rem.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Marek Barak
;;
;; Author: Marek Barak <https://github.com/mbarak>
;; Maintainer: Marek Barak <mrk.barak@gmail.com>
;; Created: September 03, 2021
;; Modified: September 03, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/mbarak/org-roam-rem
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:


(require 'org-element)

(defconst org-roam-rem-type "ORG_ROAM_REM_TYPE")

(defgroup org-roam-rem nil
  "Customizations for org-roam."
  :group 'org)


(defun org-roam-rem-init ()
  "Mark as Rem."
  (message "%s" (org-element-at-point)))

(provide 'org-roam-rem)
;;; org-roam-rem.el ends here
