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

(defconst org-roam-rem-type "REM_TYPE")
(defconst org-roam-rem-type-flat "FLAT")

(defgroup org-roam-rem nil
  "Customizations for org-roam."
  :group 'org)

(cl-defstruct org-roam-rem-card-exclusion start end)

(defun org-roam-rem-mark()
  "Mark as Rem."
        (interactive)
        (org-set-property org-roam-rem-type org-roam-rem-type-flat)
        (org-roam-rem--get-flat-note)
        (message "Here"))


(defun org-roam-rem--get-flat-note ()
  ;;Parse flat rem
  (let ((nodes '()))
    (push (org-element-at-point) nodes)
        (save-excursion
                (when (org-goto-first-child)
                  (push (org-element-at-point) nodes)))
        (message "%s" (reverse nodes))
         ))

;; :pre-blank 0 :contents-begin 113 :contents-end 471
;;
(defun org-roam-rem--note-exclusion (level)
  (let ((exclusion '()))
    (save-excursion
      (when (org-goto-first-child)
        (let* ((element (org-element-at-point))
               (child-level (org-element-property :level element))
               (if (<= child-level level)
                   (append (org-roam-rem--note-exclusion level) exclusion)
                 (progn
                   (let* ((start (org-element-property :contents-begin element))
                          (end (org-element-property  :contents-end element)))
                        (push (make-org-roam-rem-card-exclusion :start start :end end)))
                   (while (org-goto-sibling)
                     (let* ((sibling  (org-element-at-point))
                            (start (org-element-property :contents-begin sibling))
                            (end (org-element-property  :contents-end sibling)))
                        (push (make-org-roam-rem-card-exclusion :start start :end end)))
                     )))))))
    exclusion))
(provide 'org-roam-rem)
;;; org-roam-rem.el ends here
