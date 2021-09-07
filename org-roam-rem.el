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
(require 'org-roam)

(defconst org-roam-rem-card-levels "REM_LEVELS")

(defcustom org-roam-rem-parent-in-title
  t
  "Include parent node title in card title")

(defgroup org-roam-rem nil
  "Customizations for org-roam."
  :group 'org)

(cl-defstruct org-roam-rem-card-exclusion start end)

(defun org-roam-rem--card-exlcusion-from-node (node)
  ;; Get card exclusion form node
  (when node
    (message "nod3"))
  (let* ((start (org-element-property :contents-begin node))
         (end (org-element-property :contents-end node)))
  (make-org-roam-rem-card-exclusion :start start :end end)))


(defun org-roam-rem-parent (title)
  (interactive)
  (let* ((ast (org-element-parse-buffer 'headline))
         (current-node))
    (org-element-map ast 'headline
      (lambda (headline)
        (let ((current-title (org-element-property :title headline)))
          (when (string= title current-title))
          (setq current-node headline)))))

  (message "%s" (org-roam-rem--parent (org-element-at-point) nil)))


(defun org-roam-rem--parent (current-node)
;; ParenT
)
(defun org-roam-rem--title (org-roam-node node-at-point)
;; Combine as follows org-roam-node-title -> market-title

  (let ((org-roam-title (org-roam-node-title org-roam-node))
        (node-title (org-element-property :title node-at-point))
        (title '(org-roam-title)))

    (when org-roam-rem-parent-in-title
      (save-excursion
        ()
        ))


    (concat org-roam-title " -> " node-title)))

(defun org-roam-rem-mark()
  "Mark as Rem."
        (interactive)
        (let* ((current-node (org-element-at-point))
               (org-roam-node (org-roam-node-at-point))
               (card-title (org-roam-rem--title org-roam-node current-node))
               (start (org-element-property :contents-begin current-node))
               (end (org-element-property :contents-end current-node))
               (levels-to-read (read-from-minibuffer "Levels: "))
               (current-level (org-element-property :level current-node))
               (levels (+ (- current-level 1) 1))
               (exclusion (org-roam-rem--note-exclusion levels))
               (card (org-roam-rem--fold-exclusion exclusion start end)))

         (org-set-property org-roam-rem-card-levels levels-to-read)
         (message "Title: %s card %s" card-title card)
         (message "%s" (org-roam-node-at-point))))


(defun org-roam-rem--fold-exclusion (exclusions node-start node-end)
  ;; Fold exclusion into a string
  (let ((buffer "")
        (current-start node-start))
    (dolist (element exclusions buffer)
      (let* ((start (org-roam-rem-card-exclusion-start element))
             (end (org-roam-rem-card-exclusion-end element))
             (buffer-fragment (buffer-substring current-start start)))
        (setq current-start end)
        (setq buffer (concat buffer buffer-fragment))))
     (concat buffer (buffer-substring current-start node-end))))

;;
;;
(defun org-roam-rem--note-exclusion (level)
  ;; Perform breath first search untill we get to level +1 and retrieve ther content start and end
  (let ((exclusion '()))
    (save-excursion
      (when (org-goto-first-child)

        (let* ((element (org-element-at-point))
               (child-level (org-element-property :level element)))
                (message "Level %s" child-level)
               (if (< child-level level)
                   (setq exclusion (append (org-roam-rem--note-exclusion level) exclusion))
                 (progn
                   (push (org-roam-rem--card-exlcusion-from-node element) exclusion)
                   (while (org-goto-sibling)
                     (let ((sibling (org-element-at-point)))
                       (push (org-roam-rem--card-exlcusion-from-node sibling) exclusion))))))))
    exclusion))
(provide 'org-roam-rem)
;;; org-roam-rem.el ends here
