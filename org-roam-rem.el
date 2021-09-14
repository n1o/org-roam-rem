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
;; Package-Requires: ((emacs "25.1"))
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
(require 'subr-x)
(require 'request)
(require 'ox-html)
(require 'ox)
(require 'json)

(defconst org-roam-rem-card-levels "REM_LEVELS")
(defconst org-roam-rem-card-title "CARD_TITLE")
(defconst org-roam-rem-anki-deck-name "ANKI_DECK_NAME")
(defconst org-roam-rem-anki-note-id "ANKI_NOTE_ID")

(defcustom org-roam-rem-roam-title-in-card-title t "Include parent node title in card title.")
(defcustom org-roam-rem-ancestor-path-card-title t "Include ancestor path in card title.")
(defcustom org-roam-rem-anki-connect-listening-address "127.0.0.1" "The network address AnkiConnect is listening.")
(defcustom org-roam-rem-anki-connect-listening-port "8765" "The port number AnkiConnect is listening.")
(defcustom org-roam-rem-break-consecutive-braces-in-latex nil "If non-nil, consecutive `}' will be automatically separated by spaces to prevent early-closing of cloze.
See https://apps.ankiweb.net/docs/manual.html#latex-conflicts.")
(defcustom org-roam-rem-use-math-jax nil "Use Anki's built in MathJax support instead of LaTeX.")

(defgroup org-roam-rem nil
  "Customizations for org-roam."
  :group 'org)

(defconst org-roam-rem--ox-anki-html-backend
  (if org-roam-rem-use-math-jax
      (org-export-create-backend
       :parent 'html
       :transcoders '((latex-fragment . org-roam-rem--ox-latex-for-mathjax)
                      (latex-environment . org-roam-rem--ox-latex-for-mathjax)))
    (org-export-create-backend
     :parent 'html
     :transcoders '((latex-fragment . org-roam-rem--ox-latex)
                    (latex-environment . org-roam-rem--ox-latex)))))

(defconst org-roam-rem--ox-export-ext-plist
  '(:with-toc nil :org-roam-rem-mode t))

(cl-defstruct org-roam-rem-card-exclusion start end)
(cl-defstruct org-roam-rem-anki-card front back deck (note-id -1))

(defmacro org-roam-rem--anki-connect-invoke-result (&rest args)
  "Invoke AnkiConnect with ARGS, return the result from response or raise an error."
  `(let-alist (org-roam-rem--anki-connect-invoke ,@args)
     (when .error (error .error))
     .result))


(defun org-roam-rem--card-exlcusion-from-node (node)
  ;; Get card exclusion form node
  (let* ((start (org-element-property :contents-begin node))
         (end (org-element-property :contents-end node)))
  (make-org-roam-rem-card-exclusion :start start :end end)))


(defun org-roam-rem--ancestor-path-title (current-node)
;; ParenT
  (let* ((title (org-element-property :title current-node))
         (ast (org-element-parse-buffer 'headline))
         (full-title ""))
    (org-element-map ast 'headline
      (lambda  (headline)
        (let ((current-title (org-element-property :title headline)))
          (when (string= title current-title)
            (setq full-title (org-roam-rem--title-from-path headline))))))
    (message full-title)
    full-title))

(defun org-roam-rem--title-from-path (current-node)
  ;;Recursivelly get title walking up the ancestor path
  (if current-node
      (let* ((title (org-element-property :title current-node))
            (parent (org-element-property :parent current-node))
            (parent-title (org-element-property :title parent)))
        ;; (message "%s" (not (null title)))
        (if (or (null title) (string-blank-p title))
            (org-roam-rem--title-from-path parent)
          (progn
            (if (not (null parent-title))
                (concat (org-roam-rem--title-from-path parent) " -> " title)
                title))))
    ""))

(defun org-roam-rem--title (org-roam-node current-node)
;; Combine as follows org-roam-node-title -> market-title

  (let ((title
    (if org-roam-rem-roam-title-in-card-title
        (concat (org-roam-node-title org-roam-node) " -> ")
      "")))

    (if org-roam-rem-ancestor-path-card-title
        (concat title (org-roam-rem--ancestor-path-title current-node))
        (concat title (org-element-property :title current-node)))))

(defun org-roam-rem--as-html (text)
  ;; Export text as html
  (org-export-string-as text org-roam-rem--ox-anki-html-backend t org-roam-rem--ox-export-ext-plist))

(cl-defstruct org-roam-rem title card levels-to-read)

(defun org-roam-rem-new (current-node org-roam-node levels-to-read)
;; Create new rem
  (let* ((card-title (org-roam-rem--title org-roam-node current-node))
         (start (org-element-property :contents-begin current-node))
         (end (org-element-property :contents-end current-node))
         (current-level (org-element-property :level current-node))
         (levels (+ (- current-level 1) levels-to-read))
         (exclusion (org-roam-rem--note-exclusion levels))
         (card (org-roam-rem--fold-exclusion exclusion start end)))
    (make-org-roam-rem :title card-title :card card :levels-to-read levels-to-read)))

(defun org-roam-rem-mark()
  "Mark as Rem."
        (interactive)
        (let* ((current-node (org-element-at-point))
               (org-roam-node (org-roam-node-at-point))
               (levels-to-read (read-from-minibuffer "Levels: "))
               (deck (completing-read "Choose a deck: " (sort (org-roam-rem--deck-names) #'string-lessp)))
               (rem (org-roam-new-rem current-node org-roam-node levels-to-read))
               (note (make-org-roam-rem-anki-card
                      :front (org-roam-rem--as-html (org-roam-rem-title rem))
                      :back (org-roam-rem--as-html (org-roam-rem-card rem))
                      :deck deck)))
         (org-roam-rem--create-anki-note note)
         (org-set-property org-roam-rem-card-levels levels-to-read)
         (org-set-property org-roam-rem-card-title (org-roam-rem-title rem))
         (org-set-property org-roam-rem-anki-deck-name deck)))

(defun org-roam-rem-update ()
  ;; Update existing note
  (interactive)
  (let* ((current-node (org-element-at-point))
         (org-roam-node (org-roam-node-at-point))
         (levels-to-read (org-element-property org-roam-rem-card-levels current-node))
         (deck (org-element-property org-roam-rem-anki-deck-name current-node))
         (rem (org-roam-rem-new current-node org-roam-node levels-to-read))
         (note (make-org-roam-rem-anki-card
                :front (org-roam-rem--as-html (org-roam-rem-title rem))
                :back (org-roam-rem--as-html (org-roam-rem-card rem))
                :deck deck)))

    (org-roam-rem--update-anki-note note)))


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
               (if (< child-level level)
                   (setq exclusion (append (org-roam-rem--note-exclusion level) exclusion))
                 (progn
                   (push (org-roam-rem--card-exlcusion-from-node element) exclusion)
                   (while (org-goto-sibling)
                     (let ((sibling (org-element-at-point)))
                       (push (org-roam-rem--card-exlcusion-from-node sibling) exclusion))))))))
    exclusion))


(defun org-roam-rem--set-note-id (id)
  (unless id
    (error "Note creation failed for unknown reason"))
  (org-set-property org-roam-rem-anki-note-id (number-to-string id)))


(defun org-roam-rem--create-anki-note (note)
  "Request AnkiConnect for creating NOTE."
  (let* ((note '((note . ,(org-roam-rem--anki-connect-map-note note))))
         (action (org-roam-rem--anki-connect-action 'addNote note))
         (response (org-roam-rem--anki-connect-invoke-result action)))
    (org-roam-rem--set-note-id response)))

(defun org-roam-rem--update-anki-note (note)
  ;;update existing note
  (let* ((note '((note . ,(org-roam-rem--anki-connect-map-note note))))
         (action (org-roam-rem--anki-connect-action 'updateNoteFields note)))))

(defun org-roam-rem--anki-connect-map-note (note)
  "Convert NOTE to the form that AnkiConnect accepts."
  (let ((card (list (cons "id" (org-roam-rem-anki-card-note-id note))
          (cons "deckName" (org-roam-rem-anki-card-deck note))
          (cons "fields" (list
                          (cons "Front" (org-roam-rem-anki-card-front note))
                          (cons "Back" (org-roam-rem-anki-card-back note))))
          (cons "modelName" "Basic"))))
    (message "%s" card)
    card))
          ;; Convert tags to a vector since empty list is identical to nil
          ;; which will become None in Python, but AnkiConnect requires it
          ;; to be type of list.

(defun org-roam-rem--anki-connect-action (action &optional params version)
  (let (a)
    (when version
      (push `(version . ,version) a))
    (when params
      (push `(params . ,params) a))
    (push `(action . ,action) a)))


(defun org-roam-rem--anki-connect-invoke (action &optional params)
  "Invoke AnkiConnect with ACTION and PARAMS."
  (let ((request-body (json-encode (org-roam-rem--anki-connect-action action params 5)))
        (request-backend 'curl)
        (json-array-type 'list)
        reply err)

    (let ((response (request (format "http://%s:%s"
                                     org-roam-rem-anki-connect-listening-address
                                     org-roam-rem-anki-connect-listening-port)
                             :type "POST"
                             :parser 'json-read
                             :data request-body
                             :success (cl-function (lambda (&key data &allow-other-keys)
                                                     (setq reply data)))
                             :error (cl-function (lambda (&key _ &key error-thrown &allow-other-keys)
                                                   (setq err (string-trim (cdr error-thrown)))))
                             :sync t)))

      ;; HACK: With sync set to t, `request' waits for curl process to
      ;; exit, then response data becomes available, but callbacks
      ;; might not be called right away but at a later time, that's
      ;; why here we manually invoke callbacks to receive the result.
      (unless (request-response-done-p response)
        (request--curl-callback (get-buffer-process (request-response--buffer response)) "finished\n")))

    (when err (error "Error communicating with AnkiConnect using cURL: %s" err))
    (or reply (error "Got empty reply from AnkiConnect"))))

(defun org-roam-rem--anki-connect-store-media-file (path)
  "Store media file for PATH, which is an absolute file name.
The result is the path to the newly stored media file."
  (let* ((hash (secure-hash 'sha1 path))
         (media-file-name (format "%s-%s%s"
                                  (file-name-base path)
                                  hash
                                  (file-name-extension path t)))
         content)
    (when (equal :json-false (org-roam-rem--anki-connect-invoke-result
                              "retrieveMediaFile"
                              `((filename . ,media-file-name))))
      (message "Storing media file to Anki for %s..." path)
      (setq content (base64-encode-string
		     (with-temp-buffer
		       (insert-file-contents path)
		       (buffer-string))))
      (org-roam-rem--anki-connect-invoke-result
       "storeMediaFile"
       `((filename . ,media-file-name)
         (data . ,content))))
    media-file-name))


(defun org-roam-rem--translate-latex-delimiters (latex-code)
  (catch 'done
    (let ((delimiter-map (list (list (cons (format "^%s" (regexp-quote "$$")) "[$$]")
                                     (cons (format "%s$" (regexp-quote "$$")) "[/$$]"))
                               (list (cons (format "^%s" (regexp-quote "$")) "[$]")
                                     (cons (format "%s$" (regexp-quote "$")) "[/$]"))
                               (list (cons (format "^%s" (regexp-quote "\\(")) "[$]")
                                     (cons (format "%s$" (regexp-quote "\\)")) "[/$]"))
                               (list (cons (format "^%s" (regexp-quote "\\[")) "[$$]")
                                     (cons (format "%s$" (regexp-quote "\\]")) "[/$$]"))))
          (matched nil))
      (save-match-data
        (dolist (pair delimiter-map)
          (dolist (delimiter pair)
            (when (setq matched (string-match (car delimiter) latex-code))
              (setq latex-code (replace-match (cdr delimiter) t t latex-code))))
          (when matched (throw 'done latex-code)))))
    latex-code))

(defun org-roam-rem--translate-latex-delimiters-to-anki-mathjax-delimiters (latex-code)
  (catch 'done
    (let ((delimiter-map (list (list (cons (format "^%s" (regexp-quote "$$")) "\\[")
                                     (cons (format "%s$" (regexp-quote "$$")) "\\]"))
                               (list (cons (format "^%s" (regexp-quote "$")) "\\(")
                                     (cons (format "%s$" (regexp-quote "$")) "\\)"))))
          (matched nil))
      (save-match-data
        (dolist (pair delimiter-map)
          (dolist (delimiter pair)
            (when (setq matched (string-match (car delimiter) latex-code))
              (setq latex-code (replace-match (cdr delimiter) t t latex-code))))
          (when matched (throw 'done latex-code)))))
    latex-code))

(defun org-roam-rem--wrap-latex (content)
  "Wrap CONTENT with Anki-style latex markers."
  (format "<p><div>[latex]</div>%s<div>[/latex]</div></p>" content))

(defun org-roam-rem--wrap-latex-for-mathjax (content)
  "Wrap CONTENT for Anki's native MathJax support."
  (format "<p>%s</p>" content))

(defun org-roam-rem--wrap-div (content)
  (format "<div>%s</div>" content))

(defun org-roam-rem--ox-latex (latex _contents _info)
  "Transcode LATEX from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((code (org-remove-indentation (org-element-property :value latex))))
    (setq code
          (pcase (org-element-type latex)
            ('latex-fragment (org-roam-rem--translate-latex-delimiters code))
            ('latex-environment (org-roam-rem--wrap-latex
                                 (mapconcat #'org-roam-rem--wrap-div
                                            (split-string (org-html-encode-plain-text code) "\n")
                                            "")))))

    (if org-roam-rem-break-consecutive-braces-in-latex
        (replace-regexp-in-string "}}" "} } " code)
      code)))

(defun org-roam-rem--ox-latex-for-mathjax (latex _contents _info)
  "Transcode LATEX from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((code (org-remove-indentation (org-element-property :value latex))))
    (setq code
          (pcase (org-element-type latex)
            ('latex-fragment (org-roam-rem--translate-latex-delimiters-to-anki-mathjax-delimiters code))
            ('latex-environment (org-roam-rem--wrap-latex-for-mathjax
                                 (mapconcat #'org-roam-rem--wrap-div
                                            (split-string (org-html-encode-plain-text code) "\n")
                                            "")))))

    (if org-roam-rem-break-consecutive-braces-in-latex
        (replace-regexp-in-string "}}" "} } " code)
      code)))

(defun org-roam-rem--ox-html-link (oldfun link desc info)
  "When LINK is a link to local file, transcodes it to html and stores the target file to Anki, otherwise calls OLDFUN for help.
The implementation is borrowed and simplified from ox-html."

  (or (catch 'giveup
        (unless (plist-get info :org-roam-rem-mode)
          (throw 'giveup nil))

        (let* ((type (org-element-property :type link))
               (raw-path (org-element-property :path link))
               (desc (org-string-nw-p desc))
               (path
                (cond
                 ((string= type "file")
                  ;; Possibly append `:html-link-home' to relative file
                  ;; name.
                  (let ((inhibit-message nil)
                        (home (and (plist-get info :html-link-home)
                                   (org-trim (plist-get info :html-link-home)))))
                    (when (and home
                               (plist-get info :html-link-use-abs-url)
                               (file-name-absolute-p raw-path))
                      (setq raw-path (concat (file-name-as-directory home) raw-path)))
                    ;; storing file to Anki and return the modified path
                    (org-roam-rem--anki-connect-store-media-file (expand-file-name (url-unhex-string raw-path)))))
                 (t (throw 'giveup nil))))
               (attributes-plist
                (let* ((parent (org-export-get-parent-element link))
                       (link (let ((container (org-export-get-parent link)))
                               (if (and (eq (org-element-type container) 'link)
                                        (org-html-inline-image-p link info))
                                   container
                                 link))))
                  (and (eq (org-element-map parent 'link 'identity info t) link)
                       (org-export-read-attribute :attr_html parent))))
               (attributes
                (let ((attr (org-html--make-attribute-string attributes-plist)))
                  (if (org-string-nw-p attr) (concat " " attr) ""))))
          (cond
           ;; Image file.
           ((and (plist-get info :html-inline-images)
                 (org-export-inline-image-p
                  link (plist-get info :html-inline-image-rules)))
            (org-html--format-image path attributes-plist info))

           ;; Audio file.
           ((string-suffix-p ".mp3" path t)
              (format "[sound:%s]" path))

           ;; External link with a description part.
           ((and path desc) (format "<a href=\"%s\"%s>%s</a>"
                                    (org-html-encode-plain-text path)
                                    attributes
                                    desc))

           ;; External link without a description part.
           (path (let ((path (org-html-encode-plain-text path)))
                   (format "<a href=\"%s\"%s>%s</a>"
                           path
                           attributes
                           (org-link-unescape path))))

           (t (throw 'giveup nil)))))
      (funcall oldfun link desc info)))

(defun org-roam-rem--deck-names ()
  "Get all decks names from Anki."
  (org-roam-rem--anki-connect-invoke-result "deckNames"))

(define-minor-mode org-roam-rem-mode
  "org-roam-rem-mode"
  :lighter "org-roam-rem"
  (if org-roam-rem-mode (org-roam-rem-setup-minor-mode)
    (org-roam-rem-teardown-minor-mode)))

(defun org-roam-rem-setup-minor-mode ()
  "Set up this minor mode."
  (advice-add 'org-html-link :around #'org-roam-rem--ox-html-link))

(defun org-roam-rem-teardown-minor-mode ()
  "Tear down this minor mode."
  (remove-hook 'org-property-allowed-value-functions #'org-roam-rem--get-allowed-values-for-property t))
(provide 'org-roam-rem)
;;; org-roam-rem.el ends here
