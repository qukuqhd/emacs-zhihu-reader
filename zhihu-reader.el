;;; zhihu-reader.el --- Read Zhihu inside Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 qukuqhd
;; Author: qukuqhd
;; Version: 0.3.0
;; Package-Requires: ((emacs "31.1"))
;; Keywords: hypermedia, convenience
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; A read-only Zhihu client for Emacs.  It follows the architecture of
;; `zhihu-vscode-reader', `zhihu_obsidian' and `zhihu-fisher-vscode':
;;
;;   external browser login -> cookies -> list/detail APIs -> Org buffer
;;
;; Login is deliberately external: use Zen or Firefox, then the reader
;; picks up z_c0/__zse_ck automatically.  EAF is only a fallback for
;; JavaScript-heavy pages.
;;
;; Lists are rendered as nested Org trees:
;;
;;   * Question
;;   ** Answer
;;      content
;;   *** Comments (N)
;;   ** Another answer
;;
;; TAB folds, RET enters question/answer detail, c loads comments,
;; g refreshes, q quits.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'json)
(require 'url)
(require 'url-util)
(require 'url-http)
(require 'shr)
(require 'org)
(require 'org-element)
(require 'dom)
(require 'sqlite)

(defgroup zhihu-reader nil
  "Read Zhihu inside Emacs."
  :group 'hypermedia
  :prefix "zhihu-reader-")

(defcustom zhihu-reader-cookie-db
  (expand-file-name "~/.local/share/eaf.py/QtWebEngine/eaf/Cookies")
  "EAF QtWebEngine cookie database, used only as a fallback."
  :type 'file
  :group 'zhihu-reader)

(defcustom zhihu-reader-cookie-file
  (expand-file-name "~/.local/state/zhihu-reader/cookies")
  "File containing a manually supplied Zhihu Cookie header.
Written by `zhihu-reader-set-cookie' with mode 0600."
  :type 'file
  :group 'zhihu-reader)

(defcustom zhihu-reader-cookie-function nil
  "Optional function returning a Zhihu Cookie header string."
  :type '(choice (const :tag "Unset" nil) function)
  :group 'zhihu-reader)

(defcustom zhihu-reader-login-browser "zen-browser"
  "External browser used by `zhihu-reader-login'."
  :type '(choice string (const :tag "System default" nil))
  :group 'zhihu-reader)

(defcustom zhihu-reader-browser-cookie-files
  (list "~/.config/zen/*/cookies.sqlite"
        "~/.config/mozilla/firefox/*/cookies.sqlite"
        "~/.mozilla/firefox/*/cookies.sqlite")
  "Firefox/Zen cookie databases scanned for a Zhihu session."
  :type '(repeat string)
  :group 'zhihu-reader)

(defcustom zhihu-reader-feed-url
  "https://www.zhihu.com/api/v3/feed/topstory/recommend?action=down&ad_interval=-10&desktop=true&page_number=1"
  "Zhihu recommendation feed endpoint."
  :type 'string
  :group 'zhihu-reader)

(defcustom zhihu-reader-seen-file
  (expand-file-name "~/.local/state/zhihu-reader/seen.eld")
  "File used to persist recommendation IDs that have been shown."
  :type 'file
  :group 'zhihu-reader)

(defcustom zhihu-reader-filter-seen t
  "Filter recommendation items that were already shown in previous sessions."
  :type 'boolean
  :group 'zhihu-reader)

(defvar zhihu-reader--seen (make-hash-table :test #'equal))
(defvar zhihu-reader--seen-loaded nil)

(defcustom zhihu-reader-article-directory
  (expand-file-name "~/org/zhihu/articles/")
  "Directory used by `zhihu-reader-new-article'."
  :type 'directory
  :group 'zhihu-reader)

(defcustom zhihu-reader-hot-url
  "https://www.zhihu.com/api/v3/feed/topstory/hot-lists/total?limit=50&desktop=true"
  "Zhihu hot-list endpoint."
  :type 'string
  :group 'zhihu-reader)

(defcustom zhihu-reader-search-url
  "https://www.zhihu.com/api/v4/search_v3?t=general&q=%s&correction=1&offset=0&limit=20"
  "Zhihu search endpoint.  %s is the URL-encoded query."
  :type 'string
  :group 'zhihu-reader)

(defcustom zhihu-reader-user-agent
  "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/140.0 Safari/537.36"
  "User-Agent used for Zhihu requests."
  :type 'string
  :group 'zhihu-reader)

(defcustom zhihu-reader-enable-write nil
  "Enable destructive Zhihu operations such as editing answers or comments.
Leave this nil for read-only browsing."
  :type 'boolean
  :group 'zhihu-reader)

(defcustom zhihu-reader-zhihu-library-path
  (expand-file-name "~/.emacs.d/site-lisp/zhihu-el")
  "Directory containing zhihu.el, ox-zhihu.el and plz.el."
  :type 'directory
  :group 'zhihu-reader)

(defcustom zhihu-reader-max-excerpt 1200
  "Maximum number of characters of an excerpt shown in a list."
  :type 'integer
  :group 'zhihu-reader)

(defcustom zhihu-reader-render-images t
  "When non-nil, download and inline images from answer/article HTML.
Images are cached under `zhihu-reader-image-cache'.  Use
`zhihu-reader-toggle-images' in a reader buffer to switch this on."
  :type 'boolean
  :group 'zhihu-reader)

(defcustom zhihu-reader-image-cache
  (expand-file-name "~/.cache/zhihu-reader/images")
  "Directory used for downloaded Zhihu images."
  :type 'directory
  :group 'zhihu-reader)

(defcustom zhihu-reader-image-width 500
  "Maximum inline image width in pixels."
  :type 'integer
  :group 'zhihu-reader)

(defvar-local zhihu-reader--view nil
  "Current reader view: (KIND ARG).")
(defvar-local zhihu-reader--generation 0
  "Incremented whenever a reader buffer is replaced.")

(defconst zhihu-reader--main-buffer "*Zhihu*")
(defconst zhihu-reader--question-buffer "*Zhihu Question*")
(defconst zhihu-reader--answer-buffer "*Zhihu Answer*")

(defvar zhihu-reader--page-cache (make-hash-table :test #'equal)
  "Cache of rendered list pages, keyed by view.")
(defvar zhihu-reader--page-points (make-hash-table :test #'equal)
  "Saved point/window-start for cached pages.")
(defvar zhihu-reader--navigation-anchor nil
  "Point/window-start to restore after a page change.")

;;;; Cookies

(defun zhihu-reader--rows-to-header (rows)
  "Convert (NAME VALUE HOST) ROWS to a Cookie header."
  (let ((seen (make-hash-table :test #'equal))
        (parts nil))
    (dolist (row rows)
      (let ((name (nth 0 row))
            (value (nth 1 row)))
        (unless (or (null value) (string-empty-p value) (gethash name seen))
          (puthash name t seen)
          (push (concat name "=" value) parts))))
    (and parts (mapconcat #'identity (nreverse parts) "; "))))

(defun zhihu-reader--login-p (cookie)
  "Return non-nil if COOKIE contains a non-empty z_c0."
  (and (stringp cookie)
       (string-match-p "\\(?:^\\|; \\)z_c0=[^;]" cookie)))

(defun zhihu-reader--file-cookies ()
  "Return cookies from `zhihu-reader-cookie-file'."
  (when (file-readable-p zhihu-reader-cookie-file)
    (with-temp-buffer
      (insert-file-contents zhihu-reader-cookie-file)
      (let ((cookie (string-trim (buffer-string))))
        (unless (string-empty-p cookie) cookie)))))

(defun zhihu-reader--browser-cookies ()
  "Return the newest Firefox/Zen Zhihu Cookie header containing z_c0."
  (let ((best nil) (best-time 0))
    (dolist (pattern zhihu-reader-browser-cookie-files)
      (dolist (file (file-expand-wildcards (expand-file-name pattern)))
        (when (and (file-readable-p file)
                   (> (float-time (file-attribute-modification-time
                                   (file-attributes file)))
                      best-time))
          (condition-case nil
              (let ((db (sqlite-open file)))
                (unwind-protect
                    (let ((header
                           (zhihu-reader--rows-to-header
                            (sqlite-select
                             db
                             "select name, value, host from moz_cookies
                              where host like '%zhihu.com' and value != ''
                              order by case when host = '.zhihu.com' then 0 else 1 end, name"))))
                      (when (and header (zhihu-reader--login-p header))
                        (setq best header
                              best-time (float-time
                                         (file-attribute-modification-time
                                          (file-attributes file))))))
                  (sqlite-close db)))
            (error nil)))))
    best))

(defun zhihu-reader--eaf-cookies ()
  "Return cookies from EAF's QtWebEngine database, if present."
  (when (file-readable-p zhihu-reader-cookie-db)
    (condition-case nil
        (let ((db (sqlite-open zhihu-reader-cookie-db)))
          (unwind-protect
              (zhihu-reader--rows-to-header
               (sqlite-select
                db
                "select name, value, host_key from cookies
                 where host_key like '%zhihu.com' and value != ''
                 order by case when host_key like '.zhihu.com' then 0 else 1 end, name"))
            (sqlite-close db)))
      (error nil))))

(defun zhihu-reader--cookies ()
  "Return a Zhihu Cookie header from the best available source."
  (or (and (functionp zhihu-reader-cookie-function)
           (funcall zhihu-reader-cookie-function))
      (zhihu-reader--file-cookies)
      (zhihu-reader--browser-cookies)
      (zhihu-reader--eaf-cookies)
      (user-error
       "没有找到知乎 Cookie；请用 Zen/Firefox 登录，或 M-x zhihu-reader-set-cookie")))

(defun zhihu-reader--require-login ()
  "Return cookies or signal a login error."
  (let ((cookie (zhihu-reader--cookies)))
    (unless (zhihu-reader--login-p cookie)
      (user-error "知乎登录态未找到（z_c0）；请先用 Zen/Firefox 登录"))
    cookie))

;;;; HTTP

(defun zhihu-reader--fetch-json (url)
  "Fetch URL with Zhihu cookies and return parsed JSON."
  (let* ((cookie (zhihu-reader--require-login))
         (url-request-extra-headers
          `(("Cookie" . ,cookie)
            ("User-Agent" . ,zhihu-reader-user-agent)
            ("Referer" . "https://www.zhihu.com/")
            ("Origin" . "https://www.zhihu.com")
            ("x-requested-with" . "fetch")
            ("x-api-version" . "3.0.53")
            ("Accept" . "application/json, text/plain, */*")))
         (buf (condition-case err
                  (url-retrieve-synchronously url t t 30)
                (error (user-error "知乎请求失败: %s" (error-message-string err))))))
    (unless buf
      (user-error "知乎请求超时"))
    (unwind-protect
        (with-current-buffer buf
          (goto-char (point-min))
          (let ((status url-http-response-status))
            (unless (equal status 200)
              (goto-char (point-max))
              (user-error "知乎请求失败 (HTTP %s): %s"
                          status
                          (string-trim
                           (buffer-substring-no-properties
                            (max (point-min) (- (point-max) 250))
                            (point-max))))))
          (goto-char url-http-end-of-headers)
          (json-parse-buffer :object-type 'alist
                             :null-object nil
                             :false-object nil))
      (kill-buffer buf))))

(defun zhihu-reader--fetch-json-async (url success &optional failure)
  "Fetch URL asynchronously; call SUCCESS with JSON or FAILURE with a string.
The request uses the current Zhihu cookies and does not block Emacs."
  (let* ((cookie (zhihu-reader--require-login))
         (url-request-extra-headers
          `(("Cookie" . ,cookie)
            ("User-Agent" . ,zhihu-reader-user-agent)
            ("Referer" . "https://www.zhihu.com/")
            ("Origin" . "https://www.zhihu.com")
            ("x-requested-with" . "fetch")
            ("x-api-version" . "3.0.53")
            ("Accept" . "application/json, text/plain, */*")))
         (callback
          (lambda (status)
            (let ((request-buffer (current-buffer)))
              (unwind-protect
                  (condition-case err
                      (progn
                        (when-let* ((network-error (plist-get status :error)))
                          (signal 'error
                                  (list (format "网络错误: %s"
                                                (error-message-string network-error)))))
                        (goto-char (point-min))
                        (let ((code url-http-response-status))
                          (unless (equal code 200)
                            (goto-char (point-max))
                            (signal 'error
                                    (list (format "知乎请求失败 (HTTP %s): %s"
                                                  code
                                                  (string-trim
                                                   (buffer-substring-no-properties
                                                    (max (point-min) (- (point-max) 250))
                                                    (point-max))))))))
                        (goto-char url-http-end-of-headers)
                        (let ((json (json-parse-buffer :object-type 'alist
                                                       :null-object nil
                                                       :false-object nil)))
                          (funcall success json)))
                    (error
                     (if failure
                         (funcall failure (error-message-string err))
                       (message "知乎加载失败: %s" (error-message-string err)))))
                (when (buffer-live-p request-buffer)
                  (kill-buffer request-buffer))))))
         (request (url-retrieve url callback nil t)))
    (when request
      (message "知乎加载中…"))
    request))

(defun zhihu-reader--prepare-buffer (title view buffer-name)
  "Prepare and display a loading buffer."
  (let ((buffer (get-buffer-create (or buffer-name zhihu-reader--main-buffer))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (zhihu-reader-mode)
        (setq-local zhihu-reader--generation (1+ zhihu-reader--generation))
        (setq-local zhihu-reader--view view)
        (setq-local header-line-format
                    (format " Zhihu %s  | TAB fold  RET enter  c comments  ] next  [ prev  i images  g refresh  q quit"
                            title))
        (insert (format "#+TITLE: %s\n#+STARTUP: overview\n\n正在加载…\n" title))
        (setq buffer-read-only t)))
    (pop-to-buffer buffer)
    buffer))

(defun zhihu-reader--show-load-error (buffer error)
  "Replace BUFFER's loading text with ERROR."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "#+TITLE: 知乎加载失败\n\n%s\n" error))
        (setq buffer-read-only t)))
    (setq zhihu-reader--navigation-anchor nil)
    (message "知乎加载失败: %s" error)))

(defun zhihu-reader--generation-current-p (buffer generation)
  "Return non-nil if BUFFER is alive and still at GENERATION."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (equal zhihu-reader--generation generation))))

(defun zhihu-reader--capture-navigation-anchor ()
  "Remember the current screen position before changing pages."
  (when (derived-mode-p 'zhihu-reader-mode)
    (let ((window (selected-window)))
      (setq zhihu-reader--navigation-anchor
            (list (point) (and (window-live-p window)
                               (window-start window)))))))

(defun zhihu-reader--restore-navigation-anchor ()
  "Restore the screen position saved before a page change."
  (when zhihu-reader--navigation-anchor
    (let* ((anchor zhihu-reader--navigation-anchor)
           (point (nth 0 anchor))
           (start (nth 1 anchor))
           (window (selected-window)))
      (setq zhihu-reader--navigation-anchor nil)
      (when (and (integer-or-marker-p point)
                 (<= (point-min) point)
                 (<= point (point-max)))
        (goto-char point))
      (when (and start (window-live-p window)
                 (<= (point-min) start)
                 (<= start (point-max)))
        (ignore-errors (set-window-start window start))))))

(defun zhihu-reader--remember-point ()
  "Remember point/window start for the current view."
  (when zhihu-reader--view
    (puthash zhihu-reader--view
             (cons (point) (window-start))
             zhihu-reader--page-points)))

(defun zhihu-reader--restore-point (key)
  "Restore point/window start saved for KEY."
  (when-let* ((saved (gethash key zhihu-reader--page-points))
              ((consp saved)))
    (let ((position (car saved))
          (start (cdr saved)))
      (when (and (integer-or-marker-p position)
                 (<= (point-min) position)
                 (<= position (point-max)))
        (goto-char position))
      (when (and start (window-live-p (selected-window)))
        (set-window-start (selected-window) start)))))

(defun zhihu-reader--show-cached (key)
  "Render cached page KEY if present.  Return non-nil on success."
  (let ((entry (gethash key zhihu-reader--page-cache)))
    (when entry
      (let ((anchor zhihu-reader--navigation-anchor))
        (zhihu-reader--render (car entry) (cdr entry) key)
        (unless anchor
          (zhihu-reader--restore-point key)))
      t)))

(defun zhihu-reader--async-list (title view url buffer-name make-result
                                      &optional cache-key)
  "Fetch URL asynchronously and render the result in BUFFER-NAME.
MAKE-RESULT receives JSON and returns (TITLE . ITEMS)."
  (let* ((buffer (zhihu-reader--prepare-buffer title view buffer-name))
         (generation (with-current-buffer buffer zhihu-reader--generation)))
    (zhihu-reader--fetch-json-async
     url
     (lambda (json)
       (when (and (zhihu-reader--generation-current-p buffer generation)
                  (equal view
                         (buffer-local-value 'zhihu-reader--view buffer)))
         (condition-case err
             (let ((result (funcall make-result json)))
               (when cache-key
                 (puthash cache-key result zhihu-reader--page-cache))
               (zhihu-reader--render (car result) (cdr result) view buffer-name)
               (when cache-key
                 (zhihu-reader--restore-point cache-key)))
           (error (zhihu-reader--show-load-error buffer (error-message-string err))))))
     (lambda (error)
       (when (and (zhihu-reader--generation-current-p buffer generation)
                  (equal view
                         (buffer-local-value 'zhihu-reader--view buffer)))
         (zhihu-reader--show-load-error buffer error))))))

;;;; Normalization

(defun zhihu-reader--alist-get (key alist)
  "Like `alist-get' but tolerate strings and nil."
  (and (listp alist) (alist-get key alist)))

(defun zhihu-reader--first (&rest values)
  "Return the first non-nil, non-empty string in VALUES."
  (seq-find (lambda (v)
              (and v (not (and (stringp v) (string-empty-p v)))))
            values))

(defun zhihu-reader--string (value)
  "Convert VALUE to a string."
  (cond ((null value) "")
        ((stringp value) value)
        ((numberp value) (number-to-string value))
        (t (format "%s" value))))

(defun zhihu-reader--time-string (value)
  "Format Zhihu epoch VALUE as a human-readable time."
  (let ((seconds (cond ((numberp value) value)
                       ((and (stringp value)
                             (string-match-p "\\`[0-9]+\\'" value))
                        (string-to-number value))
                       (t nil))))
    (when (and seconds (> seconds 0))
      (when (> seconds 100000000000) ; milliseconds
        (setq seconds (/ seconds 1000.0)))
      (let ((delta (- (float-time) seconds)))
        (cond
         ((< delta 60) "刚刚")
         ((< delta 3600) (format "%d分钟前" (floor (/ delta 60))))
         ((< delta 86400) (format "%d小时前" (floor (/ delta 3600))))
         ((< delta 604800) (format "%d天前" (floor (/ delta 86400))))
         (t (format-time-string "%Y-%m-%d" (seconds-to-time seconds))))))))

(defun zhihu-reader--strip-html (html)
  "Render HTML fragment as plain text."
  (if (or (null html) (not (stringp html)) (string-empty-p html))
      ""
    (condition-case nil
        (with-temp-buffer
          (insert html)
          (let* ((shr-width 10000)
                 (shr-inhibit-images t)
                 (shr-use-fonts nil)
                 (dom (libxml-parse-html-region (point-min) (point-max))))
            (erase-buffer)
            (shr-insert-document dom)
            (substring-no-properties (string-trim (buffer-string)))))
      (error ""))))

(defun zhihu-reader--dom-text (node)
  "Return textual children of NODE."
  (if (stringp node)
      node
    (mapconcat #'zhihu-reader--dom-text
               (if (consp node) (dom-children node) nil)
               "")))

(defun zhihu-reader--clean-html-text (text)
  "Normalize whitespace in HTML text."
  (replace-regexp-in-string "[ \t\n\r]+" " " (or text "")))

(defun zhihu-reader--dom-to-org (node &optional list-type)
  "Convert DOM NODE to an Org string.
LIST-TYPE is `ul' or `ol' when inside a list."
  (cond
   ((stringp node)
    (zhihu-reader--clean-html-text node))
   ((not (consp node)) "")
   ((not (dom-tag node))
    (mapconcat (lambda (child) (zhihu-reader--dom-to-org child list-type))
               (dom-children node) ""))
   (t
    (let* ((tag (dom-tag node))
           (children (dom-children node))
           (inner (lambda (&optional type)
                    (mapconcat (lambda (child)
                                 (zhihu-reader--dom-to-org child (or type list-type)))
                               children "")))
           (text (lambda () (zhihu-reader--clean-html-text
                             (zhihu-reader--dom-text node)))))
      (pcase tag
        ('br "\n")
        ('hr "\n---\n")
        ((or 'p 'div 'section 'article 'header 'footer 'figcaption)
         (concat (funcall inner) "\n\n"))
        ((or 'h1 'h2 'h3 'h4 'h5 'h6)
         (concat "*" (string-trim (funcall text)) "*\n\n"))
        ('blockquote
         (concat "#+begin_quote\n" (string-trim (funcall inner)) "\n#+end_quote\n\n"))
        ('pre
         (concat "#+begin_example\n"
                 (string-trim (zhihu-reader--dom-text node))
                 "\n#+end_example\n\n"))
        ('code
         (concat "~" (string-trim (funcall text)) "~"))
        ('strong (concat "*" (string-trim (funcall inner)) "*"))
        ('b (concat "*" (string-trim (funcall inner)) "*"))
        ('em (concat "/" (string-trim (funcall inner)) "/"))
        ('i (concat "/" (string-trim (funcall inner)) "/"))
        ('del (concat "+" (string-trim (funcall inner)) "+"))
        ('u (concat "_" (string-trim (funcall inner)) "_"))
        ('sup (concat "^{" (string-trim (funcall inner)) "}"))
        ('sub (concat "_{" (string-trim (funcall inner)) "}"))
        ('a
         (let* ((href (or (dom-attr node 'href) ""))
                (href (if (string-match "link\\.zhihu\\.com/?target=\\([^&]+\\)" href)
                          (url-unhex-string (match-string 1 href))
                        href))
                (label (string-trim (funcall inner))))
           (if (and href (string-match-p "\\`https?://" href))
               (org-make-link-string href (if (string-empty-p label) href label))
             label)))
        ('ul
         (concat "\n" (funcall inner 'ul) "\n"))
        ('ol
         (concat "\n" (funcall inner 'ol) "\n"))
        ('li
         (concat (if (eq list-type 'ol) "1. " "- ")
                 (string-trim (funcall inner))
                 "\n"))
        ('span
         (let ((tex (dom-attr node 'data-tex)))
           (if (and tex (not (string-empty-p tex)))
               (concat "$" tex "$")
             (funcall inner))))
        ('img "")
        ('table
         (concat "#+begin_example\n" (string-trim (funcall text)) "\n#+end_example\n\n"))
        (_ (funcall inner)))))))

(defun zhihu-reader--content-blocks-to-org (blocks)
  "Convert Zhihu Pin content BLOCKS (a vector of alists) to Org."
  (mapconcat
   (lambda (block)
     (when (listp block)
       (let ((type (zhihu-reader--alist-get 'type block)))
         (cond
          ((equal type "text")
           (zhihu-reader--html-to-org
            (or (zhihu-reader--alist-get 'own_text block)
                (zhihu-reader--alist-get 'content block)
                (zhihu-reader--alist-get 'title block))))
          ((equal type "image")
           (let ((url (or (zhihu-reader--alist-get 'original_url block)
                          (zhihu-reader--alist-get 'url block))))
             (when (and (stringp url) (not (string-empty-p url)))
               (org-make-link-string url "图片"))))
          (t "")))))
   (if (vectorp blocks) (append blocks nil) blocks)
   "\n\n"))

(defun zhihu-reader--html-to-org (html)
  "Convert an HTML fragment to an Org string."
  (cond
   ((null html) "")
   ((vectorp html) (zhihu-reader--content-blocks-to-org html))
   ((listp html) (zhihu-reader--content-blocks-to-org html))
   ((not (stringp html)) (format "%s" html))
   ((string-empty-p html) "")
   (t
    (condition-case nil
        (let* ((dom (with-temp-buffer
                      (insert html)
                      (libxml-parse-html-region (point-min) (point-max))))
               (org (zhihu-reader--dom-to-org dom)))
          (string-trim
           (replace-regexp-in-string "\n\\{3,\\}" "\n\n" org)))
      (error (zhihu-reader--strip-html html))))))

(defun zhihu-reader--block-images (blocks)
  "Extract image URLs from Zhihu content BLOCKS."
  (delete-dups
   (delq nil
         (mapcar
          (lambda (block)
            (when (and (listp block)
                       (equal (zhihu-reader--alist-get 'type block) "image"))
              (or (zhihu-reader--alist-get 'original_url block)
                  (zhihu-reader--alist-get 'url block))))
          (if (vectorp blocks) (append blocks nil) blocks)))))

(defun zhihu-reader--extract-images (html)
  "Extract image URLs from HTML or Zhihu content blocks."
  (cond
   ((vectorp html) (zhihu-reader--block-images html))
   ((listp html) (zhihu-reader--block-images html))
   ((not (stringp html)) nil)
   (t
    (when (and (stringp html) (not (string-empty-p html)))
      (let* ((dom (with-temp-buffer
                    (insert html)
                    (libxml-parse-html-region (point-min) (point-max))))
             (images (dom-by-tag dom 'img)))
        (delete-dups
         (delq nil
               (mapcar (lambda (node)
                         (let ((url (or (dom-attr node 'data-original)
                                        (dom-attr node 'data-actualsrc)
                                        (dom-attr node 'src))))
                           (and (stringp url)
                                (string-match-p "\\`https?://" url)
                                url)))
                       images))))))))

(defun zhihu-reader--truncate (text &optional limit)
  "Trim TEXT to LIMIT or `zhihu-reader-max-excerpt' characters."
  (let ((text (replace-regexp-in-string "[ \t\n\r]+" " " (or text "")))
        (limit (or limit zhihu-reader-max-excerpt)))
    (if (> (length text) limit)
        (concat (substring text 0 limit) "…")
      text)))

(defun zhihu-reader--web-url (url)
  "Convert a Zhihu API URL to a browser-facing URL when possible."
  (cond
   ((not (stringp url)) nil)
   ((string-match "\\`https?://api\\.zhihu\\.com/articles/\\([0-9]+\\)" url)
    (format "https://zhuanlan.zhihu.com/p/%s" (match-string 1 url)))
   ((string-match "\\`https?://api\\.zhihu\\.com/answers/\\([0-9]+\\)" url)
    (format "https://www.zhihu.com/answer/%s" (match-string 1 url)))
   ((string-match "\\`https?://api\\.zhihu\\.com/questions/\\([0-9]+\\)" url)
    (format "https://www.zhihu.com/question/%s" (match-string 1 url)))
   ((string-match-p "\\`https?://api\\.zhihu\\.com/" url) nil)
   (t url)))

(defun zhihu-reader--item-target (entry)
  "Return the content target of ENTRY."
  (or (zhihu-reader--alist-get 'target entry)
      (zhihu-reader--alist-get 'object entry)
      entry))

(defun zhihu-reader--kind (entry target)
  "Infer the content kind of ENTRY/TARGET."
  (let ((type (zhihu-reader--string
               (or (zhihu-reader--alist-get 'type target)
                   (zhihu-reader--alist-get 'type entry))))
        (url (zhihu-reader--string
              (or (zhihu-reader--alist-get 'url target)
                  (zhihu-reader--alist-get 'url entry)))))
    (cond
     ((member type '("answer" "article" "question" "pin")) type)
     ((string-match-p "/answers/" url) "answer")
     ((string-match-p "/articles/" url) "article")
     ((string-match-p "/questions/" url) "question")
     (t type))))

(defun zhihu-reader--item-url (entry target kind)
  "Return a canonical URL for ENTRY/TARGET of KIND."
  (let* ((id (zhihu-reader--string (zhihu-reader--alist-get 'id target)))
         (question (zhihu-reader--alist-get 'question target))
         (author (or (zhihu-reader--alist-get 'author target)
                     (zhihu-reader--alist-get 'author entry)))
         (question-id (zhihu-reader--string
                       (or (zhihu-reader--alist-get 'id question)
                           (zhihu-reader--alist-get 'question_id target)
                           (and (equal kind "question") id))))
         (link (or (zhihu-reader--alist-get 'link entry)
                   (zhihu-reader--alist-get 'link target)))
         (raw (seq-find
               #'identity
               (mapcar #'zhihu-reader--web-url
                       (list (zhihu-reader--alist-get 'url target)
                             (zhihu-reader--alist-get 'url entry)
                             (zhihu-reader--alist-get 'url link))))))
    (or raw
        (pcase kind
          ((or "article" "post")
           (format "https://zhuanlan.zhihu.com/p/%s" id))
          ("answer"
           (if (and (not (string-empty-p question-id))
                    (not (string-empty-p id)))
               (format "https://www.zhihu.com/question/%s/answer/%s"
                       question-id id)
             (format "https://www.zhihu.com/answer/%s" id)))
          ("question"
           (format "https://www.zhihu.com/question/%s"
                   (or question-id id)))))))

(defun zhihu-reader--item-title (entry target kind)
  "Return a title for ENTRY/TARGET of KIND."
  (let* ((question (zhihu-reader--alist-get 'question target))
         (title (zhihu-reader--first
                 (and (equal kind "article")
                      (zhihu-reader--alist-get 'title target))
                 (zhihu-reader--alist-get 'title question)
                 (zhihu-reader--alist-get 'title target)
                 (zhihu-reader--alist-get 'title entry)
                 (zhihu-reader--alist-get 'detail_text entry)
                 (zhihu-reader--alist-get 'detail_text target)
                 (zhihu-reader--truncate (zhihu-reader--strip-html
                                          (zhihu-reader--alist-get 'excerpt target)) 80))))
    (and title (zhihu-reader--strip-html title))))

(defun zhihu-reader--item-content (entry target)
  "Return plain-text content for ENTRY/TARGET."
  (let ((html (zhihu-reader--first
               (zhihu-reader--alist-get 'content target)
               (zhihu-reader--alist-get 'excerpt_new target)
               (zhihu-reader--alist-get 'excerpt target)
               (zhihu-reader--alist-get 'content entry))))
    (zhihu-reader--html-to-org html)))

(defun zhihu-reader--normalize (entry)
  "Convert ENTRY to a reader plist, or nil."
  (let* ((target (zhihu-reader--item-target entry))
         (kind (zhihu-reader--kind entry target))
         (title (zhihu-reader--item-title entry target kind))
         (content (zhihu-reader--item-content entry target))
         (question (zhihu-reader--alist-get 'question target))
         (author (or (zhihu-reader--alist-get 'author target)
                     (zhihu-reader--alist-get 'author entry)))
         (question-id (zhihu-reader--string
                       (or (zhihu-reader--alist-get 'id question)
                           (zhihu-reader--alist-get 'question_id target)
                           (and (equal kind "question")
                                (zhihu-reader--alist-get 'id target)))))
         (id (zhihu-reader--string (zhihu-reader--alist-get 'id target))))
    (when (or (and title (not (string-empty-p title)))
              (not (string-empty-p content)))
      (list :kind kind
            :id id
            :question-id question-id
            :question-title (or (zhihu-reader--alist-get 'title question) title)
            :question-detail (zhihu-reader--html-to-org
                             (zhihu-reader--alist-get 'detail question))
            :question-images (zhihu-reader--extract-images
                              (zhihu-reader--alist-get 'detail question))
            :title (or title (zhihu-reader--truncate content 80))
            :url (or (zhihu-reader--item-url entry target kind) "")
            :author (zhihu-reader--string
                     (zhihu-reader--first
                      (zhihu-reader--alist-get 'name author)
                      (zhihu-reader--alist-get 'author_name entry)))
            :author-token (zhihu-reader--string
                           (or (zhihu-reader--alist-get 'url_token author)
                               (zhihu-reader--alist-get 'url_token
                                                        (zhihu-reader--alist-get 'member author))))
            :content content
            :images (zhihu-reader--extract-images
                     (zhihu-reader--first
                      (zhihu-reader--alist-get 'content target)
                      (zhihu-reader--alist-get 'excerpt_new target)
                      (zhihu-reader--alist-get 'excerpt target)))
            :comment-count (zhihu-reader--string
                            (or (zhihu-reader--alist-get 'comment_count target)
                                (zhihu-reader--alist-get 'comment_count entry)))
            :truncated (or (zhihu-reader--alist-get 'content_need_truncated target)
                           (zhihu-reader--alist-get 'content_need_truncated entry))
            :created-time (zhihu-reader--first
                           (zhihu-reader--alist-get 'created_time target)
                           (zhihu-reader--alist-get 'created target)
                           (zhihu-reader--alist-get 'updated_time target))
            :voteup-count (zhihu-reader--string
                           (or (zhihu-reader--alist-get 'voteup_count target)
                               (zhihu-reader--alist-get 'voteup_count entry)))))))

(defun zhihu-reader--normalize-items (json)
  "Extract normalized items from JSON."
  (let ((data (zhihu-reader--alist-get 'data json)))
    (when (vectorp data)
      (delq nil (mapcar #'zhihu-reader--normalize (append data nil))))))

;;;; Rendering

(defun zhihu-reader--safe-title (title)
  "Make TITLE safe for an Org heading."
  (replace-regexp-in-string "[\n\r]" " " (or title "")))

(defun zhihu-reader--hide-drawers ()
  "Hide all :PROPERTIES: drawers in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^:PROPERTIES:\n\\(?:.*\n\\)*?:END:\n" nil t)
      (let ((beg (match-beginning 0))
            (end (match-end 0)))
        (unless (cl-some (lambda (overlay)
                           (overlay-get overlay 'zhihu-drawer))
                         (overlays-in beg end))
          (let ((overlay (make-overlay beg end)))
            (overlay-put overlay 'invisible t)
            (overlay-put overlay 'zhihu-drawer t)))))))

(defun zhihu-reader--heading (level title props)
  "Insert an Org heading at LEVEL with TITLE and PROPS."
  (insert (make-string level ?*) " " (zhihu-reader--safe-title title) "\n")
  (when props
    (insert ":PROPERTIES:\n")
    (dolist (prop props)
      (insert (format ":%s: %s\n" (car prop) (cdr prop))))
    (insert ":END:\n")))

(defun zhihu-reader--body (text)
  "Insert TEXT as an Org body, protecting leading stars."
  (dolist (line (split-string (or text "") "\n"))
    (insert (if (string-match-p "\\`\\*+" line)
                (concat "  " line)
              line)
            "\n")))

(defun zhihu-reader--comment-heading (level count id &optional kind)
  "Insert a comments heading at LEVEL with COUNT, ID and KIND."
  (zhihu-reader--heading
   level (format "评论 (%s) · c 加载" (or count "0"))
   (list (cons "ZHIHU-COMMENTS" id)
         (cons "ZHIHU-COMMENTS-COUNT" (or count "0"))
         (cons "ZHIHU-COMMENTS-KIND" (or kind "answer"))
         (cons "ZHIHU-COMMENTS-STATUS" "idle")
         (cons "ZHIHU-COMMENTS-PAGE" "1")
         (cons "ZHIHU-COMMENTS-HAS-NEXT" "1")
         (cons "ZHIHU-COMMENTS-HAS-PREV" "0"))))

(defun zhihu-reader--show-comment-subtree (marker)
  "Reveal the comments subtree at MARKER, including its body."
  (when (and (markerp marker)
             (marker-buffer marker)
             (buffer-live-p (marker-buffer marker)))
    (with-current-buffer (marker-buffer marker)
      (ignore-errors (org-fold-show-all))
      (ignore-errors (force-window-update (current-buffer))))))

(defun zhihu-reader--set-comment-status (marker status &optional actual)
  "Update comment heading at MARKER to STATUS.
ACTUAL is an optional loaded comment count."
  (when (and (markerp marker)
             (marker-buffer marker)
             (buffer-live-p (marker-buffer marker)))
    (with-current-buffer (marker-buffer marker)
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char marker)
          (org-back-to-heading t)
          (let* ((level (or (org-current-level) 1))
                 (count (or (org-entry-get nil "ZHIHU-COMMENTS-COUNT") "0"))
                 (title
                  (pcase status
                    ("loading" (format "评论 (%s) · 加载中…" count))
                    ("loaded" (format "评论 (%s) · 第 %s 页 · 已加载 %s 条"
                                      count
                                      (or (org-entry-get nil "ZHIHU-COMMENTS-PAGE") "1")
                                      (or actual "")))
                    ("empty" (format "评论 (%s) · 无内容" count))
                    ("failed" (format "评论 (%s) · 加载失败" count))
                    (_ (format "评论 (%s) · c 加载" count)))))
            (delete-region (line-beginning-position) (line-end-position))
            (insert (make-string level ?*) " " title)
            (org-entry-put nil "ZHIHU-COMMENTS-STATUS" status)))))))

(defun zhihu-reader--image-file (url)
  "Download URL into the image cache and return its local path."
  (let* ((path (url-generic-parse-url url))
         (extension (or (and (stringp (url-filename path))
                             (file-name-extension (url-filename path)))
                        "jpg"))
         (file (expand-file-name
                (concat (secure-hash 'sha1 url) "." extension)
                zhihu-reader-image-cache)))
    (unless (file-exists-p file)
      (make-directory zhihu-reader-image-cache t)
      (let ((url-request-extra-headers
             `(("User-Agent" . ,zhihu-reader-user-agent)
               ("Referer" . "https://www.zhihu.com/")
               ("Accept" . "image/avif,image/webp,image/apng,image/*,*/*;q=0.8")))
            (buf (condition-case nil
                     (url-retrieve-synchronously url t t 25)
                   (error nil))))
        (when buf
          (unwind-protect
              (with-current-buffer buf
                (goto-char (point-min))
                (when (equal url-http-response-status 200)
                  (goto-char url-http-end-of-headers)
                  (skip-chars-forward "\r\n")
                  (let ((data (buffer-substring-no-properties
                               (point) (point-max))))
                    (with-temp-file file
                      (set-buffer-multibyte nil)
                      (insert data)))))
            (kill-buffer buf)))))
    (and (file-exists-p file) file)))

(defvar zhihu-reader--image-inflight (make-hash-table :test #'equal)
  "Image URLs currently being downloaded.")
(defvar zhihu-reader--image-failed (make-hash-table :test #'equal)
  "Image URLs that failed to download.")
(defcustom zhihu-reader-auto-comments t
  "Automatically load comments when a subtree is rendered or expanded."
  :type 'boolean
  :group 'zhihu-reader)

(defcustom zhihu-reader-auto-comments-limit 10
  "Maximum number of comment sections auto-loaded per render."
  :type 'integer
  :group 'zhihu-reader)

(defvar zhihu-reader--comments-inflight (make-hash-table :test #'equal))

(defvar-local zhihu-reader--image-loading 0)
(defvar-local zhihu-reader--image-spinner-timer nil)
(defvar-local zhihu-reader--image-spinner-index 0)
(defconst zhihu-reader--image-spinner
  ["⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"])

(defun zhihu-reader--image-spinner-tick (buffer)
  "Advance the image-loading spinner in BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (> zhihu-reader--image-loading 0)
        (setq zhihu-reader--image-spinner-index
              (mod (1+ zhihu-reader--image-spinner-index)
                   (length zhihu-reader--image-spinner)))
        (setq mode-line-process
              (list (concat " 图片加载中 "
                            (aref zhihu-reader--image-spinner
                                  zhihu-reader--image-spinner-index))))))))

(defvar-local zhihu-reader--image-display-timer nil)

(defun zhihu-reader--queue-image-display ()
  "Debounce `org-display-inline-images' in the current buffer."
  (when (display-graphic-p)
    (when (timerp zhihu-reader--image-display-timer)
      (cancel-timer zhihu-reader--image-display-timer))
    (let ((buffer (current-buffer)))
      (setq zhihu-reader--image-display-timer
            (run-with-timer
             0.2 nil
             (lambda (buf)
               (when (buffer-live-p buf)
                 (with-current-buffer buf
                   (setq zhihu-reader--image-display-timer nil)
                   (zhihu-reader--queue-image-display))))
             buffer)))))

(defun zhihu-reader--image-start-loading ()
  "Mark one image download as started."
  (setq zhihu-reader--image-loading (1+ zhihu-reader--image-loading))
  (unless zhihu-reader--image-spinner-timer
    (let ((buffer (current-buffer)))
      (setq zhihu-reader--image-spinner-timer
            (run-with-timer 0 0.2 #'zhihu-reader--image-spinner-tick buffer)))))

(defun zhihu-reader--image-finish-loading ()
  "Mark one image download as finished."
  (setq zhihu-reader--image-loading (max 0 (1- zhihu-reader--image-loading)))
  (when (<= zhihu-reader--image-loading 0)
    (when zhihu-reader--image-spinner-timer
      (cancel-timer zhihu-reader--image-spinner-timer)
      (setq zhihu-reader--image-spinner-timer nil))
    (setq mode-line-process nil)))

(defun zhihu-reader--image-valid-p (file)
  "Return non-nil if FILE starts with a known image signature."
  (when (and file (file-readable-p file))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents-literally file)
      (goto-char (point-min))
      (or (looking-at "\xff\xd8")
          (looking-at "\x89PNG")
          (looking-at "GIF8")
          (looking-at "RIFF")))))

(defun zhihu-reader--image-cache-file (url)
  "Return the cache file path for image URL."
  (let* ((parsed (url-generic-parse-url url))
         (filename (and (stringp (url-filename parsed)) (url-filename parsed)))
         (extension (or (and filename (file-name-extension filename)) "jpg"))
         (file (expand-file-name
                (concat (secure-hash 'sha1 url) "." extension)
                zhihu-reader-image-cache)))
    file))

(defun zhihu-reader--replace-image-link (marker file)
  "Replace the Org link at MARKER with a local FILE link."
  (when (and (markerp marker)
             (marker-buffer marker)
             (buffer-live-p (marker-buffer marker))
             (file-exists-p file))
    (with-current-buffer (marker-buffer marker)
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char marker)
          (when (looking-at "\\[\\[")
            (delete-region (point) (line-end-position))
            (insert (org-make-link-string (concat "file:" file) "图片"))))
        (when (and zhihu-reader-render-images
                   (display-graphic-p))
          (setq-local org-image-actual-width
                      (list zhihu-reader-image-width))
          (zhihu-reader--queue-image-display))))))

(defun zhihu-reader--image-failed (marker url)
  "Replace MARKER with a failed-image label."
  (puthash url t zhihu-reader--image-failed)
  (when (and (markerp marker)
             (marker-buffer marker)
             (buffer-live-p (marker-buffer marker)))
    (with-current-buffer (marker-buffer marker)
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char marker)
          (when (looking-at "\\[\\[")
            (delete-region (point) (line-end-position))
            (insert (org-make-link-string url "图片加载失败"))))))))

(defun zhihu-reader--download-image-async (url marker)
  "Download image URL asynchronously and replace MARKER on success."
  (let* ((file (zhihu-reader--image-cache-file url))
         (buffer (current-buffer))
         (generation zhihu-reader--generation))
    (if (zhihu-reader--image-valid-p file)
        (progn
          (remhash url zhihu-reader--image-inflight)
          (zhihu-reader--replace-image-link marker file))
      (when (file-exists-p file) (ignore-errors (delete-file file)))
      (let ((url-request-extra-headers
             `(("User-Agent" . ,zhihu-reader-user-agent)
               ("Referer" . "https://www.zhihu.com/")
               ("Accept" . "image/avif,image/webp,image/apng,image/*,*/*;q=0.8"))))
        (zhihu-reader--image-start-loading)
        (condition-case err
            (url-retrieve
             url
             (lambda (_status)
               (let ((request (current-buffer)))
                 (unwind-protect
                     (condition-case nil
                         (progn
                           (goto-char (point-min))
                           (if (and (equal url-http-response-status 200)
                                    (zhihu-reader--generation-current-p
                                     buffer generation))
                               (progn
                                 (goto-char url-http-end-of-headers)
                                 (skip-chars-forward "\r\n")
                                 (make-directory zhihu-reader-image-cache t)
                                 (let ((data (buffer-substring-no-properties
                                              (point) (point-max))))
                                   (with-temp-file file
                                     (set-buffer-multibyte nil)
                                     (insert data))
                                   (when (buffer-live-p buffer)
                                     (zhihu-reader--replace-image-link
                                      marker file))))
                             (when (buffer-live-p buffer)
                               (zhihu-reader--image-failed marker url))))
                       (error
                        (when (buffer-live-p buffer)
                          (zhihu-reader--image-failed marker url))))
                   (when (buffer-live-p request)
                     (kill-buffer request))
                   (remhash url zhihu-reader--image-inflight)
                   (when (buffer-live-p buffer)
                     (with-current-buffer buffer
                       (zhihu-reader--image-finish-loading))))))
             nil t)
          (error
           (remhash url zhihu-reader--image-inflight)
           (when (buffer-live-p buffer)
             (zhihu-reader--image-failed marker url)
             (with-current-buffer buffer
               (zhihu-reader--image-finish-loading)))
           (signal (car err) (cdr err))))))))

(defun zhihu-reader--close-open-org-blocks ()
  "Close any unclosed #+begin_ block in the current subtree body.
This keeps image links inserted after HTML content outside code blocks."
  (save-excursion
    (let ((start (save-excursion
                   (org-back-to-heading t)
                   (forward-line 1)
                   (when (looking-at ":PROPERTIES:")
                     (re-search-forward "^:END:" nil t)
                     (forward-line 1))
                   (point)))
          (case-fold-search t)
          (stack nil))
      (goto-char start)
      (while (re-search-forward "^\(#\+\(begin\|end\)_\([a-zA-Z0-9_-]+\)\)" nil t)
        (let ((kind (match-string 2))
              (name (downcase (match-string 3))))
          (if (string-equal kind "begin")
              (push name stack)
            (when (equal (car stack) name)
              (pop stack)))))
      (goto-char (point))
      (dolist (name (reverse stack))
        (insert "#+end_" name "
")))))

(defun zhihu-reader--insert-images (images)
  "Insert IMAGES as Org links and download them asynchronously."
  (when (and zhihu-reader-render-images (display-graphic-p))
    (setq-local org-image-actual-width (list zhihu-reader-image-width)))
  (dolist (url images)
    (let* ((file (and zhihu-reader-render-images
                      (zhihu-reader--image-cache-file url)))
           (cached (and file (zhihu-reader--image-valid-p file)))
           (marker nil))
      (if cached
          (insert (org-make-link-string (concat "file:" file) "图片"))
        (setq marker (copy-marker (point)))
        (insert (org-make-link-string url "图片")))
      (insert "\n")
      ;; Images are downloaded lazily by `zhihu-reader--process-buffer-images'
      ;; when the subtree is expanded or when `i' is pressed.
      (ignore marker)))
  (when (and zhihu-reader-render-images
             (display-graphic-p)
             images)
    (zhihu-reader--queue-image-display)))

(defun zhihu-reader--render-answer (item level)
  "Render answer ITEM at LEVEL."
  (let ((author (plist-get item :author))
        (votes (plist-get item :voteup-count))
        (time (zhihu-reader--time-string (plist-get item :created-time))))
    (zhihu-reader--heading
     level
     (format "回答%s%s%s"
             (if (string-empty-p author) "" (concat " · " author))
             (if (string-empty-p votes) "" (concat " · " votes " 赞"))
             (if time (concat " · " time) ""))
     (list (cons "ZHIHU-ANSWER" (plist-get item :id))
           (cons "ZHIHU-QUESTION" (plist-get item :question-id))
           (cons "ZHIHU-AUTHOR-TOKEN" (plist-get item :author-token))))
    (when (not (string-empty-p (or (plist-get item :content) "")))
      (zhihu-reader--body (plist-get item :content)))
    (when (plist-get item :images)
      (zhihu-reader--close-open-org-blocks)
      (zhihu-reader--insert-images (plist-get item :images)))
    (when (plist-get item :truncated)
      (insert "（内容有截断，RET 查看全文）\n"))
    (insert "\n")
    (zhihu-reader--comment-heading
     (1+ level) (plist-get item :comment-count) (plist-get item :id))
    (insert "\n")))

(defun zhihu-reader--render-group (group)
  "Render a group of items sharing a question/article."
  (let* ((first (car group))
         (kind (plist-get first :kind))
         (qid (plist-get first :question-id))
         (qtitle (plist-get first :question-title))
         (answers (seq-filter (lambda (item)
                                (equal (plist-get item :kind) "answer"))
                              group)))
    (cond
     ((equal kind "answer")
      (zhihu-reader--heading
       1 (or qtitle "知乎问题")
       (when (and qid (not (string-empty-p qid)))
         (list (cons "ZHIHU-QUESTION" qid))))
      (when (not (string-empty-p (or (plist-get first :question-detail) "")))
        (zhihu-reader--body (plist-get first :question-detail)))
      (when (plist-get first :question-images)
        (zhihu-reader--close-open-org-blocks)
        (zhihu-reader--insert-images (plist-get first :question-images)))
      (when (or (not (string-empty-p (or (plist-get first :question-detail) "")))
                (plist-get first :question-images))
        (insert "\n"))
      (dolist (item answers)
        (zhihu-reader--render-answer item 2)))
     ((equal kind "article")
      (let ((item (car group)))
        (zhihu-reader--heading
         1 (plist-get item :title)
         (list (cons "ZHIHU-ARTICLE" (plist-get item :id))))
        (zhihu-reader--body (plist-get item :content))
        (when (plist-get item :images)
          (zhihu-reader--close-open-org-blocks)
          (zhihu-reader--insert-images (plist-get item :images)))
        (insert "\n")
        (zhihu-reader--comment-heading
         2 (plist-get item :comment-count) (plist-get item :id) "article")
        (insert "\n")))
     (t
      (let ((item (car group)))
        (zhihu-reader--heading
         1 (plist-get item :title)
         (list (cons "ZHIHU-QUESTION" (plist-get item :id))))
        (zhihu-reader--body (plist-get item :content))
        (when (plist-get item :images)
          (zhihu-reader--close-open-org-blocks)
          (zhihu-reader--insert-images (plist-get item :images)))
        (insert "\n"))))))

(defun zhihu-reader--group-key (item)
  "Return a grouping key for ITEM."
  (pcase (plist-get item :kind)
    ("answer" (list 'question (plist-get item :question-id)))
    ("article" (list 'article (plist-get item :id)))
    (_ (list 'question (or (plist-get item :question-id)
                           (plist-get item :id))))))

(defun zhihu-reader--render-items (items)
  "Render ITEMS as grouped Org entries."
  (let ((groups (make-hash-table :test #'equal))
        (order nil))
    (dolist (item items)
      (let ((key (zhihu-reader--group-key item)))
        (unless (gethash key groups)
          (push key order))
        (push item (gethash key groups))))
    (dolist (key (nreverse order))
      (zhihu-reader--render-group (nreverse (gethash key groups))))
    (when (null order)
      (insert "没有返回内容。\n"))))

(defun zhihu-reader--render (title items view &optional buffer-name)
  "Render ITEMS into an Org buffer.
TITLE is the buffer title, VIEW is stored for refresh, BUFFER-NAME is
the target buffer."
  (let ((buffer (get-buffer-create (or buffer-name zhihu-reader--main-buffer))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (zhihu-reader-mode)
        (setq-local zhihu-reader--generation (1+ zhihu-reader--generation))
        (setq-local zhihu-reader--view view)
        (setq-local header-line-format
                    (format " Zhihu %s  | TAB fold  RET enter  c comments  ] next  [ prev  i images  g refresh  q quit"
                            title))
        (insert (format "#+TITLE: %s\n#+STARTUP: overview\n\n" title))
        (zhihu-reader--render-items items)
        (goto-char (point-min))
        (if (memq (car-safe view) '(question answer))
            (ignore-errors (org-fold-show-all))
          (org-overview))
        (ignore-errors (org-fold-hide-drawer-all))
        (zhihu-reader--hide-drawers)
        (setq buffer-read-only t)))
    (pop-to-buffer buffer)
    (when (and zhihu-reader-render-images (display-graphic-p))
      (with-current-buffer buffer
        (if (memq (car-safe view) '(question answer))
            (zhihu-reader--process-buffer-images)
          (zhihu-reader--queue-image-display))))
    (when (memq (car-safe view) '(question answer))
      (with-current-buffer buffer
        (zhihu-reader--auto-load-comments)))
    (zhihu-reader--restore-navigation-anchor)))

;;;; Detail APIs

(defun zhihu-reader--question-answers-url (question-id &optional page order)
  "Return the answers API URL for QUESTION-ID, PAGE and ORDER."
  (format "https://www.zhihu.com/api/v4/questions/%s/answers?include=data%%5B*%%5D.content%%2Cexcerpt%%2Cauthor%%2Ccomment_count%%2Cvoteup_count%%2Cquestion.detail&limit=20&offset=%d&order=%s"
          question-id (* 20 (1- (or page 1))) (or order "default")))

(defun zhihu-reader--answer-url (answer-id)
  "Return the answer detail API URL for ANSWER-ID."
  (format "https://www.zhihu.com/api/v4/answers/%s?include=content%%2Cexcerpt%%2Cauthor%%2Cquestion%%2Ccomment_count%%2Cvoteup_count"
          answer-id))

(defun zhihu-reader--comments-url (id &optional kind offset)
  "Return the root comments API URL for ID, KIND and OFFSET."
  (pcase kind
    ("article"
     (format "https://www.zhihu.com/api/v4/comment_v5/articles/%s/root_comment?order_by=score&limit=20&offset=%d"
             id (or offset 0)))
    (_
     (format "https://www.zhihu.com/api/v4/answers/%s/root_comments?order=normal&limit=20&offset=%d&status=open"
             id (or offset 0)))))

(defun zhihu-reader--child-comments-url (comment-id &optional offset)
  "Return the child comments API URL for COMMENT-ID and OFFSET."
  (format "https://www.zhihu.com/api/v4/comment_v5/comment/%s/child_comment?order=normal&limit=20&offset=%d&status=open"
          comment-id (or offset 0)))

(defun zhihu-reader--cookie-alist ()
  "Return the current Zhihu cookie header as a name/value alist."
  (mapcar
   (lambda (part)
     (let ((i (string-match "=" part)))
       (when i
         (cons (substring part 0 i) (substring part (1+ i))))))
   (split-string (zhihu-reader--cookies) "; " t)))

(defun zhihu-reader--fetch-question-detail (question-id)
  "Fetch question detail JSON with ZSE signing, or nil."
  (condition-case nil
      (progn
        (zhihu-reader--ensure-zhihu)
        (let* ((cookie (zhihu-reader--cookies))
               (dc0 (cdr (assoc-string "d_c0" (zhihu-reader--cookie-alist))))
               (url (format "https://www.zhihu.com/api/v4/questions/%s?include=detail"
                            question-id))
               (url-request-extra-headers
                (append (zhihu--zse-request-headers url nil dc0)
                        (list (cons "Cookie" cookie)
                              (cons "Referer" "https://www.zhihu.com/")
                              (cons "Accept" "application/json, text/plain, */*")))))
          (with-current-buffer (url-retrieve-synchronously url t t 20)
            (unwind-protect
                (progn
                  (goto-char (point-min))
                  (when (equal url-http-response-status 200)
                    (goto-char url-http-end-of-headers)
                    (decode-coding-region (point) (point-max) 'utf-8)
                    (json-parse-buffer :object-type 'alist
                                       :null-object nil
                                       :false-object nil)))
              (kill-buffer (current-buffer))))))
    (error nil)))

(defun zhihu-reader--attach-question-detail (items question-id)
  "Attach question detail and images to ITEMS."
  (let ((detail (zhihu-reader--fetch-question-detail question-id)))
    (if (not detail)
        items
      (let ((html (or (zhihu-reader--alist-get 'detail detail) "")))
        (mapcar
         (lambda (item)
           (setq item (plist-put item :question-detail
                                 (zhihu-reader--html-to-org html)))
           (plist-put item :question-images
                      (zhihu-reader--extract-images html)))
         items)))))

(defun zhihu-reader--id-or-url (value)
  "Extract a numeric Zhihu id from VALUE, which may be an id or URL."
  (cond
   ((string-match "\\([0-9]\\{6,\\}\\)" (or value ""))
    (match-string 1 value))
   (t (user-error "无法从 %s 解析知乎 ID" value))))

(defun zhihu-reader-question (id-or-url &optional page order force)
  "Open question ID-OR-URL with its answer list.
PAGE selects the answer page; ORDER is `default', `updated' or `created'.
With FORCE non-nil, bypass the page cache."
  (interactive (list (read-string "知乎问题 ID 或 URL: ")
                     (prefix-numeric-value current-prefix-arg)
                     nil
                     nil))
  (let* ((id (zhihu-reader--id-or-url id-or-url))
         (page (or page 1))
         (order (or order "default"))
         (key (list 'question id page order))
         (url (zhihu-reader--question-answers-url id page order))
         (title (format "问题 %s · 第 %d 页 · %s" id page order)))
    (if (and (not force) (zhihu-reader--show-cached key))
        (message "使用缓存的问题第 %d 页" page)
      (if noninteractive
          (let* ((items (zhihu-reader--normalize-items
                         (zhihu-reader--fetch-json url)))
                 (qtitle (or (plist-get (car items) :question-title)
                             (format "问题 %s" id))))
            (puthash key
                     (cons (format "问题 · %s · 第 %d 页 · %s"
                                   qtitle page order)
                           items)
                     zhihu-reader--page-cache)
            (zhihu-reader--render (format "问题 · %s · 第 %d 页 · %s"
                                         qtitle page order)
                                  items key
                                  zhihu-reader--question-buffer)
            (zhihu-reader--restore-point key))
        (zhihu-reader--async-list
         title key url zhihu-reader--question-buffer
         (lambda (json)
           (let* ((items (zhihu-reader--normalize-items json))
                  (qtitle (or (plist-get (car items) :question-title)
                              (format "问题 %s" id))))
             (cons (format "问题 · %s · 第 %d 页 · %s" qtitle page order)
                   items)))
         key)))))

(defun zhihu-reader-answer (answer-id)
  "Open answer ANSWER-ID with its comments."
  (interactive "s知乎回答 ID: ")
  (let* ((id (zhihu-reader--id-or-url answer-id))
         (url (zhihu-reader--answer-url id)))
    (if noninteractive
        (let* ((json (zhihu-reader--fetch-json url))
               (items (zhihu-reader--normalize-items
                       (list (cons 'data (vector json)))))
               (title (or (plist-get (car items) :question-title)
                          (format "回答 %s" id))))
          (zhihu-reader--render (format "回答 · %s" title)
                                items (list 'answer id)
                                zhihu-reader--answer-buffer))
      (zhihu-reader--async-list
       (format "回答 %s" id) (list 'answer id) url zhihu-reader--answer-buffer
       (lambda (json)
         (let* ((items (zhihu-reader--normalize-items
                        (list (cons 'data (vector json)))))
                (title (or (plist-get (car items) :question-title)
                           (format "回答 %s" id))))
           (cons (format "回答 · %s" title) items)))))))

;;;; Comments

(defun zhihu-reader--normalize-comments (json)
  "Return comment plists from JSON."
  (let ((data (zhihu-reader--alist-get 'data json)))
    (when (vectorp data)
      (delq nil
            (mapcar
             (lambda (comment)
               (let* ((author (zhihu-reader--alist-get 'author comment))
                      (content (zhihu-reader--html-to-org
                                (zhihu-reader--alist-get 'content comment)))
                      (child-count (or (zhihu-reader--alist-get 'child_comment_count comment)
                                       (length (alist-get 'child_comments comment)))))
                 (when (not (string-empty-p content))
                   (list :id (zhihu-reader--string
                              (zhihu-reader--alist-get 'id comment))
                         :author (zhihu-reader--first
                                  (zhihu-reader--alist-get 'name author)
                                  (zhihu-reader--alist-get
                                   'name (zhihu-reader--alist-get 'member author))
                                  (zhihu-reader--alist-get 'author_name comment))
                         :content content
                         :author-token (zhihu-reader--string
                                        (or (zhihu-reader--alist-get 'url_token author)
                                            (zhihu-reader--alist-get
                                             'url_token
                                             (zhihu-reader--alist-get 'member author))))
                         :child-count (zhihu-reader--string child-count)
                         :votes (zhihu-reader--string
                                 (or (zhihu-reader--alist-get 'vote_count comment)
                                     (zhihu-reader--alist-get 'voteup_count comment)))
                         :time (zhihu-reader--time-string
                                (zhihu-reader--alist-get 'created_time comment))))))
             (append data nil))))))

(defun zhihu-reader--insert-comments (comments level)
  "Insert COMMENTS at Org LEVEL."
  (dolist (comment comments)
    (let ((id (plist-get comment :id))
          (child-count (plist-get comment :child-count)))
      (zhihu-reader--heading
       level
       (format "%s%s%s%s"
               (or (plist-get comment :author) "匿名")
               (let ((votes (plist-get comment :votes)))
                 (if (and votes (not (string-empty-p votes)))
                     (format " · %s 赞" votes) ""))
               (let ((time (plist-get comment :time)))
                 (if time (concat " · " time) ""))
               (let ((count (and child-count
                                 (not (string= child-count "0"))
                                 child-count)))
                 (if count (format " · 回复 %s" count) "")))
       (when (and id (not (string-empty-p id)))
         (list (cons "ZHIHU-COMMENT" id)
               (cons "ZHIHU-CHILD-COUNT" (or child-count "0"))
               (cons "ZHIHU-AUTHOR-TOKEN"
                     (or (plist-get comment :author-token) "")))))
      (when (not (string-empty-p (or (plist-get comment :content) "")))
        (zhihu-reader--body (plist-get comment :content)))
      (insert "\n"))))

(defun zhihu-reader--clear-comment-body (marker)
  "Remove existing comments below MARKER, preserving heading properties."
  (when (and (markerp marker)
             (marker-buffer marker)
             (buffer-live-p (marker-buffer marker)))
    (with-current-buffer (marker-buffer marker)
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char marker)
          (org-back-to-heading t)
          (let ((start (progn
                         (forward-line 1)
                         (when (looking-at ":PROPERTIES:")
                           (re-search-forward "^:END:" nil t)
                           (forward-line 1))
                         (point)))
                (end (save-excursion
                       (org-back-to-heading t)
                       (org-end-of-subtree t t))))
            (when (< start end)
              (delete-region start end))))))))

(defun zhihu-reader--set-comment-paging (marker json offset &optional count)
  "Set paging properties at MARKER from comment JSON, OFFSET and COUNT."
  (when (and (markerp marker) (buffer-live-p (marker-buffer marker)))
    (with-current-buffer (marker-buffer marker)
      (let* ((paging (zhihu-reader--alist-get 'paging json))
             (is-end (zhihu-reader--alist-get 'is_end paging))
             (has-next
              (if (or (and count (>= count 20))
                      (not (eq is-end t)))
                  "1" "0")))
        (save-excursion
          (goto-char marker)
          (org-entry-put nil "ZHIHU-COMMENTS-OFFSET"
                         (number-to-string offset))
          (org-entry-put nil "ZHIHU-COMMENTS-PAGE"
                         (number-to-string (1+ (/ offset 20))))
          (org-entry-put nil "ZHIHU-COMMENTS-HAS-NEXT" has-next)
          (org-entry-put nil "ZHIHU-COMMENTS-HAS-PREV"
                         (if (> offset 0) "1" "0")))))))

(defun zhihu-reader--load-comments-at (id marker kind &optional offset)
  "Load root comments for ID at MARKER.
KIND is \"answer\" or \"article\"; OFFSET selects the next page."
  (let* ((offset (or offset 0))
         (json (zhihu-reader--fetch-json
                (zhihu-reader--comments-url id kind offset)))
         (comments (zhihu-reader--normalize-comments json))
         (level (save-excursion
                  (goto-char marker)
                  (org-back-to-heading t)
                  (1+ (org-current-level)))))
    (let ((inhibit-read-only t))
      (goto-char marker)
      (org-back-to-heading t)
      (org-end-of-subtree)
      (insert "\n")
      (unless comments
        (message "知乎返回 0 条评论（可能已删除或折叠）"))
      (zhihu-reader--clear-comment-body marker)
      (zhihu-reader--set-comment-status
       marker (if comments "loaded" "empty") (length comments))
      (zhihu-reader--set-comment-paging marker json offset (length comments))
      (zhihu-reader--insert-comments comments level)
      (save-excursion
        (goto-char marker)
        (org-entry-put nil "ZHIHU-COMMENTS-LOADED" "t")
        (org-entry-put nil "ZHIHU-COMMENTS-OFFSET" (number-to-string offset))
        (zhihu-reader--show-comment-subtree marker)
        (zhihu-reader--hide-drawers))
      (setq buffer-read-only t))))

(defun zhihu-reader--load-comments-async (id marker kind &optional offset show)
  "Load root comments for ID asynchronously at MARKER.
KIND is \"answer\" or \"article\"; OFFSET selects the next page.
When SHOW is non-nil, expand the comments subtree."
  (let ((offset (or offset 0))
        (buffer (current-buffer))
        (generation zhihu-reader--generation)
        (level (save-excursion
                 (goto-char marker)
                 (org-back-to-heading t)
                 (1+ (org-current-level)))))
    (zhihu-reader--set-comment-status marker "loading")
    (zhihu-reader--fetch-json-async
     (zhihu-reader--comments-url id kind offset)
     (lambda (json)
       (unwind-protect
           (when (zhihu-reader--generation-current-p buffer generation)
             (with-current-buffer buffer
               (let ((inhibit-read-only t))
                 (goto-char marker)
                 (org-back-to-heading t)
                 (org-end-of-subtree)
                 (insert "\n")
                 (let ((comments (zhihu-reader--normalize-comments json)))
                 (unless comments
                   (message "知乎返回 0 条评论（可能已删除或折叠）"))
                 (zhihu-reader--clear-comment-body marker)
                 (zhihu-reader--insert-comments comments level)
                 (zhihu-reader--set-comment-paging marker json offset (length comments))
                 (zhihu-reader--set-comment-status
                  marker (if comments "loaded" "empty") (length comments)))
                 (save-excursion
                   (goto-char marker)
                   (org-entry-put nil "ZHIHU-COMMENTS-LOADED" "t")
                   (org-entry-put nil "ZHIHU-COMMENTS-OFFSET"
                                  (number-to-string offset))
                   (if show
                       (zhihu-reader--show-comment-subtree marker)
                     (ignore-errors (org-fold-hide-subtree)))
                   (zhihu-reader--hide-drawers))
                 (setq buffer-read-only t))))
         (remhash id zhihu-reader--comments-inflight)))
     (lambda (error)
       (remhash id zhihu-reader--comments-inflight)
       (zhihu-reader--set-comment-status marker "failed")
       (message "评论加载失败: %s" error)))))

(defun zhihu-reader--load-child-comments-async (comment-id marker &optional offset show)
  "Load child comments for COMMENT-ID asynchronously at MARKER.
OFFSET selects the next page; SHOW expands the subtree."
  (let ((offset (or offset 0))
        (buffer (current-buffer))
        (generation zhihu-reader--generation)
        (level (save-excursion
                 (goto-char marker)
                 (org-back-to-heading t)
                 (1+ (org-current-level)))))
    (zhihu-reader--fetch-json-async
     (zhihu-reader--child-comments-url comment-id offset)
     (lambda (json)
       (when (zhihu-reader--generation-current-p buffer generation)
         (with-current-buffer buffer
           (let ((inhibit-read-only t))
             (goto-char marker)
             (org-back-to-heading t)
             (org-end-of-subtree)
             (insert "\n")
             (let ((comments (zhihu-reader--normalize-comments json)))
               (unless comments
                 (message "知乎返回 0 条评论（可能已删除或折叠）"))
               (zhihu-reader--insert-comments comments level))
             (save-excursion
               (goto-char marker)
               (org-entry-put nil "ZHIHU-CHILDREN-LOADED" "t")
               (org-entry-put nil "ZHIHU-CHILDREN-OFFSET" (number-to-string (+ offset 20)))
               (if show
                   (zhihu-reader--show-comment-subtree marker)
                 (ignore-errors (org-fold-hide-subtree)))
               (zhihu-reader--hide-drawers))
             (setq buffer-read-only t)))))
     (lambda (error)
       (message "子评论加载失败: %s" error)))))

(defun zhihu-reader--comment-target ()
  "Return (ROOT ID KIND MARKER COMMENT-ID) for the heading at point."
  (let* ((comment-id (org-entry-get nil "ZHIHU-COMMENT"))
         (marker (point-marker))
         (root (null comment-id))
         (id nil)
         (kind "answer"))
    (unless comment-id
      (setq id (or (org-entry-get nil "ZHIHU-COMMENTS")
                   (save-excursion
                     (when (or (org-entry-get nil "ZHIHU-ANSWER")
                               (org-entry-get nil "ZHIHU-ARTICLE"))
                       (org-end-of-subtree)
                       (when (re-search-backward
                              "^:ZHIHU-COMMENTS: \\([0-9]+\\)" nil t)
                         (org-back-to-heading t)
                         (match-string 1))))))
      (setq kind (or (org-entry-get nil "ZHIHU-COMMENTS-KIND") "answer")))
    (unless (or id comment-id)
      (user-error "光标不在评论标题上"))
    (list root id kind marker comment-id)))

(defun zhihu-reader-comments-more ()
  "Load the next page of comments at point."
  (interactive)
  (let* ((target (zhihu-reader--comment-target))
         (root (nth 0 target))
         (id (nth 1 target))
         (kind (nth 2 target))
         (marker (nth 3 target))
         (comment-id (nth 4 target)))
    (when (and root
               (equal (org-entry-get nil "ZHIHU-COMMENTS-HAS-NEXT") "0"))
      (user-error "已经是最后一页评论"))
    (let* ((offset (string-to-number
                    (or (org-entry-get nil "ZHIHU-COMMENTS-OFFSET") "0")))
           (offset (if root (+ offset 20) offset)))
      (message "加载下一页评论…")
      (if root
          (zhihu-reader--load-comments-async id marker kind offset t)
        (zhihu-reader--load-child-comments-async comment-id marker offset t)))))

(defun zhihu-reader-comments-previous ()
  "Load the previous page of comments at point."
  (interactive)
  (let* ((target (zhihu-reader--comment-target))
         (root (nth 0 target))
         (id (nth 1 target))
         (kind (nth 2 target))
         (marker (nth 3 target)))
    (unless root
      (user-error "子评论暂不支持上一页"))
    (when (equal (org-entry-get nil "ZHIHU-COMMENTS-HAS-PREV") "0")
      (user-error "已经是第一页评论"))
    (let ((offset (max 0 (- (string-to-number
                             (or (org-entry-get nil "ZHIHU-COMMENTS-OFFSET") "0"))
                            20))))
      (message "加载上一页评论…")
      (zhihu-reader--load-comments-async id marker kind offset t))))

(defun zhihu-reader-comments ()
  "Load root or child comments at point."
  (interactive)
  (let ((comment-id (org-entry-get nil "ZHIHU-COMMENT"))
        (id nil)
        (kind "answer")
        (root t))
    (if comment-id
        (setq root nil)
      (setq id (or (org-entry-get nil "ZHIHU-COMMENTS")
                   (save-excursion
                     (when (or (org-entry-get nil "ZHIHU-ANSWER")
                               (org-entry-get nil "ZHIHU-ARTICLE"))
                       (org-end-of-subtree)
                       (when (re-search-backward
                              "^:ZHIHU-COMMENTS: \\([0-9]+\\)" nil t)
                         (org-back-to-heading t)
                         (match-string 1))))))
      (setq kind (or (org-entry-get nil "ZHIHU-COMMENTS-KIND") "answer")))
    (unless (or id comment-id)
      (user-error "光标不在评论标题上"))
    (let* ((marker (point-marker))
           (loaded (if root
                       (org-entry-get nil "ZHIHU-COMMENTS-LOADED")
                     (org-entry-get nil "ZHIHU-CHILDREN-LOADED"))))
      (cond
       (loaded
        (message "评论已经加载"))
       (root
        (if noninteractive
            (zhihu-reader--load-comments-at id marker kind)
          (message "评论加载中…")
          (zhihu-reader--load-comments-async id marker kind 0 t)))
       (t
        (if noninteractive
            (let ((comments (zhihu-reader--normalize-comments
                             (zhihu-reader--fetch-json
                              (zhihu-reader--child-comments-url comment-id)))))
              (goto-char marker)
              (org-back-to-heading t)
              (let ((inhibit-read-only t)
                    (level (1+ (org-current-level))))
                (org-end-of-subtree)
                (insert "\n")
                (zhihu-reader--insert-comments comments level)
                (org-entry-put nil "ZHIHU-CHILDREN-LOADED" "t")
                (org-entry-put nil "ZHIHU-CHILDREN-OFFSET" "20")
                (zhihu-reader--show-comment-subtree marker)
                (zhihu-reader--hide-drawers)
                (setq buffer-read-only t)))
          (message "子评论加载中…")
          (zhihu-reader--load-child-comments-async comment-id marker 0 t)))))))

(defvar-local zhihu-reader-compose-kind nil)
(defvar-local zhihu-reader-compose-id nil)

(defun zhihu-reader--open-compose-buffer (kind id label)
  "Open a compose buffer for KIND/ID with LABEL."
  (let ((buffer (get-buffer-create "*Zhihu Comment*")))
    (with-current-buffer buffer
      (erase-buffer)
      (text-mode)
      (setq-local zhihu-reader-compose-kind kind)
      (setq-local zhihu-reader-compose-id id)
      (setq-local header-line-format
                  (format " 评论 %s  | C-c C-p 发布  C-c C-k 取消" label))
      (local-set-key (kbd "C-c C-p") #'zhihu-reader-publish-comment)
      (local-set-key (kbd "C-c C-k") #'zhihu-reader-cancel-compose))
    (pop-to-buffer buffer)))

(defun zhihu-reader-compose-comment ()
  "Compose a comment or reply at point."
  (interactive)
  (let ((comment-id (org-entry-get nil "ZHIHU-COMMENT"))
        (answer-id (org-entry-get nil "ZHIHU-ANSWER" t))
        (article-id (org-entry-get nil "ZHIHU-ARTICLE" t)))
    (cond
     (comment-id
      (zhihu-reader--open-compose-buffer
       'reply comment-id (format "回复 %s" comment-id)))
     (answer-id
      (zhihu-reader--open-compose-buffer
       'answer answer-id (format "回答 %s" answer-id)))
     (article-id
      (zhihu-reader--open-compose-buffer
       'article article-id (format "文章 %s" article-id)))
     (t
      (user-error "光标不在回答、文章或评论标题上")))))

(defun zhihu-reader-cancel-compose ()
  "Close the comment compose buffer."
  (interactive)
  (when (derived-mode-p 'text-mode)
    (kill-buffer)))

(defun zhihu-reader-publish-comment ()
  "Publish the current comment compose buffer."
  (interactive)
  (unless zhihu-reader-enable-write
    (user-error "写操作已关闭；请设置 `zhihu-reader-enable-write' 为 t"))
  (unless (derived-mode-p 'text-mode)
    (user-error "当前 buffer 不是评论编辑 buffer"))
  (let ((kind zhihu-reader-compose-kind)
        (id zhihu-reader-compose-id)
        (text (string-trim (buffer-string))))
    (when (string-empty-p text)
      (user-error "评论不能为空"))
    (unless (yes-or-no-p "确认发布这条评论？")
      (user-error "已取消"))
    (zhihu-reader--ensure-zhihu)
    (let* ((xsrf (zhihu--make-xsrf-state))
           (token (zhihu--ensure-xsrf-token xsrf))
           (payload
            (pcase kind
              ('reply
               (list :content text
                     :resource_type "comment"
                     :resource_id id))
              ('answer
               (list :content text
                     :resource_type "answer"
                     :resource_id id))
              ('article
               (list :content text
                     :resource_type "article"
                     :resource_id id))
              (_ (user-error "未知评论类型"))))
           (resp (condition-case err
                     (zhihu--http-json
                      "POST" "https://www.zhihu.com/api/v4/comment_v5/comment"
                      :body payload
                      :xsrf-state xsrf
                      :extra-headers `(("x-xsrftoken" . ,token)))
                   (error err))))
      (if (and (listp resp) (memq (plist-get resp :status) '(200 201 202)))
          (progn
            (message "评论已发布")
            (kill-buffer))
        (error "评论发布失败：%s" (or (plist-get resp :body) resp))))))


;;;; Editing

(defun zhihu-reader--ensure-zhihu ()
  "Load zhihu.el and bind its cookie function to the reader's database."
  (add-to-list 'load-path zhihu-reader-zhihu-library-path)
  (require 'plz)
  (require 'ox-zhihu)
  (require 'zhihu)
  (setq zhihu-cookie-function
        (lambda (_url) (zhihu-reader--cookies))))

(defun zhihu-reader--subtree-body ()
  "Return the body of the current Org subtree, excluding child headings."
  (let* ((level (or (org-current-level) 1))
         (start (save-excursion
                  (org-back-to-heading t)
                  (forward-line 1)
                  (when (looking-at ":PROPERTIES:")
                    (re-search-forward "^:END:" nil t)
                    (forward-line 1))
                  (point)))
         (end (save-excursion
                (org-back-to-heading t)
                (org-end-of-subtree t)
                (when (re-search-backward
                       (format "^\\*\\{%d\\} " (1+ level)) nil t)
                  (match-beginning 0))
                (point))))
    (string-trim (buffer-substring-no-properties start end))))

(defun zhihu-reader--open-edit-buffer (kind id question-id title body)
  "Open an editable Org buffer for KIND/ID with TITLE and BODY."
  (let ((buffer (get-buffer-create "*Zhihu Edit*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert (or body ""))
      (zhihu-reader-edit-mode)
      (setq-local zhihu-reader-edit-kind kind)
      (setq-local zhihu-reader-edit-id id)
      (setq-local zhihu-reader-edit-question-id question-id)
      (setq-local zhihu-reader-edit-title title)
      (setq-local header-line-format
                  (format " 编辑 %s %s  | C-c C-c 预览  C-c C-p 发布  C-c C-k 取消"
                          kind id))
      (goto-char (point-min)))
    (pop-to-buffer buffer)))

(defun zhihu-reader-new-article (title)
  "Create a new Zhihu article Org source file."
  (interactive "s文章标题: ")
  (unless (and (stringp title) (not (string-empty-p (string-trim title))))
    (user-error "标题不能为空"))
  (let* ((dir (expand-file-name zhihu-reader-article-directory))
         (slug (replace-regexp-in-string
                "[^[:alnum:]]+" "-"
                (string-trim title)))
         (file (expand-file-name
                (format "%s-%s.org"
                        (format-time-string "%Y%m%d-%H%M%S")
                        slug)
                dir))
         (topics (read-string "话题 JSON 数组（可空）: " "[]"))
         (column (read-string "专栏 ID（可空）: " "")))
    (make-directory dir t)
    (find-file file)
    (insert (format "#+TITLE: %s\n" (string-trim title)))
    (insert "#+ZHIHU_ARTICLE_ID:\n")
    (insert (format "#+ZHIHU_TOPICS: %s\n" (if (string-empty-p topics) "[]" topics)))
    (when (not (string-empty-p column))
      (insert (format "#+ZHIHU_COLUMN_ID: %s\n" column)))
    (insert "#+TOC: headlines 2\n\n* 正文\n\n")
    (save-buffer)
    (condition-case nil
        (progn
          (require 'zhihu)
          (when (fboundp 'zhihu-mode) (zhihu-mode 1)))
      (error nil))
    (message "文章源稿已创建: %s\n发布: M-x zhihu-publish" file)))

(defun zhihu-reader-new-answer (question-id)
  "Start a new answer draft for QUESTION-ID."
  (interactive "s知乎问题 ID 或 URL: ")
  (let ((qid (zhihu-reader--id-or-url question-id)))
    (zhihu-reader--open-edit-buffer
     "answer" nil qid "新回答" "")))

(defun zhihu-reader-save-draft ()
  "Save the current edit buffer as an Org draft."
  (interactive)
  (unless (derived-mode-p 'zhihu-reader-edit-mode)
    (user-error "当前 buffer 不是知乎编辑 buffer"))
  (let* ((dir (expand-file-name "~/.local/state/zhihu-reader/drafts/"))
         (file (expand-file-name
                (format "%s-%s.org"
                        (format-time-string "%Y%m%d-%H%M%S")
                        (or zhihu-reader-edit-id "new"))
                dir)))
    (make-directory dir t)
    (with-temp-file file
      (insert (format "#+ZHIHU_KIND: %s\n#+ZHIHU_ID: %s\n#+ZHIHU_QUESTION_ID: %s\n\n"
                      zhihu-reader-edit-kind
                      (or zhihu-reader-edit-id "")
                      (or zhihu-reader-edit-question-id "")))
      (insert (buffer-string)))
    (message "草稿已保存: %s" file)))

(defun zhihu-reader-open-draft (file)
  "Open a saved Org draft FILE."
  (interactive
   (list (read-file-name "草稿: "
                         (expand-file-name "~/.local/state/zhihu-reader/drafts/")
                         nil t nil)))
  (let (kind id qid)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (when (re-search-forward "^#\\+ZHIHU_KIND: \\(.*\\)$" nil t)
        (setq kind (match-string 1)))
      (when (re-search-forward "^#\\+ZHIHU_ID: \\(.*\\)$" nil t)
        (setq id (match-string 1)))
      (when (re-search-forward "^#\\+ZHIHU_QUESTION_ID: \\(.*\\)$" nil t)
        (setq qid (match-string 1))))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (when (re-search-forward "^#\\+ZHIHU_QUESTION_ID: .*\\n\\n" nil t)
        (delete-region (point-min) (point)))
      (zhihu-reader--open-edit-buffer
       (or kind "answer") (and id (not (string-empty-p id)) id) qid
       "草稿" (buffer-string)))))

(defun zhihu-reader-edit-at-point ()
  "Edit the answer or comment at point in an Org buffer."
  (interactive)
  (let ((comment-id (org-entry-get nil "ZHIHU-COMMENT"))
        (answer-id (org-entry-get nil "ZHIHU-ANSWER"))
        (question-id (org-entry-get nil "ZHIHU-QUESTION")))
    (cond
     (comment-id
      (zhihu-reader--open-edit-buffer
       "comment" comment-id nil
       (org-get-heading t t t t)
       (zhihu-reader--subtree-body)))
     (answer-id
      (unless question-id
        (user-error "缺少问题 ID，无法编辑回答"))
      (zhihu-reader--open-edit-buffer
       "answer" answer-id question-id
       (org-get-heading t t t t)
       (zhihu-reader--subtree-body)))
     (t
      (user-error "光标不在回答或评论标题上")))))

(defun zhihu-reader-edit-preview ()
  "Render the current edit buffer as HTML in Eww."
  (interactive)
  (zhihu-reader--ensure-zhihu)
  (let ((html (zhihu--org->html (buffer-string)))
        (file (make-temp-file "zhihu-preview-" nil ".html")))
    (with-temp-file file (insert html))
    (require 'eww)
    (eww-open-file file)))

(defun zhihu-reader-edit-publish ()
  "Preview and publish the current edit buffer.
This is destructive and requires `zhihu-reader-enable-write'."
  (interactive)
  (unless zhihu-reader-enable-write
    (user-error "写操作已关闭；请设置 `zhihu-reader-enable-write' 为 t"))
  (unless (derived-mode-p 'zhihu-reader-edit-mode)
    (user-error "当前 buffer 不是知乎编辑 buffer"))
  (zhihu-reader--ensure-zhihu)
  (let ((kind zhihu-reader-edit-kind)
        (id zhihu-reader-edit-id)
        (qid zhihu-reader-edit-question-id)
        (text (string-trim (buffer-string))))
    (unless (yes-or-no-p
             (format "确认发布这个%s？此操作会直接更新知乎线上内容。"
                     (if (equal kind "answer") "回答" "评论")))
      (user-error "已取消发布"))
    (pcase kind
      ("answer"
       (let* ((html (zhihu--org->html text))
              (new-id (zhihu--publish-answer id qid html)))
         (unless id
           (setq-local zhihu-reader-edit-id new-id))
         (message (if id "回答已更新" "新回答已发布"))))
      ("comment"
       (let* ((url (format "https://www.zhihu.com/api/v4/comment_v5/comment/%s" id))
              (resp (zhihu--http-json "PATCH" url
                                      :body (list :content text)))
              (status (plist-get resp :status)))
         (unless (equal status 200)
           (error "评论更新失败（HTTP %s）：%s"
                  status (plist-get resp :body)))
         (message "评论已更新")))
      (_ (user-error "未知编辑类型: %s" kind)))))

(defun zhihu-reader-edit-cancel ()
  "Leave the current edit buffer without publishing."
  (interactive)
  (when (derived-mode-p 'zhihu-reader-edit-mode)
    (kill-buffer)))

(defvar zhihu-reader-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'zhihu-reader-edit-preview)
    (define-key map (kbd "C-c C-p") #'zhihu-reader-edit-publish)
    (define-key map (kbd "C-c C-k") #'zhihu-reader-edit-cancel)
    (define-key map (kbd "C-c C-s") #'zhihu-reader-save-draft)
    map)
  "Keymap for `zhihu-reader-edit-mode'.")

(define-derived-mode zhihu-reader-edit-mode org-mode "Zhihu-Edit"
  "Major mode for editing a Zhihu answer or comment before publishing."
  (setq-local buffer-read-only nil))


;;;; Commands and mode

(defun zhihu-reader--parse-target (value)
  "Parse VALUE into a plist of question/answer ids."
  (let ((value (or value "")))
    (cond
     ((string-match "question/\\([0-9]+\\)/answer/\\([0-9]+\\)" value)
      (list :question (match-string 1 value) :answer (match-string 2 value)))
     ((string-match "answer/\\([0-9]+\\)" value)
      (list :answer (match-string 1 value)))
     ((string-match "question/\\([0-9]+\\)" value)
      (list :question (match-string 1 value)))
     ((string-match "p/\\([0-9]+\\)" value)
      (list :article (match-string 1 value)))
     ((string-match "\\`\\([0-9]\\{6,\\}\\)\\'" value)
      (list :question (match-string 1 value)))
     (t nil))))

(defun zhihu-reader--current-url ()
  "Return a URL for the current heading or Org link."
  (or (zhihu-reader--link-at-point)
      (let ((q (org-entry-get nil "ZHIHU-QUESTION"))
            (a (org-entry-get nil "ZHIHU-ANSWER"))
            (article (org-entry-get nil "ZHIHU-ARTICLE")))
        (cond
         (a (format "https://www.zhihu.com/answer/%s" a))
         (q (format "https://www.zhihu.com/question/%s" q))
         (article (format "https://zhuanlan.zhihu.com/p/%s" article))))))

(defun zhihu-reader--link-at-point ()
  "Return the Org link URL at point, if any."
  (let ((context (org-element-context)))
    (when (eq (org-element-type context) 'link)
      (org-element-property :raw-link context))))

(defun zhihu-reader-open-at-point ()
  "Enter the question/answer at point, or open its link."
  (interactive)
  (let ((q (org-entry-get nil "ZHIHU-QUESTION"))
        (a (org-entry-get nil "ZHIHU-ANSWER"))
        (article (org-entry-get nil "ZHIHU-ARTICLE"))
        (url (zhihu-reader--link-at-point)))
    (cond
     (a (zhihu-reader-answer a))
     (q (zhihu-reader-question q))
     (url (zhihu-reader-read-url url))
     (article (zhihu-reader-read-url
               (format "https://zhuanlan.zhihu.com/p/%s" article)))
     (t (org-open-at-point)))))

(defun zhihu-reader-copy-link ()
  "Copy the current Zhihu link."
  (interactive)
  (let ((url (zhihu-reader--current-url)))
    (if (and url (not (string-empty-p url)))
        (progn (kill-new url) (message "已复制: %s" url))
      (user-error "光标不在链接上"))))

(defun zhihu-reader-read-url (url)
  "Read URL through EAF + Readability."
  (interactive "sZhihu URL: ")
  (cond
   ((fboundp 'my/zhihu-read)
    (my/zhihu-read url))
   (t (browse-url-default-browser url))))

(defun zhihu-reader-login ()
  "Open Zhihu sign-in in an external browser."
  (interactive)
  (let ((url "https://www.zhihu.com/signin"))
    (if (and zhihu-reader-login-browser
             (executable-find zhihu-reader-login-browser))
        (start-process "zhihu-reader-login" nil
                       zhihu-reader-login-browser url)
      (browse-url url))
    (message "登录后用 Zen/Firefox 会自动读取 Cookie；其它浏览器请 M-x zhihu-reader-set-cookie")))

(defun zhihu-reader-set-cookie (cookie)
  "Store a manually copied Zhihu Cookie header."
  (interactive
   (list (read-string "Zhihu Cookie: "
                      (or (ignore-errors (gui-get-selection 'CLIPBOARD)) ""))))
  (unless (string-match-p "\\(?:^\\|; \\)z_c0=[^;]" (or cookie ""))
    (user-error "Cookie 里没有有效的 z_c0"))
  (make-directory (file-name-directory zhihu-reader-cookie-file) t)
  (write-region cookie nil zhihu-reader-cookie-file nil 'silent)
  (set-file-modes zhihu-reader-cookie-file #o600)
  (message "Zhihu Cookie 已保存到 %s" zhihu-reader-cookie-file))

(defun zhihu-reader-clear-cookie ()
  "Remove the manually stored Zhihu Cookie."
  (interactive)
  (when (file-exists-p zhihu-reader-cookie-file)
    (delete-file zhihu-reader-cookie-file)
    (message "已删除 %s" zhihu-reader-cookie-file)))

(defun zhihu-reader-cookie-status ()
  "Show the active cookie source and z_c0 status."
  (interactive)
  (let* ((file (zhihu-reader--file-cookies))
         (browser (zhihu-reader--browser-cookies))
         (eaf (zhihu-reader--eaf-cookies))
         (cookie (or file browser eaf "")))
    (message "source=%s z_c0=%s length=%d"
             (cond (file "file") (browser "browser") (eaf "eaf") (t "none"))
             (if (zhihu-reader--login-p cookie) "yes" "no")
             (length cookie))))

(defun zhihu-reader--load-seen ()
  "Load the persisted seen-ID set."
  (unless zhihu-reader--seen-loaded
    (setq zhihu-reader--seen-loaded t)
    (when (file-readable-p zhihu-reader-seen-file)
      (condition-case nil
          (with-temp-buffer
            (insert-file-contents zhihu-reader-seen-file)
            (dolist (key (read (current-buffer)))
              (puthash key t zhihu-reader--seen)))
        (error nil)))))

(defun zhihu-reader--save-seen ()
  "Persist the seen-ID set."
  (make-directory (file-name-directory zhihu-reader-seen-file) t)
  (let ((keys (let (result)
                (maphash (lambda (key _) (push key result)) zhihu-reader--seen)
                (sort result #'string<))))
    (with-temp-file zhihu-reader-seen-file
      (prin1 keys (current-buffer)))))

(defun zhihu-reader--item-seen-key (item)
  "Return a deduplication key for feed ITEM."
  (let ((kind (plist-get item :kind))
        (qid (plist-get item :question-id))
        (id (plist-get item :id)))
    (cond
     ((and (equal kind "answer") qid (not (string-empty-p qid)))
      (concat "question:" qid))
     ((not (string-empty-p (or id "")))
      (concat (or kind "item") ":" id))
     (t nil))))

(defun zhihu-reader--filter-seen (items)
  "Return unseen ITEMS and persist the updated seen set."
  (zhihu-reader--load-seen)
  (let ((new-items (zhihu-reader--filter-seen-1 items)))
    (when (null new-items)
      (message "本页推荐都已经看过"))
    new-items))

(defun zhihu-reader--filter-seen-1 (items)
  "Return ITEMS not seen before, then mark them seen."
  (let (result changed)
    (dolist (item items)
      (let ((key (zhihu-reader--item-seen-key item)))
        (if (and key (gethash key zhihu-reader--seen))
            nil
          (when key
            (puthash key t zhihu-reader--seen)
            (setq changed t))
          (push item result))))
    (when changed (zhihu-reader--save-seen))
    (nreverse result)))

(defun zhihu-reader-clear-comment-state ()
  "Clear stale asynchronous comment request state."
  (interactive)
  (clrhash zhihu-reader--comments-inflight)
  (message "评论请求状态已清空"))

(defun zhihu-reader-clear-seen ()
  "Clear the recommendation seen-ID set."
  (interactive)
  (clrhash zhihu-reader--seen)
  (when (file-exists-p zhihu-reader-seen-file)
    (delete-file zhihu-reader-seen-file))
  (message "推荐去重记录已清空"))

(defun zhihu-reader--normalize-feed (json)
  "Normalize JSON and filter already-seen recommendations."
  (zhihu-reader--filter-seen (zhihu-reader--normalize-items json)))

(defun zhihu-reader-feed (&optional page force)
  "Fetch and display the recommendation feed.
With FORCE non-nil, bypass the page cache."
  (interactive "p")
  (let* ((page (or page 1))
         (key (list 'feed page))
         (url (if (> page 1)
                  (replace-regexp-in-string
                   "page_number=[0-9]+" (format "page_number=%d" page)
                   zhihu-reader-feed-url)
                zhihu-reader-feed-url))
         (title (format "推荐流 · 第 %d 页" page)))
    (if (and (not force) (zhihu-reader--show-cached key))
        (message "使用缓存的第 %d 页" page)
      (if noninteractive
          (let ((items (zhihu-reader--normalize-feed
                        (zhihu-reader--fetch-json url))))
            (puthash key (cons title items) zhihu-reader--page-cache)
            (zhihu-reader--render title items key)
            (zhihu-reader--restore-point key))
        (zhihu-reader--async-list
         title key url nil
         (lambda (json)
           (cons title (zhihu-reader--normalize-feed json)))
         key)))))

(defun zhihu-reader--author-url (token page)
  "Return the member answers API URL for TOKEN and PAGE."
  (format "https://www.zhihu.com/api/v4/members/%s/answers?include=data%%5B*%%5D.content%%2Cexcerpt%%2Cauthor%%2Cquestion%%2Ccomment_count%%2Cvoteup_count&limit=20&offset=%d&order_by=created"
          token (* 20 (1- (or page 1)))))

(defun zhihu-reader-author (token &optional page)
  "Open author TOKEN's answer list."
  (interactive
   (list (or (org-entry-get nil "ZHIHU-AUTHOR-TOKEN")
             (read-string "知乎作者 url_token: "))
         (prefix-numeric-value current-prefix-arg)))
  (let* ((token (or token (read-string "知乎作者 url_token: ")))
         (page (or page 1))
         (url (zhihu-reader--author-url token page))
         (title (format "作者 %s · 第 %d 页" token page)))
    (if noninteractive
        (zhihu-reader--render title
                              (zhihu-reader--normalize-items
                               (zhihu-reader--fetch-json url))
                              (list 'author token page))
      (zhihu-reader--async-list
       title (list 'author token page) url nil
       (lambda (json)
         (cons title (zhihu-reader--normalize-items json)))))))

(defun zhihu-reader-hot (&optional force)
  "Fetch and display the hot list.
With FORCE non-nil, bypass the page cache."
  (interactive)
  (let ((title "热榜")
        (key (list 'hot nil)))
    (if (and (not force) (zhihu-reader--show-cached key))
        (message "使用缓存的热榜")
      (if noninteractive
          (let ((items (zhihu-reader--normalize-items
                        (zhihu-reader--fetch-json zhihu-reader-hot-url))))
            (puthash key (cons title items) zhihu-reader--page-cache)
            (zhihu-reader--render title items key)
            (zhihu-reader--restore-point key))
        (zhihu-reader--async-list
         title key zhihu-reader-hot-url nil
         (lambda (json)
           (cons title (zhihu-reader--normalize-items json)))
         key)))))

(defun zhihu-reader--search-url (query page)
  "Build a paginated search URL for QUERY and PAGE."
  (let ((url (format zhihu-reader-search-url (url-hexify-string query))))
    (replace-regexp-in-string
     "offset=0" (format "offset=%d" (* 20 (1- (or page 1)))) url)))

(defun zhihu-reader-search (&optional query page force)
  "Search Zhihu for QUERY, starting at PAGE.
With FORCE non-nil, bypass the page cache."
  (interactive (list (read-string "搜索知乎: ")
                     (prefix-numeric-value current-prefix-arg)))
  (let* ((query (or query (read-string "搜索知乎: ")))
         (page (or page 1))
         (key (list 'search query page))
         (url (zhihu-reader--search-url query page))
         (title (format "搜索 · %s · 第 %d 页" query page)))
    (if (and (not force) (zhihu-reader--show-cached key))
        (message "使用缓存的搜索第 %d 页" page)
      (if noninteractive
          (let ((items (zhihu-reader--normalize-items
                        (zhihu-reader--fetch-json url))))
            (puthash key (cons title items) zhihu-reader--page-cache)
            (zhihu-reader--render title items key)
            (zhihu-reader--restore-point key))
        (zhihu-reader--async-list
         title key url nil
         (lambda (json)
           (cons title (zhihu-reader--normalize-items json)))
         key)))))

(defun zhihu-reader-refresh ()
  "Refresh the current Zhihu view."
  (interactive)
  (pcase (car-safe zhihu-reader--view)
    ('feed (zhihu-reader-feed (nth 1 zhihu-reader--view) t))
    ('hot (zhihu-reader-hot t))
    ('search (zhihu-reader-search (nth 1 zhihu-reader--view)
                                   (nth 2 zhihu-reader--view) t))
    ('question (zhihu-reader-question (nth 1 zhihu-reader--view)
                                       (nth 2 zhihu-reader--view)
                                       (nth 3 zhihu-reader--view) t))
    ('answer (zhihu-reader-answer (nth 1 zhihu-reader--view)))
    ('author (zhihu-reader-author (nth 1 zhihu-reader--view)
                                  (nth 2 zhihu-reader--view)))
    (_ (user-error "当前 buffer 没有可刷新的知乎视图"))))

(defun zhihu-reader-sort ()
  "Change answer sort order for the current question view."
  (interactive)
  (unless (eq (car-safe zhihu-reader--view) 'question)
    (user-error "只有问题详情可以切换回答排序"))
  (let* ((id (nth 1 zhihu-reader--view))
         (page (or (nth 2 zhihu-reader--view) 1))
         (order (completing-read "回答排序: "
                                 '("default" "updated" "created")
                                 nil t nil nil
                                 (or (nth 3 zhihu-reader--view) "default"))))
    (zhihu-reader-question id page order)))

(defun zhihu-reader-next-page ()
  "Go to the next page of the current list."
  (interactive)
  (let ((view zhihu-reader--view))
    (zhihu-reader--capture-navigation-anchor)
    (zhihu-reader--remember-point)
    (pcase (car-safe view)
      ('feed (zhihu-reader-feed (1+ (or (nth 1 view) 1))))
      ('search (zhihu-reader-search (nth 1 view)
                                     (1+ (or (nth 2 view) 1))))
      ('question (zhihu-reader-question (nth 1 view)
                                        (1+ (or (nth 2 view) 1))
                                        (nth 3 view)))
      (_ (user-error "当前视图没有下一页")))))

(defun zhihu-reader-previous-page ()
  "Go to the previous page of the current list."
  (interactive)
  (let ((view zhihu-reader--view))
    (zhihu-reader--capture-navigation-anchor)
    (zhihu-reader--remember-point)
    (pcase (car-safe view)
      ('feed (zhihu-reader-feed (max 1 (1- (or (nth 1 view) 1)))))
      ('search (zhihu-reader-search (nth 1 view)
                                     (max 1 (1- (or (nth 2 view) 1)))))
      ('question (zhihu-reader-question (nth 1 view)
                                         (max 1 (1- (or (nth 2 view) 1)))
                                         (nth 3 view)))
      (_ (user-error "当前视图没有上一页")))))

(defun zhihu-reader--process-buffer-images (&optional beg end)
  "Download remote image links between BEG and END, and display local ones."
  (let ((count 0))
    (save-excursion
      (save-restriction
        (when (or beg end)
          (narrow-to-region (or beg (point-min)) (or end (point-max))))
        (goto-char (point-min))
        (while (re-search-forward "\\[\\[\\(https?://[^]\n]+\\)\\]\\[图片\\]\\]" nil t)
        (let ((url (match-string 1))
              (marker (copy-marker (match-beginning 0))))
          (unless (or (gethash url zhihu-reader--image-inflight)
                      (gethash url zhihu-reader--image-failed))
            (puthash url t zhihu-reader--image-inflight)
            (setq count (1+ count))
            (zhihu-reader--download-image-async url marker))))))
    (when (display-graphic-p)
      (zhihu-reader--queue-image-display))
    count))

(defun zhihu-reader-clear-image-cache ()
  "Delete all cached Zhihu images."
  (interactive)
  (when (file-directory-p zhihu-reader-image-cache)
    (delete-directory zhihu-reader-image-cache t))
  (clrhash zhihu-reader--image-failed)
  (clrhash zhihu-reader--image-inflight)
  (message "已清空 %s" zhihu-reader-image-cache))

(defun zhihu-reader-toggle-images (&optional off)
  "Ensure images are rendered in the current buffer.
With prefix argument OFF, disable inline images instead."
  (interactive "P")
  (if off
      (progn
        (setq zhihu-reader-render-images nil)
        (when (fboundp 'org-remove-inline-images)
          (ignore-errors (org-remove-inline-images)))
        (message "图片渲染已关闭"))
    (setq zhihu-reader-render-images t)
    (let ((count (zhihu-reader--process-buffer-images)))
      (message "图片渲染已开启；处理了 %d 个图片链接" count))))

(defun zhihu-reader-read-url-from-point ()
  "Open the URL at point or the current heading with EAF + Readability."
  (interactive)
  (let ((url (zhihu-reader--current-url)))
    (if (and url (not (string-empty-p url)))
        (zhihu-reader-read-url url)
      (user-error "光标不在链接或知乎标题上"))))

(defun zhihu-reader-help ()
  "Show Zhihu reader key bindings."
  (interactive)
  (with-help-window "*Zhihu Help*"
    (princ
     "Zhihu Reader\n\n\
TAB     fold/unfold\n\
RET     enter question/answer\n\
c       load comments\n\
C-c ]   next comments page\n\
C-c [   previous comments page\n\
C-c c   write a comment/reply\n\
C-c a   author page\n\
e       edit answer/comment\n\
g       refresh current view\n\
n/p     next/previous heading\n\
o       open current URL with EAF + Readability\n\
w       copy link\n\
r       recommendation feed\n\
h       hot list\n\
s       search\n\
S       answer sort order (question view)\n\
] / [   next/previous page\n\
i       toggle inline images\n\
q       quit\n\
?       this help\n")))

(defvar zhihu-reader-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'zhihu-reader-open-at-point)
    (define-key map (kbd "]") #'zhihu-reader-next-page)
    (define-key map (kbd "[") #'zhihu-reader-previous-page)
    (define-key map (kbd "i") #'zhihu-reader-toggle-images)
    (define-key map (kbd "g") #'zhihu-reader-refresh)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "n") #'org-next-visible-heading)
    (define-key map (kbd "p") #'org-previous-visible-heading)
    (define-key map (kbd "c") #'zhihu-reader-comments)
    (define-key map (kbd "C-c ]") #'zhihu-reader-comments-more)
    (define-key map (kbd "C-c [") #'zhihu-reader-comments-previous)
    (define-key map (kbd "C-c c") #'zhihu-reader-compose-comment)
    (define-key map (kbd "C-c a") #'zhihu-reader-author)
    (define-key map (kbd "C-c n") #'zhihu-reader-new-answer)
    (define-key map (kbd "C-c N") #'zhihu-reader-new-article)
    (define-key map (kbd "C-c s") #'zhihu-reader-save-draft)
    (define-key map (kbd "e") #'zhihu-reader-edit-at-point)
    (define-key map (kbd "o") #'zhihu-reader-read-url-from-point)
    (define-key map (kbd "w") #'zhihu-reader-copy-link)
    (define-key map (kbd "s") #'zhihu-reader-search)
    (define-key map (kbd "S") #'zhihu-reader-sort)
    (define-key map (kbd "h") #'zhihu-reader-hot)
    (define-key map (kbd "r") #'zhihu-reader-feed)
    (define-key map (kbd "?") #'zhihu-reader-help)
    map)
  "Keymap for `zhihu-reader-mode'.")

(defun zhihu-reader--auto-load-comments (&optional beg end)
  "Auto-load comments for visible comment headings.
Limit to `zhihu-reader-auto-comments-limit' sections."
  (when (and zhihu-reader-auto-comments
             (derived-mode-p 'zhihu-reader-mode))
    (let ((count 0))
      (save-restriction
        (when (or beg end)
          (narrow-to-region (or beg (point-min)) (or end (point-max))))
        (org-map-entries
         (lambda ()
           (when (< count zhihu-reader-auto-comments-limit)
             (let* ((id (org-entry-get nil "ZHIHU-COMMENTS"))
                    (loaded (org-entry-get nil "ZHIHU-COMMENTS-LOADED"))
                    (heading (org-get-heading t t t t))
                    (kind (or (org-entry-get nil "ZHIHU-COMMENTS-KIND") "answer")))
               (when (and id (not loaded)
                          (not (gethash id zhihu-reader--comments-inflight))
                          (string-match "评论 (\\([0-9]+\\))" heading)
                          (> (string-to-number (match-string 1 heading)) 0))
                 (puthash id t zhihu-reader--comments-inflight)
                 (setq count (1+ count))
                 (zhihu-reader--load-comments-async
                  id (point-marker) kind 0 t))))))))))

(defun zhihu-reader--after-cycle (state &rest _)
  "Load comments and images after a visibility STATE change."
  (unless (eq state 'folded)
    (when (derived-mode-p 'zhihu-reader-mode)
      (condition-case nil
          (save-excursion
            (org-back-to-heading t)
            (let ((beg (point))
                  (end (save-excursion (org-end-of-subtree t t))))
              ;; Comments must not depend on image rendering being enabled.
              (zhihu-reader--auto-load-comments beg end)
              (when (and zhihu-reader-render-images (display-graphic-p))
                (zhihu-reader--process-buffer-images beg end))
              ;; Refresh the outline/body after async insertions.
              (ignore-errors (org-fold-show-all))
              (ignore-errors (force-window-update (current-buffer)))))
        (error nil)))))

(define-derived-mode zhihu-reader-mode org-mode "Zhihu"
  "Major mode for Zhihu reader buffers."
  (setq-local org-startup-folded 'overview)
  (setq-local truncate-lines nil)
  (setq-local org-display-remote-inline-images 'skip)
  (add-hook 'org-cycle-hook #'zhihu-reader--after-cycle nil t))

(provide 'zhihu-reader)
;;; zhihu-reader.el ends here
