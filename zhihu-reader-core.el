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
(require 'xml)
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

(defcustom zhihu-reader-bookmarks-file
  (expand-file-name "~/.local/state/zhihu-reader/bookmarks.eld")
  "File used to persist reader bookmarks."
  :type 'file
  :group 'zhihu-reader)

(defcustom zhihu-reader-history-file
  (expand-file-name "~/.local/state/zhihu-reader/history.eld")
  "File used to persist reading history."
  :type 'file
  :group 'zhihu-reader)

(defcustom zhihu-reader-search-history-file
  (expand-file-name "~/.local/state/zhihu-reader/search-history.eld")
  "File used to persist search history."
  :type 'file
  :group 'zhihu-reader)

(defcustom zhihu-reader-capture-file
  (expand-file-name "~/org/inbox.org")
  "Default Org file used by `zhihu-reader-capture'."
  :type 'file
  :group 'zhihu-reader)

(defcustom zhihu-reader-later-file
  (expand-file-name "~/.local/state/zhihu-reader/later.eld")
  "File used to persist read-later entries."
  :type 'file
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

(defvar zhihu-reader--unread-total 0
  "Last known total of unread Zhihu notifications/messages.")

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

(defcustom zhihu-reader-search-content-type "all"
  "Search content filter: all, answer or article."
  :type '(choice (const "all") (const "answer") (const "article"))
  :group 'zhihu-reader)

(defcustom zhihu-reader-search-time-range "all"
  "Search time filter: all, day, week, month, three-months, half-year or year."
  :type '(choice (const "all") (const "day") (const "week") (const "month")
                 (const "three-months") (const "half-year") (const "year"))
  :group 'zhihu-reader)

(defcustom zhihu-reader-copy-link-include-title t
  "When non-nil, prepend the question/article title to copied share links."
  :type 'boolean
  :group 'zhihu-reader)

(defcustom zhihu-reader-search-sort nil
  "Optional search sort, e.g. \"upvoted_count\" or \"created_time\"."
  :type '(choice (const :tag "Default" nil)
                 (const "upvoted_count")
                 (const "created_time")
                 string)
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
(defvar-local zhihu-reader--following-next nil
  "Next cursor URL for the following feed.")

(defvar-local zhihu-reader--following-prev nil
  "Previous cursor URL for the following feed.")

(defvar-local zhihu-reader--collection-has-next nil
  "Non-nil when the current collection view has another page.")

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

(defun zhihu-reader--cookie-alist ()
  "Return the current Zhihu cookie header as a name/value alist."
  (mapcar
   (lambda (part)
     (let ((i (string-match "=" part)))
       (when i
         (cons (substring part 0 i) (substring part (1+ i))))))
   (split-string (zhihu-reader--cookies) "; " t)))

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

(defun zhihu-reader--fetch-json-async (url success &optional failure sign)
  "Fetch URL asynchronously; call SUCCESS with JSON or FAILURE with a string.
When SIGN is non-nil add ZSE headers.  The request does not block Emacs."
  (let* ((cookie (zhihu-reader--require-login))
         (url-request-extra-headers
          (append
           `(("Cookie" . ,cookie)
             ("User-Agent" . ,zhihu-reader-user-agent)
             ("Referer" . "https://www.zhihu.com/")
             ("Origin" . "https://www.zhihu.com")
             ("x-requested-with" . "fetch")
             ("x-api-version" . "3.0.53")
             ("Accept" . "application/json, text/plain, */*"))
           (when sign
             (zhihu-reader--zse-headers
              url (cdr (assoc-string "d_c0" (zhihu-reader--cookie-alist)))))))
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

(defun zhihu-reader--show-cached (key &optional buffer-name)
  "Render cached page KEY in BUFFER-NAME if present.  Return non-nil on success."
  (let ((entry (gethash key zhihu-reader--page-cache)))
    (when entry
      (let ((anchor zhihu-reader--navigation-anchor))
        (zhihu-reader--render (car entry) (cdr entry) key buffer-name)
        (unless anchor
          (zhihu-reader--restore-point key)))
      t)))

(defun zhihu-reader--async-list (title view url buffer-name make-result
                                      &optional cache-key sign after-render)
  "Fetch URL asynchronously and render the result in BUFFER-NAME.
MAKE-RESULT receives JSON and returns (TITLE . ITEMS).
AFTER-RENDER is called with JSON after rendering."
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
                 (zhihu-reader--restore-point cache-key))
               (when after-render
                 (funcall after-render json)))
           (error (zhihu-reader--show-load-error buffer (error-message-string err))))))
     (lambda (error)
       (when (and (zhihu-reader--generation-current-p buffer generation)
                  (equal view
                         (buffer-local-value 'zhihu-reader--view buffer)))
         (zhihu-reader--show-load-error buffer error)))
     sign)))

(defun zhihu-reader--async-custom (title view url buffer render-fn)
  "Fetch URL asynchronously and call RENDER-FN with JSON in BUFFER.
TITLE/VIEW initialize the loading buffer.  RENDER-FN must replace the
loading contents; errors are shown in BUFFER."
  (let* ((target (zhihu-reader--prepare-buffer title view buffer))
         (generation (with-current-buffer target zhihu-reader--generation)))
    (zhihu-reader--fetch-json-async
     url
     (lambda (json)
       (when (and (zhihu-reader--generation-current-p target generation)
                  (equal view
                         (buffer-local-value 'zhihu-reader--view target)))
         (condition-case err
             (with-current-buffer target
               (funcall render-fn json))
           (error (zhihu-reader--show-load-error
                   target (error-message-string err))))))
     (lambda (error)
       (when (zhihu-reader--generation-current-p target generation)
         (zhihu-reader--show-load-error target error))))))

;;; Normalization

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
         (concat "\n#+begin_quote\n" (string-trim (funcall inner)) "\n#+end_quote\n\n"))
        ('pre
         (concat "\n#+begin_example\n"
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
           (cond
            ((zhihu-reader--image-url-p href) "")
            ((and href (string-match-p "\\`https?://" href))
             (org-make-link-string href (if (string-empty-p label) href label)))
            (t label))))
        ('ul
         (concat "\n" (funcall inner 'ul) "\n"))
        ('ol
         (concat "\n" (funcall inner 'ol) "\n"))
        ('li
         (let* ((body (string-trim (funcall inner)))
                (prefix (if (eq list-type 'ol) "1. " "- ")))
           (if (string-prefix-p "#+begin_" body)
               ;; A block cannot follow "- " on the same line; indent it as
               ;; the list item body so begin/end markers stay balanced.
               (concat prefix "\n"
                       (mapconcat (lambda (line) (concat "  " line))
                                  (split-string body "\n") "\n")
                       "\n")
             (concat prefix body "\n"))))
        ('span
         (let ((tex (dom-attr node 'data-tex)))
           (if (and tex (not (string-empty-p tex)))
               (concat "$" tex "$")
             (funcall inner))))
        ('img "")
        ('table
         (concat "\n#+begin_example\n" (string-trim (funcall text)) "\n#+end_example\n\n"))
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

(defun zhihu-reader--image-url-p (url)
  "Return non-nil when URL looks like an image URL."
  (and (stringp url)
       (or (string-match-p "\\.\\(?:png\\|jpe?g\\|gif\\|webp\\)\\(?:[?#].*\\)?\\'" url)
           (string-match-p "zhimg\\.com/" url))))

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
             (images (dom-by-tag dom 'img))
             (links (dom-by-tag dom 'a)))
        (delete-dups
         (delq nil
               (append
                (mapcar (lambda (node)
                          (let ((url (or (dom-attr node 'data-original)
                                         (dom-attr node 'data-actualsrc)
                                         (dom-attr node 'src))))
                            (and (zhihu-reader--image-url-p url) url)))
                        images)
                (mapcar (lambda (node)
                          (let ((url (dom-attr node 'href)))
                            (and (zhihu-reader--image-url-p url) url)))
                        links)))))))))

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

(provide 'zhihu-reader-core)
;;; zhihu-reader-core.el ends here
