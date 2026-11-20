;;; zhihu-reader-views.el --- Views and navigation for Zhihu  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 qukuqhd
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Part of emacs-zhihu-reader.  See zhihu-reader.el for the entry point.

;;; Code:

(require 'zhihu-reader-core)
(require 'zhihu-reader-api)
(require 'zhihu-reader-ui)
(require 'zhihu-reader-comments)

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
  "Return a canonical share URL for the current heading or Org link."
  (or (zhihu-reader--link-at-point)
      (let ((q (org-entry-get nil "ZHIHU-QUESTION"))
            (a (org-entry-get nil "ZHIHU-ANSWER"))
            (article (org-entry-get nil "ZHIHU-ARTICLE"))
            (comment (org-entry-get nil "ZHIHU-COMMENT")))
        (cond
         (comment (format "https://www.zhihu.com/comment/%s" comment))
         ((and a q)
          (format "https://www.zhihu.com/question/%s/answer/%s" q a))
         (a (format "https://www.zhihu.com/answer/%s" a))
         (q (format "https://www.zhihu.com/question/%s" q))
         (article (format "https://zhuanlan.zhihu.com/p/%s" article))))))

(defun zhihu-reader--link-at-point ()
  "Return the Org link URL at point, if any."
  (let ((context (org-element-context)))
    (when (eq (org-element-type context) 'link)
      (org-element-property :raw-link context))))

(defun zhihu-reader-article (id &optional buffer-name)
  "Open Zhihu article ID in a dedicated Org buffer."
  (interactive
   (list (or (read-string "知乎文章 ID 或 URL: ")
             (user-error "文章 ID 为空"))))
  (let* ((id (zhihu-reader--id-or-url id))
         (url (format "https://www.zhihu.com/api/v4/articles/%s?include=content,author,comment_count,voteup_count,title" id))
         (resp (zhihu-reader--http-json "GET" url nil nil))
         (json (plist-get resp :json))
         (status (plist-get resp :status)))
    (unless (memq status '(200 201))
      (user-error "文章加载失败 (HTTP %s)" status))
    (let* ((item (zhihu-reader--normalize
                  (list (cons 'target json) (cons 'type "article"))))
           (buffer (or buffer-name "*Zhihu Article*")))
      (if item
          (zhihu-reader--render
           (format "文章 · %s" (or (plist-get item :title) id))
           (list item) (list 'article id) buffer)
        (user-error "文章没有可用内容")))))

(defun zhihu-reader--zhihu-url-p (url)
  "Return non-nil when URL is a Zhihu/Zhuanlan URL."
  (let* ((parsed (ignore-errors (url-generic-parse-url (or url ""))))
         (host (and parsed (url-host parsed))))
    (and host (string-match-p "\\(?:^\\|\\.\\)zhihu\\.com\\'" host))))

(defun zhihu-reader--url-query (url)
  "Return decoded query parameters of URL as an alist."
  (let* ((filename (or (url-filename (url-generic-parse-url url)) ""))
         (pos (string-match "\\?" filename)))
    (if pos (url-parse-query-string (substring filename (1+ pos))) nil)))

(defun zhihu-reader--query-value (query key)
  "Return value for KEY from QUERY alist."
  (cadr (assoc key query)))

(defun zhihu-reader-open-zhihu-url (url &optional buffer-name)
  "Open Zhihu URL inside Emacs, using a dedicated buffer when appropriate.
BUFFER-NAME overrides the target buffer for search/article views."
  (interactive "s知乎 URL: ")
  (if (not (zhihu-reader--zhihu-url-p url))
      (zhihu-reader-read-url url)
    (let* ((parsed (url-generic-parse-url url))
           (path (or (url-filename parsed) "/"))
           (query (zhihu-reader--url-query url))
           (content-type (zhihu-reader--query-value query "content_type"))
           (content-id (zhihu-reader--query-value query "content_id")))
      (cond
       ;; Search result / comment deep links carry content_type/content_id.
       ((and content-type content-id)
        (cond
         ((equal content-type "article")
          (zhihu-reader-article content-id (or buffer-name "*Zhihu Article*")))
         ((equal content-type "answer")
          (zhihu-reader-answer content-id))
         ((equal content-type "question")
          (zhihu-reader-question content-id))
         (t (zhihu-reader-read-url url))))
       ((string-match "/search" path)
        (let ((q (or (zhihu-reader--query-value query "q") "")))
          (if (string-empty-p q)
              (zhihu-reader-read-url url)
            (zhihu-reader-search q nil nil
                                (or buffer-name "*Zhihu Search*")))))
       ((string-match "/question/\\([0-9]+\\)/answer/\\([0-9]+\\)" path)
        (zhihu-reader-answer (match-string 2 path)))
       ((string-match "/answer/\\([0-9]+\\)" path)
        (zhihu-reader-answer (match-string 1 path)))
       ((string-match "/question/\\([0-9]+\\)" path)
        (zhihu-reader-question (match-string 1 path)))
       ((string-match "/p/\\([0-9]+\\)" path)
        (zhihu-reader-article (match-string 1 path)))
       ((string-match "/people/\\([^/?]+\\)" path)
        (zhihu-reader-author (match-string 1 path) nil "*Zhihu Author*"))
       ((string-match "/topic/\\([0-9]+\\)" path)
        (zhihu-reader-topic (match-string 1 path) nil "*Zhihu Topic*"))
       ((string-match "/collection/\\([0-9]+\\)" path)
        (zhihu-reader-collection (match-string 1 path) nil "*Zhihu Collection*"))
       ((string= path "/hot")
        (zhihu-reader-hot))
       ((or (string= path "/") (string-match-p "/follow" path))
        (zhihu-reader-following))
       (t (zhihu-reader-read-url url))))))

(defun zhihu-reader-open-at-point ()
  "Enter the question/answer at point, or open its link."
  (interactive)
  (let ((q (org-entry-get nil "ZHIHU-QUESTION"))
        (a (org-entry-get nil "ZHIHU-ANSWER"))
        (article (org-entry-get nil "ZHIHU-ARTICLE"))
        (url (zhihu-reader--link-at-point)))
    (cond
     (url (zhihu-reader-open-zhihu-url url))
     (a (zhihu-reader-answer a))
     (q (zhihu-reader-question q))
     (article (zhihu-reader-article article))
     (t (org-open-at-point)))))

(defun zhihu-reader-copy-share-link ()
  "Copy the canonical share link for the content at point."
  (interactive)
  (let ((url (zhihu-reader--current-url)))
    (unless (and url (not (string-empty-p url)))
      (user-error "光标不在问题、回答、文章或评论标题上"))
    (kill-new url)
    ;; In a graphical or XWayland session also write the system clipboard
    ;; explicitly; `kill-new' alone is not enough for every selection setup.
    (when (fboundp 'gui-set-selection)
      (ignore-errors (gui-set-selection 'CLIPBOARD url)))
    (message "已复制分享链接: %s" url)))

(defun zhihu-reader-copy-link ()
  "Copy the current Zhihu link (alias for share link)."
  (interactive)
  (zhihu-reader-copy-share-link))

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

(defun zhihu-reader--normalize-wrapped-items (json)
  "Normalize collection-style JSON entries that wrap content."
  (let ((data (zhihu-reader--alist-get 'data json)))
    (when (vectorp data)
      (delq nil
            (mapcar
             (lambda (entry)
               (let ((content (zhihu-reader--alist-get 'content entry))
                     (type (zhihu-reader--alist-get 'type entry)))
                 (if content
                     (zhihu-reader--normalize
                      (list (cons 'target content) (cons 'type type)))
                   (zhihu-reader--normalize entry))))
             (append data nil))))))

(defun zhihu-reader--normalize-moments (json)
  "Normalize moments_v3 JSON."
  (let ((data (zhihu-reader--alist-get 'data json)))
    (when (vectorp data)
      (delq nil
            (mapcar (lambda (entry)
                      (when (zhihu-reader--alist-get 'target entry)
                        (zhihu-reader--normalize entry)))
                    (append data nil))))))

(defun zhihu-reader-following (&optional cursor)
  "Show the signed-in account's following feed.
CURSOR is an optional `moments_v3' paging URL."
  (let* ((url (if (and cursor (stringp cursor)
                       (string-prefix-p "https://api.zhihu.com/moments_v3" cursor))
                  cursor
                "https://api.zhihu.com/moments_v3?feed_type=recommend&limit=20"))
         (title "关注动态")
         (view (list 'following cursor)))
    (if noninteractive
        (let* ((resp (zhihu-reader--http-json "GET" url nil nil))
               (json (plist-get resp :json)))
          (unless (memq (plist-get resp :status) '(200 201))
            (user-error "关注流加载失败 (HTTP %s)" (plist-get resp :status)))
          (let* ((paging (zhihu-reader--alist-get 'paging json))
                 (items (zhihu-reader--normalize-moments json)))
            (zhihu-reader--render title items view)
            (with-current-buffer (get-buffer zhihu-reader--main-buffer)
              (setq-local zhihu-reader--following-next
                          (zhihu-reader--alist-get 'next paging))
              (setq-local zhihu-reader--following-prev
                          (zhihu-reader--alist-get 'previous paging)))))
      (let* ((buffer (zhihu-reader--prepare-buffer title view nil))
             (generation (with-current-buffer buffer zhihu-reader--generation)))
        (zhihu-reader--fetch-json-async
         url
         (lambda (json)
           (when (and (zhihu-reader--generation-current-p buffer generation)
                      (equal view
                             (buffer-local-value 'zhihu-reader--view buffer)))
             (condition-case err
                 (with-current-buffer buffer
                   (let* ((paging (zhihu-reader--alist-get 'paging json))
                          (items (zhihu-reader--normalize-moments json)))
                     (zhihu-reader--render title items view buffer)
                     (setq-local zhihu-reader--following-next
                                 (zhihu-reader--alist-get 'next paging))
                     (setq-local zhihu-reader--following-prev
                                 (zhihu-reader--alist-get 'previous paging))
                     (message "关注动态：%d 条" (length items))))
               (error (zhihu-reader--show-load-error
                       buffer (error-message-string err))))))
         (lambda (error)
           (when (and (zhihu-reader--generation-current-p buffer generation)
                      (equal view
                             (buffer-local-value 'zhihu-reader--view buffer)))
             (zhihu-reader--show-load-error buffer error))))))))

(defun zhihu-reader-collection (id &optional page buffer-name)
  "Open collection ID in BUFFER-NAME."
  (interactive
   (list (or (read-string "知乎收藏夹 ID: ")
             (user-error "收藏夹 ID 为空"))
         (prefix-numeric-value current-prefix-arg)))
  (let* ((page (or page 1))
         (url (format "https://www.zhihu.com/api/v4/collections/%s/items?limit=20&offset=%d"
                      id (* 20 (1- page))))
         (title (format "收藏夹 %s · 第 %d 页" id page))
         (key (list 'collection id page buffer-name)))
    (if noninteractive
        (let* ((json (zhihu-reader--fetch-json url))
               (paging (zhihu-reader--alist-get 'paging json)))
          (zhihu-reader--render title
                                (zhihu-reader--normalize-wrapped-items json)
                                key buffer-name)
          (with-current-buffer (get-buffer (or buffer-name zhihu-reader--main-buffer))
            (setq-local zhihu-reader--collection-has-next
                        (and (zhihu-reader--alist-get 'next paging)
                             (not (eq (zhihu-reader--alist-get 'is_end paging) t))))))
      (zhihu-reader--async-list
       title key url buffer-name
       (lambda (json)
         (cons title (zhihu-reader--normalize-wrapped-items json)))
       key nil
       (lambda (json)
         (let ((paging (zhihu-reader--alist-get 'paging json)))
           (with-current-buffer (get-buffer (or buffer-name zhihu-reader--main-buffer))
             (setq-local zhihu-reader--collection-has-next
                         (and (zhihu-reader--alist-get 'next paging)
                              (not (eq (zhihu-reader--alist-get 'is_end paging) t)))))))))))

(defun zhihu-reader-topic (id &optional page buffer-name)
  "Open topic ID's essence feed in BUFFER-NAME."
  (interactive
   (list (or (read-string "知乎话题 ID: ")
             (user-error "话题 ID 为空"))
         (prefix-numeric-value current-prefix-arg)))
  (let* ((page (or page 1))
         (url (format "https://www.zhihu.com/api/v5.1/topics/%s/feeds/essence?limit=20&offset=%d"
                      id (* 20 (1- page))))
         (title (format "话题 %s · 第 %d 页" id page))
         (key (list 'topic id page buffer-name)))
    (if noninteractive
        (zhihu-reader--render title
                              (zhihu-reader--normalize-items
                               (plist-get (zhihu-reader--http-json "GET" url nil t)
                                          :json))
                              key buffer-name)
      (zhihu-reader--async-list
       title key url buffer-name
       (lambda (json) (cons title (zhihu-reader--normalize-items json)))
       key t))))

(defun zhihu-reader--load-bookmarks ()
  "Return the persisted bookmark list."
  (if (file-readable-p zhihu-reader-bookmarks-file)
      (condition-case nil
          (with-temp-buffer
            (insert-file-contents zhihu-reader-bookmarks-file)
            (read (current-buffer)))
        (error nil))
    nil))

(defun zhihu-reader--save-bookmarks (bookmarks)
  "Persist BOOKMARKS."
  (make-directory (file-name-directory zhihu-reader-bookmarks-file) t)
  (with-temp-file zhihu-reader-bookmarks-file
    (prin1 bookmarks (current-buffer))))

(defun zhihu-reader-bookmark ()
  "Bookmark the answer or article at point."
  (interactive)
  (pcase-let* ((`(,type ,id) (zhihu-reader--content-target))
               (title (string-trim (org-get-heading t t t t)))
               (url (pcase type
                      ("answers"
                       (format "https://www.zhihu.com/answer/%s" id))
                      (_ (format "https://zhuanlan.zhihu.com/p/%s" id))))
               (bookmarks (zhihu-reader--load-bookmarks))
               (entry (list :id id :type type :title title :url url
                            :saved (float-time))))
    (setq bookmarks (cons entry (seq-remove
                                 (lambda (item)
                                   (and (equal (plist-get item :id) id)
                                        (equal (plist-get item :type) type)))
                                 bookmarks)))
    (zhihu-reader--save-bookmarks bookmarks)
    (message "已加入书签")))

(defun zhihu-reader-bookmarks ()
  "Show saved reader bookmarks."
  (interactive)
  (let ((buffer (get-buffer-create "*Zhihu Bookmarks*"))
        (bookmarks (zhihu-reader--load-bookmarks)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (zhihu-reader-mode)
        (insert "#+TITLE: 知乎书签\n#+STARTUP: overview\n\n")
        (if (null bookmarks)
            (insert "还没有书签。\n")
          (dolist (item bookmarks)
            (insert (format "* [[%s][%s]]\n  %s\n"
                            (plist-get item :url)
                            (org-link-escape (plist-get item :title))
                            (format-time-string
                             "%Y-%m-%d %H:%M"
                             (seconds-to-time (or (plist-get item :saved) 0)))))))
        (goto-char (point-min))
        (setq buffer-read-only t)))
    (pop-to-buffer buffer)))

(defun zhihu-reader-mark-read ()
  "Mark the answer or article at point as read."
  (interactive)
  (let ((target (zhihu-reader--content-target)))
    (zhihu-reader--load-seen)
    (puthash (format "%s:%s"
                     (if (equal (car target) "answers") "answer" "article")
                     (cadr target))
             t zhihu-reader--seen)
    (zhihu-reader--save-seen)
    (message "已标记为已读")))

(defun zhihu-reader-mark-unread ()
  "Mark the answer or article at point as unread."
  (interactive)
  (let ((target (zhihu-reader--content-target)))
    (zhihu-reader--load-seen)
    (remhash (format "%s:%s"
                     (if (equal (car target) "answers") "answer" "article")
                     (cadr target))
             zhihu-reader--seen)
    (zhihu-reader--save-seen)
    (message "已标记为未读")))

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

(defun zhihu-reader-author (token &optional page buffer-name)
  "Open author TOKEN's answer list in BUFFER-NAME."
  (interactive
   (list (or (org-entry-get nil "ZHIHU-AUTHOR-TOKEN")
             (read-string "知乎作者 url_token: "))
         (prefix-numeric-value current-prefix-arg)))
  (let* ((token (or token (read-string "知乎作者 url_token: ")))
         (page (or page 1))
         (url (zhihu-reader--author-url token page))
         (title (format "作者 %s · 第 %d 页" token page))
         (view (list 'author token page buffer-name)))
    (if noninteractive
        (zhihu-reader--render title
                              (zhihu-reader--normalize-items
                               (zhihu-reader--fetch-json url))
                              view buffer-name)
      (zhihu-reader--async-list
       title view url buffer-name
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

(defun zhihu-reader-search (&optional query page force buffer-name)
  "Search Zhihu for QUERY, starting at PAGE.
With FORCE non-nil, bypass the page cache.  BUFFER-NAME selects a
dedicated buffer (used by internal link routing)."
  (interactive (list (read-string "搜索知乎: ")
                     (prefix-numeric-value current-prefix-arg)))
  (let* ((query (or query (read-string "搜索知乎: ")))
         (page (or page 1))
         (key (list 'search query page buffer-name))
         (url (zhihu-reader--search-url query page))
         (title (format "搜索 · %s · 第 %d 页" query page)))
    (if (and (not force) (zhihu-reader--show-cached key buffer-name))
        (message "使用缓存的搜索第 %d 页" page)
      (if noninteractive
          (let ((items (zhihu-reader--normalize-items
                        (zhihu-reader--fetch-json url))))
            (puthash key (cons title items) zhihu-reader--page-cache)
            (zhihu-reader--render title items key buffer-name)
            (zhihu-reader--restore-point key))
        (zhihu-reader--async-list
         title key url buffer-name
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
                                   (nth 2 zhihu-reader--view) t
                                   (nth 3 zhihu-reader--view)))
    ('question (zhihu-reader-question (nth 1 zhihu-reader--view)
                                       (nth 2 zhihu-reader--view)
                                       (nth 3 zhihu-reader--view) t))
    ('answer (zhihu-reader-answer (nth 1 zhihu-reader--view)))
    ('article (zhihu-reader-article (nth 1 zhihu-reader--view)))
    ('author (zhihu-reader-author (nth 1 zhihu-reader--view)
                                  (nth 2 zhihu-reader--view)
                                  (nth 3 zhihu-reader--view)))
    ('collection (zhihu-reader-collection (nth 1 zhihu-reader--view)
                                          (nth 2 zhihu-reader--view)
                                          (nth 3 zhihu-reader--view)))
    ('topic (zhihu-reader-topic (nth 1 zhihu-reader--view)
                                (nth 2 zhihu-reader--view)
                                (nth 3 zhihu-reader--view)))
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
                                     (1+ (or (nth 2 view) 1))
                                     nil (nth 3 view)))
      ('question (zhihu-reader-question (nth 1 view)
                                        (1+ (or (nth 2 view) 1))
                                        (nth 3 view)))
      ('collection
       (unless zhihu-reader--collection-has-next
         (user-error "已经是最后一页收藏夹"))
       (zhihu-reader-collection (nth 1 view)
                                (1+ (or (nth 2 view) 1))
                                (nth 3 view)))
      ('topic (zhihu-reader-topic (nth 1 view)
                                  (1+ (or (nth 2 view) 1))
                                  (nth 3 view)))
      ('following
       (let ((next zhihu-reader--following-next))
         (unless (and next (not (string-empty-p next)))
           (user-error "已经是最后一页关注动态"))
         (zhihu-reader-following next)))
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
                                     (max 1 (1- (or (nth 2 view) 1)))
                                     nil (nth 3 view)))
      ('question (zhihu-reader-question (nth 1 view)
                                         (max 1 (1- (or (nth 2 view) 1)))
                                         (nth 3 view)))
      ('collection (zhihu-reader-collection (nth 1 view)
                                            (max 1 (1- (or (nth 2 view) 1)))
                                            (nth 3 view)))
      ('topic (zhihu-reader-topic (nth 1 view)
                                  (max 1 (1- (or (nth 2 view) 1)))
                                  (nth 3 view)))
      ('following
       (let ((previous zhihu-reader--following-prev))
         (unless (and previous (not (string-empty-p previous)))
           (user-error "已经是第一页关注动态"))
         (zhihu-reader-following previous)))
      (_ (user-error "当前视图没有上一页")))))

(provide 'zhihu-reader-views)
;;; zhihu-reader-views.el ends here
