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

(defvar-local zhihu-reader--notifications-next nil)
(defvar-local zhihu-reader--notifications-prev nil)

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
        (let ((q (or (zhihu-reader--query-value query "q") ""))
              (type (or (zhihu-reader--query-value query "type") "general")))
          (cond
           ((string-empty-p q) (zhihu-reader-read-url url))
           ((equal type "people")
            (zhihu-reader-search-people q nil (or buffer-name "*Zhihu People Search*")))
           ((equal type "topic")
            (zhihu-reader-search-topics q nil (or buffer-name "*Zhihu Topic Search*")))
           (t (zhihu-reader-search q nil nil (or buffer-name "*Zhihu Search*"))))))
       ((string-match "/question/\\([0-9]+\\)/answer/\\([0-9]+\\)" path)
        (zhihu-reader-answer (match-string 2 path)))
       ((string-match "/answer/\\([0-9]+\\)" path)
        (zhihu-reader-answer (match-string 1 path)))
       ((string-match "/question/\\([0-9]+\\)" path)
        (zhihu-reader-question (match-string 1 path)))
       ((string-match "/p/\\([0-9]+\\)" path)
        (zhihu-reader-article (match-string 1 path)))
       ((string-match "/people/\\([^/?]+\\)/followers" path)
        (zhihu-reader-user-followers (match-string 1 path) nil "*Zhihu Followers*"))
       ((string-match "/people/\\([^/?]+\\)/following" path)
        (zhihu-reader-user-followees (match-string 1 path) nil "*Zhihu Following*"))
       ((string-match "/people/\\([^/?]+\\)" path)
        (zhihu-reader-user (match-string 1 path) "*Zhihu User*"))
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

(defun zhihu-reader--share-title ()
  "Return the top-level question/article title at point."
  (save-excursion
    (ignore-errors (org-back-to-heading t))
    (let ((title (ignore-errors (org-get-heading t t t t))))
      (while (ignore-errors (org-up-heading-safe))
        (setq title (org-get-heading t t t t)))
      title)))

(defun zhihu-reader-copy-share-link ()
  "Copy the canonical share link for the content at point."
  (interactive)
  (let* ((url (zhihu-reader--current-url))
         (title (and zhihu-reader-copy-link-include-title
                     (zhihu-reader--share-title))))
    (unless (and url (not (string-empty-p url)))
      (user-error "光标不在问题、回答、文章或评论标题上"))
    (let ((text (if (and title (not (string-empty-p title)))
                    (concat title "\n" url)
                  url)))
      (kill-new text)
    ;; In a graphical or XWayland session also write the system clipboard
    ;; explicitly; `kill-new' alone is not enough for every selection setup.
      (when (fboundp 'gui-set-selection)
        (ignore-errors (gui-set-selection 'CLIPBOARD text)))
      (message "已复制分享链接: %s" text))))

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

(defun zhihu-reader-create-collection (title &optional description public)
  "Create a collection titled TITLE."
  (interactive (list (read-string "收藏夹标题: ")
                     (read-string "收藏夹描述（可空）: ")
                     (yes-or-no-p "公开收藏夹？")))
  (unless zhihu-reader-enable-write
    (user-error "写操作已关闭；请设置 `zhihu-reader-enable-write' 为 t"))
  (when (string-empty-p (string-trim title))
    (user-error "收藏夹标题不能为空"))
  (let* ((body (list :title (string-trim title)
                     :description (or description "")
                     :is_public (if public t :json-false)))
         (resp (zhihu-reader--http-json
                "POST" "https://www.zhihu.com/api/v4/collections" body))
         (json (plist-get resp :json))
         (status (plist-get resp :status)))
    (unless (and (memq status '(200 201))
                 (equal (zhihu-reader--alist-get 'status json) 100))
      (error "创建收藏夹失败 (HTTP %s): %s" status
             (or (zhihu-reader--alist-get 'message json) (plist-get resp :body))))
    (message "收藏夹已创建: %s"
             (or (zhihu-reader--alist-get 'id (zhihu-reader--alist-get 'collection json))
                 ""))))

(defun zhihu-reader-delete-collection (id)
  "Delete collection ID after confirmation."
  (interactive (list (or (read-string "收藏夹 ID: ")
                         (user-error "收藏夹 ID 为空"))))
  (unless zhihu-reader-enable-write
    (user-error "写操作已关闭；请设置 `zhihu-reader-enable-write' 为 t"))
  (unless (yes-or-no-p (format "确认删除收藏夹 %s？" id))
    (user-error "已取消"))
  (let* ((resp (zhihu-reader--http-json
                "DELETE" (format "https://www.zhihu.com/api/v4/collections/%s" id) nil))
         (json (plist-get resp :json))
         (status (plist-get resp :status)))
    (unless (and (memq status '(200 201 202 204))
                 (not (eq (zhihu-reader--alist-get 'success json) :json-false)))
      (error "删除收藏夹失败 (HTTP %s): %s" status (plist-get resp :body)))
    (message "收藏夹已删除")))

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

(defun zhihu-reader--insert-notification-entry (entry)
  "Insert one notification ENTRY at point."
  (let* ((content (or (zhihu-reader--alist-get 'content entry) entry))
         (title (or (zhihu-reader--alist-get 'title content) "通知"))
         (text (or (zhihu-reader--alist-get 'text content) ""))
         (link (or (zhihu-reader--alist-get 'target_link content)
                   (zhihu-reader--alist-get 'target_link entry)))
         (created (or (zhihu-reader--alist-get 'created_str entry)
                      (zhihu-reader--alist-get 'created entry)))
         (read (zhihu-reader--alist-get 'is_read entry))
         (time (and created
                    (ignore-errors
                      (format-time-string "%Y-%m-%d %H:%M"
                                          (seconds-to-time
                                           (string-to-number (format "%s" created))))))))
    (insert (format "* %s%s\n"
                    (zhihu-reader--safe-title (format "%s" title))
                    (if read "" " · 未读")))
    (when (and text (not (string-empty-p text)))
      (insert "  " (replace-regexp-in-string "[\r\n]+" " " text) "\n"))
    (when (and link (not (string-empty-p link)))
      (insert "  " (org-link-make-string link "打开") "\n"))
    (when time (insert "  " time "\n"))
    (insert "\n")))

(defun zhihu-reader-notification-timeline (category &optional cursor)
  "Open notifications for CATEGORY, optionally at CURSOR."
  (interactive
   (list (completing-read "通知分类: "
                          '("comment" "like" "follow" "favlist_me" "invite")
                          nil t nil nil "comment")))
  (let* ((url (if (and cursor (stringp cursor)
                       (string-prefix-p "https://api.zhihu.com/notifications/" cursor))
                  cursor
                (if (equal category "invite")
                    "https://api.zhihu.com/notifications/v3/timeline/entry/invite?invite_with_time_slice=1&limit=20"
                  (format "https://api.zhihu.com/notifications/v3/timeline/entry/%s?limit=20" category))))
         (resp (zhihu-reader--http-json "GET" url nil nil))
         (json (plist-get resp :json))
         (status (plist-get resp :status))
         (buffer "*Zhihu Notifications*"))
    (unless (memq status '(200 201))
      (user-error "通知加载失败 (HTTP %s)" status))
    (let* ((data (append (or (zhihu-reader--alist-get 'data json) []) nil))
           (paging (zhihu-reader--alist-get 'paging json)))
      (with-current-buffer (get-buffer-create buffer)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (zhihu-reader-mode)
          (setq-local zhihu-reader--view (list 'notification-timeline category cursor))
          (setq-local zhihu-reader--notification-category category)
          (setq-local header-line-format
                      (format " 知乎通知 · %s  | TAB fold  RET open  C-c C-r 全部已读  ] next  [ prev  g refresh  q close"
                              category))
          (insert (format "#+TITLE: 知乎通知 · %s\n#+STARTUP: overview\n\n" category))
          (dolist (entry data) (zhihu-reader--insert-notification-entry entry))
          (if (null data) (insert "没有通知。\n"))
          (goto-char (point-min))
          (ignore-errors (org-overview))
          (setq-local zhihu-reader--notifications-next
                      (zhihu-reader--alist-get 'next paging))
          (setq-local zhihu-reader--notifications-prev
                      (zhihu-reader--alist-get 'previous paging))
          (setq buffer-read-only t)))
      (pop-to-buffer (get-buffer buffer))
      (message "通知 · %s：%d 条" category (length data)))))

(defun zhihu-reader-notifications-readall (&optional category)
  "Mark CATEGORY notifications as read."
  (interactive)
  (unless zhihu-reader-enable-write
    (user-error "写操作已关闭；请设置 `zhihu-reader-enable-write' 为 t"))
  (let ((category (or category zhihu-reader--notification-category
                      (completing-read "全部已读分类: "
                                       '("comment" "like" "follow" "favlist_me")
                                       nil t nil nil "comment"))))
    (unless (member category '("comment" "like" "follow" "favlist_me"))
      (user-error "不支持该通知分类: %s" category))
    (unless (yes-or-no-p (format "确认把 %s 通知全部标为已读？" category))
      (user-error "已取消"))
    (let* ((url (format "https://api.zhihu.com/notifications/v3/timeline/entry/%s/actions/readall" category))
           (resp (zhihu-reader--http-json "POST" url nil nil))
           (status (plist-get resp :status)))
      (unless (memq status '(200 201 202 204))
        (error "标记已读失败 (HTTP %s): %s" status (plist-get resp :body)))
      (message "%s 通知已全部标为已读" category))))

(defvar-local zhihu-reader--notification-category nil)

(defun zhihu-reader-user (token &optional buffer-name)
  "Open Zhihu user profile TOKEN in BUFFER-NAME."
  (interactive
   (list (or (org-entry-get nil "ZHIHU-AUTHOR-TOKEN")
             (read-string "知乎用户 url_token: "))))
  (let* ((token (or token (read-string "知乎用户 url_token: ")))
         (url (format "https://www.zhihu.com/api/v4/members/%s?include=name,headline,description,follower_count,following_count,answer_count,articles_count,is_following,url_token"
                      token))
         (resp (zhihu-reader--http-json "GET" url nil nil))
         (json (plist-get resp :json))
         (status (plist-get resp :status))
         (buffer (or buffer-name "*Zhihu User*")))
    (unless (memq status '(200 201))
      (user-error "用户主页加载失败 (HTTP %s)" status))
    (let ((name (or (zhihu-reader--alist-get 'name json) token)))
      (with-current-buffer (get-buffer-create buffer)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (zhihu-reader-mode)
          (setq-local zhihu-reader--view (list 'user token buffer))
          (setq-local header-line-format
                      (format " 知乎用户 %s  | TAB fold  C-c f 关注  g 刷新  q 关闭" name))
          (insert (format "#+TITLE: 用户 · %s\n#+STARTUP: overview\n\n" name))
          (zhihu-reader--heading
           1 name
           (list (cons "ZHIHU-AUTHOR-TOKEN" token)
                 (cons "ZHIHU-FOLLOWING"
                       (if (zhihu-reader--alist-get 'is_following json) "t" "nil"))))
          (zhihu-reader--body
           (mapconcat
            #'identity
            (delq nil
                  (list (zhihu-reader--alist-get 'headline json)
                        (zhihu-reader--alist-get 'description json)
                        (format "关注 %s · 粉丝 %s · 回答 %s · 文章 %s"
                                (or (zhihu-reader--alist-get 'following_count json) 0)
                                (or (zhihu-reader--alist-get 'follower_count json) 0)
                                (or (zhihu-reader--alist-get 'answer_count json) 0)
                                (or (zhihu-reader--alist-get 'articles_count json) 0))))
            "\n\n"))
          (insert (format "\n[[https://www.zhihu.com/people/%s/answers][查看回答]]\n" token))
          (goto-char (point-min))
          (ignore-errors (org-overview))
          (zhihu-reader--hide-drawers)
          (setq buffer-read-only t)))
      (pop-to-buffer (get-buffer buffer))
      (message "用户主页：%s" name))))

(defun zhihu-reader-notifications (&optional cursor)
  "Show Zhihu notifications, optionally at CURSOR URL."
  (interactive)
  (let* ((url (if (and cursor (stringp cursor)
                       (string-prefix-p "https://api.zhihu.com/notifications/" cursor))
                  cursor
                "https://api.zhihu.com/notifications/v3/message/v3"))
         (resp (zhihu-reader--http-json "GET" url nil nil))
         (json (plist-get resp :json))
         (status (plist-get resp :status))
         (buffer "*Zhihu Notifications*"))
    (unless (memq status '(200 201))
      (user-error "通知加载失败 (HTTP %s)" status))
    (let* ((paging (zhihu-reader--alist-get 'paging json))
           (data (append (or (zhihu-reader--alist-get 'data json) []) nil))
           (unread (zhihu-reader--alist-get 'unread json)))
      (with-current-buffer (get-buffer-create buffer)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (zhihu-reader-mode)
          (setq-local zhihu-reader--view (list 'notifications cursor))
          (setq-local header-line-format
                      (format " 知乎通知  | TAB fold  RET open  ] next  [ prev  g refresh  q close"))
          (insert (format "#+TITLE: 知乎通知%s\n#+STARTUP: overview\n\n"
                          (if unread
                              (format " · 未读 %s"
                                      (or (zhihu-reader--alist-get 'message unread) "?"))
                            "")))
          (dolist (entry data)
            (let* ((content (or (zhihu-reader--alist-get 'content entry) entry))
                   (title (or (zhihu-reader--alist-get 'title content) "通知"))
                   (text (or (zhihu-reader--alist-get 'text content) ""))
                   (link (or (zhihu-reader--alist-get 'target_link content)
                             (zhihu-reader--alist-get 'target_link entry)))
                   (created (or (zhihu-reader--alist-get 'created_str entry)
                                (zhihu-reader--alist-get 'created entry)))
                   (read (zhihu-reader--alist-get 'is_read entry))
                   (time (and created
                              (ignore-errors
                                (format-time-string
                                 "%Y-%m-%d %H:%M"
                                 (seconds-to-time (string-to-number (format "%s" created))))))))
                (insert (format "* %s%s\n"
                                (zhihu-reader--safe-title (format "%s" title))
                                (if read "" " · 未读")))
                (when (and text (not (string-empty-p text)))
                  (insert "  " (replace-regexp-in-string "[\r\n]+" " " text) "\n"))
                (when (and link (not (string-empty-p link)))
                  (insert "  " (org-link-make-string link "打开") "\n"))
                (when time (insert "  " time "\n"))
                (insert "\n")))
          (if (null data) (insert "没有通知。\n"))
          (goto-char (point-min))
          (ignore-errors (org-overview))
          (setq-local zhihu-reader--notifications-next
                      (zhihu-reader--alist-get 'next paging))
          (setq-local zhihu-reader--notifications-prev
                      (zhihu-reader--alist-get 'previous paging))
          (setq buffer-read-only t)))
      (pop-to-buffer (get-buffer buffer))
      (message "通知：%d 条" (length data)))))

(defun zhihu-reader--render-user-list (title users view buffer)
  "Render USER records in BUFFER with TITLE and VIEW."
  (with-current-buffer (get-buffer-create buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (zhihu-reader-mode)
      (setq-local zhihu-reader--view view)
      (setq-local header-line-format
                  (format " 知乎用户列表 %s  | RET 打开  ] next  [ prev  g refresh  q quit" title))
      (insert (format "#+TITLE: %s\n#+STARTUP: overview\n\n" title))
      (dolist (user users)
        (let ((token (or (zhihu-reader--alist-get 'url_token user)
                         (zhihu-reader--alist-get 'id user)))
              (name (or (zhihu-reader--alist-get 'name user) "匿名"))
              (headline (or (zhihu-reader--alist-get 'headline user) "")))
          (insert (format "* [[https://www.zhihu.com/people/%s][%s]]\n"
                          token name))
          (when (not (string-empty-p headline))
            (insert "  " (replace-regexp-in-string "[\r\n]+" " " headline) "\n"))
          (insert "\n")))
      (if (null users) (insert "没有返回用户。\n"))
      (goto-char (point-min))
      (ignore-errors (org-overview))
      (setq buffer-read-only t)))
  (pop-to-buffer buffer))

(defun zhihu-reader-user-followers (token &optional page buffer-name)
  "Open TOKEN's followers list."
  (interactive (list (or (org-entry-get nil "ZHIHU-AUTHOR-TOKEN")
                         (read-string "知乎用户 url_token: "))
                     (prefix-numeric-value current-prefix-arg)))
  (zhihu-reader--user-relations token "followers" page buffer-name))

(defun zhihu-reader-user-followees (token &optional page buffer-name)
  "Open TOKEN's following-users list."
  (interactive (list (or (org-entry-get nil "ZHIHU-AUTHOR-TOKEN")
                         (read-string "知乎用户 url_token: "))
                     (prefix-numeric-value current-prefix-arg)))
  (zhihu-reader--user-relations token "followees" page buffer-name))

(defun zhihu-reader--user-relations (token kind &optional page buffer-name)
  "Fetch TOKEN's KIND relations (followers/followees)."
  (let* ((token (or token (read-string "知乎用户 url_token: ")))
         (page (or page 1))
         (buffer (or buffer-name
                     (if (equal kind "followers")
                         "*Zhihu Followers*" "*Zhihu Following*")))
         (url (format "https://www.zhihu.com/api/v4/members/%s/%s?limit=20&offset=%d"
                      token kind (* 20 (1- page))))
         (resp (zhihu-reader--http-json "GET" url nil nil))
         (json (plist-get resp :json))
         (status (plist-get resp :status)))
    (unless (memq status '(200 201))
      (user-error "用户列表加载失败 (HTTP %s)" status))
    (let ((users (append (or (zhihu-reader--alist-get 'data json) []) nil))
          (title (format "%s · %s · 第 %d 页"
                         (if (equal kind "followers") "粉丝" "关注")
                         token page)))
      (zhihu-reader--render-user-list
       title users (list 'user-relations token kind page buffer) buffer))))

(defun zhihu-reader-user-articles (token &optional page buffer-name)
  "Open TOKEN's articles."
  (interactive (list (or (org-entry-get nil "ZHIHU-AUTHOR-TOKEN")
                         (read-string "知乎用户 url_token: "))
                     (prefix-numeric-value current-prefix-arg)))
  (let* ((token (or token (read-string "知乎用户 url_token: ")))
         (page (or page 1))
         (buffer (or buffer-name "*Zhihu User Articles*"))
         (url (format "https://www.zhihu.com/api/v4/members/%s/articles?limit=20&offset=%d"
                      token (* 20 (1- page))))
         (resp (zhihu-reader--http-json "GET" url nil nil))
         (json (plist-get resp :json)))
    (unless (memq (plist-get resp :status) '(200 201))
      (user-error "文章列表加载失败 (HTTP %s)" (plist-get resp :status)))
    (zhihu-reader--render
     (format "用户文章 · %s · 第 %d 页" token page)
     (zhihu-reader--normalize-items (list (cons 'data (or (zhihu-reader--alist-get 'data json) []))))
     (list 'user-articles token page buffer) buffer)))

(defun zhihu-reader-user-pins (token &optional page buffer-name)
  "Open TOKEN's pins."
  (interactive (list (or (org-entry-get nil "ZHIHU-AUTHOR-TOKEN")
                         (read-string "知乎用户 url_token: "))
                     (prefix-numeric-value current-prefix-arg)))
  (let* ((token (or token (read-string "知乎用户 url_token: ")))
         (page (or page 1))
         (buffer (or buffer-name "*Zhihu User Pins*"))
         (url (format "https://www.zhihu.com/api/v4/members/%s/pins?limit=20&offset=%d"
                      token (* 20 (1- page))))
         (resp (zhihu-reader--http-json "GET" url nil nil))
         (json (plist-get resp :json)))
    (unless (memq (plist-get resp :status) '(200 201))
      (user-error "想法列表加载失败 (HTTP %s)" (plist-get resp :status)))
    (zhihu-reader--render
     (format "用户想法 · %s · 第 %d 页" token page)
     (zhihu-reader--normalize-items (list (cons 'data (or (zhihu-reader--alist-get 'data json) []))))
     (list 'user-pins token page buffer) buffer)))

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
  (let* ((url (format zhihu-reader-search-url (url-hexify-string query)))
         (url (replace-regexp-in-string
               "offset=0" (format "offset=%d" (* 20 (1- (or page 1)))) url))
         (url (if (and zhihu-reader-search-sort
                       (not (string-empty-p zhihu-reader-search-sort)))
                  (concat url "&sort=" (url-hexify-string zhihu-reader-search-sort))
                url)))
    url))

(defun zhihu-reader-search-sort ()
  "Change search sort and refresh the current search if applicable."
  (interactive)
  (setq zhihu-reader-search-sort
        (completing-read "搜索排序: "
                         '("" "upvoted_count" "created_time")
                         nil nil nil nil
                         (or zhihu-reader-search-sort "")))
  (setq zhihu-reader-search-sort
        (if (string-empty-p zhihu-reader-search-sort)
            nil zhihu-reader-search-sort))
  (if (eq (car-safe zhihu-reader--view) 'search)
      (zhihu-reader-refresh)
    (message "搜索排序: %s" (or zhihu-reader-search-sort "默认"))))

(defun zhihu-reader--search-type-url (type query page)
  "Build a people/topic search URL."
  (format "https://www.zhihu.com/api/v4/search_v3?t=%s&q=%s&correction=1&offset=%d&limit=20"
          type (url-hexify-string query) (* 20 (1- (or page 1)))))

(defun zhihu-reader--search-objects (json)
  "Extract search result objects from JSON."
  (let ((data (append (or (zhihu-reader--alist-get 'data json) []) nil)))
    (delq nil (mapcar (lambda (entry)
                        (or (zhihu-reader--alist-get 'object entry)
                            entry))
                      data))))

(defun zhihu-reader--render-topic-list (title topics view buffer)
  "Render topic search results."
  (with-current-buffer (get-buffer-create buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (zhihu-reader-mode)
      (setq-local zhihu-reader--view view)
      (insert (format "#+TITLE: %s\n#+STARTUP: overview\n\n" title))
      (dolist (topic topics)
        (let ((id (or (zhihu-reader--alist-get 'id topic)
                      (zhihu-reader--alist-get 'topic_id topic)))
              (name (or (zhihu-reader--alist-get 'name topic)
                        (zhihu-reader--alist-get 'title topic) "话题"))
              (desc (or (zhihu-reader--alist-get 'excerpt topic)
                        (zhihu-reader--alist-get 'description topic) "")))
          (insert (format "* [[https://www.zhihu.com/topic/%s][%s]]\n"
                          id name))
          (when (not (string-empty-p desc))
            (insert "  " (replace-regexp-in-string "[\r\n]+" " " desc) "\n"))
          (insert "\n")))
      (if (null topics) (insert "没有返回话题。\n"))
      (goto-char (point-min))
      (ignore-errors (org-overview))
      (setq buffer-read-only t)))
  (pop-to-buffer buffer))

(defun zhihu-reader-search-people (query &optional page buffer-name)
  "Search people named QUERY."
  (interactive (list (read-string "搜索用户: ")
                     (prefix-numeric-value current-prefix-arg)))
  (let* ((query (or query (read-string "搜索用户: ")))
         (page (or page 1))
         (buffer (or buffer-name "*Zhihu People Search*"))
         (url (zhihu-reader--search-type-url "people" query page))
         (json (zhihu-reader--fetch-json url)))
    (zhihu-reader--render-user-list
     (format "用户搜索 · %s · 第 %d 页" query page)
     (zhihu-reader--search-objects json)
     (list 'search-people query page buffer) buffer)))

(defun zhihu-reader-search-topics (query &optional page buffer-name)
  "Search topics named QUERY."
  (interactive (list (read-string "搜索话题: ")
                     (prefix-numeric-value current-prefix-arg)))
  (let* ((query (or query (read-string "搜索话题: ")))
         (page (or page 1))
         (buffer (or buffer-name "*Zhihu Topic Search*"))
         (url (zhihu-reader--search-type-url "topic" query page))
         (json (zhihu-reader--fetch-json url)))
    (zhihu-reader--render-topic-list
     (format "话题搜索 · %s · 第 %d 页" query page)
     (zhihu-reader--search-objects json)
     (list 'search-topics query page buffer) buffer)))

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
    ('search-people (zhihu-reader-search-people (nth 1 zhihu-reader--view)
                                                 (nth 2 zhihu-reader--view)
                                                 (nth 3 zhihu-reader--view)))
    ('search-topics (zhihu-reader-search-topics (nth 1 zhihu-reader--view)
                                                 (nth 2 zhihu-reader--view)
                                                 (nth 3 zhihu-reader--view)))
    ('question (zhihu-reader-question (nth 1 zhihu-reader--view)
                                       (nth 2 zhihu-reader--view)
                                       (nth 3 zhihu-reader--view) t))
    ('answer (zhihu-reader-answer (nth 1 zhihu-reader--view)))
    ('article (zhihu-reader-article (nth 1 zhihu-reader--view)))
    ('user (zhihu-reader-user (nth 1 zhihu-reader--view)
                              (nth 2 zhihu-reader--view)))
    ('user-articles
     (zhihu-reader-user-articles (nth 1 zhihu-reader--view)
                                 (nth 2 zhihu-reader--view)
                                 (nth 3 zhihu-reader--view)))
    ('user-pins
     (zhihu-reader-user-pins (nth 1 zhihu-reader--view)
                             (nth 2 zhihu-reader--view)
                             (nth 3 zhihu-reader--view)))
    ('user-relations
     (zhihu-reader--user-relations (nth 1 zhihu-reader--view)
                                   (nth 2 zhihu-reader--view)
                                   (nth 3 zhihu-reader--view)
                                   (nth 4 zhihu-reader--view)))
    ('notifications (zhihu-reader-notifications (nth 1 zhihu-reader--view)))
    ('notification-timeline
     (zhihu-reader-notification-timeline (nth 1 zhihu-reader--view)
                                         (nth 2 zhihu-reader--view)))
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
      ('search-people (zhihu-reader-search-people (nth 1 view)
                                                  (1+ (or (nth 2 view) 1))
                                                  (nth 3 view)))
      ('search-topics (zhihu-reader-search-topics (nth 1 view)
                                                  (1+ (or (nth 2 view) 1))
                                                  (nth 3 view)))
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
      ('notification-timeline
       (let ((next zhihu-reader--notifications-next))
         (unless (and next (not (string-empty-p next)))
           (user-error "已经是最后一页通知"))
         (zhihu-reader-notification-timeline
          (nth 1 view) next)))
      ('notifications
       (let ((next zhihu-reader--notifications-next))
         (unless (and next (not (string-empty-p next)))
           (user-error "已经是最后一页通知"))
         (zhihu-reader-notifications next)))
      ('user-articles
       (zhihu-reader-user-articles (nth 1 view) (1+ (or (nth 2 view) 1)) (nth 3 view)))
      ('user-pins
       (zhihu-reader-user-pins (nth 1 view) (1+ (or (nth 2 view) 1)) (nth 3 view)))
      ('user-relations
       (zhihu-reader--user-relations (nth 1 view) (nth 2 view)
                                     (1+ (or (nth 3 view) 1))
                                     (nth 4 view)))
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
      ('search-people (zhihu-reader-search-people (nth 1 view)
                                                  (max 1 (1- (or (nth 2 view) 1)))
                                                  (nth 3 view)))
      ('search-topics (zhihu-reader-search-topics (nth 1 view)
                                                  (max 1 (1- (or (nth 2 view) 1)))
                                                  (nth 3 view)))
      ('question (zhihu-reader-question (nth 1 view)
                                         (max 1 (1- (or (nth 2 view) 1)))
                                         (nth 3 view)))
      ('collection (zhihu-reader-collection (nth 1 view)
                                            (max 1 (1- (or (nth 2 view) 1)))
                                            (nth 3 view)))
      ('topic (zhihu-reader-topic (nth 1 view)
                                  (max 1 (1- (or (nth 2 view) 1)))
                                  (nth 3 view)))
      ('notification-timeline
       (let ((previous zhihu-reader--notifications-prev))
         (unless (and previous (not (string-empty-p previous)))
           (user-error "已经是第一页通知"))
         (zhihu-reader-notification-timeline
          (nth 1 view) previous)))
      ('notifications
       (let ((previous zhihu-reader--notifications-prev))
         (unless (and previous (not (string-empty-p previous)))
           (user-error "已经是第一页通知"))
         (zhihu-reader-notifications previous)))
      ('user-articles
       (zhihu-reader-user-articles (nth 1 view) (max 1 (1- (or (nth 2 view) 1))) (nth 3 view)))
      ('user-pins
       (zhihu-reader-user-pins (nth 1 view) (max 1 (1- (or (nth 2 view) 1))) (nth 3 view)))
      ('user-relations
       (zhihu-reader--user-relations (nth 1 view) (nth 2 view)
                                     (max 1 (1- (or (nth 3 view) 1)))
                                     (nth 4 view)))
      ('following
       (let ((previous zhihu-reader--following-prev))
         (unless (and previous (not (string-empty-p previous)))
           (user-error "已经是第一页关注动态"))
         (zhihu-reader-following previous)))
      (_ (user-error "当前视图没有上一页")))))

(provide 'zhihu-reader-views)
;;; zhihu-reader-views.el ends here
