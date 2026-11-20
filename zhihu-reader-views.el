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

(defvar-local zhihu-reader--inbox-next nil)

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
         (buffer (or buffer-name "*Zhihu Article*"))
         (view (list 'article id)))
    (if noninteractive
        (let* ((resp (zhihu-reader--http-json "GET" url nil nil))
               (json (plist-get resp :json)))
          (unless (memq (plist-get resp :status) '(200 201))
            (user-error "文章加载失败 (HTTP %s)" (plist-get resp :status)))
          (let ((item (zhihu-reader--normalize
                       (list (cons 'target json) (cons 'type "article")))))
            (if item
                (zhihu-reader--render
                 (format "文章 · %s" (or (plist-get item :title) id))
                 (list item) view buffer)
              (user-error "文章没有可用内容"))))
      (zhihu-reader--async-list
       (format "文章 %s" id) view url buffer
       (lambda (json)
         (let ((item (zhihu-reader--normalize
                      (list (cons 'target json) (cons 'type "article")))))
           (unless item (error "文章没有可用内容"))
           (cons (format "文章 · %s" (or (plist-get item :title) id))
                 (list item))))
       nil))))

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
       ((string-match "/people/\\([^/?]+\\)/answers" path)
        (zhihu-reader-author (match-string 1 path) nil "*Zhihu Author*"))
       ((string-match "/people/\\([^/?]+\\)/articles" path)
        (zhihu-reader-user-articles (match-string 1 path) nil "*Zhihu User Articles*"))
       ((string-match "/people/\\([^/?]+\\)/pins" path)
        (zhihu-reader-user-pins (match-string 1 path) nil "*Zhihu User Pins*"))
       ((string-match "/people/\\([^/?]+\\)/columns" path)
        (zhihu-reader-user-columns (match-string 1 path) nil "*Zhihu User Columns*"))
       ((string-match "/people/\\([^/?]+\\)/followers" path)
        (zhihu-reader-user-followers (match-string 1 path) nil "*Zhihu Followers*"))
       ((string-match "/people/\\([^/?]+\\)/following" path)
        (zhihu-reader-user-followees (match-string 1 path) nil "*Zhihu Following*"))
       ((string-match "/people/\\([^/?]+\\)" path)
        (zhihu-reader-user (match-string 1 path) "*Zhihu User*"))
       ((string-match "/topic/\\([0-9]+\\)" path)
        (zhihu-reader-topic (match-string 1 path) nil "*Zhihu Topic*"))
       ((string-match "/inbox" path) (zhihu-reader-inbox))
       ((string-match "/chat/\\([^/?]+\\)" path)
        (zhihu-reader-chat (match-string 1 path)))
       ((string-match "/pin/\\([0-9]+\\)" path)
        (zhihu-reader-pin (match-string 1 path) "*Zhihu Pin*"))
       ((string-match "/column/\\([^/?]+\\)" path)
        (zhihu-reader-column (match-string 1 path) nil "*Zhihu Column*"))
       ((string-match "/collection/\\([0-9]+\\)" path)
        (zhihu-reader-collection (match-string 1 path) nil "*Zhihu Collection*"))
       ((string= path "/hot")
        (zhihu-reader-hot))
       ((or (string= path "/") (string-match-p "/follow" path))
        (zhihu-reader-following))
       (t (zhihu-reader-read-url url))))))

(defun zhihu-reader-open-author ()
  "Open the Zhihu profile of the author at point."
  (interactive)
  (let ((token (or (org-entry-get nil "ZHIHU-AUTHOR-TOKEN" t)
                   (org-entry-get nil "ZHIHU-AUTHOR-TOKEN"))))
    (unless token
      (user-error "当前内容没有作者信息"))
    (zhihu-reader-user token "*Zhihu User*")))

(defun zhihu-reader-open-comment ()
  "Open the comment/reply detail at point in a separate buffer."
  (interactive)
  (let ((target (zhihu-reader--comment-target)))
    (zhihu-reader--open-comments-buffer target)))

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
     ((or (org-entry-get nil "ZHIHU-COMMENT")
          (org-entry-get nil "ZHIHU-COMMENTS"))
      (zhihu-reader-open-comment))
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

(defun zhihu-reader-inbox (&optional after-id)
  "Open Zhihu private-message inbox."
  (interactive)
  (let* ((url (if after-id
                  (format "https://www.zhihu.com/api/v4/inbox?limit=20&after_id=%s" after-id)
                "https://www.zhihu.com/api/v4/inbox?limit=20"))
         (resp (zhihu-reader--http-json "GET" url nil nil))
         (json (plist-get resp :json))
         (status (plist-get resp :status))
         (buffer "*Zhihu Inbox*"))
    (unless (memq status '(200 201))
      (user-error "私信列表加载失败 (HTTP %s)" status))
    (let* ((data (append (or (zhihu-reader--alist-get 'data json) []) nil))
           (unread (or (zhihu-reader--alist-get 'new_count json) 0))
           (paging (zhihu-reader--alist-get 'paging json)))
      (setq zhihu-reader--unread-total unread)
      (with-current-buffer (get-buffer-create buffer)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (zhihu-reader-mode)
          (setq-local zhihu-reader--view (list 'inbox after-id))
          (setq-local zhihu-reader--inbox-next
                      (zhihu-reader--alist-get 'next paging))
          (setq-local header-line-format
                      (format " 知乎私信 · 未读 %s  | RET 打开  g 刷新  ] 下一页  q 关闭" unread))
          (insert (format "#+TITLE: 知乎私信 · 未读 %s\n#+STARTUP: overview\n\n" unread))
          (dolist (thread data)
            (let* ((participant (zhihu-reader--alist-get 'participant thread))
                   (name (or (zhihu-reader--alist-get 'name participant) "未知"))
                   (pid (or (zhihu-reader--alist-get 'id participant)
                            (zhihu-reader--alist-get 'url_token participant)))
                   (snippet (or (zhihu-reader--alist-get 'snippet thread) ""))
                   (unread-count (or (zhihu-reader--alist-get 'unread_count thread) 0))
                   (time (zhihu-reader--alist-get 'updated_time thread)))
              (insert (format "* %s%s\n  %s\n"
                              (org-link-make-string
                               (format "https://www.zhihu.com/chat/%s" pid)
                               (zhihu-reader--safe-title name))
                              (if (> (or unread-count 0) 0)
                                  (format " · 未读 %s" unread-count) "")
                              (replace-regexp-in-string "[\r\n]+" " " snippet)))
              (when time
                (insert "  " (format-time-string "%Y-%m-%d %H:%M"
                                                  (seconds-to-time time))))
              (insert "\n\n")))
          (if (null data) (insert "没有私信。\n"))
          (goto-char (point-min))
          (ignore-errors (org-overview))
          (setq buffer-read-only t))
        (pop-to-buffer (get-buffer buffer))))))

(defun zhihu-reader--chat-image-url (message)
  "Return an image URL for a chat MESSAGE, including private message id."
  (let* ((image (zhihu-reader--alist-get 'image message))
         (url (and image (zhihu-reader--alist-get 'url image)))
         (id (zhihu-reader--alist-get 'id message)))
    (when (and url id (string-match-p "pic-private\.zhihu\.com" url)
               (not (string-match-p "message_id=" url)))
      (setq url (concat url (if (string-match-p "?" url) "&" "?")
                        "message_id=" (url-hexify-string id))))
    url))

(defun zhihu-reader-chat (peer-id)
  "Open private conversation with PEER-ID."
  (interactive "s知乎用户 _id: ")
  (let* ((url (format "https://www.zhihu.com/api/v4/chat?sender_id=%s" peer-id))
         (resp (zhihu-reader--http-json "GET" url nil nil))
         (json (plist-get resp :json))
         (status (plist-get resp :status))
         (data (zhihu-reader--alist-get 'data json))
         (messages (append (or (zhihu-reader--alist-get 'messages data) []) nil))
         (buffer "*Zhihu Chat*"))
    (unless (memq status '(200 201))
      (user-error "私信加载失败 (HTTP %s)" status))
    (with-current-buffer (get-buffer-create buffer)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (zhihu-reader-mode)
        (setq-local zhihu-reader--view (list 'chat peer-id))
        (setq-local header-line-format
                    (format " 知乎私信  | C-c C-c 发送  g 刷新  q 关闭"))
        (insert "#+TITLE: 知乎私信\n#+STARTUP: overview\n\n")
        (dolist (message (reverse messages))
          (let* ((ct (zhihu-reader--alist-get 'content_type message))
                 (text (or (zhihu-reader--alist-get 'text message) ""))
                 (image (and (equal ct 1) (zhihu-reader--chat-image-url message)))
                 (images (delq nil (append (and image (list image))
                                           (and (not (equal ct 1))
                                                (zhihu-reader--extract-images text)))))
                 (content (if (equal ct 1) "" (zhihu-reader--html-to-org text)))
                 (sent (equal (zhihu-reader--alist-get 'user_type message) "sender"))
                 (sender (if sent "我" "对方"))
                 (time (zhihu-reader--alist-get 'created_time message)))
            (insert (format "* %s%s\n"
                            sender
                            (if time
                                (concat " · " (format-time-string
                                               "%Y-%m-%d %H:%M"
                                               (seconds-to-time time)))
                              "")))
            (when (not (string-empty-p content)) (insert content "\n"))
            (when images
              (zhihu-reader--insert-images images))
            (insert "\n")))
        (if (null messages) (insert "还没有消息。\n"))
        (goto-char (point-min))
        (when (and zhihu-reader-render-images (display-graphic-p))
          (zhihu-reader--process-buffer-images))
        (ignore-errors (org-overview))
        (setq buffer-read-only t))
      (pop-to-buffer (get-buffer buffer)))))

(defun zhihu-reader-chat-send (text)
  "Send TEXT to the peer in the current chat buffer."
  (interactive (list (read-string "发送私信: ")))
  (unless zhihu-reader-enable-write
    (user-error "写操作已关闭；请设置 `zhihu-reader-enable-write' 为 t"))
  (unless (eq (car-safe zhihu-reader--view) 'chat)
    (user-error "当前不是私信会话"))
  (let* ((peer (nth 1 zhihu-reader--view))
         (resp (zhihu-reader--http-json
                "POST" "https://www.zhihu.com/api/v4/chat"
                (list :content_type 0 :text text :receiver_id peer) nil))
         (status (plist-get resp :status)))
    (unless (memq status '(200 201 202))
      (error "私信发送失败 (HTTP %s): %s" status (plist-get resp :body)))
    (message "私信已发送")
    (zhihu-reader-chat peer)))

(defun zhihu-reader--render-collections (json buffer token)
  "Render collection list JSON in BUFFER."
  (let ((data (append (or (zhihu-reader--alist-get 'data json) []) nil)))
    (with-current-buffer (get-buffer-create buffer)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (zhihu-reader-mode)
        (setq-local zhihu-reader--view (list 'collections token))
        (insert (format "#+TITLE: 收藏夹 · %s\n#+STARTUP: overview\n\n" token))
        (dolist (collection data)
          (let ((id (zhihu-reader--alist-get 'id collection))
                (title (or (zhihu-reader--alist-get 'title collection) "收藏夹"))
                (description (or (zhihu-reader--alist-get 'description collection) ""))
                (count (or (zhihu-reader--alist-get 'answer_count collection)
                           (zhihu-reader--alist-get 'item_count collection) 0)))
            (insert (format "* %s\n  %s\n  %s 条\n\n"
                            (org-link-make-string
                             (format "https://www.zhihu.com/collection/%s" id)
                             (zhihu-reader--safe-title title))
                            (replace-regexp-in-string "[\r\n]+" " " description)
                            count))))
        (if (null data) (insert "没有收藏夹。\n"))
        (goto-char (point-min))
        (ignore-errors (org-overview))
        (setq buffer-read-only t)))
    (pop-to-buffer (get-buffer-create buffer))))

(defun zhihu-reader-collections (token)
  "List collections belonging to TOKEN."
  (interactive (list (or (org-entry-get nil "ZHIHU-AUTHOR-TOKEN")
                         (read-string "知乎用户 url_token: "))))
  (let* ((token (or token (read-string "知乎用户 url_token: ")))
         (buffer "*Zhihu Collections*")
         (url (format "https://www.zhihu.com/api/v4/members/%s/favlists?limit=20&offset=0&include=data%%5B*%%5D.answer_count%%2Carticles_count%%2Cfollowers" token)))
    (if noninteractive
        (zhihu-reader--render-collections
         (plist-get (zhihu-reader--http-json "GET" url nil nil) :json) buffer token)
      (zhihu-reader--async-custom
       "收藏夹" (list 'collections token) url buffer
       (lambda (json) (zhihu-reader--render-collections json buffer token))))))

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

(defun zhihu-reader-remove-from-collection ()
  "Remove the answer/article at point from the current collection."
  (interactive)
  (unless zhihu-reader-enable-write
    (user-error "写操作已关闭；请设置 `zhihu-reader-enable-write' 为 t"))
  (let ((view zhihu-reader--view))
    (unless (eq (car-safe view) 'collection)
      (user-error "当前不是收藏夹视图"))
    (let* ((collection (nth 1 view))
           (target (zhihu-reader--content-target))
           (type (if (equal (car target) "answers") "answer" "article"))
           (id (cadr target)))
      (unless (yes-or-no-p "确认从收藏夹移除当前内容？")
        (user-error "已取消"))
      (let* ((resp (zhihu-reader--http-form
                    "PUT"
                    (format "https://api.zhihu.com/collections/contents/%s/%s" type id)
                    (list (cons "remove_collections" collection))))
             (status (plist-get resp :status)))
        (unless (memq status '(200 201 202 204))
          (error "移除收藏失败 (HTTP %s): %s" status (plist-get resp :body)))
        (save-excursion
          (org-back-to-heading t)
          (let ((beg (point))
                (end (save-excursion (org-end-of-subtree t t) (point))))
            (let ((inhibit-read-only t))
              (delete-region beg end))))
        (message "已从收藏夹移除")))))

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

(defun zhihu-reader--load-entry-file (file)
  "Load a plist list from FILE."
  (if (file-readable-p file)
      (condition-case nil
          (with-temp-buffer
            (insert-file-contents file)
            (read (current-buffer)))
        (error nil))
    nil))

(defun zhihu-reader--save-entry-file (file entries)
  "Persist ENTRIES to FILE."
  (make-directory (file-name-directory file) t)
  (with-temp-file file
    (prin1 entries (current-buffer))))

(defun zhihu-reader--record-history (kind id title url)
  "Record a read item in the history."
  (when (and url (not (string-empty-p url)))
    (let* ((file zhihu-reader-history-file)
           (entries (zhihu-reader--load-entry-file file))
           (entry (list :kind kind :id id :title (or title "")
                        :url url :time (float-time)))
           (entries (cons entry
                          (seq-remove (lambda (item)
                                        (equal (plist-get item :url) url))
                                      entries))))
      (setq entries (seq-take entries 300))
      (zhihu-reader--save-entry-file file entries))))

(defun zhihu-reader--render-link-list (title entries view buffer)
  "Render ENTRIES as a simple Org link list in BUFFER."
  (with-current-buffer (get-buffer-create buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (zhihu-reader-mode)
      (setq-local zhihu-reader--view view)
      (setq-local header-line-format
                  (format " %s  | RET 打开  g 刷新  q 关闭" title))
      (insert (format "#+TITLE: %s\n#+STARTUP: overview\n\n" title))
      (dolist (entry entries)
        (let ((url (plist-get entry :url))
              (name (or (plist-get entry :title) url))
              (time (plist-get entry :time)))
          (insert (format "* %s\n  %s\n"
                          (org-link-make-string url (zhihu-reader--safe-title (format "%s" name)))
                          (if time
                              (format-time-string "%Y-%m-%d %H:%M"
                                                  (seconds-to-time time))
                            "")))
          (insert "\n")))
      (if (null entries) (insert "没有记录。\n"))
      (goto-char (point-min))
      (ignore-errors (org-overview))
      (setq buffer-read-only t))
    (pop-to-buffer (get-buffer buffer))))

(defun zhihu-reader-history ()
  "Open reading history."
  (interactive)
  (zhihu-reader--render-link-list
   "知乎阅读历史" (zhihu-reader--load-entry-file zhihu-reader-history-file)
   '(history) "*Zhihu History*"))

(defun zhihu-reader-clear-history ()
  "Clear reading history."
  (interactive)
  (when (yes-or-no-p "清空阅读历史？")
    (zhihu-reader--save-entry-file zhihu-reader-history-file nil)
    (message "阅读历史已清空")))

(defun zhihu-reader-read-later ()
  "Add the current content to read later."
  (interactive)
  (let* ((url (zhihu-reader--current-url))
         (title (zhihu-reader--share-title))
         (entries (zhihu-reader--load-entry-file zhihu-reader-later-file))
         (entry (list :title (or title url) :url url :time (float-time)))
         (entries (cons entry
                        (seq-remove (lambda (item)
                                      (equal (plist-get item :url) url))
                                    entries))))
    (unless (and url (not (string-empty-p url)))
      (user-error "光标不在可收藏内容上"))
    (zhihu-reader--save-entry-file zhihu-reader-later-file entries)
    (message "已加入稍后读: %s" (or title url))))

(defun zhihu-reader-later-list ()
  "Open read-later list."
  (interactive)
  (zhihu-reader--render-link-list
   "知乎稍后读" (zhihu-reader--load-entry-file zhihu-reader-later-file)
   '(later) "*Zhihu Later*"))

(defun zhihu-reader-clear-later ()
  "Clear read-later list."
  (interactive)
  (when (yes-or-no-p "清空稍后读？")
    (zhihu-reader--save-entry-file zhihu-reader-later-file nil)
    (message "稍后读已清空")))

(defun zhihu-reader--record-search-history (query)
  "Record QUERY in search history."
  (when (and query (not (string-empty-p query)))
    (let* ((url (format "https://www.zhihu.com/search?q=%s&type=content"
                        (url-hexify-string query)))
           (entries (zhihu-reader--load-entry-file zhihu-reader-search-history-file))
           (entry (list :title query :url url :time (float-time)))
           (entries (cons entry
                          (seq-remove (lambda (item)
                                        (equal (plist-get item :url) url))
                                      entries))))
      (zhihu-reader--save-entry-file
       zhihu-reader-search-history-file (seq-take entries 100)))))

(defun zhihu-reader-search-history ()
  "Open search history."
  (interactive)
  (zhihu-reader--render-link-list
   "知乎搜索历史"
   (zhihu-reader--load-entry-file zhihu-reader-search-history-file)
   '(search-history) "*Zhihu Search History*"))

(defun zhihu-reader-clear-search-history ()
  "Clear search history."
  (interactive)
  (when (yes-or-no-p "清空搜索历史？")
    (zhihu-reader--save-entry-file zhihu-reader-search-history-file nil)
    (message "搜索历史已清空")))

(defun zhihu-reader-remove-later ()
  "Remove the URL at point from read-later."
  (interactive)
  (let ((url (or (zhihu-reader--link-at-point)
                 (zhihu-reader--current-url))))
    (unless url (user-error "光标不在稍后读条目上"))
    (let* ((entries (zhihu-reader--load-entry-file zhihu-reader-later-file))
           (remaining (seq-remove (lambda (item)
                                    (equal (plist-get item :url) url))
                                  entries)))
      (zhihu-reader--save-entry-file zhihu-reader-later-file remaining)
      (message "已从稍后读移除")
      (when (derived-mode-p 'zhihu-reader-mode)
        (zhihu-reader-later-list)))))

(defun zhihu-reader-store-link ()
  "Store the current Zhihu content as an Org link."
  (interactive)
  (let ((url (zhihu-reader--current-url))
        (title (or (zhihu-reader--share-title) "知乎")))
    (unless (and url (not (string-empty-p url)))
      (user-error "光标不在知乎内容上"))
    (require 'org)
    (org-link-store-props :type "https" :link url :description title)
    (message "已保存 Org 链接: %s" title)))

(defun zhihu-reader-capture ()
  "Append the current Zhihu content to `zhihu-reader-capture-file'."
  (interactive)
  (let ((url (zhihu-reader--current-url))
        (title (or (zhihu-reader--share-title) "知乎")))
    (unless (and url (not (string-empty-p url)))
      (user-error "光标不在知乎内容上"))
    (make-directory (file-name-directory zhihu-reader-capture-file) t)
    (with-current-buffer (find-file-noselect zhihu-reader-capture-file)
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert (format "* [[%s][%s]]\\n  %s\\n"
                      url (zhihu-reader--safe-title title)
                      (format-time-string "%Y-%m-%d %H:%M")))
      (save-buffer))
    (message "已收藏到 %s" zhihu-reader-capture-file)))

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
         (url (format "https://www.zhihu.com/api/v4/members/%s?include=name,headline,description,follower_count,following_count,answer_count,question_count,articles_count,pins_count,voteup_count,thanked_count,is_following,url_token,badge_v2,business,locations,avatar_url"
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
                  (list
                   (zhihu-reader--alist-get 'headline json)
                   (zhihu-reader--alist-get 'description json)
                   (let ((badge (zhihu-reader--alist-get 'title (zhihu-reader--alist-get 'badge_v2 json))))
                     (and badge (format "认证：%s" badge)))
                   (let ((business (zhihu-reader--alist-get 'name (zhihu-reader--alist-get 'business json))))
                     (and business (format "领域：%s" business)))
                   (let ((locations (zhihu-reader--alist-get 'locations json)))
                     (when locations
                       (format "所在地：%s"
                               (mapconcat (lambda (loc) (or (zhihu-reader--alist-get 'name loc) ""))
                                          (if (vectorp locations) (append locations nil) locations)
                                          " · "))))
                   (format "回答 %s · 文章 %s · 想法 %s · 提问 %s"
                           (or (zhihu-reader--alist-get 'answer_count json) 0)
                           (or (zhihu-reader--alist-get 'articles_count json) 0)
                           (or (zhihu-reader--alist-get 'pins_count json) 0)
                           (or (zhihu-reader--alist-get 'question_count json) 0))
                   (format "关注 %s · 粉丝 %s · 获赞 %s · 感谢 %s"
                           (or (zhihu-reader--alist-get 'following_count json) 0)
                           (or (zhihu-reader--alist-get 'follower_count json) 0)
                           (or (zhihu-reader--alist-get 'voteup_count json) 0)
                           (or (zhihu-reader--alist-get 'thanked_count json) 0))))
            "\n\n"))
          (insert (format "\n[[https://www.zhihu.com/people/%s/answers][回答]]  [[https://www.zhihu.com/people/%s/articles][文章]]  [[https://www.zhihu.com/people/%s/columns][专栏]]  [[https://www.zhihu.com/people/%s/pins][想法]]  [[https://www.zhihu.com/people/%s/followers][粉丝]]  [[https://www.zhihu.com/people/%s/following][关注]]\n"
                          token token token token token token))
          (goto-char (point-min))
          (ignore-errors (org-fold-show-all))
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
      (setq zhihu-reader--unread-total
            (or (zhihu-reader--alist-get 'count (zhihu-reader--alist-get 'message unread))
                zhihu-reader--unread-total))
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

(defun zhihu-reader--render-columns (json buffer token)
  "Render column contributions JSON in BUFFER."
  (let ((data (append (or (zhihu-reader--alist-get 'data json) []) nil)))
    (with-current-buffer (get-buffer-create buffer)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (zhihu-reader-mode)
        (setq-local zhihu-reader--view (list 'user-columns token 1 buffer))
        (insert (format "#+TITLE: 专栏 · %s\n#+STARTUP: overview\n\n" token))
        (dolist (entry data)
          (let* ((column (or (zhihu-reader--alist-get 'column entry) entry))
                 (id (zhihu-reader--alist-get 'id column))
                 (title (or (zhihu-reader--alist-get 'title column) "专栏"))
                 (count (or (zhihu-reader--alist-get 'contributions_count entry) 0)))
            (insert (format "* %s\n  %s 篇\n\n"
                            (org-link-make-string
                             (format "https://www.zhihu.com/column/%s" id)
                             (zhihu-reader--safe-title title))
                            count))))
        (if (null data) (insert "没有专栏。\n"))
        (goto-char (point-min))
        (ignore-errors (org-overview))
        (setq buffer-read-only t)))
    (pop-to-buffer (get-buffer-create buffer))))

(defun zhihu-reader-user-columns (token &optional page buffer-name)
  "List TOKEN's column contributions."
  (interactive (list (or (org-entry-get nil "ZHIHU-AUTHOR-TOKEN")
                         (read-string "知乎用户 url_token: "))
                     (prefix-numeric-value current-prefix-arg)))
  (let* ((token (or token (read-string "知乎用户 url_token: ")))
         (page (or page 1))
         (buffer (or buffer-name "*Zhihu User Columns*"))
         (url (format "https://www.zhihu.com/api/v4/members/%s/column-contributions?limit=20&offset=%d"
                      token (* 20 (1- page)))))
    (if noninteractive
        (zhihu-reader--render-columns
         (plist-get (zhihu-reader--http-json "GET" url nil nil) :json) buffer token)
      (zhihu-reader--async-custom
       "用户专栏" (list 'user-columns token page buffer) url buffer
       (lambda (json) (zhihu-reader--render-columns json buffer token))))))

(defun zhihu-reader-pin (id &optional buffer-name)
  "Open Zhihu pin ID in BUFFER-NAME."
  (interactive (list (or (read-string "知乎想法 ID: ")
                         (user-error "想法 ID 为空"))))
  (let* ((id (zhihu-reader--id-or-url id))
         (buffer (or buffer-name "*Zhihu Pin*"))
         (url (format "https://www.zhihu.com/api/v4/pins/%s" id))
         (resp (zhihu-reader--http-json "GET" url nil nil))
         (json (plist-get resp :json))
         (status (plist-get resp :status)))
    (unless (memq status '(200 201))
      (user-error "想法加载失败 (HTTP %s)" status))
    (let* ((html (or (zhihu-reader--alist-get 'content_html json) ""))
           (item (list :kind "pin" :id id
                       :title (or (zhihu-reader--truncate
                                   (zhihu-reader--strip-html
                                    (zhihu-reader--alist-get 'excerpt_title json))
                                   80)
                                  (format "想法 %s" id))
                       :content (zhihu-reader--html-to-org html)
                       :images (zhihu-reader--extract-images html)
                       :comment-count (zhihu-reader--string
                                       (zhihu-reader--alist-get 'comment_count json))
                       :author (zhihu-reader--alist-get
                                'name (zhihu-reader--alist-get 'author json)))))
      (zhihu-reader--render
       (format "想法 · %s" (plist-get item :title))
       (list item) (list 'pin id) buffer))))

(defun zhihu-reader-column (id &optional page buffer-name)
  "Open column ID's articles."
  (interactive (list (or (read-string "知乎专栏 ID: ")
                         (user-error "专栏 ID 为空"))
                     (prefix-numeric-value current-prefix-arg)))
  (let* ((page (or page 1))
         (buffer (or buffer-name "*Zhihu Column*"))
         (url (format "https://www.zhihu.com/api/v4/columns/%s/articles?limit=20&offset=%d"
                      id (* 20 (1- page))))
         (title (format "专栏 %s · 第 %d 页" id page))
         (view (list 'column id page buffer)))
    (if noninteractive
        (zhihu-reader--render title
                              (zhihu-reader--normalize-items
                               (zhihu-reader--fetch-json url))
                              view buffer)
      (zhihu-reader--async-list
       title view url buffer
       (lambda (json)
         (cons title (zhihu-reader--normalize-items json)))
       nil))))

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
         (url (if (and zhihu-reader-search-content-type
                       (not (equal zhihu-reader-search-content-type "all")))
                  (concat url "&vertical="
                          (url-hexify-string zhihu-reader-search-content-type)
                          "&vertical_info=0,0,0,0,0,0,0,0,0,0,0,0")
                url))
         (time-map '(("day" . "a_day") ("week" . "a_week") ("month" . "a_month")
                     ("three-months" . "three_months") ("half-year" . "half_a_year")
                     ("year" . "a_year")))
         (time (cdr (assoc zhihu-reader-search-time-range time-map)))
         (url (if time (concat url "&time_interval=" (url-hexify-string time)) url))
         (url (if (and zhihu-reader-search-sort
                       (not (string-empty-p zhihu-reader-search-sort)))
                  (concat url "&sort=" (url-hexify-string zhihu-reader-search-sort))
                url)))
    url))

(defun zhihu-reader-search-filter ()
  "Set search content/time filters and refresh the current search."
  (interactive)
  (setq zhihu-reader-search-content-type
        (completing-read "内容类型: " '("all" "answer" "article")
                         nil t nil nil zhihu-reader-search-content-type))
  (setq zhihu-reader-search-time-range
        (completing-read "时间范围: "
                         '("all" "day" "week" "month" "three-months" "half-year" "year")
                         nil t nil nil zhihu-reader-search-time-range))
  (if (eq (car-safe zhihu-reader--view) 'search)
      (zhihu-reader-refresh)
    (message "搜索筛选已设置")))

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
    (zhihu-reader--record-search-history query)
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
    ('collections
     (zhihu-reader-collections (nth 1 zhihu-reader--view)))
    ('pin (zhihu-reader-pin (nth 1 zhihu-reader--view)))
    ('column (zhihu-reader-column (nth 1 zhihu-reader--view)
                                  (nth 2 zhihu-reader--view)
                                  (nth 3 zhihu-reader--view)))
    ('user-columns (zhihu-reader-user-columns (nth 1 zhihu-reader--view)
                                              (nth 2 zhihu-reader--view)
                                              (nth 3 zhihu-reader--view)))
    ('user-relations
     (zhihu-reader--user-relations (nth 1 zhihu-reader--view)
                                   (nth 2 zhihu-reader--view)
                                   (nth 3 zhihu-reader--view)
                                   (nth 4 zhihu-reader--view)))
    ('notifications (zhihu-reader-notifications (nth 1 zhihu-reader--view)))
    ('inbox (zhihu-reader-inbox (nth 1 zhihu-reader--view)))
    ('chat (zhihu-reader-chat (nth 1 zhihu-reader--view)))
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
      ('author
       (zhihu-reader-author (nth 1 view) (1+ (or (nth 2 view) 1)) (nth 3 view)))
      ('column
       (zhihu-reader-column (nth 1 view) (1+ (or (nth 2 view) 1)) (nth 3 view)))
      ('user-columns
       (zhihu-reader-user-columns (nth 1 view) (1+ (or (nth 2 view) 1)) (nth 3 view)))
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
      ('author
       (zhihu-reader-author (nth 1 view) (max 1 (1- (or (nth 2 view) 1))) (nth 3 view)))
      ('column
       (zhihu-reader-column (nth 1 view) (max 1 (1- (or (nth 2 view) 1))) (nth 3 view)))
      ('user-columns
       (zhihu-reader-user-columns (nth 1 view) (max 1 (1- (or (nth 2 view) 1))) (nth 3 view)))
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


;;; Asynchronous wrappers for read-only views

(defun zhihu-reader--asyncify-call (orig args title view url buffer)
  "Run ORIG with mocked HTTP after asynchronously fetching URL."
  (if noninteractive
      (apply orig args)
    (zhihu-reader--async-custom
     title view url buffer
     (lambda (json)
       (let ((noninteractive t))
         (cl-letf (((symbol-function 'zhihu-reader--http-json)
                    (lambda (&rest _)
                      (list :status 200 :json json :body ""))))
           (apply orig args)))))))

(defun zhihu-reader--asyncify-user (orig token &optional buffer-name)
  (zhihu-reader--asyncify-call
   orig (list token buffer-name) "用户主页"
   (list 'user token (or buffer-name "*Zhihu User*"))
   (format "https://www.zhihu.com/api/v4/members/%s?include=name,headline,description,follower_count,following_count,answer_count,articles_count,is_following,url_token" token)
   (or buffer-name "*Zhihu User*")))

(defun zhihu-reader--asyncify-notifications (orig &optional cursor)
  (zhihu-reader--asyncify-call
   orig (list cursor) "通知"
   (list 'notifications cursor)
   (if (and cursor (stringp cursor)
            (string-prefix-p "https://api.zhihu.com/notifications/" cursor))
       cursor
     "https://api.zhihu.com/notifications/v3/message/v3")
   "*Zhihu Notifications*"))

(defun zhihu-reader--asyncify-notification-timeline (orig category &optional cursor)
  (let ((url (if (and cursor (stringp cursor)
                       (string-prefix-p "https://api.zhihu.com/notifications/" cursor))
                  cursor
                (if (equal category "invite")
                    "https://api.zhihu.com/notifications/v3/timeline/entry/invite?invite_with_time_slice=1&limit=20"
                  (format "https://api.zhihu.com/notifications/v3/timeline/entry/%s?limit=20" category)))))
    (zhihu-reader--asyncify-call
     orig (list category cursor) (format "通知 · %s" category)
     (list 'notification-timeline category cursor) url "*Zhihu Notifications*")))

(defun zhihu-reader--asyncify-user-relations (orig token kind &optional page buffer-name)
  (let* ((page (or page 1))
         (buffer (or buffer-name
                     (if (equal kind "followers")
                         "*Zhihu Followers*" "*Zhihu Following*")))
         (url (format "https://www.zhihu.com/api/v4/members/%s/%s?limit=20&offset=%d"
                      token kind (* 20 (1- page)))))
    (zhihu-reader--asyncify-call
     orig (list token kind page buffer-name)
     (format "%s · %s" (if (equal kind "followers") "粉丝" "关注") token)
     (list 'user-relations token kind page buffer) url buffer)))

(defun zhihu-reader--asyncify-user-articles (orig token &optional page buffer-name)
  (let* ((page (or page 1))
         (buffer (or buffer-name "*Zhihu User Articles*"))
         (url (format "https://www.zhihu.com/api/v4/members/%s/articles?limit=20&offset=%d"
                      token (* 20 (1- page)))))
    (zhihu-reader--asyncify-call
     orig (list token page buffer-name) "用户文章"
     (list 'user-articles token page buffer) url buffer)))

(defun zhihu-reader--asyncify-user-pins (orig token &optional page buffer-name)
  (let* ((page (or page 1))
         (buffer (or buffer-name "*Zhihu User Pins*"))
         (url (format "https://www.zhihu.com/api/v4/members/%s/pins?limit=20&offset=%d"
                      token (* 20 (1- page)))))
    (zhihu-reader--asyncify-call
     orig (list token page buffer-name) "用户想法"
     (list 'user-pins token page buffer) url buffer)))

(defun zhihu-reader--asyncify-search-people (orig query &optional page buffer-name)
  (let* ((page (or page 1))
         (buffer (or buffer-name "*Zhihu People Search*"))
         (url (zhihu-reader--search-type-url "people" query page)))
    (zhihu-reader--asyncify-call
     orig (list query page buffer-name) "用户搜索"
     (list 'search-people query page buffer) url buffer)))

(defun zhihu-reader--asyncify-search-topics (orig query &optional page buffer-name)
  (let* ((page (or page 1))
         (buffer (or buffer-name "*Zhihu Topic Search*"))
         (url (zhihu-reader--search-type-url "topic" query page)))
    (zhihu-reader--asyncify-call
     orig (list query page buffer-name) "话题搜索"
     (list 'search-topics query page buffer) url buffer)))

(advice-add 'zhihu-reader-user :around #'zhihu-reader--asyncify-user)
(advice-add 'zhihu-reader-notifications :around #'zhihu-reader--asyncify-notifications)
(advice-add 'zhihu-reader-notification-timeline :around #'zhihu-reader--asyncify-notification-timeline)
(advice-add 'zhihu-reader--user-relations :around #'zhihu-reader--asyncify-user-relations)
(advice-add 'zhihu-reader-user-articles :around #'zhihu-reader--asyncify-user-articles)
(advice-add 'zhihu-reader-user-pins :around #'zhihu-reader--asyncify-user-pins)
(advice-add 'zhihu-reader-search-people :around #'zhihu-reader--asyncify-search-people)
(advice-add 'zhihu-reader-search-topics :around #'zhihu-reader--asyncify-search-topics)

(defun zhihu-reader-export-org (file)
  "Export the current Zhihu buffer as an Org file FILE."
  (interactive "F导出 Org 到: ")
  (with-temp-file file
    (insert (buffer-substring-no-properties (point-min) (point-max))))
  (message "已导出 Org: %s" file))

(defun zhihu-reader-export-html (file)
  "Export the current Zhihu buffer as an HTML file FILE."
  (interactive "F导出 HTML 到: ")
  (require 'ox-html)
  (let ((html (org-export-as 'html nil nil t)))
    (with-temp-file file (insert html))
    (message "已导出 HTML: %s" file)))

(defun zhihu-reader-export-markdown (file)
  "Export the current Zhihu buffer as Markdown FILE."
  (interactive "F导出 Markdown 到: ")
  (require 'ox-md)
  (let ((md (org-md-export-as-markdown nil nil t)))
    (with-temp-file file
      (insert (if (bufferp md)
                  (with-current-buffer md (buffer-string))
                md)))
    (message "已导出 Markdown: %s" file)))

(provide 'zhihu-reader-views)
;;; zhihu-reader-views.el ends here

(defvar org-store-link-functions nil)
(add-to-list 'org-store-link-functions #'zhihu-reader-store-link)
