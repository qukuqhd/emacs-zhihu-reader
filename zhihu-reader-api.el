;;; zhihu-reader-api.el --- HTTP actions and publishing for Zhihu  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 qukuqhd
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Part of emacs-zhihu-reader.  See zhihu-reader.el for the entry point.

;;; Code:

(require 'zhihu-reader-core)

;;;; Standalone HTTP and Org export

(defun zhihu-reader--org-to-html (text)
  "Export Org TEXT to an HTML body using built-in ox-html."
  (require 'ox-html)
  (with-temp-buffer
    (insert text)
    (delay-mode-hooks (org-mode))
    (org-export-as 'html nil nil t nil)))

(defconst zhihu-reader--zse-version "101_3_3.0")

(defconst zhihu-reader--zse-zk
  [1170614578 1024848638 1413669199 3951632832
   3528873006 2921909214 4151847688 3997739139
   1933479194 3323781115 3888513386 460404854
   3747539722 2403641034 2615871395 2119585428
   2265697227 2035090028 2773447226 4289380121
   4217216195 2200601443 3051914490 1579901135
   1321810770 456816404 2903323407 4065664991
   330002838 3506006750 363569021 2347096187])

(defconst zhihu-reader--zse-zb
  [20 223 245 7 248 2 194 209 87 6 227 253 240 128 222 91
   237 9 125 157 230 93 252 205 90 79 144 199 159 197 186 167
   39 37 156 198 38 42 43 168 217 153 15 103 80 189 71 191
   97 84 247 95 36 69 14 35 12 171 28 114 178 148 86 182
   32 83 158 109 22 255 94 238 151 85 77 124 254 18 4 26
   123 176 232 193 131 172 143 142 150 30 10 146 162 62 224 218
   196 229 1 192 213 27 110 56 231 180 138 107 242 187 54 120
   19 44 117 228 215 203 53 239 251 127 81 11 133 96 204 132
   41 115 73 55 249 147 102 48 122 145 106 118 74 190 29 16
   174 5 177 129 63 113 99 31 161 76 246 34 211 13 60 68
   207 160 65 111 82 165 67 169 225 57 112 244 155 51 236 200
   233 58 61 47 100 137 185 64 17 70 234 163 219 108 170 166
   59 149 52 105 24 212 78 173 45 0 116 226 119 136 206 135
   175 195 25 92 121 208 126 139 3 75 141 21 130 98 241 40
   154 66 184 49 181 46 243 88 101 183 8 23 72 188 104 179
   210 134 250 201 164 89 216 202 220 50 221 152 140 33 235 214])

(defconst zhihu-reader--zse-alphabet
  "6fpLRqJO8M/c3jnYxFkUVC4ZIG12SiH=5v0mXDazWBTsuw7QetbKdoPyAl+hN9rgE")

(defconst zhihu-reader--zse-key (vconcat "059053f7d15e01d7"))

(defun zhihu-reader--zse-rotl (n bits)
  (setq n (logand n #xffffffff))
  (logand #xffffffff (logior (ash n bits) (ash n (- bits 32)))))

(defun zhihu-reader--zse-u32 (bytes pos)
  (logior (ash (aref bytes pos) 24)
          (ash (aref bytes (1+ pos)) 16)
          (ash (aref bytes (+ pos 2)) 8)
          (aref bytes (+ pos 3))))

(defun zhihu-reader--zse-put-u32 (value bytes pos)
  (dotimes (i 4)
    (aset bytes (+ pos i)
          (logand #xff (ash value (- (* 8 (- 3 i))))))))

(defun zhihu-reader--zse-g (value)
  (let* ((b0 (logand #xff (ash value -24)))
         (b1 (logand #xff (ash value -16)))
         (b2 (logand #xff (ash value -8)))
         (b3 (logand #xff value))
         (sub (logior (ash (aref zhihu-reader--zse-zb b0) 24)
                      (ash (aref zhihu-reader--zse-zb b1) 16)
                      (ash (aref zhihu-reader--zse-zb b2) 8)
                      (aref zhihu-reader--zse-zb b3))))
    (logand #xffffffff
            (logxor sub
                    (zhihu-reader--zse-rotl sub 2)
                    (zhihu-reader--zse-rotl sub 10)
                    (zhihu-reader--zse-rotl sub 18)
                    (zhihu-reader--zse-rotl sub 24)))))

(defun zhihu-reader--zse-round (input)
  (let ((state (make-vector 36 0))
        (output (make-vector 16 0)))
    (dotimes (i 4)
      (aset state i (zhihu-reader--zse-u32 input (* i 4))))
    (dotimes (i 32)
      (aset state (+ i 4)
            (logand #xffffffff
                    (logxor (aref state i)
                            (zhihu-reader--zse-g
                             (logxor (aref state (+ i 1))
                                     (aref state (+ i 2))
                                     (aref state (+ i 3))
                                     (aref zhihu-reader--zse-zk i)))))))
    (dotimes (i 4)
      (zhihu-reader--zse-put-u32 (aref state (- 35 i))
                                 output (* i 4)))
    output))

(defun zhihu-reader--zse-chain (data iv)
  (let ((output (make-vector (length data) 0))
        (offset 0))
    (while (< offset (length data))
      (let ((mixed (make-vector 16 0)))
        (dotimes (i 16)
          (aset mixed i (logxor (aref data (+ offset i)) (aref iv i))))
        (setq iv (zhihu-reader--zse-round mixed))
        (dotimes (i 16)
          (aset output (+ offset i) (aref iv i))))
      (setq offset (+ offset 16)))
    output))

(defun zhihu-reader--zse-encode (input)
  (let* ((remainder (% (length input) 3))
         (padding (if (zerop remainder) 0 (- 3 remainder)))
         (bytes (make-vector (+ (length input) padding) 0))
         (index 0)
         (position (1- (+ (length input) padding)))
         chars)
    (dotimes (i (length input))
      (aset bytes i (aref input i)))
    (while (>= position 0)
      (let ((value 0))
        (dotimes (byte-index 3)
          (let* ((byte (aref bytes (- position byte-index)))
                 (mask (logand #xff (ash 58 (- (* 8 (% index 4)))))))
            (setq index (1+ index))
            (setq value (logior value
                                (ash (logand #xff (logxor byte mask))
                                     (* 8 byte-index))))))
        (dotimes (i 4)
          (push (aref zhihu-reader--zse-alphabet
                      (logand 63 (ash value (- (* 6 i)))))
                chars)))
      (setq position (- position 3)))
    (concat (nreverse chars))))

(defun zhihu-reader--zse-uri-byte-p (byte)
  (or (and (>= byte ?A) (<= byte ?Z))
      (and (>= byte ?a) (<= byte ?z))
      (and (>= byte ?0) (<= byte ?9))
      (memq byte '(?- ?_ ?. ?! ?~ ?* ?' ?\( ?\)))))

(defun zhihu-reader--zse-uri-bytes (input)
  (let ((hex "0123456789ABCDEF")
        encoded)
    (dolist (byte (append (encode-coding-string input 'utf-8 t) nil))
      (if (zhihu-reader--zse-uri-byte-p byte)
          (push byte encoded)
        (push ?% encoded)
        (push (aref hex (ash byte -4)) encoded)
        (push (aref hex (logand byte #x0f)) encoded)))
    (vconcat (nreverse encoded))))

(defun zhihu-reader--zse-encrypt (input)
  (let* ((bytes (zhihu-reader--zse-uri-bytes input))
         (unpadded (+ 2 (length bytes)))
         (padding (- 16 (% unpadded 16)))
         (plain (make-vector (+ unpadded padding) padding))
         (first (make-vector 16 0))
         cipher)
    (aset plain 0 12)
    (aset plain 1 0)
    (dotimes (i (length bytes))
      (aset plain (+ i 2) (aref bytes i)))
    (dotimes (i 16)
      (aset first i
            (logand #xff
                    (logxor (aref plain i) (aref zhihu-reader--zse-key i) 42))))
    (let ((c0 (zhihu-reader--zse-round first)))
      (setq cipher (make-vector (length plain) 0))
      (dotimes (i 16)
        (aset cipher i (aref c0 i)))
      (when (> (length plain) 16)
        (let ((rest (zhihu-reader--zse-chain (cl-subseq plain 16) c0)))
          (dotimes (i (length rest))
            (aset cipher (+ i 16) (aref rest i))))))
    (zhihu-reader--zse-encode cipher)))

(defun zhihu-reader--zse-path (url)
  (let ((filename (url-filename (url-generic-parse-url url))))
    (cond
     ((string-empty-p filename) "/")
     ((string-prefix-p "?" filename) (concat "/" filename))
     (t filename))))

(defun zhihu-reader--zse-headers (url dc0)
  (when (and (stringp dc0) (not (string-empty-p dc0)))
    (let* ((source (concat zhihu-reader--zse-version "+"
                           (zhihu-reader--zse-path url) "+" dc0))
           (digest (secure-hash 'md5
                                (encode-coding-string source 'utf-8 t))))
      `(("x-zse-93" . ,zhihu-reader--zse-version)
        ("x-zse-96" . ,(concat "2.0_" (zhihu-reader--zse-encrypt digest)))))))

(defun zhihu-reader--http-json (method url &optional body sign)
  "Send METHOD URL with BODY as JSON using the reader's cookies.
When SIGN is non-nil, add Zhihu ZSE headers for the request.
Return a plist (:status :json :body)."
  (let* ((cookies (zhihu-reader--cookie-alist))
         (xsrf (cdr (assoc-string "_xsrf" cookies)))
         (data (and body
                    (encode-coding-string
                     (json-serialize body
                                     :null-object :json-null
                                     :false-object :json-false)
                     'utf-8)))
         (url-request-method (upcase method))
         (url-request-extra-headers
          (append
           `(("Cookie" . ,(zhihu-reader--cookies))
             ("User-Agent" . ,zhihu-reader-user-agent)
             ("Referer" . "https://www.zhihu.com/")
             ("Origin" . "https://www.zhihu.com")
             ("x-requested-with" . "fetch")
             ("Accept" . "application/json, text/plain, */*")
             ("Content-Type" . "application/json"))
           (when (and xsrf (not (string-empty-p xsrf)))
             `(("x-xsrftoken" . ,xsrf)))
           (when sign
             (zhihu-reader--zse-headers
              url (cdr (assoc-string "d_c0" cookies))))))
         (url-request-data data))
    (with-current-buffer (url-retrieve-synchronously url t t 30)
      (unwind-protect
          (progn
            (goto-char (point-min))
            (let ((status url-http-response-status))
              (goto-char url-http-end-of-headers)
              (decode-coding-region (point) (point-max) 'utf-8)
              (let ((text (buffer-substring-no-properties (point) (point-max))))
                (list :status status
                      :json (condition-case nil
                                (json-parse-string text
                                                   :object-type 'alist
                                                   :null-object nil
                                                   :false-object nil)
                              (error nil))
                      :body text))))
        (kill-buffer (current-buffer))))))

(defun zhihu-reader--http-form (method url fields)
  "Send METHOD to URL with form-encoded FIELDS using reader cookies."
  (let* ((cookies (zhihu-reader--cookie-alist))
         (xsrf (cdr (assoc-string "_xsrf" cookies)))
         (data (mapconcat (lambda (pair)
                            (concat (url-hexify-string (car pair)) "="
                                    (url-hexify-string (cdr pair))))
                          fields "&"))
         (url-request-method (upcase method))
         (url-request-extra-headers
          (append
           `(("Cookie" . ,(zhihu-reader--cookies))
             ("User-Agent" . ,zhihu-reader-user-agent)
             ("Referer" . "https://www.zhihu.com/")
             ("Origin" . "https://www.zhihu.com")
             ("Accept" . "application/json, text/plain, */*")
             ("Content-Type" . "application/x-www-form-urlencoded"))
           (when (and xsrf (not (string-empty-p xsrf)))
             `(("x-xsrftoken" . ,xsrf)))))
         (url-request-data data))
    (with-current-buffer (url-retrieve-synchronously url t t 30)
      (unwind-protect
          (progn
            (goto-char (point-min))
            (let ((status url-http-response-status))
              (goto-char url-http-end-of-headers)
              (decode-coding-region (point) (point-max) 'utf-8)
              (list :status status
                    :body (buffer-substring-no-properties (point) (point-max)))))
        (kill-buffer (current-buffer))))))

(defun zhihu-reader--action-check (resp action)
  (let ((status (plist-get resp :status)))
    (unless (memq status '(200 201 202 204))
      (error "%s失败 (HTTP %s): %s" action status (plist-get resp :body)))
    resp))

(defun zhihu-reader--content-target ()
  "Return (TYPE ID) for the answer or article at point."
  (let ((answer (org-entry-get nil "ZHIHU-ANSWER" t))
        (article (org-entry-get nil "ZHIHU-ARTICLE" t)))
    (cond
     (answer (list "answers" answer))
     (article (list "articles" article))
     (t (user-error "光标不在回答或文章标题上")))))

(defun zhihu-reader-vote ()
  "Upvote the answer or article at point; `C-u' downvotes."
  (interactive)
  (unless zhihu-reader-enable-write
    (user-error "写操作已关闭；请设置 `zhihu-reader-enable-write' 为 t"))
  (pcase-let* ((`(,type ,id) (zhihu-reader--content-target))
               (down (equal current-prefix-arg '(4)))
               (_ (when (and down (equal type "articles"))
                    (user-error "知乎文章没有可确认的反对操作")))
               (voted (equal (org-entry-get nil "ZHIHU-VOTED") "t"))
               (next (if (and voted (not down)) "neutral" (if down "down" "up")))
               (body (if (equal type "answers")
                         (list :type next)
                       (list :voting (if (equal next "up") 1 0))))
               (url (format "https://www.zhihu.com/api/v4/%s/%s/voters" type id)))
    (unless (yes-or-no-p (format "确认将状态改为 %s？" next))
      (user-error "已取消"))
    (zhihu-reader--action-check
     (zhihu-reader--http-json "POST" url body nil)
     "操作")
    (org-entry-put nil "ZHIHU-VOTED" (if (equal next "neutral") "nil" "t"))
    (message "已更新赞同状态: %s" next)))

(defun zhihu-reader-follow ()
  "Toggle following the author, question or topic at point."
  (interactive)
  (unless zhihu-reader-enable-write
    (user-error "写操作已关闭；请设置 `zhihu-reader-enable-write' 为 t"))
  (let* ((author (org-entry-get nil "ZHIHU-AUTHOR-TOKEN" t))
         (question (org-entry-get nil "ZHIHU-QUESTION" t))
         (topic (org-entry-get nil "ZHIHU-TOPIC" t))
         (following (equal (org-entry-get nil "ZHIHU-FOLLOWING") "t"))
         (target
          (cond
           (author (cons "members" author))
           (question (cons "questions" question))
           (topic (cons "topics" topic))
           (t (user-error "光标不在作者、问题或话题标题上"))))
         (url (format "https://www.zhihu.com/api/v4/%s/%s/followers"
                      (car target) (cdr target))))
    (unless (yes-or-no-p (format "%s关注？" (if following "取消" "确认")))
      (user-error "已取消"))
    (zhihu-reader--action-check
     (zhihu-reader--http-json (if following "DELETE" "POST") url nil nil)
     (if following "取消关注" "关注"))
    (org-entry-put nil "ZHIHU-FOLLOWING" (if following "nil" "t"))
    (message (if following "已取消关注" "已关注"))))

(defun zhihu-reader-thank ()
  "Thank the answer at point."
  (interactive)
  (unless zhihu-reader-enable-write
    (user-error "写操作已关闭；请设置 `zhihu-reader-enable-write' 为 t"))
  (let* ((id (org-entry-get nil "ZHIHU-ANSWER" t))
         (thanked (equal (org-entry-get nil "ZHIHU-THANKED") "t")))
    (unless id (user-error "光标不在回答标题上"))
    (unless (yes-or-no-p (if thanked "取消感谢这个回答？" "感谢这个回答？"))
      (user-error "已取消"))
    (zhihu-reader--action-check
     (zhihu-reader--http-json
      (if thanked "DELETE" "POST")
      (format "https://www.zhihu.com/api/v4/answers/%s/thankers" id)
      nil nil)
     (if thanked "取消感谢" "感谢"))
    (org-entry-put nil "ZHIHU-THANKED" (if thanked "nil" "t"))
    (message (if thanked "已取消感谢" "已感谢"))))

(defun zhihu-reader-favorite ()
  "Add the answer or article at point to a collection."
  (interactive)
  (unless zhihu-reader-enable-write
    (user-error "写操作已关闭；请设置 `zhihu-reader-enable-write' 为 t"))
  (pcase-let* ((`(,type ,id) (zhihu-reader--content-target))
               (type (if (equal type "answers") "answer" "article"))
               (collection (read-string "收藏夹 ID: ")))
    (when (string-empty-p collection)
      (user-error "收藏夹 ID 为空"))
    (unless (yes-or-no-p (format "加入收藏夹 %s？" collection))
      (user-error "已取消"))
    (zhihu-reader--action-check
     (zhihu-reader--http-form
      "PUT" (format "https://api.zhihu.com/collections/contents/%s/%s" type id)
      (list (cons "add_collections" collection)))
     "收藏")
    (message "已加入收藏夹")))

(defun zhihu-reader--http-binary (method url data extra-headers)
  "Send raw DATA to URL with EXTRA-HEADERS and return (STATUS . BODY)."
  (let ((url-request-method (upcase method))
        (url-request-extra-headers extra-headers)
        (url-request-data data))
    (with-current-buffer (url-retrieve-synchronously url t t 60)
      (unwind-protect
          (progn
            (goto-char (point-min))
            (let ((status url-http-response-status))
              (goto-char url-http-end-of-headers)
              (cons status (buffer-substring-no-properties (point) (point-max)))))
        (kill-buffer (current-buffer))))))

(defun zhihu-reader--hmac-sha1-base64 (key data)
  "Return HMAC-SHA1(KEY, DATA) as base64, using openssl."
  (let ((file (make-temp-file "zhihu-hmac-" nil ".bin")))
    (unwind-protect
        (progn
          (with-temp-file file
            (set-buffer-multibyte nil)
            (insert data))
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (call-process "openssl" file t nil
                          "dgst" "-sha1" "-hmac" key "-binary")
            (base64-encode-string (buffer-string) t)))
      (ignore-errors (delete-file file)))))

(defun zhihu-reader--image-media-type (file)
  (pcase (downcase (or (file-name-extension file) ""))
    ("png" "image/png")
    ("gif" "image/gif")
    ("webp" "image/webp")
    (_ "image/jpeg")))

(defun zhihu-reader-upload-image (file &optional source)
  "Upload local image FILE to Zhihu and return its remote URL.
SOURCE defaults to \"article\".  This is a write operation."
  (unless zhihu-reader-enable-write
    (user-error "写操作已关闭；请设置 `zhihu-reader-enable-write' 为 t"))
  (unless (file-readable-p file)
    (user-error "图片不存在: %s" file))
  (let* ((bytes (with-temp-buffer
                  (set-buffer-multibyte nil)
                  (insert-file-contents-literally file)
                  (buffer-string)))
         (hash (secure-hash 'md5 bytes nil nil nil))
         (media (zhihu-reader--image-media-type file))
         (resp (zhihu-reader--http-json
                "POST" "https://api.zhihu.com/images"
                (list :image_hash hash :source (or source "article")) nil))
         (json (plist-get resp :json))
         (status (plist-get resp :status)))
    (unless (memq status '(200 201))
      (error "图片申请失败 (HTTP %s): %s" status (plist-get resp :body)))
    (let* ((upload-file (or (zhihu-reader--alist-get 'upload_file json) json))
           (image-id (or (zhihu-reader--alist-get 'image_id upload-file)
                         (zhihu-reader--alist-get 'id upload-file)))
           (token (or (zhihu-reader--alist-get 'upload_token json)
                      (zhihu-reader--alist-get 'token json))))
      (unless image-id
        (error "知乎没有返回 image_id: %s" (plist-get resp :body)))
      (when token
        (when (equal media "image/gif")
          (error "暂不支持 GIF 分片上传；请使用 PNG/JPEG/WebP"))
        (let* ((date (format-time-string "%a, %d %b %Y %H:%M:%S GMT" nil t))
               (access-id (or (zhihu-reader--alist-get 'access_id token)
                              (zhihu-reader--alist-get 'accessId token)))
               (access-key (or (zhihu-reader--alist-get 'access_key token)
                               (zhihu-reader--alist-get 'accessKey token)))
               (access-token (or (zhihu-reader--alist-get 'access_token token)
                                 (zhihu-reader--alist-get 'accessToken token)))
               (resource (format "/zhihu-pics/v2-%s" hash))
               (source (mapconcat #'identity
                                   (list "PUT" "" media date
                                         (format "x-oss-date:%s" date)
                                         (format "x-oss-security-token:%s" access-token)
                                         "x-oss-user-agent:aliyun-sdk-js/6.8.0 Chrome 99.0.4844.84 on Windows 10 64-bit"
                                         resource)
                                   "\n"))
               (signature (zhihu-reader--hmac-sha1-base64 access-key source))
               (put-resp (zhihu-reader--http-binary
                          "PUT" (format "https://zhihu-pics-upload.zhimg.com/v2-%s" hash)
                          bytes
                          `(("Content-Type" . ,media)
                            ("x-oss-date" . ,date)
                            ("x-oss-user-agent" . "aliyun-sdk-js/6.8.0 Chrome 99.0.4844.84 on Windows 10 64-bit")
                            ("x-oss-security-token" . ,access-token)
                            ("Authorization" . ,(format "OSS %s:%s" access-id signature))))))
          (unless (and (>= (car put-resp) 200) (< (car put-resp) 300))
            (error "图片 OSS 上传失败 (HTTP %s): %s" (car put-resp) (cdr put-resp))))
        (let* ((status-resp (zhihu-reader--http-json
                             "PUT"
                             (format "https://api.zhihu.com/images/%s/uploading_status" image-id)
                             (list :upload_result "success") nil)))
          (unless (memq (plist-get status-resp :status) '(200 201 202 204))
            (error "图片上传状态提交失败 (HTTP %s)" (plist-get status-resp :status)))))
      (let ((url nil))
        (dotimes (_ 10)
          (unless url
            (let* ((r (zhihu-reader--http-json
                       "GET" (format "https://api.zhihu.com/images/%s" image-id) nil nil))
                   (j (plist-get r :json)))
              (setq url (or (zhihu-reader--alist-get 'src j)
                            (zhihu-reader--alist-get 'original_src j)
                            (zhihu-reader--alist-get 'watermark_src j)))
              (unless url (sleep-for 2)))))
        (or url (error "图片处理超时: %s" image-id))))))

(defun zhihu-reader--response-id (json)
  "Extract a content id from a publish response JSON."
  (let* ((data (zhihu-reader--alist-get 'data json))
         (result (zhihu-reader--alist-get 'result data))
         (publish (zhihu-reader--alist-get 'publish result)))
    (or (zhihu-reader--alist-get 'id publish)
        (zhihu-reader--alist-get 'id result)
        (zhihu-reader--alist-get 'id data)
        (zhihu-reader--alist-get 'id json))))

(defun zhihu-reader--publish-answer (answer-id question-id html)
  "Publish HTML as an answer to QUESTION-ID, updating ANSWER-ID if non-nil."
  (let* ((draft (if answer-id
                    (list :disabled 1 :isPublished t :contentId answer-id)
                  (list :disabled 1 :isPublished :json-false)))
         (body (list :action "answer"
                     :data (list :draft draft
                                 :question_id question-id
                                 :content html
                                 :comment_permission "all"
                                 :reprint_permission "allowed")))
         (resp (zhihu-reader--http-json
                "POST" "https://www.zhihu.com/api/v4/content/publish" body))
         (status (plist-get resp :status))
         (json (plist-get resp :json)))
    (unless (and (memq status '(200 201 202))
                 (equal (zhihu-reader--alist-get 'code json) 0))
      (error "回答发布失败 (HTTP %s): %s"
             status (or (plist-get resp :body) json)))
    (or (zhihu-reader--response-id json) answer-id)))

(defun zhihu-reader--create-article-draft (title html)
  "Create an article draft with TITLE and HTML, returning its id."
  (let* ((body (list :title title :content html :delta_time 30 :can_reward :json-false))
         (resp (zhihu-reader--http-json
                "POST" "https://zhuanlan.zhihu.com/api/articles/drafts" body))
         (id (zhihu-reader--response-id (plist-get resp :json))))
    (unless id
      (error "创建文章草稿失败 (HTTP %s): %s"
             (plist-get resp :status) (plist-get resp :body)))
    id))

(defun zhihu-reader--patch-article-draft (article-id title html)
  "Save TITLE and HTML to article draft ARTICLE-ID."
  (let* ((url (format "https://zhuanlan.zhihu.com/api/articles/%s/draft" article-id))
         (body (list :title title
                     :content html
                     :titleImage ""
                     :isTitleImageFullScreen :json-false
                     :delta_time 30
                     :table_of_contents :json-false
                     :can_reward :json-false))
         (resp (zhihu-reader--http-json "PATCH" url body)))
    (unless (memq (plist-get resp :status) '(200 201 202))
      (error "保存文章草稿失败 (HTTP %s): %s"
             (plist-get resp :status) (plist-get resp :body)))))

(defun zhihu-reader--publish-article-native (file)
  "Publish article source FILE using only this package's HTTP layer."
  (let* ((meta (zhihu-reader--article-source-meta file))
         (title (plist-get meta :title))
         (html (zhihu-reader--org-to-html
                (with-temp-buffer
                  (insert-file-contents file)
                  (buffer-string))))
         (article-id (plist-get meta :article-id)))
    (unless (and title (not (string-empty-p title)))
      (user-error "文章缺少 #+TITLE"))
    (unless (and article-id (not (string-empty-p article-id)))
      (setq article-id (zhihu-reader--create-article-draft title html))
      (zhihu-reader--write-article-id file article-id))
    (zhihu-reader--patch-article-draft article-id title html)
    (let* ((body (list :action "article"
                       :data (list :draft (list :disabled 1
                                                :isPublished t
                                                :id article-id)
                                   :title title
                                   :content html
                                   :comment_permission "all"
                                   :reprint_permission "allowed")))
           (resp (zhihu-reader--http-json
                  "POST" "https://www.zhihu.com/api/v4/content/publish" body)))
      (unless (memq (plist-get resp :status) '(200 201 202))
        (error "文章发布失败 (HTTP %s): %s"
               (plist-get resp :status) (plist-get resp :body))))
    article-id))

(defun zhihu-reader--publish-comment-native (root-kind root-id reply-id text)
  "Publish comment TEXT on ROOT-KIND/ROOT-ID, optionally REPLY-ID."
  (let* ((type (if (equal root-kind "article") "articles" "answers"))
         (url (format "https://www.zhihu.com/api/v4/comment_v5/%s/%s/comment"
                      type root-id))
         (content (concat "<p>"
                          (xml-escape-string (string-trim text))
                          "</p>"))
         (body (append (list :content content)
                       (when (and reply-id (not (string-empty-p reply-id)))
                         (list :reply_comment_id reply-id))))
         (resp (zhihu-reader--http-json "POST" url body))
         (json (plist-get resp :json)))
    (unless (memq (plist-get resp :status) '(200 201 202))
      (error "评论发布失败 (HTTP %s): %s"
             (plist-get resp :status) (plist-get resp :body)))
    json))

(defun zhihu-reader--article-source-meta (file)
  "Read article metadata from Org FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let (title article-id topics column-id banner)
      (when (re-search-forward "^#\+TITLE:[ 	]*\(.*\)$" nil t)
        (setq title (string-trim (match-string 1))))
      (when (re-search-forward "^#\+ZHIHU_ARTICLE_ID:[ 	]*\(.*\)$" nil t)
        (setq article-id (string-trim (match-string 1))))
      (when (re-search-forward "^#\+ZHIHU_TOPICS:[ 	]*\(.*\)$" nil t)
        (setq topics (condition-case nil
                         (json-parse-string (match-string 1) :array-type 'list)
                       (error nil))))
      (when (re-search-forward "^#\+ZHIHU_COLUMN_ID:[ 	]*\(.*\)$" nil t)
        (setq column-id (string-trim (match-string 1))))
      (when (re-search-forward "^#\+BANNER:[ 	]*\(.*\)$" nil t)
        (setq banner (string-trim (match-string 1))))
      (list :title title :article-id article-id :topics topics
            :column-id column-id :banner banner))))

(defun zhihu-reader--write-article-id (file id)
  "Write article ID into the Org source FILE."
  (with-current-buffer (find-file-noselect file)
    (goto-char (point-min))
    (if (re-search-forward "^#\+ZHIHU_ARTICLE_ID:.*$" nil t)
        (replace-match (concat "#+ZHIHU_ARTICLE_ID: " id))
      (goto-char (point-min))
      (insert (format "#+ZHIHU_ARTICLE_ID: %s
" id)))
    (save-buffer)))

(provide 'zhihu-reader-api)
;;; zhihu-reader-api.el ends here
