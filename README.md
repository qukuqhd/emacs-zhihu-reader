# zhihu-reader.el

A Zhihu reader and writer for Emacs, modeled after:

- `lxwallac/zhihu-vscode-reader`: browser login + cookie session + feed API
- `zimya/zhihu_obsidian`: Web Viewer login + v3 feed API + Markdown
- `crispyChicken999/zhihu-fisher-vscode`: hot list, search, question/answer
  detail, comments and nested reading

## Configuration

Expansion defaults:

```elisp
(setq zhihu-reader-default-expand-questions nil) ; 默认折叠问题
(setq zhihu-reader-default-expand-answers nil)   ; 默认折叠回答
(setq zhihu-reader-default-expand-comments nil) ; 不自动展开评论
```

Comments in a separate buffer:

```elisp
(setq zhihu-reader-comments-separate-buffer t)
```

Then `c` on a comments heading opens `*Zhihu Comments*`; `c` on a
comment opens `*Zhihu Comment Replies*`.  `C-c C-o` always opens the
selected comments/replies in a separate buffer.

### All user options

| Variable | Default | Purpose |
|---|---|---|
| `zhihu-reader-cookie-db` | EAF cookies DB | Legacy cookie source |
| `zhihu-reader-cookie-file` | `~/.local/state/.../cookies` | Manual Cookie header file |
| `zhihu-reader-cookie-function` | nil | Custom Cookie provider |
| `zhihu-reader-login-browser` | `zen-browser` | Browser used by `zhihu-reader-login` |
| `zhihu-reader-browser-cookie-files` | Zen/Firefox globs | Cookie DB search paths |
| `zhihu-reader-user-agent` | browser UA | HTTP User-Agent |
| `zhihu-reader-feed-url` | v3 recommend URL | Recommendation feed endpoint |
| `zhihu-reader-hot-url` | hot list URL | Hot list endpoint |
| `zhihu-reader-search-url` | v4 search URL | Search endpoint |
| `zhihu-reader-search-sort` | nil | Optional search sort |
| `zhihu-reader-copy-link-include-title` | t | Prepend question/article title to copied links |
| `zhihu-reader-max-excerpt` | 1200 | Excerpt truncation length |
| `zhihu-reader-render-images` | t | Inline images |
| `zhihu-reader-image-cache` | `~/.cache/.../images` | Image cache directory |
| `zhihu-reader-image-width` | 500 | Inline image width |
| `zhihu-reader-auto-comments` | nil | Auto-load visible comments |
| `zhihu-reader-auto-comments-limit` | 10 | Max auto-loaded comment sections |
| `zhihu-reader-comment-order` | `score` | `score` or `time` |
| `zhihu-reader-default-expand-questions` | nil | Expand questions on render |
| `zhihu-reader-default-expand-answers` | nil | Expand answer bodies on render |
| `zhihu-reader-default-expand-comments` | nil | Expand loaded comments |
| `zhihu-reader-comments-separate-buffer` | nil | Open comments in a new buffer |
| `zhihu-reader-enable-write` | nil | Enable all write operations |
| `zhihu-reader-filter-seen` | t | Hide already shown recommendations |
| `zhihu-reader-seen-file` | `~/.local/state/.../seen.eld` | Seen IDs |
| `zhihu-reader-bookmarks-file` | `~/.local/state/.../bookmarks.eld` | Bookmarks |
| `zhihu-reader-history-file` | `~/.local/state/.../history.eld` | Reading history |
| `zhihu-reader-later-file` | `~/.local/state/.../later.eld` | Read later |
| `zhihu-reader-search-history-file` | `~/.local/state/.../search-history.eld` | Search history |
| `zhihu-reader-capture-file` | `~/org/inbox.org` | Org capture target |
| `zhihu-reader-article-directory` | `~/org/zhihu/articles` | New article sources |

## Files

The implementation is split into focused modules; `zhihu-reader.el` is the
entry point that loads them in dependency order.

```text
zhihu-reader.el            entry point
zhihu-reader-core.el       settings, cookies, HTTP, normalization
zhihu-reader-api.el        ZSE signing, write actions, publishing
zhihu-reader-ui.el         Org rendering, images, mode and key map
zhihu-reader-comments.el   comment trees, cursor paging and composition
zhihu-reader-edit.el       editing, drafts and article publishing
zhihu-reader-views.el      feed, hot, search, question/answer and navigation
```

All files must stay on the same `load-path`; requiring `zhihu-reader`
loads the rest.

## Architecture

```text
external Zen/Firefox login
  -> browser cookies.sqlite
  -> v3/v4 list and detail APIs
  -> nested Org tree
```

Login is external.  EAF is only an optional fallback for JavaScript-heavy
pages, not the login UI.

## Nested buffer

Recommendation and search results are grouped by question:

```text
* 问题标题
** 问题详情
** 回答 · author · N 赞
   answer body
*** 评论 (N)
** 另一个回答 · author
*** 评论 (N)
* 另一个问题
...
```

- `TAB` folds/unfolds a question or answer.
- `RET` on a question opens `*Zhihu Question*` with a separate
  `问题详情` child heading and its answer list.  The detail is fetched
  from the independent question endpoint when the answer API has no detail,
  so questions with zero answers still show their title/detail.
- `RET` on an answer opens `*Zhihu Answer*` with the full answer body.
- Content HTML is converted to Org rather than plain text: paragraphs, bold,
  italic, lists, code blocks, quotes, links, Zhihu formulas and tables are
  preserved where possible.
- `c` on an answer/comments heading loads comments; on a comment heading it
  loads child replies.
- Answer and comment headings show relative time (`3小时前`, `6天前`).
- Comments are inserted as `**** author · votes · relative-time` children.
- Pressing `c` on a comment heading loads its child replies as `*****` entries.
  Child replies also use `paging.next` cursor pagination.
- Root comments use the `comment_v5` API with cursor-based `paging.next` /
  `paging.previous` pagination, so pages no longer depend on numeric offsets.
- `C-c o` changes root comment order (`score` / `time`);
  `C-c l` likes or unlikes a comment.
- `]` / `[` move to the next/previous page.  Previously visited pages are
  served from an in-memory page cache; only `g` forces a network refresh.
- `e` edits the answer/comment at point in `*Zhihu Edit*`.
- In the edit buffer: `C-c C-c` preview, `C-c C-p` publish, `C-c C-k` cancel.

> Writing is disabled by default.  Set `zhihu-reader-enable-write` to t and
> confirm each action.  Publishing uses this package's own HTTP and built-in
> Org HTML exporter; no external Zhihu package is required.

- Inline images are enabled by default and load lazily when a subtree is
  expanded; a spinner appears in the mode line while downloads are active.  They are downloaded asynchronously to
  `~/.cache/zhihu-reader/images` and replace remote Org links with `file:`
  links when ready.  `i` ensures the current buffer's images are loaded;
  `C-u i` turns inline images off.
- `C-c ]` / `C-c [` load the next/previous root comment page using the
  server-provided cursor.  Child replies support both directions too.
- `C-c v` upvotes (`C-u` downvotes) the answer/article at point.
- `C-c f` follows/unfollows the author, question or topic at point.
- `C-c t` thanks or un-thanks an answer.
- `C-c F` adds an answer/article to a collection by collection ID.
- `C-c b` bookmarks the current answer/article; `C-c B` opens the bookmark
  list.  `C-c m` / `C-c u` mark read/unread.
- `w` or `C-c w` copies a canonical share link for the current question,
  answer, article or comment (`🔗`).  By default the top-level question or
  article title is prepended; set `zhihu-reader-copy-link-include-title`
  to nil for link-only copying.
- Question descriptions are fetched with a native ZSE v4 signature.
- Recommendation items are deduplicated by question/article ID.  Seen IDs
  persist in `~/.local/state/zhihu-reader/seen.eld`; clear them with
  `M-x zhihu-reader-clear-seen` or disable filtering with
  `(setq zhihu-reader-filter-seen nil)`.
- `C-c c` opens `*Zhihu Comment*` for a new comment/reply.
  `C-c C-p` publishes it; `C-c C-k` cancels.  This is a beta endpoint and
  requires `zhihu-reader-enable-write`.
- Answer and comment headings show relative times (for example `3小时前`),
  not raw epoch timestamps.
- `]` / `[` also page collections, topics and the following feed.  The
  following feed uses the server-provided `paging.next` / `previous` cursor
  and loads asynchronously.  Collection pagination stops at the last page.

## Zhihu links inside buffers

`RET` and `o` on a Zhihu link open it inside Emacs instead of sending it to
an external browser:

```text
/search?q=...                  -> *Zhihu Search*
/search?...&content_type=article&content_id=... -> *Zhihu Article*
/search?...&content_type=answer&content_id=...  -> *Zhihu Answer*
/search?...&content_type=question&content_id=... -> *Zhihu Question*
/question/<id>/answer/<id>     -> *Zhihu Answer*
/question/<id>                 -> *Zhihu Question*
/answer/<id>                   -> *Zhihu Answer*
/p/<id>                        -> *Zhihu Article*
/people/<token>                -> author view
/topic/<id>                    -> topic view
/collection/<id>               -> collection view
/hot                           -> hot list
```

Search and article views use dedicated buffers; existing question/answer
views already use their own buffers.  Non-Zhihu links keep the
EAF/Readability fallback.

## Commands

Global keys from `custom-post.el`:

```text
C-c Z   recommendation feed
C-c H   hot list
C-c S   search
C-c C-Z read URL
C-c G   following feed
C-c T   topic feed
C-c L   collection
C-c M   notifications
```

```elisp
M-x zhihu-reader-login     ; external Zen/Firefox sign-in
M-x zhihu-reader-feed      ; recommendation feed
M-x zhihu-reader-hot       ; hot list
M-x zhihu-reader-search    ; general search
M-x zhihu-reader-search-people ; search users
M-x zhihu-reader-search-topics ; search topics
M-x zhihu-reader-question  ; question + answer list
M-x zhihu-reader-answer    ; answer + comments
M-x zhihu-reader-article   ; article in its own buffer
M-x zhihu-reader-user      ; user profile
M-x zhihu-reader-notifications ; notifications
M-x zhihu-reader-user-followers ; followers list
M-x zhihu-reader-user-followees ; following-users list
M-x zhihu-reader-user-columns ; user column contributions
M-x zhihu-reader-column ; column articles
M-x zhihu-reader-user-articles ; user articles
M-x zhihu-reader-user-pins ; user pins
M-x zhihu-reader-collections ; list user's collections
M-x zhihu-reader-create-collection ; create collection
M-x zhihu-reader-delete-collection ; delete collection
M-x zhihu-reader-notification-timeline ; categorized notifications
M-x zhihu-reader-insert-uploaded-image ; upload a local image into Org
M-x zhihu-reader-search-sort ; change search sort
M-x zhihu-reader-search-history ; search history
M-x zhihu-reader-store-link ; store Org link
M-x zhihu-reader-capture ; append to Org inbox
M-x zhihu-reader-open-zhihu-url ; route a Zhihu URL internally
M-x zhihu-reader-read-url  ; EAF + Readability fallback
M-x zhihu-reader-edit-at-point ; edit answer/comment in an Org buffer
M-x zhihu-reader-new-article ; create an article Org source file
M-x zhihu-reader-publish-article ; publish the current article source
M-x zhihu-reader-compose-comment ; write a new comment/reply
M-x zhihu-reader-following ; following feed
M-x zhihu-reader-topic ; topic hot feed
M-x zhihu-reader-collection ; collection items
M-x zhihu-reader-bookmark ; bookmark current content
M-x zhihu-reader-bookmarks ; show bookmarks
(setq zhihu-reader-enable-write t) ; required before writing
M-x zhihu-reader-set-cookie
M-x zhihu-reader-cookie-status
```

## Cookie sources

Priority:

1. `zhihu-reader-cookie-function`
2. `zhihu-reader-cookie-file` (mode 0600)
3. newest Firefox/Zen `cookies.sqlite` containing `z_c0`
4. EAF cookie database (legacy fallback)

For Chrome/Chromium, copy the Cookie header from DevTools and run
`M-x zhihu-reader-set-cookie`.

## Keys in the reader buffer

```text
TAB     fold/unfold
RET     enter question/answer detail
c       load comments
C-c ]   next comments page (cursor)
C-c [   previous comments page (cursor)
C-c o   comment sort order
C-c l   like/unlike comment
C-c d   delete comment
C-c C-r mark notifications read
C-c v   upvote/downvote
C-c f   follow/unfollow
C-c t   thank answer
C-c F   add to collection
C-c b   bookmark
C-c B   bookmark list
C-c m/u mark read/unread
C-c C-h reading history
C-c C-d remove from current collection
C-c C-l add to read later
C-c C-L read later list
C-c +/-/0 zoom
] / [   next/previous page
i       toggle inline images
g       refresh current view
n/p     next/previous heading
o       open current URL with EAF + Readability
w       copy share link（🔗）
r       recommendation feed
h       hot list
s       search
S       answer sort order
e       edit answer/comment
q       quit
?       help
```


## License

GPL-3.0-or-later. See `LICENSE`.
