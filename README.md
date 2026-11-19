# zhihu-reader.el

A Zhihu reader and writer for Emacs, modeled after:

- `lxwallac/zhihu-vscode-reader`: browser login + cookie session + feed API
- `zimya/zhihu_obsidian`: Web Viewer login + v3 feed API + Markdown
- `crispyChicken999/zhihu-fisher-vscode`: hot list, search, question/answer
  detail, comments and nested reading

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
** 回答 · author · N 赞
   answer body
*** 评论 (N)
** 另一个回答 · author
*** 评论 (N)
* 另一个问题
...
```

- `TAB` folds/unfolds a question or answer.
- `RET` on a question opens `*Zhihu Question*` with its answer list.
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
  server-provided cursor.
- `C-c v` upvotes (`C-u` downvotes) the answer/article at point.
- `C-c f` follows/unfollows the author, question or topic at point.
- `C-c t` thanks or un-thanks an answer.
- `C-c F` adds an answer/article to a collection by collection ID.
- `C-c b` bookmarks the current answer/article; `C-c B` opens the bookmark
  list.  `C-c m` / `C-c u` mark read/unread.
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
```

```elisp
M-x zhihu-reader-login     ; external Zen/Firefox sign-in
M-x zhihu-reader-feed      ; recommendation feed
M-x zhihu-reader-hot       ; hot list
M-x zhihu-reader-search    ; search
M-x zhihu-reader-question  ; question + answer list
M-x zhihu-reader-answer    ; answer + comments
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
C-c v   upvote/downvote
C-c f   follow/unfollow
C-c t   thank answer
C-c F   add to collection
C-c b   bookmark
C-c B   bookmark list
C-c m/u mark read/unread
] / [   next/previous page
i       toggle inline images
g       refresh current view
n/p     next/previous heading
o       open current URL with EAF + Readability
w       copy link
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
