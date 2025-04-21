---
layout: post
title: "emacs Code Navigation"
date: 2024-07-26 20:00:00 +0800
categories: Technology
tags: Technology emacs
published: true
---

This is a summary and record of the tools and methods I use in emacs for editing and browsing code. I use these in my daily development projects. Some of the shortcuts are default, while others are custom; if you are not used to them, you can redefine them as needed.

I have uploaded my emacs configuration directory to: [gitee](https://gitee.com/chunhui_true/dot.emacs.d) and [github](https://github.com/chunhuitrue/dot.emacs.d). You can use it directly as your .emacs.d. It works for both emacs 28 and 29.

#### Online Code Search

You can search code directly here, though I rarely use it.
[grep.app](https://grep.app/) and [sourcegraph](https://sourcegraph.com/search)

#### Project File Search

* You can use command-line tools outside of emacs:

  fd, find, locate, which

* In emacs, you can use helm-projectile

The most commonly used is helm-projectile-find-file (C-c p f) to search for files.
For example, to quickly locate the file "plugin/reassemble/app_smtp.c":
> After pressing C-c p f, just type smtp.c, then press space to locate it.

For stepwise narrowing, type:
> plug (space) reas (space) smtp

This way, you don't need to look through the list file by file; you can quickly locate and open files via fuzzy search. Other useful commands include:
> helm-projectile-find-file-dwim

> helm-projectile-find-dir

> helm-projectile-recentf

All these commands can be found via the C-c p menu, which also shows their shortcuts.

#### Code Navigation

##### global

global can generate gtags, allowing emacs to search code based on index files. It's more convenient when used with helm-gtags. You need to run gtags in the code directory first to generate the index files before searching. For example:

* Jump to definition:
> helm-gtags-dwim  M-.

* Find references to a function or variable:
> helm-gtags-find-rtag M-r

* Return after jumping:
> helm-gtags-pop-stack M-,

* Project-wide search:
> helm-gtags-select C-c h

##### lsp

global is sufficient for C language and is simple to install with few dependencies. But nowadays, lsp is even better and supports more languages. For some new languages, global's indexing is not as effective.

* Jump to the definition at the cursor:
> xref-find-definitions M-.

* Find references at the cursor:
> lsp-find-references M-r

* Return after jumping:
> xref-go-back M-,

* Search for functions and variables within the buffer:
> helm-imenu C-c i
>
> lsp-ui-imenu M-j  (no fuzzy search)
>
> lsp-treemacs-symbols C-c C-c S  (no fuzzy search)

* Search for functions and variables project-wide:
> helm-lsp-workspace-symbol  C-c x i

* Search for references project-wide:
> lsp-find-references M-r
>
> lsp-treemacs-references
>
> lsp-ui-peek-find-references C-c C-p r

* Call hierarchy:
> lsp-treemacs-call-hierarchy C-c C-c h

* Jump back within the buffer

After jumping to a position with imenu, C-u C-SPC will return to the previous position. This only works within the current buffer using imenu.
You can temporarily mark a position with C-SPC, and after moving elsewhere, use C-u C-SPC to jump back. If you mark multiple positions, you can return in reverse order, also only within the current buffer.

#### Keyword Search

* Search for keywords within the buffer:
> isearch C-s
>
> There are also isearch series searches in the M-s menu

* Faster keyword search in the current buffer:
> helm-occur   C-c o or C-x c M-s o  (interactive)
>
> occur  M-s o  (lists all matches, allows preview, but does not auto-fill the keyword when running the command)

* Highlight keywords in the buffer:
> isearch C-s
>
> isearch-forward-symbol-at-point M-s .   Highlights the current "thing" at the cursor (regardless of position), faster than C-s, and is syntax-aware for variables
>
> isearch-forward-thing-at-point  M-s M-. Highlights the current "thing" at the cursor (regardless of position), faster than C-s, with an extra M key
>
> symbol-overlay  M-i  Highlights symbols within the function scope. Can lock, cancel, and highlight multiple symbols.


* Search for keywords in files:
> rg (ripgrep). Powerful and fast

```