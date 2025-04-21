---
layout: post
title: "tmux, emacs, and the Clipboard"
date: 2024-03-09 20:00:00 +0800
categories: Technology
tags: Technology emacs
published: true
---

Since development environments are often on servers, or even if set up locally, they are usually in virtual machines, which is essentially a remote environment. Although local emacs can access remote files, I am more accustomed to installing emacs directly on the server, then logging in via ssh and using it in TUI mode. I also open a tmux session to keep my work persistent.

Sometimes, I need to copy some content from emacs to my local machine, such as a warning message to search for, or a segment of logs. In these cases, the content needs to traverse several layers: from emacs to tmux, to ssh, to the local terminal, and finally to the local clipboard. The configuration is actually quite simple, but it took me a while to figure it out...

You need to install this plugin in emacs:
```lisp
(use-package clipetty
  :ensure t
  :hook (after-init . global-clipetty-mode)
  )
```

At the same time, you need the following configuration in tmux:
```tmux.conf
set -s set-clipboard on
set -ag update-environment "SSH_TTY"
```
Of course, it's best to enable mouse support in tmux as well, so you can select text with the mouse:
> set -g mouse on

Finally, your terminal must support this feature. Both Windows Terminal and iTerm2 work fine.

With this setup, when you ssh from macOS using iTerm2 to a Linux server, open a tmux session, and run emacs in that session, you can select text in emacs and paste it into any application on your local macOS.

If you also use emacs inside tmux locally on macOS, the above configuration may not work as expected. The solution is as follows:

- Install reattach-to-user-namespace
> brew install reattach-to-user-namespace

- Add the following configuration to emacs:
```lisp
(when (eq system-type 'darwin) 
  (defun copy-from-osx ()
    "Use OSX clipboard to paste."
    (shell-command-to-string "reattach-to-user-namespace pbpaste"))

  (defun paste-to-osx (text &optional push)
    "Add kill ring entries (TEXT) to OSX clipboard.  PUSH."
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "reattach-to-user-namespace" "pbcopy")))
	(process-send-string proc text)
	(process-send-eof proc))))

  (setq interprogram-cut-function 'paste-to-osx)
  (setq interprogram-paste-function 'copy-from-osx))
```

With this, the experience on local macOS will be the same as with the remote setup.

References:

[clipetty](https://github.com/spudlyo/clipetty)
[tmux wiki](https://github.com/tmux/tmux/wiki/Clipboard)

```