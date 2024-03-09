---
layout: post
title: "tmux，emacs和剪切板"
date: 2024-03-09 20:00:00 +0800
categories: 技术
tags: 技术 emacs
published: true
---

因为开发环境很多时候都是在服务器上，即便在本地搭建，也虚拟机，这也相当于是远程环境。虽然本地emacs也可以访问远程文件，不过我比较习惯在服务器上安装emacs，然后ssh登陆上去在TUI下用。然后再开一个tmux保持会话。

有时候需要在emacs中copy一些内容到本地，比如一段警告需要搜一下。或者一段日志之类。这时候这段内容就需要一个穿越过程：emacs到tmux到ssh到本地终端到本地剪切板。配置起来其实很简单，但配置之前却找了半天...

emacs上需要安装这个插件：
```lisp
(use-package clipetty
  :ensure t
  :hook (after-init . global-clipetty-mode)
  )
```

同时tmux中需要配合的配置如下：
```tmux.conf
set -s set-clipboard on
set -ag update-environment "SSH_TTY"
```
当然tmux最好也打开鼠标支持，这样就可以用鼠标选择文本：
> set -g mouse on

最后终端的term也需要支持，windows terminal和iterm2没有问题。

至此，当你在macos中通过iterm2 ssh到一台linux服务器上，打开了一个tmux会话，并在这个会话中运行emacs，在emacs中选择了一段文本。你可以粘贴到本地macos的应用中了。

如果在macos的本地也在tmux中开emacs使用，以上的配置会遇到问题。解决办法如下：

- 安装reattach-to-user-namespace
> brew install reattach-to-user-namespace

- 为emacs增加一段配置：
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

这样，在macos本地的效果就和远程方式一样了。


参考：

[clipetty](https://github.com/spudlyo/clipetty)
[tmux wiki](https://github.com/tmux/tmux/wiki/Clipboard)
