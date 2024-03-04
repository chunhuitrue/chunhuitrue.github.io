---
layout: post
title: "网络流量处理中的协议解析:流重组"
date: 2024-03-03 14:00:00 +0800
categories: 技术
tags: 技术 协议解析
published: true
---

#### 对数据的假设

在数据包基础上开始协议解析不是不可以，甚至有时候还更简单。比如我们想解析SMTP的maifrom命令，它几乎在所有邮件通信过程中都完整地在一个单独的包中。甚至字符数量更多的邮件头也是如此。但是却不能作出它们一定就完整地在一个包中的假设，因为TCP协议并没有这种要求。虽然在绝大多数邮件服务器的实现中都会表现如上面描述，但是如果你想故意逃避被流量检测程序还原你的邮件通信，你完全可以手动telnet上去，然后一个字符一个字符地发送，使mailfrom分散在多个包中。

此外还会有数据包乱序重传丢包等情况。

所以上面的假设是不完整的。遇到特殊情况会解析失败。

#### TCP重组

为避免上述的的缺点，需要对TCP作重组，也就是按照TCP协议来把数据还原成原始发送和接收的状态。给解析工作提供一个完整准确简洁的视角。重组模块把丢包重传乱等TCC层的问题序屏蔽掉。解析器看到的数据不是数据包而是和邮件服务器客户端看到的一样，是一条流。

重组的方式有个简单办法。可以把链接的数据包都保存下来等待链接结束，然后按照序列号的顺序放到一个buff中。然后提交给解析器在这个buffer中解析邮件协议。就好像处理一个文件一样。

这样的实现最简单直接。但是这样会有一些问题：

- 一是必须等待链接结束才可以开始解析，因为如果只有部分不完整的数据解析器无法工作，buffer的结束边界可能正好在mailfrom这个命令的中间，解析器会中途失败或者需要重复扫描。

- 再一个就是这样会需要很多的内存。如果流量处理程序需要处理大量链接，那内存消耗会无法接受。

- 还有一个问题，就是等链接结束才开始解析，如果需要根据提取出来的字段作出针对链接的动作（比如阻断），为时已晚。

所以需要更合适的重组方式。

#### 流式重组和解码

如果可以不缓存所有数据等到链接结束，边收数据包，边重组，边解码，边释放数据包，把收到的数据及时消耗掉，那就可以避免上面的缓存完整链接占用太多内存的问题。

找一个简单的协议考虑一下这种方式，未必不可行。要说简单的协议没有比SMTP更合适的了，毕竟它的名字就叫简单邮件协议。最基本的，这个协议就是以\r\n结束的行为单位。构造命令，邮件头和邮件内容。如果我们在收到完整的一行的数据后就及时把这一行消耗并释放掉。那就不需要缓存所有数据，最多只需要缓存一个不完整的行就可以了。而且这个行的字符数有限，比如78。其他基于文本的协议也类似，其他二进制结构的协议可以对比TCP/IP的头，也都是固定大小的结构体。总之，并不需要缓存完整的流才能开始解析，只需要缓存最小的不完整的一个单位（一行，或者一个结构体大小）就可以开始协议解析了。解析完成一行，立刻释放这一行。这样就不会占用太多的内存。并且可以实时解析提取数据，立刻根据数据作出动作。前面方式的缺点都可以避免。

SMTP的例子如下：
```plaintext
220 smtp.qq.com Esmtp QQ QMail Server
EHLO ABC01234567
250-smtp.qq.com
250-PIPELINING
250-SIZE 73400320
250-STARTTLS
250-AUTH LOGIN PLAIN
250-AUTH=LOGIN
250-MAILCOMPRESS
250 8BITMIME
AUTH LOGIN
334 VXNlcm5hbWU6
dXNlcjEyMzQ1QGV4YW1wbGUxMjMuY29t
334 UGFzc3dvcmQ6
MTIzNDU2Nzg=
235 Authentication successful
MAIL FROM: <user12345@example123.com> SIZE=300
250 Ok
RCPT TO: <user12345@example123.com>
250 Ok
DATA
354 End data with <CR><LF>.<CR><LF>
Date: Mon, 27 Jun 2022 17:01:55 +0800
From: "user12345@example123.com" <user12345@example123.com>
To: =?GB2312?B?wO60urvU?= <user12345@example123.com>
Subject: biaoti
X-Priority: 3
X-Has-Attach: no
X-Mailer: Foxmail 7.2.19.158[cn]
Mime-Version: 1.0
Message-ID: <202206271701548584972@example123.com>
Content-Type: multipart/alternative;
	boundary="----=_001_NextPart572182624333_=----"

This is a multi-part message in MIME format.

------=_001_NextPart572182624333_=----
Content-Type: text/plain;
	charset="GB2312"
Content-Transfer-Encoding: base64

aGVsbG8gZGRkZGRkZGRkZGRkZGRkZGRkaGVsbG8gZGRkZGRkZGRkZGRkZGRkZGRkaGVsbG8gZGRk
ZGRkZGRkZGRkZGRkZGRkaGVsbG8gZGRkZGRkZGRkZGRkZGRkZGRkaGVsbG8gZGRkZGRkZGRkZGRk
ZGRkZGRkZGRkZGRkaGVsbG8gZGRkZGRkZGRkZGRkZGRkZGRkaGVsbG8gZGRkZGRkZGRkZGRkZGRk
ZGRkaGVsbG8gZGRkZGRkZGRkZGRkZGRkZGRkaGVsbG8gZGRkZGRkZGRkZGRkZGRkZGRkDQoNCg0K
DQo=

------=_001_NextPart572182624333_=----
Content-Type: text/html;
	charset="GB2312"
Content-Transfer-Encoding: quoted-printable

<html><head><meta http-equiv=3D"content-type" content=3D"text/html; charse=
t=3DGB2312"><style>body { line-height: 1.5; }body { font-size: 14px; font-=
yle></head><body>=0A<div><span></span>hello dddddddddddddddddd<span style=
=3D"#b5c4df" size=3D"1" align=3D"left">=0A<div><span></span></div>=0A</bod=
y></html>
------=_001_NextPart572182624333_=------

.
250 Ok: queued as 
QUIT
221 Bye
```

#### 重组的实现方式

既然可以基于行的方式来实现流式重组和解码，那可以为此设计一个重组的实现。目的是为解析器一行一行地提供数据，供解析器使用。那只需要提供这样一个API：
> get_line(node, line, len) 

其中node就是这条链接所在流表的节点，前面说了，我们可以在流表的节点中保存本链接相关的数据。line就保存在这里。line是完整一行的数据，len是这一行的长度。这个API如果到来的数据包中不足以凑成一个完整的行，那就返回-1，如果凑够了就返回0。这样，就给解析器提供了一个行数据的视角，它屏蔽了TCP层的麻烦事儿。

当然，它内部在数据包乱序的情况下仍然需要缓存数据包，这是无法避免的。每次调用这个函数，就从数据包中copy数据，直到\r\n结束。如果数据包被读完，就立刻释放掉这个数据包。如果当前数据不足，就返回失败，等到下次数据包到来被调用。同时node上保存着不完整的line。这样就不需要缓存整个链接的数据，仅需要占用一个line大小的内存即可。根据序列号对数据包排序之类自然也是必须，不必细说。

这样，一个API，就解决了流式重组流式解码的关键问题...
