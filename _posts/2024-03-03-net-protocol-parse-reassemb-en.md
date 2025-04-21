---
layout: post
title: "Protocol Parsing in Network Traffic Processing (Part 2): Stream Reassembly"
date: 2024-03-03 14:00:00 +0800
categories: Technology
tags: Technology Protocol Parsing
published: true
---

#### Assumptions About Data

Starting protocol parsing directly on packets is not impossible; sometimes, it can even be simpler. For example, if we want to parse the SMTP `mailfrom` command, it is almost always contained completely within a single packet during email communication. Even email headers with more characters are often like this. However, we cannot assume that they will always be complete in a single packet, because the TCP protocol does not guarantee this. Although most email server implementations behave as described above, if you intentionally want to evade traffic detection and reassembly, you can manually telnet to the server and send one character at a time, causing the `mailfrom` command to be split across multiple packets.

Additionally, there can be issues such as packet reordering, retransmission, and packet loss.

Therefore, the above assumption is incomplete. In special cases, parsing will fail.

#### TCP Reassembly

To avoid the shortcomings mentioned above, TCP reassembly is required, i.e., reconstructing the data according to the TCP protocol to restore it to its original sent and received state. This provides the parser with a complete, accurate, and concise view. The reassembly module shields the parser from TCP layer issues such as packet loss, retransmission, and disorder. The data seen by the parser is not individual packets, but a stream, just like what the mail server client sees.

There is a simple way to implement reassembly: you can save all the packets of a connection until the connection ends, then arrange them in order according to their sequence numbers into a buffer. This buffer is then handed over to the parser to parse the mail protocol, just like processing a file.

This implementation is the simplest and most straightforward. However, it has some problems:

- First, you must wait until the connection ends before parsing can begin. If only partial or incomplete data is available, the parser cannot work. The buffer's end boundary may be right in the middle of the `mailfrom` command, causing the parser to fail midway or require repeated scanning.

- Second, this approach requires a lot of memory. If the traffic processing program needs to handle a large number of connections, the memory consumption will be unacceptable.

- Another problem is that if you wait until the connection ends to start parsing, any actions that need to be taken based on extracted fields (such as blocking the connection) will be too late.

Therefore, a more suitable reassembly method is needed.

#### Streamed Reassembly and Decoding

If you can avoid caching all data until the connection ends, and instead reassemble, decode, and release packets as they arrive—consuming the received data in a timely manner—you can avoid the problem of excessive memory usage caused by caching entire connections.

Let's consider this approach with a simple protocol. SMTP is the most suitable example, as its name literally means "Simple Mail Transfer Protocol." At its core, this protocol is line-based, with each command, header, and content ending with `\r\n`. If we consume and release each complete line as soon as it is received, there is no need to cache all data—at most, only an incomplete line needs to be cached. The number of characters in a line is limited, for example, 78. Other text-based protocols are similar, and for binary protocols, you can compare with the TCP/IP header, which is also a fixed-size structure. In short, you do not need to cache the entire stream to start parsing; you only need to cache the smallest incomplete unit (a line, or a structure size) to begin protocol parsing. Once a line is parsed, it is immediately released. This way, memory usage is minimized, and data can be parsed and acted upon in real time. All the shortcomings of the previous approach are avoided.

An SMTP example is as follows:
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
#### Implementation of Reassembly
Since line-based stream reassembly and decoding are possible, a reassembly implementation can be designed for this purpose. The goal is to provide the parser with data line by line. You only need to provide an API like this:
```
get_line(node, line, len)
```
Here, node is the flow table node for the connection. As mentioned earlier, we can store connection-related data in the flow table node. The line is stored here as well. line is a complete line of data, and len is the length of that line. If the data in the incoming packets is not enough to form a complete line, this API returns -1; if a complete line is available, it returns 0. In this way, the parser is provided with a line-based view of the data, shielding it from TCP-level complexities.

Of course, internally, it still needs to buffer packets in case of out-of-order delivery, which is unavoidable. Each time this function is called, it copies data from the packets until it reaches \r\n . If a packet is fully read, it is immediately released. If the current data is insufficient, it returns failure and waits for the next packet to arrive before being called again. At the same time, the node stores the incomplete line. This way, there is no need to buffer the entire connection's data—only enough memory for a single line is needed. Sorting packets by sequence number is, of course, also necessary, but need not be discussed in detail here.

In this way, a single API solves the key problem of streamed reassembly and decoding...