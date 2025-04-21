---
layout: post
title: "Protocol Parsing in Network Traffic Processing (Part 1): Traffic Processing Model"
date: 2024-03-03 13:00:00 +0800
categories: Technology
tags: Technology Protocol Parsing
published: true
---

The network traffic processing discussed here includes common systems such as IDS, IPS, NTA, NDR, XDR, DPI, network behavior auditing, firewalls, and other operations that require judgment and auditing of traffic at the packet level. If only simple processing such as traffic statistics is needed, simple increment operations are sufficient. However, for more accurate and in-depth analysis and judgment, decoding of these data is required—from IP and TCP all the way up to application layers like SMTP. For example, to analyze and audit the content of the SMTP protocol, you need to parse the SMTP protocol.

Traffic processing engines such as IDS and IPS have different focuses, so their requirements for protocol parsing also vary. Generally speaking, it is better to extract protocol fields completely, accurately, and in detail, such as the subject, recipient, and sender of an email. There are various ways to implement parsing and extraction, but the implementation of the parsing part is influenced by the traffic processing model. Here, the implementation of protocol parsing is based on the common processing model. Therefore, let's briefly describe this processing model first. There may be other processing models, in which case the protocol parsing methods discussed here may not apply. Typically, the traffic processing model consists of the following parts: for each captured packet, each function is called in sequence, meaning the packet passes through each module one by one, and each module is triggered by the packet.

#### Packet Capture

Packets are obtained from the system. Performance and similar concerns are not discussed here. The focus is on the fact that the captured packets are at the network layer—they are scattered, individual IP packets that do not provide any convenience for protocol parsing unless you only parse based on single packets. Although this part solves the fundamental problem of obtaining packets from nothing, it only provides a simple perspective: individual packets that are random, disordered, completely out of sequence, and may even be corrupted or erroneous.

#### Decoding

This decoding is limited to the decoding of TCP/IP headers and does not yet involve application layer protocol parsing. The packets provided by the previous packet capture function are just raw data, i.e., a buffer, without any structural information. Therefore, TCP/IP parsing is needed, at least to obtain information such as the source and destination IP and port (the five-tuple).

#### Flow Table

With the TCP/IP information of the packets, the flow table classifies individual packets according to connections. Packets belonging to the same connection are associated with the same connection node. This node can store information related to the connection, such as the number of packets for each connection. The flow table is a hash table that uniquely identifies nodes using the five-tuple. For each randomly arriving packet, the corresponding hash node is found based on the packet's IP and port five-tuple, and this node records data related to the connection, such as packet statistics. You certainly can't count packets from connection A under connection B; otherwise, both protocol parsing and even billing would be incorrect. After processing by the flow table, packets now have the perspective of a connection.

#### Protocol Identification

Protocol identification determines what protocol a connection uses—whether it's email, web, etc. Internally, this function can be complex, but its external output is simple: it tags a connection. With the support of the flow table, it does not need to tag each packet with a protocol type; it only needs to tag the connection node in the flow table. Protocol parsing also relies on this tag; only when a connection is marked as SMTP can SMTP protocol parsing begin. Otherwise, there is no way to proceed. In practice, protocol parsing and protocol identification may not be strictly sequential; protocol identification may also depend on the content parsed by protocol parsing. For example, after identifying HTTP, HTTP parsing is performed to extract the URI field from the HTTP header. The URI content is then sent back to the protocol identification module for further determination, such as identifying that the connection is a chat application based on HTTP. However, we will not focus on these details here and will only discuss the protocol parsing process.

Now, the basic conditions required for protocol parsing have been met. The connection can enter the protocol parsing module for parsing. If it is an SMTP protocol, parsing is performed according to the SMTP specification; if it is a POP protocol, parsing is performed according to the POP specification...