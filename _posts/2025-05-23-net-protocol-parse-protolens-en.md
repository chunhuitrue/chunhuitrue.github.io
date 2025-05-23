


          
---
layout: post
title: "Protocol Parsing in Network Traffic Processing (Part 7): Protolens"
date: 2025-05-23 15:00:00 +0800
categories: Technology
tags: Technology Protocol-Parsing
published: true
---

### Implementation
Combining the previous articles, all aspects of stream reassembly and protocol decoding are now working properly. Based on this, we can implement a complete protocol parsing library: [Protolens](https://github.com/chunhuitrue/protolens)

We limit this library to the following use cases:
1. As a library, it is called by conventional multi-threaded traffic processing engines.
2. The library itself doesn't need to be cross-thread capable, meaning each thread of the traffic processing engine has its own instance of the protocol parsing library.
3. Each five-tuple session, commonly known as a flow node, has a decoder. The decoder decodes for each connection.
4. The decoder itself doesn't have protocol identification capability. Protocol identification is provided by the traffic processing engine. While protocol parsing and protocol identification seem similar, combining these two functions would clearly be a wrong approach.
5. The decoder's input is packets, and the output is decoded protocol data.
6. The decoder returns protocol data to users through callback functions.

Furthermore, as a library, it should maximize the use of user program functionality to avoid redundancy. For example, packet decoding (IP addresses, TCP, UDP, ports, etc.) shouldn't be repeated within the library. Since the caller is a traffic processing engine, it should already have these capabilities. If the library were to do this again internally, it would create redundant work and waste performance. For this, the library can set up a trait for incoming packets, requiring callers to implement this trait. The library can then obtain packet information internally through this trait.

### Usage
[Protolens](https://github.com/chunhuitrue/protolens) is used for packet processing, TCP stream reassembly, protocol parsing, and protocol reconstruction scenarios. As a library, it's typically used in network security monitoring, network traffic analysis, and network traffic reconstruction engines.

Traffic engines usually have multiple threads, with each thread having its own flow table. Each flow node is a five-tuple. Protolens is based on this architecture and cannot be used across threads.

Each thread should initialize a [Protolens](https://github.com/chunhuitrue/protolens). When your flow table creates a new node for a connection, you should create a new task for this connection.

To get results, you need to set callback functions for each field of each protocol you're interested in. For example, after setting protolens.set_cb_smtp_user(user_callback), the SMTP user field will be called back through user_callback.

Afterward, for each incoming packet in this connection, you need to use the run method to add this packet to the task.

However, Protolens and tasks internally don't have protocol identification capability. At this point, although packets are passed into the task, the task hasn't started decoding. It will cache a certain number of packets, defaulting to 128. So you should tell the task what protocol this connection is through set_task_parser before exceeding the cached packets. After this, the task will start decoding and return the reconstructed content to you through callback functions.

Protolens will also be compiled as a C-callable shared object. The usage process is similar to Rust.

For specific usage, please refer to the rust_example directory and c_example directory. For more detailed callback function usage, you can refer to the test cases in smtp.rs.

You can get protocol fields through callback functions, such as SMTP user, email content, HTTP header fields, request lines, body, etc. When you get this data in callback functions, they are references to internal data. So, you can process them immediately at this time. But if you need to continue using them later, you need to make a copy and place it where you specify. You cannot keep references externally. Rust programs will prevent you from doing this, but in C programs as pointers, if you only keep pointers for subsequent processes, they will point to incorrect locations.

If you want to get the original TCP stream, there are corresponding callback functions. At this time, you get segments of raw bytes, but they are continuous streams after reassembly. They also have corresponding sequence numbers.

Suppose you need to audit protocol fields, such as checking if HTTP URLs meet requirements. You can register corresponding callback functions. In the function, make judgments or save them on flow nodes for subsequent module judgment. This is the most direct usage method.

The above only shows independent protocol fields like URL and host. Suppose you have this requirement: locate the URL position in the original TCP stream because you want to find what's before and after the URL. You need to do this:

Through the original TCP stream callback function, you can get the original TCP stream and sequence number. Copy it to a buffer you maintain. Through the URL callback function, get the URL and corresponding sequence. At this point, you can determine the URL's position in the buffer based on the sequence. This way, you can process things like what content is after and before the URL in a continuous buffer space.

Moreover, you can select data in the buffer based on sequence numbers. For example, if you only need to process data after the URL, you can delete the data before it from the buffer based on the URL's sequence. This way, you can process data after the URL in a continuous buffer space.

### Performance

#### Stream trait
In the initial version, Stream trait was implemented for internal reassembly structures. Simple and clear, but its next method returns byte by byte, although performance could meet conventional standards. However, byte-by-byte processing means calling the next function for each byte, which is clearly unnecessary.

Therefore, Stream trait was abandoned. Asynchronous execution is implemented in each read method. This way, when reading reassembled data, traversal can be completed in one buffer in a single function call. This greatly increases speed.

#### Packet trait
TCP packets can be out of order, so caching is needed for reassembly. Therefore, Protolens needs to obtain ownership of incoming packets. When packets are stored internally, data needs to be written.

If complete packets are written, in high-performance traffic processing engines, this means each packet needs to be copied once. This is clearly unreasonable. So initially, Protolens implemented a wrapper structure internally, used only to write pointers. But actually, this part is unnecessary. Because in typical traffic processing engines, high-performance clone attributes and wrapper structures will definitely be implemented for packets. Otherwise, the traffic processing engine itself can't efficiently process packets. Therefore, Protolens can directly accept user's Packets and write them internally as complete Packets. If users implement efficient wrapping, pointers are written. If users don't implement it, complete packets are written. This functionality depends on user implementation, avoiding redundancy.

#### mmchr
When searching for a line or segment of data in the buffer, mmchr brings noticeable performance improvements. The larger the buffer, the more obvious the effect.

#### Memory Pool
In actual testing, it was found that memory pool allocation in Protolens doesn't consume a large proportion of performance. Originally implemented with fixed-length arrays for memory pools, it was removed to simplify code. Now using simple Vec, just need to prevent Vec from growing dynamically.

#### Test Results
Below are the test results. Note that the Linux results are from a very old CPU, not from current mainstream high-performance CPUs.
Here, new_task is purely for creating a new decoder, not including the decoding process. Since the decoding process is line-by-line reading, the readline series is used to separately test the performance of reading one line, which best represents the decoding performance of protocols like HTTP and SMTP. Each line is 25 bytes, with 100 packets total. readline100 represents 100 bytes per packet, readline500 represents 500 bytes per packet. readline100_new_task represents creating a new decoder plus the decoding process. http, smtp, etc. are actual pcap packets. But smtp and pop3 are most representative because the pcaps in these test cases are completely constructed line by line. Others have size-based reading, so they're faster. When calculating statistics, bytes are used as the unit, counting only packet payload without packet headers.

| Test Item | mamini m4 | linux | linux jemalloc |
|----------|------------|--------|---------------|
| new_task | 3.1871 Melem/s | 1.4949 Melem/s | 2.6928 Melem/s |
| readline100 | 1.0737 GiB/s | 110.24 MiB/s | 223.94 MiB/s |
| readline100_new_task | 1.0412 GiB/s | 108.03 MiB/s | 219.07 MiB/s |
| readline500 | 1.8520 GiB/s | 333.28 MiB/s | 489.13 MiB/s |
| readline500_new_task | 1.8219 GiB/s | 328.57 MiB/s | 479.83 MiB/s |
| readline1000 | 1.9800 GiB/s | 455.42 MiB/s | 578.43 MiB/s |
| readline1000_new_task | 1.9585 GiB/s | 443.52 MiB/s | 574.97 MiB/s |
| http | 1.7723 GiB/s | 575.57 MiB/s | 560.65 MiB/s |
| http_new_task | 1.6484 GiB/s | 532.36 MiB/s | 524.03 MiB/s |
| smtp | 2.6351 GiB/s | 941.07 MiB/s | 831.52 MiB/s |
| smtp_new_task | 2.4620 GiB/s | 859.07 MiB/s | 793.54 MiB/s |
| pop3 | 1.8620 GiB/s | 682.17 MiB/s | 579.70 MiB/s |
| pop3_new_task | 1.8041 GiB/s | 648.92 MiB/s | 575.87 MiB/s |
| imap | 5.0228 GiB/s | 1.6325 GiB/s | 1.2515 GiB/s |
| imap_new_task | 4.9488 GiB/s | 1.5919 GiB/s | 1.2562 GiB/s |
| sip (udp) | 2.2227 GiB/s | 684.06 MiB/s | 679.15 MiB/s |
| sip_new_task (udp) | 2.1643 GiB/s | 659.30 MiB/s | 686.12 MiB/s |
        