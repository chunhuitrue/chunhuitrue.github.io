---
layout: post
title: "Protocol Parsing in Network Traffic Processing (Part 6): Executor"
date: 2025-04-20 14:20:00 +0800
categories: Technology
tags: Technology Protocol Parsing
published: true
---

#### Executor

There are many asynchronous execution environments in Rust. However, asynchronous environments have a problem: they are "contagious." If a function is asynchronous, all its callers must also be asynchronous. The previous traffic processing model is not asynchronous. Therefore, to prevent the asynchronous nature of protocol parsing from affecting the traffic processing model, we need our own executor. This executor encapsulates asynchronous operations and is then used within the traffic processing model. Minimizing the impact on users' habits and models is the hallmark of a good library.

Simply put, Rust's executor is similar to the state machine driver function described earlier in C. If a future returns Ready, it means the future has completed and the next step should be executed. If a future returns Pending, it means the future is not yet complete and needs to wait for the next call. Since we do not need a complex asynchronous runtime, the executor here only needs to handle the return value of the future. The code is as follows:

```rust
    let waker = dummy_waker();
    let mut context = Context::from_waker(&waker);
    match Pin::as_mut(parser).poll(&mut context) {
        Poll::Ready(Ok(())) => {
            self.c2s_state = TaskState::End;
            Some(Ok(()))
        }
        Poll::Ready(Err(())) => {
            self.c2s_state = TaskState::Error;
            Some(Err(()))
        }
        Poll::Pending => None,
    }
```

In this way, the internal logic is asynchronous, but the external interface is synchronous. Each time a packet arrives, the executor executes the future (i.e., the decoder) once. The external code does not need to be asynchronous. That is, the executor can be called in synchronous code:

```rust
    loop {
        pkt = get_packet();
        task.run(pkt);
    }
```

Of course, to obtain decoding results, you need to set a callback function in advance. The callback will be triggered during the decoding process.

#### Decoder

All of this work is for the decoder. Since a large number of connections need to be handled simultaneously, the decoder cannot wait for subsequent packets during decoding. Therefore, the decoder is a future, i.e., an asynchronous function. When subsequent data has not yet arrived, the decoder returns Pending and is temporarily suspended. When the executor receives Pending, it does not consider the decoding of this connection to be finished, but continues to process the next connection.

However, the complexity of this asynchronous invocation is all encapsulated by Rust's future mechanism, so the implementation of the decoder only needs to focus on itself. It can be written in a fully synchronous style. For example, part of the SMTP protocol decoder code:

```rust
    loop {
        let (line, seq) = stm.readline_str().await?;

        if line == "\r\n" {
            return Ok((boundary, te));
        }
    }
```

This is the process of reading the reassembled header data. As you can see, it does not care about whether the data has arrived. If the current packet does not contain complete data, the decoder does not need to check or exit. It executes decoding in a fully synchronous manner until the end of the header is reached. The only difference is that its data reading function is asynchronous, with the await keyword at the end.

Each await triggers a future call. If the current packet does not have complete data, await returns Pending and the decoder is suspended. The executor continues to process the next packet. When a packet arrives, the decoder is awakened and continues execution until the end of the header is reached.

In this way, we can hand over the protocol parsing work to the executor. The executor will automatically handle asynchronous situations. Users only need to focus on the implementation of the decoder, and the decoding process becomes simple and easy to understand.

From the initial big loop in C recording state, to using a state machine driver in C, to Rust's future mechanism, and now to the decoder, the process has become simpler and simpler. In the end, we use synchronous, straightforward code to implement an asynchronous decoding process.
