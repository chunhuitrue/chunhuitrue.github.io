---
layout: post
title: "Protocol Parsing in Network Traffic Processing (Part 5): async/await"
date: 2024-03-04 17:00:00 +0800
categories: Technology
tags: Technology Protocol Parsing
published: true
---

#### async/await

Let's review the previous parsing state machine. It executes on each flow_t node provided by the flow table module, that is, on each connection. When it cannot proceed, it exits and yields execution to other connections. It looks as if a miniature execution body is implemented for each connection, allowing the traffic processing program to concurrently handle a large number of connections within the same execution path. This is actually very similar to the async/await asynchronous model.

The implementation of async/await involves many concepts, but simply put, it is essentially a state machine. As a state machine, it has a poll method and returns an execution state: pending or ready. The poll method is used by the state machine driver function to execute. Pending means it is not yet complete, similar to the break in the previous parsing state machine. Ready means the state machine has finished executing.

An async function is a state machine that can be polled, and each awaited function is also a pollable state machine, with each await converted into a state. Just like the previous parsing state machine, it also needs a driver function to rotate and drive the state machine—this is the executor, which keeps calling the poll method of the state machine. If it encounters pending, it is equivalent to the break in the parsing state machine and cannot proceed for now, so it can perform other tasks. If it is ready, it means the state machine has finished.

In fact, async/await is the standard way to implement asynchronous operations. Protocol parsing in network traffic is a typical asynchronous operation. Before using async/await, we implemented it with a simple state machine. Next, we can use a better approach to implement protocol parsing.

#### Preliminary Considerations

Rust can be chosen as the language for async/await, as it implements the async/await asynchronous mechanism, and its "state machine" is the future. Asynchronous operations in Rust require an execution environment. These different execution environments are suitable for different scenarios, but for our protocol parsing, they are all too complex. As seen in the previous parsing state machine, the process of driving the state machine does not need to be complicated, because the underlying traffic processing model only requires asynchronous operations for the parsing process. Therefore, we only need to implement a simple executor.

The most basic operation of asynchronous protocol parsing is still the previously mentioned get_line. In the earlier version, get_line would return a non-zero value when a complete line was not available. However, this behavior is not convenient for async/await asynchronous operations. We need it to return pending when a complete line is not available, meaning we want to use get_line like this:
> get_line().await

Therefore, this function needs to be implemented as an async function.

get_line ultimately reads data from reassembled packets. The purpose of reassembly is to form a stream. Coincidentally, Rust has a stream trait, which can asynchronously provide bytes. We just need to use the stream trait in get_line to keep reading until the end of the line.

Stream reassembly is simply the process of sorting packets by sequence number.

With this, the most basic get_line is implemented asynchronously. We can then call it in an async function to obtain and parse data line by line.

Just like the previous parsing state machine, up to this point, the parsing process is still only a static async/await definition. It still needs an executor to drive it. Fortunately, the scenario here is simple and does not require complex scheduling—just call the poll method of the async/await future and check whether it is pending or ready...