---
layout: post
title: "Protocol Parsing in Network Traffic Processing (Part 3): Parsing"
date: 2024-03-03 17:00:00 +0800
categories: Technology
tags: Technology Protocol Parsing
published: true
---

Once you can obtain line-by-line data from a connection, you can start parsing SMTP.

#### Parsing Process

The parsing process involves looping through each line of data and parsing them sequentially according to the protocol. The pseudocode is as follows:
```c
while (get_line() != 0) {
    if (state == ERROR) return;
	
    if (state == HEAD) {	
        // Process the head
        state = BODY;
        continue;
    }
	
    if (state == BODY) {	
        // Process the body
        state = END;
        continue;		
    }
	 
    // Other parts processing
	
    if (state == END) {	
        // End and exit the processing
    }
}
```

Here, state is used to record the parsing status. As long as the email header has not been fully processed, state remains as HEAD . This way, when the next packet arrives and enters the parsing function, it will continue parsing the email header. The same logic applies to other stages. Once the header is processed, state is changed to the next stage, BODY , and parsing of the email body begins.

#### Problems in Parsing

This pseudocode looks fairly clear, but in actual implementation, many issues arise. For example, determining the end and start of a state, or the need to repeatedly enter the same state. The code structure can also become quite messy. In complex and chaotic real-world code, this may not be very clear, but this streamlined pseudocode loop provides a hint:

The parsing process is essentially a process of state transitions...