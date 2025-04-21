---
layout: post
title: "Protocol Parsing in Network Traffic Processing (Part 4): State Machine"
date: 2024-03-04 11:00:00 +0800
categories: Technology
tags: Technology Protocol Parsing
published: true
---

Since the parsing process is essentially a process of state transitions, it is natural to think of a state machine. If the parsing process can be divided into different states and then rotated as a state machine, the protocol parsing process can be split into different stages, with each stage focusing only on its own task. Parsing is parsing, state transitions are state transitions—they are separated. This makes the parsing process clear, intuitive, and concise. The entire parsing process is a state machine composed of multiple states. In fact, all protocols are state machines.

First, determine which states need to be defined. For example, SMTP can be divided into the following states:
```c
    enum smtp_state {
        START,
        MAILFROM,		
        RCPTTO,
        HEAD,
        BODY,
        END,
        ERROR,
        MAX_STATE		
    };
```

Each state is a function, and these functions are placed in a function array. Inside each function, while parsing, it also determines whether a state transition is needed:
```c
    struct state_machine {
        state_fun_t     fun;
        enum smtp_state state;
    };
	
    struct state_machine smtp_sm[MAX_STATE];	
	
    smtp_sm[MAILFROM].fun   = state_mailfrom;
    smtp_sm[MAILFROM].state = MAILFROM;	
```
The smtp_sm array is the state machine we need. The function smtp_sm[MAILFROM].fun is the function that handles the mailfrom command. This function corresponds to smtp_sm[MAILFROM].state. Other handler functions and states are similar. When mailfrom processing is complete and needs to enter the rcptto state, it only needs to set state = RCPTTO inside the function, thus marking the state transition.

However, just marking is not enough; you need to call the corresponding function according to the marked state. Up to this point, this SMTP parsing state machine is still static and needs to be driven. The driver is not complicated; you just need to keep calling the corresponding function in the smtp_sm array according to the state. Here is the pseudocode:
```c
int sm_drive(struct state_machine sm[MAX_STATE], flow_t *node) {
    int ret;

    if (node->state == END || node->state == ERROR) {
        return;
    }	

    ret = SM_OK;
    while (ret == SM_OK) {	
        ret = sm[node->state].fun(node);
        if (ret == SM_BREAK) {
            break;
        }
    }

    return 0;
}
```

Each state function must be registered in the smtp_sm array. When the current state is processed and needs to transition to the next state, assign the new value to node->state, so that sm_drive will call the next state function:
```c
int state_mailfrom(flow_t *node) {
    while (get_line(node, line, line_len) == 0) {
        if (line is mailfrom) {
            // Extract email address
            node->state = RCPTTO;
            return SM_OK;
        }
    }
    return SM_BREAK;
}
```
In this way, the parsing state machine starts to run. After one state is processed, it enters the next state and continues until protocol parsing is complete or an error occurs.

One thing to note here is the return value: SM_BREAK. Why is this needed? As mentioned in the previous traffic processing model, packet processing, reassembly, and other modules are all part of the packet processing process. That is, the protocol parsing state machine is also on this processing path. If SM_BREAK is not returned and it keeps waiting, the entire path will be blocked. This traffic processing model is not multithreaded, and the parsing state machine is not an independent thread. So if get_line cannot read data and the state function has no data to process and cannot proceed, it needs to exit the state machine and continue with other subsequent processes in the traffic processing model. When the next packet arrives, it re-enters the state machine. At this point, the state machine's state has not changed and will continue to enter the previous handler function, continue to try get_line, and continue processing.

At this point, the protocol parsing process has been split into different state handler functions through stream reassembly and the state machine, and can interrupt and yield execution to other functions on the packet processing path, re-entering next time. The protocol parsing process is about splitting states, implementing each state function, and transitioning between states. This greatly simplifies and clarifies the process, saving a lot of trouble...