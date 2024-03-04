---
layout: post
title: "网络流量处理中的协议解析:状态机"
date: 2024-03-04 11:00:00 +0800
categories: 技术
tags: 技术 协议解析
published: true
---

既然解析的过程就是状态转换，那自然想到状态机。如果能把解析过程分为不同的状态，然后作为一个状态机来轮转。那协议解析过程可以切分为不同的阶段，每个阶段仅关注自己的事儿。解析就是解析，状态转换就是转换，他们分开。这样的解析过程会清晰直观简洁。整个解析过程就是有多个状态组合起来的状态机。其实，所有协议都是状态机。

首先确定都需要划分哪些状态，比如SMTP可以分成下面几个状态：
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

每个状态都是一个函数，这些函数放在一个函数数组中。函数内部在解析的同时判断是否需要切换状态：
```c
    struct state_machine {
        state_fun_t     fun;
        enum smtp_state state;
    };
	
    struct state_machine smtp_sm[MAX_STATE];	
	
    stmpt_sm[MAILFROM].fun   = state_mailfrom;
    stmpt_sm[MAILFROM].state = MAILFROM;	
```
smtp_sm这个数组就是我们需要的状态机。smtp_sm[MAILFROM].fun这个函数就是处理mailfrom命令的函数。这个函数和 smtp_sm[MAILFROM].state对应。其他的处理函数和状态也一样。当mailfrom处理完毕需要进入recptto状态的时候，它只需在函数内部让state = RCPTTO 即可，这样就标记了状态的切换。

但是只标记还不行，需要根据标记的状态来调用对应的函数。到此为止，这个smtp解析状态机还是静态的，需要驱动状态机转起来。这个驱动也不复杂，只需要在smtp_sm这个数组上不断地根据state调用对应函数即可，这里仍然用伪代码来表示：
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

每个状态函数都要注册到smtp_sm这个数组中，处理完本状态需要转换到下一个状态的时候，给node->state赋值，这样在sm_drive中，就会调用到下一个状态函数：
```c
int state_mailfrom(flow_t *node) {
    while (get_line(node, line, line_len) == 0) {
        if (line is mailfrom) {
            // 提取邮件地址
            node->state = RCPTTO;
            return SM_OK;
        }
    }
    return SM_BREAK;
}
```
这样，解析状态机就转动了起来。在一个状态处理完毕之后，会进入下一个状态一直走到协议解析完毕或者出错。

这里有一个注意的地方就是返回值：SM_BREAK。为什么需要这个？因为在前面提到的流量处理模型中可以知道，数据包的处理，重组的处理和其他模块的处理，都是在数据包处理过程中的。也就是说协议解析的状态机也在这个处理路径上，如果不返回SM_BREAK，一直等待的话，那整条路径都会被阻塞。这个流量处理模型不是多线程，解析状态机也不是独立的线程。所以get_line读不到数据，状态函数没有数据可处理，进行不下去的时候，它需要从状态机中跳出来，继续流量处理模型中的其他后续过程。等下一个数据包到来，再重新进入状态机。此时状态机所处的状态没有变，会继续进入上次的处理函数。继续试图get_line，继续处理。

至此，协议解析过程通过流式重组和状态机已经被切分为不同的状态处理函数，并且可以在数据包处理路径上中断跳出把执行机会让给路径后的其他功能，下次重新进入。协议解析过程就是切分状态，实现每个状态函数，跳转状态的过程。这样简洁了不少，清晰了不少，省掉了很多麻烦事儿...
