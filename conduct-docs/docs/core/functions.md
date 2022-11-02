---
sidebar_position: 5
title: Functions
description: Conduct function definitions
tags:
    - Syntax
---

There are two kinds of functions in Conduct.

## Function Statements {#statement}

Functions are declared using the `fn` keyword.

```
fn <name>(<param names>) {
    <code>
}
```

Here is an example:

```conduct
fn say_hello(name) {
    println("Hello, ${name}!")
}
```

## Arrow Functions {#arrow}

Arrow functions are declared using the `=>` operator.

```
(<param names>) => { <code> }
```

Here is an example:

```conduct
const say_hello = (name) => {
    println("Hello, ${name}!")
}
```