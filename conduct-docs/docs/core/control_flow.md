---
sidebar_position: 3
title: Control Flow
tags:
    - Syntax
description: Control Flow description
---

# Control Flow

## If statement {#if}

Control flow in Conduct is represented by the if-else statements (but match statements are planned)

Syntax:
```
if <condition> {
    <if clause>
} else if <condition> {
    <optional else if clause>
} else {
    <optional else clause>
}
```

Here are some examples:

```conduct
// simple if
if condition {
    println("True!")
}

// assigning variables
let a
if 1 > 3 {
    a = nil
} else if 1 > 2 {
    a = 34
} else {
    a = true
}
```

## Ternaries {#ternary}

Ternaries are inline if-else statements, that return some value.

Syntax:
```
<condition> ? <if clause> : <else clause>
```

Here are some examples:

```conduct
// simple ternary
let var = greet ? "Hello!" : "Goodbye!"

// nesting ternaries
let nested = greet ? "Hello!" : farewell ? "Goodbye!" : nil
// this can be rewritten as

let nested
if greet {
    nested = "Hello!"
} else if farewell {
    nested = "Goodbye!"
} else {
    nested = nil
}
```