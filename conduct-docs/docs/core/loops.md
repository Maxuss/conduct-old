---
sidebar_position: 6
title: Loops
description: Conduct loop syntax
tags:
    - Syntax
---

# Loops

There are currently two kinds of loops in Conduct.

## While Loops {#while}

This is the simplest kind of loops.

```
while <condition> {
    <body>
}
```

An example:

```conduct
while check() {
    println("Checked!")
}
```

While loops evaluate condition each time they are run.

## For Loops {#for}

This loop allows to iterate over a collection.

```
for <iterable> in <iterator> {
    <body>
}
```

An example:

```conduct
for i in 0..15 {
    println("Iterated over ${i}")
}
```

Types that currently can be iterated:

### `range`
Iterates over each consecutive element in range.

### `array`
Iterates over each element in an array

### `compound`
Iterates over each key/value pair in a compound, e.g.

```conduct
for pair in { a: 0, b: 1, c: 2 } {
    let key = pair[0]
    let value = pair[1]
    println("${key} ${value}")
}
```