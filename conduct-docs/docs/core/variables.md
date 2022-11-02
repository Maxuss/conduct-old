---
sidebar_position: 2
title: Variables
description: Variable definitions in Conduct
tags:
    - Syntax
---

# Variables

Variables are used to store data.
All variables in Conduct are *heterogenous*, meaning they can be assigned different kinds of values.
Further type checks at runtime can be made with the `is` operator.

There are currently three kinds of variables:

## `let` variables {#let}

These are **mutable** variables. They can be reassigned new values at any time.

```conduct
let my_var = 4 // assigning value 4 to variable `my_var`
my_var = "Hello World!" // assigning a string value to variable
my_var = [] // assigning an array to varaible
let my_var = "overwritten" // overwriting the variable
```

When variables are created without default value, a `nil` value is assigned instead.

```conduct
let my_var // my_var is initialized with nil value
!my_var // true
```

## `const` variables {#const}

These are **immutable** variables. They can not be reassigned new values, but they can still be
overwritten.

```conduct
const my_const = 4
my_const = 2 // compile time error, constants may not be reassigned
const my_const = 2 // all fine
let my_const = 2 // all fine
```

Note that constants may not be created without value

```conduct
const my_const // unlike mutable variables throws a compile time error.
```

## `native const` variables {#native-const}

Those are **immutable native** constants. They can not be reassigned inside the Conduct code, but the external provider may mutate them.

More on native binding in the [Extending Section](../extend)

```rust title=src/main.rs
// <snip>
let mut vm: Vm;
vm.add_native_const("MY_CONSTANT", |vm| 16.variable(vm));
// <snip>
```

```conduct title=src/main.cd
native const MY_CONSTANT

MY_CONSTANT == 16 // native
```