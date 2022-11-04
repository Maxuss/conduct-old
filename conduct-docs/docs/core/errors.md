---
sidebar_position: 7
title: Errors
descriptions: Error handling in Conduct
tags:
    - Syntax
---

# Errors

To propagate an error in Conduct, you can use the `throw` keyword.

Example:

```conduct
throw "I am an error, hello!"
```

:::info
Any type can be used as an error in Conduct
:::

Throwing an error will instantly exit all scopes until encountering a [Catch Statement](#catch).
If it does not encounter any fitting catches, it will return a `nil` value.

## Handling Errors {#catch}

To handle an error you can use the `try catch` statement.

```
try {
    <code>
} catch <error type> as <error name> {
    <code>
} ...
```

Each try statement may have (practically) infinite amount of catch clauses.

Here is an example to catch previously defined error:

```conduct
try {
    throw "I am an error, hello!"
} catch Infallible as _ {
    println("This is not reached!")
} catch str as error {
    println("An error occurred: ${error}")
}
```

You can also use the `*` wildcard to catch any type

```conduct
try {
    if random_bool() {
        throw FirstCase("Error")
    } else {
        throw SecondCase("Different Error")
    }
} catch * as error {
    println("Error of type ${typeof(error)} has occured!")
}
```

## Nil Throws {#nil}

Nil throws can not be catched by previously mentioned types of catch statement, as there is no such thing as nil type.
That is where the `?` suffix comes in hand.

The `?` suffix allows to mark any type as nullable, so you can have two cases in a single catch statement:

```conduct
try {
    if random_bool() {
        throw Error("Not null")
    } else {
        throw nil
    }
} catch Error? as nullable {
    println(nullable == nil)
}
```

If you would like to catch only nil throws, there is a shorthand syntax for this too:

```conduct
try {
    if random_bool() {
        throw Error("Not null")
    } else {
        throw nil
    }
} catch Error as not_nil {
    println(not_nil != nil)
} catch? {
    // this clause only catches nil throws and does not have an associated constant
    println("We caught a nil throw")
}
```