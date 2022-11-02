---
sidebar_position: 1
title: Literals
description: Literal definitions in Conduct
tags:
  - Syntax
---

## Comments
```conduct
// Here is a simple comment
// it is one line only

/*
  Here is a
  multi
  line
  comment
  !
*/
```

## Literals

Literals are simple values. 

#### Nil (null) values: {#nil}

Nils are used to present an unitialized/empty state.

```conduct
nil
```

Trying to access any property on `nil` will instead return itself.
You can do nil checks at runtime with the `!!` operator, e.g.:

```conduct
nullable_property!!.value.accessible()
```

This will exit current scope with nil value if the `nullable_property` is nil,
otherwise it will allow you to access other properties.

#### Numbers: {#number}

Numbers are defined as decimal numbers.

```conduct
123456
123.456 // optionally with decimal places
```

They also support hexadecimal, binary and octal forms.

```conduct
// also numbers
0xFFAAFF
0b10101
0o4143
```

All numbers are stored internally as **64 bit floating point numbers**. 


#### Strings: {#string}

Strings are enclosed in either double (`"`) or single (`'`) quotes.

```conduct
'a string!' // a string
"double quoted!" // also a string
```

Strings all support some escape sequences

```conduct
"\n" // newline
"\r" // carriage return
"\t" // horizontal tab
"\'" // single quotation mark
"\"" // double quotation mark
"\\" // backslash
"\x00" // two place ASCII character
"\u0000" // four place UTF-8 character
```

All strings in Conduct are UTF-8 encoded strings, and there are no character literals.
Instead of character literals, single character strings should be used.

#### Booleans: {#boolean}

Booleans are represented by two constants:

```conduct
true
false
```

Technically, `nil` values can be treated as boolean as well, and they are transformed
to `false`.

#### Arrays: {#array}

Arrays in Conduct are ordered and heterogenous, meaning they can store different
types of values inside.

```conduct
[1, 2, 3]
[[1, 2, 3], [4, 5, 6], [7, 8, 9]] // can be nested
[1, "Hello, World!", nil, true] // can store different kinds of data
[] // an empty array may be treated as nil
```

#### Compounds {#compound}

Compounds represent all the complex data types in the Conduct ecosystem.

Each compound definition must contain property names and values, separated by colons.
The name keys may either be an identifier or a string.
```conduct
{
  key: 'value',
  'another key': 0b101010101
}
```

Compounds may also be nested indefinetly.

```conduct
// nested compounds:
{
  nested: {
    array: [1, 2, 3, 4]
  }
}
```

Empty compounds are treated as `nil` values, even though runtime type checking will
report them as compounds.

```conduct
{} // empty compound
```

#### Type definitions: {#type}

Allows for runtime type checking (`is` operator).

Each type definition must contain property names and types, separated by colons.
```conduct
const MyType = type {
  property: num, // type
  a: str,
  nullable: num?,
  b: compound,
  c: array,
  rec: MyType?, // recursive types must either be nullable or `Self`
  another_rec: Self
}
```

Note that even though types do exist in Conduct, all complex data structures are represented
by compounds