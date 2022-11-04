---
sidebar_position: 4
title: Modules
description: Conduct modules and imports
tags:
    - Syntax
    - Projects
---

# Modules

Conduct libraries are split into `modules`. Each module contains different submodules etc.
When creating a new Conduct project you need to put module information inside the main file:
`main.cd`. This is your package file.

Defining modules is easy:

```conduct title=src/main.cd
module module_name
```

And that's it!

# Imports

To import some module you will need to use the import statement

```conduct
import <mod>.<submodule>
```

After using the import statement you will be able to use all the definitions from the imported module.

To import definitions from your own package, use

```conduct
import module_name.<submodule>
```

Where `module_name` is the name of [your module](#modules) defined in the `main.cd`.

You can also import elements from from conduct libraries, or conduct header definitions:

```conduct
import '../libs/frogio.cdl' // importing a VM-bound library
import '../include/libui.cdh' // importing cross-VM header definitions
```

# Exports

Sometimes you need to re-export some multiple elements from another module, or export a static native library data.

You should use the `export` statement for this.

For example, here is your project structure:

```
my_module
|---main.cd
|---foo
|    |---bar.cd
|    |---baz.cd
|
|---api
     |---first.cd
     |---second.cd
```

Instead of having to always use

```conduct
import my_module.foo.bar
import my_module.foo.baz
import my_module.api.first
import my_module.api.second
```

You can export it all in module definition:

```conduct title=my_module/main.cd
export self.foo.bar
export self.foo.baz
export self.api.first
export self.api.second
```

So the end users can just use

```conduct
import my_module
```

To import all the nested elements! That is also how the `.prelude` submodules work.

You can also use this to export native libraries

```conduct
// same way as importing
export '../libs/frogio.cdl'
export '../include/libui.cdh'
```