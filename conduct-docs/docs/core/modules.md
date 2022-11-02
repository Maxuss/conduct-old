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