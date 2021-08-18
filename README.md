[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/felix.json)](https://alire.ada.dev/crates/felix.html)

# Felix #

Felix is an X/Open Native Language System library for Ada (1995). With
it, you can write code like the following:

```Ada
Put (Hello, Text & "Hello, world! π = " & Ada.Numerics.Pi & New_Line);
```

and have it automatically translated at runtime. For example, loading
a catalog that contains the above message translated into Greek

```
1 "Γεια σου, κόσμε! π = %1$Lf\n"
```

will yield the output

```
Γεια σου, κόσμε! π = 3.141593
```

For more programming examples, see the examples directory.
