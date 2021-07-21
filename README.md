# Felix #

Felix is an X/Open Native Language System library for Ada (1995). With it, you can write code like the following:

```Ada
Put (Hello_Message, Text & "Hello, world! π = " & 3.145791 & New_Line);
```

and have it automatically translated at runtime. For example, loading a catalog that contains the above message translated into Greek

```
1 "Γεια σου, κόσμε! π = %1$f\n"
```

will yield the output

```
Γεια σου, κόσμε! π = 3.145791
```

For more programming examples, see the examples directory.
