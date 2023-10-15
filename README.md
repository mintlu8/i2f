# i2f

Macros that converts number literals into int or float literals.

In addition to literals, we also accept negative literals `-literal`.

## Examples

```rust
i2f!(90)        //90.0
i2f!(-462)      //-462.0
i2f!(3.14)      //3.14
f2i!(3),        //3
f2i!(3.0),      //3
trunc!(3.14),   //3
f2i!(-12.0),    //-12
```
