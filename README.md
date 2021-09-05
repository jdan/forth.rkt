## forth.rkt

A [forth](https://en.wikipedia.org/wiki/Forth_(programming_language)) interpreter written in Racket.

### example

```rkt
(eval
  '(: fac
      dup 0 =
      [ drop 1 ] ; return 1
      [ dup 1 -  ; n, n-1
            fac  ; n, fac(n-1)
            * ]  ; n * fac(n-1)
      if \;

    10 fac))
; => '(3628800))
```

### inspo

https://factorcode.org/
