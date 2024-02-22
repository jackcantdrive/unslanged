

To build
```bash
$ cd l1
$ ocamlbuild -clean && ocamlbuild l1.byte
```

Then run code examples using interpreter 0:

Fibonacci using parallel
```bash
$ cat tests/fib-while-para.l1

n = 10;
a = 0;
b = 1;

while --n >= 0 do
    a = b | b = a + b
end;
a

$ ./l1.byte tests/fib-while-para.l1 -v -i0
Interpreter 0 
output> 55
```


Display parsed ast using -V
```bash
$ ./l1.byte tests/fib-while-para.l1 -V
Seq(Assign(n, Integer(10)); Seq(Assign(a, Integer(0)); Seq(Assign(b, Integer(1)); Seq(While(Op(Dec(n), GTEQ, Integer(0)), Para(Assign(a, Var(b)), Assign(b, Op(Var(a), ADD, Var(b))))); Var(a)))))
After static checks:
Seq(Assign(n, Integer(10)); Seq(Assign(a, Integer(0)); Seq(Assign(b, Integer(1)); Seq(While(Op(Dec(n), GTEQ, Integer(0)), Para(Assign(a, Var(b)), Assign(b, Op(Var(a), ADD, Var(b))))); Var(a)))))
After translation:
Seq(Assign(n, Integer(10)); Seq(Assign(a, Integer(0)); Seq(Assign(b, Integer(1)); Seq(While(Op(Dec(n), GTEQ, Integer(0)), Para(Assign(a, Var(b)), Assign(b, Op(Var(a), ADD, Var(b))))); Var(a)))))
```


