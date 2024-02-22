

To build
```bash
$ cd l1
$ ocamlbuild -clean && ocamlbuild l1.byte
```

Then run code examples using interpreter 0:

Fibonacci using parallel
```bash
$ cat tests/fib-while-para.l1
begin
    n = 10;
    a = 0;
    b = 1;
    while n >= 0 do
        n = n-1 | a = b | b = a + b
    end;
    a
end

$ ./l1.byte tests/fib-while-para.l1 -v -i0
Interpreter 0 
output> 89
```


Display parsed ast using -V
```bash
$ ./l1.byte tests/fib-while-para.l1 -V
Parsed result:
Seq(Assign(n, Integer(10)); Assign(a, Integer(0)); Assign(b, Integer(1)); While(Op(Var(n), GTEQ, Integer(0)), Para(Para(Assign(n, Op(Var(n), SUB, Integer(1))), Assign(a, Var(b))), Assign(b, Op(Var(a), ADD, Var(b))))); Var(a))
After static checks:
Seq(Assign(n, Integer(10)); Assign(a, Integer(0)); Assign(b, Integer(1)); While(Op(Var(n), GTEQ, Integer(0)), Para(Para(Assign(n, Op(Var(n), SUB, Integer(1))), Assign(a, Var(b))), Assign(b, Op(Var(a), ADD, Var(b))))); Var(a))
After translation:
Seq(Assign(n, Integer(10)); Assign(a, Integer(0)); Assign(b, Integer(1)); While(Op(Var(n), GTEQ, Integer(0)), Para(Para(Assign(n, Op(Var(n), SUB, Integer(1))), Assign(a, Var(b))), Assign(b, Op(Var(a), ADD, Var(b))))); Var(a))
```


