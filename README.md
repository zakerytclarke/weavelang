# Weave Programming Language
![Weave](https://zclarke.xyz/weavelang/src/assets/weave.jpg)

[Github](https://github.com/zakerytclarke/weavelang)__________[REPL.it](https://weavelang.weavelang.repl.run/)
Authors: [Elijah Johnson](https://github.com/ElijahJohnson5), [Zakery Clarke](https://zclarke.xyz)
Welcome to Weave Programming Language!
Weave is a programming language that transforms imperative style code into efficient functional code. Programming in functional languages is incredibley powerful- offering unmatched expressiveness and program safety. However, programming in a functional style can often be a daunting task.

We have created the Weave programming language to allow C-like programs that take advantage of the flexibility, efficiency and safety of functional languages.


## Features 
- Compiles imperative style coding => efficient functional lambda calculus
- Type Inference and strong type checking


### What makes Weave better than C?
- Types can be inferred
- Higher Order Functions
- Strong typing and memory safety

### What makes Weave better than Haskell?
- More comnfortable coding style
- 




## Sample Programs

### Hello World
```
print("Hello World!");
```



### Factorial
```
factorial:=(n){
  if (n <= 1) {
    return 1;
  }
  return n*factorial(n-1);
}

print(show(factorial(10)));
```

### Fibonacci
```
fib:=(n){
  if(n==0){
    return 0;
  }
  if(n==1){
    return 1;
  }
  return fib(n-1)+fib(n-2);
}

print(fib(5));
```


## Roadmap
We have many ideas to improve Weave before releasing v 1.0! We have outlined many of them below:

- Pattern Matching
```
//Proposed Syntax
fact(1):={return 1;}
fact(n):={return n*fact(n-1);}
```
- Lazy Evaluation
```
//Allows for infinite data structures
naturalNumbers:=pair(1,map((+1),naturalNumbers));
take(10,naturalNumbers) => [1,2,3,4,5,6,7,8,9,10]
```
- Multithreaded evalaution
  Because of its functionl nature, Weave can be efficiently evaluated in parallel
- Abstract Data types
```
//Create your own data types!
type Suite = Clubs | Diamonds | Hearts | Spades
type Point = Point(Number,Number)
type Shape = Circle(Point,Number) | Rectangle(Point,Point)
```
- Liquid typing; eg a>=0
- Monad implementation for better IO abstraction
- Recursion analysis
  Ensure functions will termintate