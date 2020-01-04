type isEven = int => bool;
let isEven: isEven = [%bs.raw {| n => n % 2 == 0 |}];

type isOdd = int => bool;
let isOdd: isOdd = value => !isEven(value);

type const('a, 'b) = ('a, 'b) => 'a;
let const: const('a, 'b) = (a, _) => a;

type compose('a, 'b, 'c) = ('b => 'c, 'a => 'b, 'a) => 'c;
let compose: compose('a, 'b, 'c) = (f, g, x) => f(g(x));
let (<.>): compose('a, 'b, 'c) = (f, g, x) => f(g(x));

type id('a) = 'a => 'a;
let id: id('a) = x => x;

type optionToString('a) = option('a) => string;
let optionToString = opt => {
  switch (opt) {
  | None => "None"
  | Some(a) => {j|Some($a)|j}
  };
};

type flip('a, 'b, 'c) = (('a, 'b) => 'c, 'b, 'a) => 'c;
let flip: flip('a, 'b, 'c) = (f, b, a) => f(a, b);