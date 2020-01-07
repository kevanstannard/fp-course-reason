type exactlyOne('a) =
  | ExactlyOne('a);

let toString = (ExactlyOne(a)) => {j|ExactlyOne($a)|j};

type runExactlyOne('a) = exactlyOne('a) => 'a;
let runExactlyOne: runExactlyOne('a) = (ExactlyOne(a)) => a;

type mapExactlyOne('a, 'b) = ('a => 'b, exactlyOne('a)) => exactlyOne('b);
let mapExactlyOne: mapExactlyOne('a, 'b) =
  (f, ExactlyOne(a)) => ExactlyOne(f(a));

type bindExactlyOne('a, 'b) =
  ('a => exactlyOne('b), exactlyOne('a)) => exactlyOne('b);
let bindExactlyOne: bindExactlyOne('a, 'b) = (f, ExactlyOne(a)) => f(a);