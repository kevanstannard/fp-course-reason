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

/*
 let test = () => {
   let n1 = ExactlyOne(1);
   let n2 = ExactlyOne(2);
   let s1 = ExactlyOne("hello");

   let r1 = n1 |> runExactlyOne;
   Js.log(r1);
   // 1

   let r2 = s1 |> runExactlyOne;
   Js.log(r2);
   // hello

   let r3 = n2 |> mapExactlyOne(x => x * 2);
   show(r3);
   // ExactlyOne(4)

   let r4 = s1 |> bindExactlyOne(s => ExactlyOne("Hi, " ++ s));
   show(r4);
   // ExactlyOne(Hi, hello)
 };

 test();
 */