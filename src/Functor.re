/*
 https://github.com/data61/fp-course/blob/master/src/Course/Functor.hs
 https://andywhite.xyz/posts/2019-11-01-a-laymans-guide-to-functors-in-reasonml/
 https://github.com/Risto-Stevcev/bs-abstract/
 https://gist.github.com/twopoint718/6c93e1f886ecd7da1aef5f5b6687cba7
 https://hackernoon.com/the-reader-monad-part-1-1e4d947983a8
 */

module type FUNCTOR = {
  type t('a);
  let fmap: ('a => 'b, t('a)) => t('b);
};

module ExactlyOneFunctor: FUNCTOR with type t('a) = ExactlyOne.exactlyOne('a) = {
  type t('a) = ExactlyOne.exactlyOne('a);
  let fmap = ExactlyOne.mapExactlyOne;
};

module ListzFunctor: FUNCTOR with type t('a) = list('a) = {
  type t('a) = list('a);
  let fmap = Listz.map;
};

module OptionFunctor: FUNCTOR with type t('a) = option('a) = {
  type t('a) = option('a);
  let fmap = (f, x) =>
    switch (x) {
    | None => None
    | Some(x) => Some(f(x))
    };
};

module type ReaderType = {type t;};

module MakeReader = (ReaderType: ReaderType) => {
  type t('a) =
    | Reader(ReaderType.t => 'a);
  let pure = a => Reader(a);
  let run = (Reader(a)) => a;
  let map = (f, Reader(g)) => Reader(t => f(g(t)));
};

module ReaderInt =
  MakeReader({
    type t = int;
  });

module ReaderIntFunctor: FUNCTOR with type t('a) = ReaderInt.t('a) = {
  type t('a) = ReaderInt.t('a);
  let fmap = ReaderInt.map;
};

let add1 = a => a + 1;
let times2 = a => a * 2;

let add1Reader = ReaderInt.Reader(add1);
let times2Reader = ReaderInt.Reader(times2);

let ReaderInt.Reader(f) = ReaderIntFunctor.fmap(add1, times2Reader);
let x = f(3);
Js.log(x);

/*
 module Reader2 = {
   type t('a) =
     | Reader('a);

   type pure('a) = 'a => t('a);
   let pure = a => Reader(a);

   type run('a) = t('a) => 'a;
   let run: run('a) = (Reader(a)) => a;

   type map('a, 'b) = ('a => 'b, t('a)) => t('b);
   let map: map('a, 'b) = (f, ta) => pure(f(run(ta)));
 };
 */

/*
 module ReaderFunctor: FUNCTOR with type t('a) = Reader.t('a) = {
   type t('a) = Reader.t('a);
   let fmap = Reader.map;
 };

 let add1 = a => a + 1;
 let times2 = a => a * 2;

 let add1Reader = Reader.Reader(add1);
 let times2Reader = Reader.Reader(times2);

 let result = ReaderFunctor.fmap(times2, times2Reader);
 */