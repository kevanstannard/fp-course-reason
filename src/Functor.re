/*
 https://github.com/data61/fp-course/blob/master/src/Course/Functor.hs
 https://andywhite.xyz/posts/2019-11-01-a-laymans-guide-to-functors-in-reasonml/
 https://github.com/Risto-Stevcev/bs-abstract/
 https://gist.github.com/twopoint718/6c93e1f886ecd7da1aef5f5b6687cba7
 https://hackernoon.com/the-reader-monad-part-1-1e4d947983a8
 */

module type FUNCTOR = {
  type t('a);
  let map: ('a => 'b, t('a)) => t('b);
};

module ExactlyOneFunctor: FUNCTOR with type t('a) = ExactlyOne.exactlyOne('a) = {
  type t('a) = ExactlyOne.exactlyOne('a);
  let map = ExactlyOne.mapExactlyOne;
};

module ListzFunctor: FUNCTOR with type t('a) = list('a) = {
  type t('a) = list('a);
  let map = Listz.map;
};

module OptionFunctor: FUNCTOR with type t('a) = option('a) = {
  type t('a) = option('a);
  let map = (f, x) =>
    switch (x) {
    | None => None
    | Some(x) => Some(f(x))
    };
};

module type TYPE = {type t;};

module MakeReaderFunctor = (TYPE: TYPE) => {
  module ReaderFunctor: FUNCTOR with type t('a) = Reader.t(TYPE.t, 'a) = {
    type t('a) = Reader.t(TYPE.t, 'a);
    let map = Reader.map;
  };
};

/*
 module IntReaderFunctor =
   MakeReaderFunctor({
     type t = int;
   });

 let f = x => x + 1;
 let g = x => x * 2;
 let fg = IntReaderFunctor.ReaderFunctor.map(f, Reader(g));
 let z = Reader.run(fg, 1);
 */