/*
 https://github.com/data61/fp-course/blob/master/src/Course/Functor.hs
 https://andywhite.xyz/posts/2019-11-01-a-laymans-guide-to-functors-in-reasonml/
 https://github.com/Risto-Stevcev/bs-abstract/
 https://gist.github.com/twopoint718/6c93e1f886ecd7da1aef5f5b6687cba7
 https://hackernoon.com/the-reader-monad-part-1-1e4d947983a8
 */

open Interface;

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

module MakeReaderFunctor = (TYPE: TYPE) => {
  type reader('e, 'a) = Reader.t('e, 'a);
  module Functor: FUNCTOR with type t('a) = reader(TYPE.t, 'a) = {
    type t('a) = reader(TYPE.t, 'a);
    let map = Reader.map;
  };
};

module MakeFunctionFunctor = (TYPE: TYPE) => {
  type xt('a) = TYPE.t => 'a;
  module Functor: FUNCTOR with type t('a) = xt('a) = {
    type t('a) = xt('a);
    let map = (f, g, x) => f(g(x));
  };
};