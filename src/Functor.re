/*
 https://github.com/data61/fp-course/blob/master/src/Course/Functor.hs
 https://andywhite.xyz/posts/2019-11-01-a-laymans-guide-to-functors-in-reasonml/
 https://github.com/Risto-Stevcev/bs-abstract/
 https://gist.github.com/twopoint718/6c93e1f886ecd7da1aef5f5b6687cba7
 https://hackernoon.com/the-reader-monad-part-1-1e4d947983a8
 */

open Interface;

/*
 -- | Maps a function on the ExactlyOne functor.
 --
 -- >>> (+1) <$> ExactlyOne 2
 -- ExactlyOne 3
 */
module ExactlyOneFunctor: FUNCTOR with type t('a) = ExactlyOne.exactlyOne('a) = {
  type t('a) = ExactlyOne.exactlyOne('a);
  let map = ExactlyOne.mapExactlyOne;
};

/*
 -- | Maps a function on the List functor.
 --
 -- >>> (+1) <$> Nil
 -- []
 --
 -- >>> (+1) <$> (1 :. 2 :. 3 :. Nil)
 -- [2,3,4]
 */
module ListzFunctor: FUNCTOR with type t('a) = list('a) = {
  type t('a) = list('a);
  let map = Listz.map;
};

/*
 -- | Maps a function on the Optional functor.
 --
 -- >>> (+1) <$> Empty
 -- Empty
 --
 -- >>> (+1) <$> Full 2
 -- Full 3
 */
module OptionFunctor: FUNCTOR with type t('a) = option('a) = {
  type t('a) = option('a);
  let map = (f, x) =>
    switch (x) {
    | None => None
    | Some(x) => Some(f(x))
    };
};

/*
 -- | Maps a function on the reader ((->) t) functor.
 --
 -- >>> ((+1) <$> (*2)) 8
 -- 17
 */
module MakeReaderFunctor = (TYPE: TYPE) => {
  type reader('e, 'a) = Reader.t('e, 'a);
  module Functor: FUNCTOR with type t('a) = reader(TYPE.t, 'a) = {
    type t('a) = reader(TYPE.t, 'a);
    let map = Reader.map;
  };
};

/*
 -- | Maps a function on the function ((->) t) functor.
 --
 -- >>> ((+1) <$> (*2)) 8
 -- 17
 */
module MakeFunctionFunctor = (TYPE: TYPE) => {
  type xt('a) = TYPE.t => 'a;
  module Functor: FUNCTOR with type t('a) = xt('a) = {
    type t('a) = xt('a);
    let map = (f, g, x) => f(g(x));
  };
};

module MakeFunctorUtils = (Functor: FUNCTOR) => {
  type t('a) = Functor.t('a);

  /*
    -- | Anonymous map. Maps a constant value on a functor.
    --
    -- >>> 7 <$ (1 :. 2 :. 3 :. Nil)
    -- [7,7,7]
    --
    -- prop> \x a b c -> x <$ (a :. b :. c :. Nil) == (x :. x :. x :. Nil)
    --
    -- prop> \x q -> x <$ Full q == Full x
   */
  type anonMap('a, 'b) = ('b, t('a)) => t('b);
  let anonMap: anonMap('a, 'b) =
    (b, ta) => {
      let f = Util.const(b);
      Functor.map(f, ta);
    };
  let (<$) = anonMap;

  /*
   -- | Anonymous map producing unit value.
   --
   -- >>> void (1 :. 2 :. 3 :. Nil)
   -- [(),(),()]
   --
   -- >>> void (Full 7)
   -- Full ()
   --
   -- >>> void Empty
   -- Empty
   --
   -- >>> void (+10) 5
   -- ()
   */
  type void('a) = t('a) => t(unit);
  let void: void('a) = f => () <$ f;
};