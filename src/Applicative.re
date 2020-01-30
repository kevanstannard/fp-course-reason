open Interface;

/*
 -- | Insert into ExactlyOne.
 --
 -- prop> \x -> pure x == ExactlyOne x
 --
 -- >>> ExactlyOne (+10) <*> ExactlyOne 8
 -- ExactlyOne 18
 */

module ExactlyOneApplicative:
  APPLICATIVE with type t('a) = Functor.ExactlyOneFunctor.t('a) = {
  type t('a) = Functor.ExactlyOneFunctor.t('a);

  let map = Functor.ExactlyOneFunctor.map;

  let pure = a => ExactlyOne.ExactlyOne(a);

  let apply = (f, a) => {
    let ExactlyOne.ExactlyOne(f') = f;
    let ExactlyOne.ExactlyOne(a') = a;
    ExactlyOne.ExactlyOne(f'(a'));
  };
};

/*
 -- | Insert into a List.
 --
 -- prop> \x -> pure x == x :. Nil
 --
 -- >>> (+1) :. (*2) :. Nil <*> 1 :. 2 :. 3 :. Nil
 -- [2,3,4,2,4,6]
 */

module ListzApplicative:
  APPLICATIVE with type t('a) = Functor.ListzFunctor.t('a) = {
  type t('a) = Functor.ListzFunctor.t('a);

  let map = Functor.ListzFunctor.map;

  let pure = a => [a];

  let apply = (fz, az) => {
    let mapFn = f => map(f, az);
    let mapResult = map(mapFn, fz);
    Listz.flatten(mapResult);
  };
};

/*
 -- | Insert into an Optional.
 --
 -- prop> \x -> pure x == Full x
 --
 -- >>> Full (+8) <*> Full 7
 -- Full 15
 --
 -- >>> Empty <*> Full 7
 -- Empty
 --
 -- >>> Full (+8) <*> Empty
 -- Empty
 */

module OptionApplicative:
  APPLICATIVE with type t('a) = Functor.OptionFunctor.t('a) = {
  type t('a) = Functor.OptionFunctor.t('a);

  let map = Functor.OptionFunctor.map;

  let pure = a => Some(a);

  let apply = (fOpt, aOpt) =>
    switch (fOpt, aOpt) {
    | (Some(f), Some(a)) => Some(f(a))
    | (_, _) => None
    };
};

/*
 -- | Insert into a constant function.
 --
 -- >>> ((+) <*> (+10)) 3
 -- 16
 --
 -- >>> ((+) <*> (+5)) 3
 -- 11
 --
 -- >>> ((+) <*> (+5)) 1
 -- 7
 --
 -- >>> ((*) <*> (+10)) 3
 -- 39
 --
 -- >>> ((*) <*> (+2)) 3
 -- 15
 --
 -- prop> \x y -> pure x y == x
 */

module MakeFunctionApplicative = (TYPE: TYPE) => {
  module FunctionFunctor = Functor.MakeFunctionFunctor(TYPE);
  module Applicative:
    APPLICATIVE with type t('a) = FunctionFunctor.Functor.t('a) = {
    type t('a) = FunctionFunctor.Functor.t('a);
    let map = FunctionFunctor.Functor.map;
    let pure = (a: 'a, _t: TYPE.t) => a;
    let apply = (tab: t('a => 'b), ta: t('a), t: TYPE.t) => tab(t, ta(t));
  };
};

module MakeApplicativeUtils = (Applicative: APPLICATIVE) => {
  type t('a) = Applicative.t('a);
  /*
   -- | Apply a binary function in the environment.
   --
   -- >>> lift2 (+) (ExactlyOne 7) (ExactlyOne 8)
   -- ExactlyOne 15
   --
   -- >>> lift2 (+) (1 :. 2 :. 3 :. Nil) (4 :. 5 :. Nil)
   -- [5,6,6,7,7,8]
   --
   -- >>> lift2 (+) (Full 7) (Full 8)
   -- Full 15
   --
   -- >>> lift2 (+) (Full 7) Empty
   -- Empty
   --
   -- >>> lift2 (+) Empty (Full 8)
   -- Empty
   --
   -- >>> lift2 (+) length sum (listh [4,5,6])
   -- 18
   */
  type lift2('a, 'b, 'c) = (('a, 'b) => 'c, t('a), t('b)) => t('c);
  let lift2: lift2('a, 'b, 'c) =
    (abc, ta, tb) => {
      let (<$>) = Applicative.map;
      let (<*>) = Applicative.apply;
      abc <$> ta <*> tb;
    };
  /*
   Note the interesting pattern here:
   (a' => b' => c') <$> t('a) = t('b => 'c)
   */

  type lift2'('a, 'b, 'c) = (('a, 'b) => 'c, t('a), t('b)) => t('c);
  let lift2': lift2('a, 'b, 'c) =
    Applicative.((abc, ta, tb) => apply(map(abc, ta), tb));
};