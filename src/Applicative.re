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
  open Applicative;

  let (<$>) = Applicative.map;
  let (<*>) = Applicative.apply;

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
  let lift2: lift2('a, 'b, 'c) = (abc, ta, tb) => abc <$> ta <*> tb;

  /*
   Note the interesting pattern here:
   (a' => b' => c') <$> t('a) = t('b => 'c)
   */

  type lift2'('a, 'b, 'c) = (('a, 'b) => 'c, t('a), t('b)) => t('c);
  let lift2': lift2('a, 'b, 'c) =
    (abc, ta, tb) => {
      let tbc = map(abc, ta);
      apply(tbc, tb);
    };

  /*
   -- | Apply a ternary function in the environment.
   -- /can be written using `lift2` and `(<*>)`./
   --
   -- >>> lift3 (\a b c -> a + b + c) (ExactlyOne 7) (ExactlyOne 8) (ExactlyOne 9)
   -- ExactlyOne 24
   --
   -- >>> lift3 (\a b c -> a + b + c) (1 :. 2 :. 3 :. Nil) (4 :. 5 :. Nil) (6 :. 7 :. 8 :. Nil)
   -- [11,12,13,12,13,14,12,13,14,13,14,15,13,14,15,14,15,16]
   --
   -- >>> lift3 (\a b c -> a + b + c) (Full 7) (Full 8) (Full 9)
   -- Full 24
   --
   -- >>> lift3 (\a b c -> a + b + c) (Full 7) (Full 8) Empty
   -- Empty
   --
   -- >>> lift3 (\a b c -> a + b + c) Empty (Full 8) (Full 9)
   -- Empty
   --
   -- >>> lift3 (\a b c -> a + b + c) Empty Empty (Full 9)
   -- Empty
   --
   -- >>> lift3 (\a b c -> a + b + c) length sum product (listh [4,5,6])
   -- 138
   */
  type lift3('a, 'b, 'c, 'd) =
    (('a, 'b, 'c) => 'd, t('a), t('b), t('c)) => t('d);
  let lift3: lift3('a, 'b, 'c, 'd) =
    (abcd, ta, tb, tc) => lift2(abcd, ta, tb) <*> tc;

  /*
   -- | Apply a quaternary function in the environment.
   -- /can be written using `lift3` and `(<*>)`./
   --
   -- >>> lift4 (\a b c d -> a + b + c + d) (ExactlyOne 7) (ExactlyOne 8) (ExactlyOne 9) (ExactlyOne 10)
   -- ExactlyOne 34
   --
   -- >>> lift4 (\a b c d -> a + b + c + d) (1 :. 2 :. 3 :. Nil) (4 :. 5 :. Nil) (6 :. 7 :. 8 :. Nil) (9 :. 10 :. Nil)
   -- [20,21,21,22,22,23,21,22,22,23,23,24,21,22,22,23,23,24,22,23,23,24,24,25,22,23,23,24,24,25,23,24,24,25,25,26]
   --
   -- >>> lift4 (\a b c d -> a + b + c + d) (Full 7) (Full 8) (Full 9) (Full 10)
   -- Full 34
   --
   -- >>> lift4 (\a b c d -> a + b + c + d) (Full 7) (Full 8) Empty  (Full 10)
   -- Empty
   --
   -- >>> lift4 (\a b c d -> a + b + c + d) Empty (Full 8) (Full 9) (Full 10)
   -- Empty
   --
   -- >>> lift4 (\a b c d -> a + b + c + d) Empty Empty (Full 9) (Full 10)
   -- Empty
   --
   -- >>> lift4 (\a b c d -> a + b + c + d) length sum product (sum . filter even) (listh [4,5,6])
   -- 148
   */
  type lift4('a, 'b, 'c, 'd, 'e) =
    (('a, 'b, 'c, 'd) => 'e, t('a), t('b), t('c), t('d)) => t('e);
  let lift4: lift4('a, 'b, 'c, 'd, 'e) =
    (abcde, ta, tb, tc, td) => lift3(abcde, ta, tb, tc) <*> td;

  /*
   -- | Apply a nullary function in the environment.
   */
  type lift0('a) = 'a => t('a);
  let lift0 = a => pure(a);

  /*
   -- | Apply a unary function in the environment.
   -- /can be written using `lift0` and `(<*>)`./
   --
   -- >>> lift1 (+1) (ExactlyOne 2)
   -- ExactlyOne 3
   --
   -- >>> lift1 (+1) Nil
   -- []
   --
   -- >>> lift1 (+1) (1 :. 2 :. 3 :. Nil)
   -- [2,3,4]
   */
  type lift1('a, 'b) = ('a => 'b, t('a)) => t('b);
  let lift1: lift1('a, 'b) = (ab, ta) => lift0(ab) <*> ta;

  /*
   -- | Apply, discarding the value of the first argument.
   -- Pronounced, right apply.
   --
   -- >>> (1 :. 2 :. 3 :. Nil) *> (4 :. 5 :. 6 :. Nil)
   -- [4,5,6,4,5,6,4,5,6]
   --
   -- >>> (1 :. 2 :. Nil) *> (4 :. 5 :. 6 :. Nil)
   -- [4,5,6,4,5,6]
   --
   -- >>> (1 :. 2 :. 3 :. Nil) *> (4 :. 5 :. Nil)
   -- [4,5,4,5,4,5]
   --
   -- >>> Full 7 *> Full 8
   -- Full 8
   */
  type rightApply('a, 'b) = (t('a), t('b)) => t('b);
  let rightApply: rightApply('a, 'b) =
    (ta, tb) => {
      let f = (_, b) => b;
      lift2(f, ta, tb);
    };

  let ( *> ) = rightApply;
  /*
   Important: *lift* functions are *apply* functions.
   */

  /*
   -- | Apply, discarding the value of the second argument.
   -- Pronounced, left apply.
   --
   -- >>> (1 :. 2 :. 3 :. Nil) <* (4 :. 5 :. 6 :. Nil)
   -- [1,1,1,2,2,2,3,3,3]
   --
   -- >>> (1 :. 2 :. Nil) <* (4 :. 5 :. 6 :. Nil)
   -- [1,1,1,2,2,2]
   --
   -- >>> (1 :. 2 :. 3 :. Nil) <* (4 :. 5 :. Nil)
   -- [1,1,2,2,3,3]
   --
   -- >>> Full 7 <* Full 8
   -- Full 7
   */
  type leftApply('a, 'b) = (t('b), t('a)) => t('b);
  let leftApply: leftApply('a, 'b) =
    (tb, ta) => {
      let f = (b, _) => b;
      lift2(f, tb, ta);
    };

  let ( <* ) = leftApply;
};