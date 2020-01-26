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
  APPLICATIVE with type t('a) = ExactlyOne.exactlyOne('a) = {
  type t('a) = ExactlyOne.exactlyOne('a);

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
module ListzApplicative: APPLICATIVE with type t('a) = list('a) = {
  type t('a) = list('a);

  let pure = a => [a];

  let apply = (fz, az) => {
    open Listz;
    let mapFn = f => map(f, az);
    let mapResult = map(mapFn, fz);
    flatten(mapResult);
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
module OptionApplicative: APPLICATIVE with type t('a) = option('a) = {
  type t('a) = option('a);

  let pure = a => Some(a);

  let apply = (fOpt, aOpt) =>
    switch (fOpt, aOpt) {
    | (Some(f), Some(a)) => Some(f(a))
    | (_, _) => None
    };
};