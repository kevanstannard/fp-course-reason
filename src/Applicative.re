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