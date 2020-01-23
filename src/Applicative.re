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