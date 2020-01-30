/*
 https://andywhite.xyz/posts/2019-11-01-a-laymans-guide-to-functors-in-reasonml/
 https://github.com/Risto-Stevcev/bs-abstract/
 */

module type TYPE = {type t;};

/*
 -- | All instances of the `Functor` type-class must satisfy two laws. These laws
 -- are not checked by the compiler. These laws are given as:
 --
 -- * The law of identity
 --   `∀x. (id <$> x) ≅ x`
 --
 -- * The law of composition
 --   `∀f g x.(f . g <$> x) ≅ (f <$> (g <$> x))`
 */
module type FUNCTOR = {
  type t('a);
  let map: ('a => 'b, t('a)) => t('b);
};

/*
 -- | All instances of the `Applicative` type-class must satisfy four laws.
 -- These laws are not checked by the compiler. These laws are given as:
 --
 -- * The law of identity
 --   `∀x. pure id <*> x = x`
 --
 -- * The law of composition
 --   `∀u v w. pure (.) <*> u <*> v <*> w = u <*> (v <*> w)`
 --
 -- * The law of homomorphism
 --   `∀f x. pure f <*> pure x = pure (f x)`
 --
 -- * The law of interchange
 --   `∀u y. u <*> pure y = pure ($ y) <*> u`
 */

module type APPLICATIVE = {
  include FUNCTOR;
  let pure: 'a => t('a);
  let apply: (t('a => 'b), t('a)) => t('b);
};