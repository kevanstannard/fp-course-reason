/*
 https://andywhite.xyz/posts/2019-11-01-a-laymans-guide-to-functors-in-reasonml/
 https://github.com/Risto-Stevcev/bs-abstract/
 */

module type TYPE = {type t;};

module type FUNCTOR = {
  type t('a);
  let map: ('a => 'b, t('a)) => t('b);
};