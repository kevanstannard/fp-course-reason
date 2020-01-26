/*
 The Reader Monad solves the problem of threading
 the same configuration to many functions.
 */
type t('e, 'a) =
  | Reader('e => 'a);

/*
 run() executes a reader.
 */
type run('e, 'a) = (t('e, 'a), 'e) => 'a;
let run: run('e, 'a) = (Reader(r), env) => r(env);

/*
 return() returns a constant value independent
 of the provided environment value.
 */
type return('e, 'a) = 'a => t('e, 'a);
let return: return('e, 'a) = a => Reader(_env => a);

/*
 ask() returns the environment value.
 */
type ask('e) = unit => t('e, 'e);
let ask: ask('e) = () => Reader(env => env);

/*
 local() accepts a function that transforms the environment
 before applying it to the provided reader.
 */
type local('e, 'a) = ('e => 'e, t('e, 'a)) => t('e, 'a);
let local: local('e, 'a) = (f, r) => Reader(env => run(r, f(env)));

/*
 map() accepts a function that transforms the result
 of the provided reader. The map function does not have access
 to the environment.
 */
type map('e, 'a, 'b) = ('a => 'b, t('e, 'a)) => t('e, 'b);
let map: map('e, 'a, 'b) = (f, r) => Reader(env => f(run(r, env)));

/*
 bind() accepts a function that creates a new reader
 from the result of the provided reader.
 */
type bind('e, 'a, 'b) = ('a => t('e, 'b), t('e, 'a)) => t('e, 'b);
let bind: bind('e, 'a, 'b) =
  (f, r) => Reader(env => run(f(run(r, env)), env));

/*
 (>>=) is identical to bind with the arguments flipped.
 */
type bindFlip('a, 'e, 'b) = (t('e, 'a), 'a => t('e, 'b)) => t('e, 'b);
let (>>=): bindFlip('a, 'e, 'b) = (r, f) => bind(f, r);