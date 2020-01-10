type t('e, 'a) =
  | Reader('e => 'a);

type run('e, 'a) = (t('e, 'a), 'e) => 'a;
let run: run('e, 'a) = (Reader(r), env) => r(env);

type return('e, 'a) = 'a => t('e, 'a);
let return: return('e, 'a) = a => Reader(_env => a);

type ask('e) = unit => t('e, 'e);
let ask: ask('e) = () => Reader(env => env);

type local('e, 'a) = ('e => 'e, t('e, 'a)) => t('e, 'a);
let local: local('e, 'a) = (f, m) => Reader(env => run(m, f(env)));

type map('e, 'a, 'b) = ('a => 'b, t('e, 'a)) => t('e, 'b);
let map: map('e, 'a, 'b) = (f, m) => Reader(env => f(run(m, env)));

type bind('e, 'a, 'b) = ('a => t('e, 'b), t('e, 'a)) => t('e, 'b);
let bind: bind('e, 'a, 'b) =
  (f, m) => Reader(env => run(f(run(m, env)), env));

type bindFlip('a, 'e, 'b) = (t('e, 'a), 'a => t('e, 'b)) => t('e, 'b);
let (>>=): bindFlip('a, 'e, 'b) = (m, f) => bind(f, m);