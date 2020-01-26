type t('e, 'a) =
  | Reader('e => 'a);

let run = (Reader(r), env) => r(env);

let return = a => Reader(_env => a);

let ask = () => Reader(env => env);

let local = (f, r) => Reader(env => run(r, f(env)));

let map = (f, r) => Reader(env => f(run(r, env)));

let bind = (f, r) => Reader(env => run(f(run(r, env)), env));

let (>>=) = (r, f) => bind(f, r);