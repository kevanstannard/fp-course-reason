open Util;
/*
 https://github.com/data61/fp-course/blob/master/src/Course/List.hs
 */

/*
 Do not use any functions in the List module, except:
 - List.fold_left
 - List.fold_right
 */

/*
 -- | Returns the head of the list or the given default.
 --
 -- >>> headOr 3 (1 :. 2 :. Nil)
 -- 1
 --
 -- >>> headOr 3 Nil
 -- 3
 --
 -- prop> \x -> x `headOr` infinity == 0
 --
 -- prop> \x -> x `headOr` Nil == x
 headOr ::
   a
   -> List a
   -> a
 headOr =
   error "todo: Course.List#headOr"
 */

type headOr('a) = ('a, list('a)) => 'a;

let headOr: headOr('a) =
  (x, xs) => {
    switch (xs) {
    | [] => x
    | [h, ..._] => h
    };
  };

let headOr2: headOr('a) = (z, xs) => List.fold_right(Util.const, xs, z);

/*
 -- | The product of the elements of a list.
 --
 -- >>> product Nil
 -- 1
 --
 -- >>> product (1 :. 2 :. 3 :. Nil)
 -- 6
 --
 -- >>> product (1 :. 2 :. 3 :. 4 :. Nil)
 -- 24
 product ::
   List Int
   -> Int
 product =
   error "todo: Course.List#product"
 */

type product = list(int) => int;
let product: product = xs => xs |> List.fold_left(( * ), 1);

/*
 -- | Sum the elements of the list.
 --
 -- >>> sum (1 :. 2 :. 3 :. Nil)
 -- 6
 --
 -- >>> sum (1 :. 2 :. 3 :. 4 :. Nil)
 -- 10
 --
 -- prop> \x -> foldLeft (-) (sum x) x == 0
 sum ::
   List Int
   -> Int
 sum =
   error "todo: Course.List#sum"
 */

type sum = list(int) => int;
let sum: sum = xs => xs |> List.fold_left((+), 0);

/*
 -- | Return the length of the list.
 --
 -- >>> length (1 :. 2 :. 3 :. Nil)
 -- 3
 --
 -- prop> \x -> sum (map (const 1) x) == length x
 length ::
   List a
   -> Int
 length =
   error "todo: Course.List#length"
 */

type length('a) = list('a) => int;
let length = xs => xs |> List.fold_left((acc, _) => acc + 1, 0);

/*
 -- | Map the given function on each element of the list.
 --
 -- >>> map (+10) (1 :. 2 :. 3 :. Nil)
 -- [11,12,13]
 --
 -- prop> \x -> headOr x (map (+1) infinity) == 1
 --
 -- prop> \x -> map id x == x
 map ::
   (a -> b)
   -> List a
   -> List b
 map =
   error "todo: Course.List#map"
 */

type map('a, 'b) = ('a => 'b, list('a)) => list('b);
let map: map('a, 'b) =
  (f, xs) => List.fold_right((x, acc) => [f(x), ...acc], xs, []);

/*
 -- | Return elements satisfying the given predicate.
 --
 -- >>> filter even (1 :. 2 :. 3 :. 4 :. 5 :. Nil)
 -- [2,4]
 --
 -- prop> \x -> headOr x (filter (const True) infinity) == 0
 --
 -- prop> \x -> filter (const True) x == x
 --
 -- prop> \x -> filter (const False) x == Nil
 filter ::
   (a -> Bool)
   -> List a
   -> List a
 filter =
   error "todo: Course.List#filter"
 */
type filter('a) = ('a => bool, list('a)) => list('a);
let filter: filter('a) =
  (f, xs) =>
    List.fold_right((x, acc) => f(x) ? [x, ...acc] : acc, xs, []);

/*
  -- | Append two lists to a new list.
  --
  -- >>> (1 :. 2 :. 3 :. Nil) ++ (4 :. 5 :. 6 :. Nil)
  -- [1,2,3,4,5,6]
  --
  -- prop> \x -> headOr x (Nil ++ infinity) == 0
  --
  -- prop> \x -> headOr x (y ++ infinity) == headOr 0 y
  --
  -- prop> \x -> (x ++ y) ++ z == x ++ (y ++ z)
  --
  -- prop> \x -> x ++ Nil == x
  (++) ::
   List a
   -> List a
   -> List a
 (++) =
   error "todo: Course.List#(++)"
  */

type append('a) = (list('a), list('a)) => list('a);
let (+++): append('a) =
  (xs, ys) => List.fold_right((x, acc) => [x, ...acc], xs, ys);

/*
 -- | Flatten a list of lists to a list.
 --
 -- >>> flatten ((1 :. 2 :. 3 :. Nil) :. (4 :. 5 :. 6 :. Nil) :. (7 :. 8 :. 9 :. Nil) :. Nil)
 -- [1,2,3,4,5,6,7,8,9]
 --
 -- prop> \x -> headOr x (flatten (infinity :. y :. Nil)) == 0
 --
 -- prop> \x -> headOr x (flatten (y :. infinity :. Nil)) == headOr 0 y
 --
 -- prop> \x -> sum (map length x) == length (flatten x)
 flatten ::
   List (List a)
   -> List a
 flatten =
   error "todo: Course.List#flatten"
 */

type flatten('a) = list(list('a)) => list('a);
let flatten: flatten('a) = xs => List.fold_right((+++), xs, []);

/*
 -- | Map a function then flatten to a list.
 --
 -- >>> flatMap (\x -> x :. x + 1 :. x + 2 :. Nil) (1 :. 2 :. 3 :. Nil)
 -- [1,2,3,2,3,4,3,4,5]
 --
 -- prop> \x -> headOr x (flatMap id (infinity :. y :. Nil)) == 0
 --
 -- prop> \x -> headOr x (flatMap id (y :. infinity :. Nil)) == headOr 0 y
 --
 -- prop> \x -> flatMap id (x :: List (List Int)) == flatten x
 flatMap ::
   (a -> List b)
   -> List a
   -> List b
 flatMap =
   error "todo: Course.List#flatMap"
 */
type flatMap('a, 'b) = ('a => list('b), list('a)) => list('b);
let flatMap: flatMap('a, 'b) = (f, xs) => flatten(map(f, xs));
let flatMap2: flatMap('a, 'b) = (f, xs) => xs |> map(f) |> flatten;
let flatMapWithCompose: flatMap('a, 'b) = f => flatten <.> map(f);

/*
 -- | Flatten a list of lists to a list (again).
 -- HOWEVER, this time use the /flatMap/ function that you just wrote.
 --
 -- prop> \x -> let types = x :: List (List Int) in flatten x == flattenAgain x
 flattenAgain ::
   List (List a)
   -> List a
 flattenAgain =
   error "todo: Course.List#flattenAgain"
 */

type flattenAgain('a) = list(list('a)) => list('a);
let flattenAgain: flattenAgain('a) = xs => flatMap(id, xs);

/*
 -- | Convert a list of optional values to an optional list of values.
 --
 -- * If the list contains all `Full` values,
 -- then return `Full` list of values.
 --
 -- * If the list contains one or more `Empty` values,
 -- then return `Empty`.
 --
 -- * The only time `Empty` is returned is
 -- when the list contains one or more `Empty` values.
 --
 -- >>> seqOptional (Full 1 :. Full 10 :. Nil)
 -- Full [1,10]
 --
 -- >>> seqOptional Nil
 -- Full []
 --
 -- >>> seqOptional (Full 1 :. Full 10 :. Empty :. Nil)
 -- Empty
 --
 -- >>> seqOptional (Empty :. map Full infinity)
 -- Empty
 seqOptional ::
   List (Optional a)
   -> Optional (List a)
 seqOptional =
   error "todo: Course.List#seqOptional"
 */

/*
 Belt.Option.map(xOpt, x => {
   Belt.Option.map(accOpt, acc => [x, ...acc])
 })
 */

type seqOptional('a) = list(option('a)) => option(list('a));
let seqOptional: seqOptional('a) =
  xs =>
    List.fold_right(
      (xOpt, accOpt) => {
        switch (xOpt) {
        | None => None
        | Some(x) =>
          switch (accOpt) {
          | None => None
          | Some(acc) => Some([x, ...acc])
          }
        }
      },
      xs,
      Some([]),
    );

let seqOptional2: seqOptional('a) =
  xs => {
    let f = (xOpt, accOpt) =>
      Belt.Option.flatMap(xOpt, x =>
        Belt.Option.flatMap(accOpt, acc => {Some([x, ...acc])})
      );
    List.fold_right(f, xs, Some([]));
  };

/*
 -- | Find the first element in the list matching the predicate.
 --
 -- >>> find even (1 :. 3 :. 5 :. Nil)
 -- Empty
 --
 -- >>> find even Nil
 -- Empty
 --
 -- >>> find even (1 :. 2 :. 3 :. 5 :. Nil)
 -- Full 2
 --
 -- >>> find even (1 :. 2 :. 3 :. 4 :. 5 :. Nil)
 -- Full 2
 --
 -- >>> find (const True) infinity
 -- Full 0
 find ::
   (a -> Bool)
   -> List a
   -> Optional a
 find =
   error "todo: Course.List#find"
 */

type find('a) = ('a => bool, list('a)) => option('a);
let rec find: find('a) =
  (f, xs) => {
    switch (xs) {
    | [] => None
    | [x, ...rest] => f(x) ? Some(x) : find(f, rest)
    };
  };

let find2: find('a) =
  (f, xs) =>
    switch (filter(f, xs)) {
    | [] => None
    | [x, ..._] => Some(x)
    };

/*
 -- | Determine if the length of the given list is greater than 4.
 --
 -- >>> lengthGT4 (1 :. 3 :. 5 :. Nil)
 -- False
 --
 -- >>> lengthGT4 Nil
 -- False
 --
 -- >>> lengthGT4 (1 :. 2 :. 3 :. 4 :. 5 :. Nil)
 -- True
 --
 -- >>> lengthGT4 infinity
 -- True
 lengthGT4 ::
   List a
   -> Bool
 lengthGT4 =
   error "todo: Course.List#lengthGT4"
 */

type lengthGT4('a) = list('a) => bool;
let lengthGT4: lengthGT4('a) =
  xs => {
    switch (xs) {
    | [_, _, _, _, _] => true
    | _ => false
    };
  };