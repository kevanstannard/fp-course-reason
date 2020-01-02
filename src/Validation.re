type err = string;

type validation('a) =
  | Error(err)
  | Value('a);

let toString = value => {
  switch (value) {
  | Error(e) => {j|Error($e)|j}
  | Value(a) => {j|Value($a)|j}
  };
};

/*
 -- | Returns whether or not the given validation is an error.
 --
 -- >>> isError (Error "message")
 -- True
 --
 -- >>> isError (Value 7)
 -- False
 --
 -- prop> \x -> isError x /= isValue x
 */
type isError('a) = validation('a) => bool;
let isError: isError('a) =
  value =>
    switch (value) {
    | Error(_) => true
    | Value(_) => false
    };

/*
 -- | Returns whether or not the given validation is a value.
 --
 -- >>> isValue (Error "message")
 -- False
 --
 -- >>> isValue (Value 7)
 -- True
 --
 -- prop> \x -> isValue x /= isError x
 */
type isValue('a) = validation('a) => bool;
let isValue: isValue('a) = value => !isError(value);

/*
 -- | Maps a function on a validation's value side.
 --
 -- >>> mapValidation (+10) (Error "message")
 -- Error "message"
 --
 -- >>> mapValidation (+10) (Value 7)
 -- Value 17
 --
 -- prop> \x -> mapValidation id x == x
 */

type mapValidation('a, 'b) = ('a => 'b, validation('a)) => validation('b);
let mapValidation: mapValidation('a, 'b) =
  (f, a) => {
    switch (a) {
    | Error(s) => Error(s)
    | Value(x) => Value(f(x))
    };
  };

/*
 -- | Binds a function on a validation's value side to a new validation.
 --
 -- >>> bindValidation (\n -> if even n then Value (n + 10) else Error "odd") (Error "message")
 -- Error "message"
 --
 -- >>> bindValidation (\n -> if even n then Value (n + 10) else Error "odd") (Value 7)
 -- Error "odd"
 --
 -- >>> bindValidation (\n -> if even n then Value (n + 10) else Error "odd") (Value 8)
 -- Value 18
 --
 -- prop> \x -> bindValidation Value x == x
 */

type bindValidation('a, 'b) =
  ('a => validation('b), validation('a)) => validation('b);
let bindValidation: bindValidation('a, 'b) =
  (f, a) => {
    switch (a) {
    | Error(s) => Error(s)
    | Value(x) => f(x)
    };
  };

/*
 -- | Returns a validation's value side or the given default if it is an error.
 --
 -- >>> valueOr (Error "message") 3
 -- 3
 --
 -- >>> valueOr (Value 7) 3
 -- 7
 --
 -- prop> \x -> isValue x || valueOr x n == n
 valueOr :: Validation a -> a -> a
 valueOr (Error _) a = a
 valueOr (Value a) _ = a
 */
type valueOr('a) = (validation('a), 'a) => 'a;
let valueOr: valueOr('a) =
  (a, b) => {
    switch (a) {
    | Error(_) => b
    | Value(x) => x
    };
  };

/*
   -- | Returns a validation's error side or the given default if it is a value.
 --
 -- >>> errorOr (Error "message") "q"
 -- "message"
 --
 -- >>> errorOr (Value 7) "q"
 -- "q"
 --
 -- prop> \x -> isError x || errorOr x e == e
 */
type errorOr('a) = (validation('a), err) => err;
let errorOr: errorOr('a) =
  (a, b) => {
    switch (a) {
    | Error(e) => e
    | Value(_) => b
    };
  };