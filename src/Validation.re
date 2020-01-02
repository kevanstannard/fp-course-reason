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