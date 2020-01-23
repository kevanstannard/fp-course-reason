open Jest;
open Expect;
open Applicative;

describe("Functor", () => {
  describe("ExactlyOne", () => {
    test("apply is correct", () => {
      open ExactlyOne;
      let f = ExactlyOne((+)(10));
      let a = ExactlyOne(8);
      let result = ExactlyOneApplicative.apply(f, a);
      expect(runExactlyOne(result)) |> toBe(18);
    })
  })
});