open Jest;
open Expect;
open Applicative;

describe("Applicative", () => {
  describe("ExactlyOne", () => {
    test("pure is correct", () => {
      open ExactlyOne;
      let a = 123;
      let result = ExactlyOneApplicative.pure(a);
      expect(runExactlyOne(result)) |> toBe(123);
    });
    test("apply is correct", () => {
      open ExactlyOne;
      let f = ExactlyOne((+)(10));
      let a = ExactlyOne(8);
      let result = ExactlyOneApplicative.apply(f, a);
      expect(runExactlyOne(result)) |> toBe(18);
    });
  });

  describe("Listz", () => {
    test("pure is correct", () => {
      let a = 123;
      let result = ListzApplicative.pure(a);
      expect(result) |> toEqual([123]);
    });
    test("apply is correct", () => {
      let fz = [(+)(1), ( * )(2)];
      let az = [1, 2, 3];
      let result = ListzApplicative.apply(fz, az);
      expect(result) |> toEqual([2, 3, 4, 2, 4, 6]);
    });
  });

  describe("Option", () => {
    test("pure is correct", () => {
      let a = 123;
      let result = OptionApplicative.pure(a);
      expect(result) |> toEqual(Some(123));
    });

    test("apply is correct when the function and value are Some", () => {
      let f = Some((+)(8));
      let a = Some(7);
      let result = OptionApplicative.apply(f, a);
      expect(result) |> toEqual(Some(15));
    });

    test("apply is correct with the function is None and value is Some", () => {
      let f = None;
      let a = Some(7);
      let result = OptionApplicative.apply(f, a);
      expect(result) |> toEqual(None);
    });

    test("apply is correct with the function is Some and value is None", () => {
      let f = Some((+)(8));
      let a = None;
      let result = OptionApplicative.apply(f, a);
      expect(result) |> toEqual(None);
    });
  });

  describe("Function", () => {
    test("apply is correct (1)", () => {
      let f = (+);
      let g = (+)(10);
      module FunctionApplicative =
        MakeFunctionApplicative({
          type t = int;
        });
      let result = FunctionApplicative.Applicative.apply(f, g, 3);
      expect(result) |> toEqual(16);
    });

    test("apply is correct (2)", () => {
      let f = (+);
      let g = (+)(5);
      module FunctionApplicative =
        MakeFunctionApplicative({
          type t = int;
        });
      let result = FunctionApplicative.Applicative.apply(f, g, 3);
      expect(result) |> toEqual(11);
    });

    test("apply is correct (3)", () => {
      let f = (+);
      let g = (+)(5);
      module FunctionApplicative =
        MakeFunctionApplicative({
          type t = int;
        });
      let result = FunctionApplicative.Applicative.apply(f, g, 1);
      expect(result) |> toEqual(7);
    });

    test("apply is correct (4)", () => {
      let f = ( * );
      let g = (+)(10);
      module FunctionApplicative =
        MakeFunctionApplicative({
          type t = int;
        });
      let result = FunctionApplicative.Applicative.apply(f, g, 3);
      expect(result) |> toEqual(39);
    });

    test("apply is correct (5)", () => {
      let f = ( * );
      let g = (+)(2);
      module FunctionApplicative =
        MakeFunctionApplicative({
          type t = int;
        });
      let result = FunctionApplicative.Applicative.apply(f, g, 3);
      expect(result) |> toEqual(15);
    });
  });
});