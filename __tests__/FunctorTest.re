open Jest;
open Expect;
open Functor;

describe("Functor", () => {
  describe("ExactlyOne", () => {
    test("map is correct", () => {
      let plusOne = (+)(1);
      let value = ExactlyOne.ExactlyOne(2);
      let result = ExactlyOneFunctor.map(plusOne, value);
      expect(ExactlyOne.toString(result)) |> toBe("ExactlyOne(3)");
    })
  });

  describe("Listz", () => {
    test("map is correct for an empty list", () => {
      let plusOne = (+)(1);
      let value = [];
      let result = ListzFunctor.map(plusOne, value);
      expect(result) |> toEqual([]);
    });

    test("map is correct for a non empty list", () => {
      let plusOne = (+)(1);
      let value = [1, 2, 3];
      let result = ListzFunctor.map(plusOne, value);
      expect(result) |> toEqual([2, 3, 4]);
    });
  });

  describe("Option", () => {
    test("map is correct for None", () => {
      let plusOne = (+)(1);
      let value = None;
      let result = OptionFunctor.map(plusOne, value);
      expect(result) |> toEqual(None);
    });

    test("map is correct for Some", () => {
      let plusOne = (+)(1);
      let value = Some(2);
      let result = OptionFunctor.map(plusOne, value);
      expect(result) |> toEqual(Some(3));
    });
  });

  describe("Reader", () => {
    test("map is correct", () => {
      let plusOne = (+)(1);
      let reader = Reader.Reader(( * )(2));
      module ReaderFunctorInt =
        MakeReaderFunctor({
          type t = int;
        });
      let result = ReaderFunctorInt.Functor.map(plusOne, reader);
      expect(Reader.run(result, 2)) |> toEqual(5);
    })
  });

  describe("Function", () => {
    test("map is correct", () => {
      let f = (+)(1);
      let g = ( * )(2);
      module FunctionFunctorInt =
        MakeFunctionFunctor({
          type t = int;
        });
      let fg = FunctionFunctorInt.Functor.map(f, g);
      expect(fg(2)) |> toEqual(5);
    })
  });
});