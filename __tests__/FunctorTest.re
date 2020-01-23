/*
 http://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html
 https://medium.com/@l.mugnaini/functors-applicatives-and-monads-in-pictures-784c2b5786f7
 */

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
      expect(Reader.run(result, 8)) |> toEqual(17);
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
      expect(fg(8)) |> toEqual(17);
    })
  });

  describe("Anon Map", () => {
    test("anon map is correct for Listz", () => {
      module FunctorUtilsList = MakeFunctorUtils(ListzFunctor);
      let value = 7;
      let list = [1, 2, 3];
      let result1 = FunctorUtilsList.anonMap(value, list);
      let result2 = FunctorUtilsList.(value <$ list);
      expect(result1) |> toEqual(result2) |> ignore;
      expect(result2) |> toEqual([7, 7, 7]);
    });

    test("anon map is correct for Option", () => {
      module FunctorUtilsReader = MakeFunctorUtils(OptionFunctor);
      let value = 7;
      let opt = Some(123);
      let result = FunctorUtilsReader.anonMap(value, opt);
      expect(result) |> toEqual(Some(7));
    });

    test("anon map is correct for Reader", () => {
      module ReaderIntFunctor =
        MakeReaderFunctor({
          type t = int;
        });
      module FunctorUtilsReader = MakeFunctorUtils(ReaderIntFunctor.Functor);
      let value = 7;
      let f = Reader.Reader((+)(1));
      let g = FunctorUtilsReader.anonMap(value, f);
      let result = Reader.run(g, 1);
      expect(result) |> toEqual(7);
    });
  });

  describe("Void", () => {
    test("void is correct for Listz", () => {
      module FunctorUtilsList = MakeFunctorUtils(ListzFunctor);
      let result = FunctorUtilsList.void([1, 2, 3]);
      expect(result) |> toEqual([(), (), ()]);
    });

    test("void is correct for Option Some", () => {
      module FunctorUtilsOption = MakeFunctorUtils(OptionFunctor);
      let result = FunctorUtilsOption.void(Some(7));
      expect(result) |> toEqual(Some());
    });

    test("void is correct for Option None", () => {
      module FunctorUtilsOption = MakeFunctorUtils(OptionFunctor);
      let result = FunctorUtilsOption.void(None);
      expect(result) |> toEqual(None);
    });

    test("void is correct for Reader", () => {
      module ReaderIntFunctor =
        MakeReaderFunctor({
          type t = int;
        });
      module FunctorUtilsReader = MakeFunctorUtils(ReaderIntFunctor.Functor);
      let f = Reader.Reader((+)(10));
      let g = FunctorUtilsReader.void(f);
      let result = Reader.run(g, 5);
      expect(result) |> toEqual();
    });

    test("void is correct for Function", () => {
      let f = (+)(10);
      module FunctionFunctorInt =
        MakeFunctionFunctor({
          type t = int;
        });
      module FunctorUtilsFunction =
        MakeFunctorUtils(FunctionFunctorInt.Functor);
      let g = FunctorUtilsFunction.void(f);
      let result = g(5);
      expect(result) |> toEqual();
    });
  });
});