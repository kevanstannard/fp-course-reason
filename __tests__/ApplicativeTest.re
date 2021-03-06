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

  describe("ApplicativeUtils", () => {
    describe("rightApply with lists", () => {
      module ApplicativeUtils = MakeApplicativeUtils(ListzApplicative);
      ApplicativeUtils.(
        test("lists are correct", () => {
          let list1 = [1, 2, 3];
          let list2 = [4, 5, 6];
          let result = list1 *> list2;
          expect(Belt.List.toArray(result))
          |> toEqual(Belt.List.toArray([4, 5, 6, 4, 5, 6, 4, 5, 6]));
        })
      );
    });

    describe("leftApply with lists", () => {
      module ApplicativeUtils = MakeApplicativeUtils(ListzApplicative);
      ApplicativeUtils.(
        test("lists are correct", () => {
          let list1 = [1, 2, 3];
          let list2 = [4, 5, 6];
          let result = list1 <* list2;
          expect(Belt.List.toArray(result))
          |> toEqual(Belt.List.toArray([1, 1, 1, 2, 2, 2, 3, 3, 3]));
        })
      );
    });

    describe("sequence with ExactlyOne", () => {
      module ApplicativeUtils = MakeApplicativeUtils(ExactlyOneApplicative);
      ApplicativeUtils.(
        test("ExactlyOne is correct", () => {
          let list = [
            ExactlyOne.ExactlyOne(7),
            ExactlyOne.ExactlyOne(8),
            ExactlyOne.ExactlyOne(9),
          ];
          let result = sequence(list);
          let expected = ExactlyOne.ExactlyOne([7, 8, 9]);
          expect(ExactlyOne.toString(result))
          |> toEqual(ExactlyOne.toString(expected));
        })
      );
    });

    describe("sequence with Lists", () => {
      module ApplicativeUtils = MakeApplicativeUtils(ListzApplicative);
      ApplicativeUtils.(
        test("list is correct", () => {
          let list = [[1, 2, 3], [1, 2]];
          let result = sequence(list);
          let expected = [
            [1, 1],
            [1, 2],
            [2, 1],
            [2, 2],
            [3, 1],
            [3, 2],
          ];
          expect(Belt.List.toArray(result))
          |> toEqual(Belt.List.toArray(expected));
        })
      );
    });

    describe("replicateA with Lists", () => {
      module ApplicativeUtils = MakeApplicativeUtils(ListzApplicative);
      ApplicativeUtils.(
        test("list is correct", () => {
          let list = ['a', 'b', 'c'];
          let result = replicateA(3, list);
          let expected = [
            // "aaa",
            ['a', 'a', 'a'],
            // "aab",
            ['a', 'a', 'b'],
            // "aac",
            ['a', 'a', 'c'],
            // "aba",
            ['a', 'b', 'a'],
            // "abb",
            ['a', 'b', 'b'],
            // "abc",
            ['a', 'b', 'c'],
            // "aca",
            ['a', 'c', 'a'],
            // "acb",
            ['a', 'c', 'b'],
            // "acc",
            ['a', 'c', 'c'],
            // "baa",
            ['b', 'a', 'a'],
            // "bab",
            ['b', 'a', 'b'],
            // "bac",
            ['b', 'a', 'c'],
            // "bba",
            ['b', 'b', 'a'],
            // "bbb",
            ['b', 'b', 'b'],
            // "bbc",
            ['b', 'b', 'c'],
            // "bca",
            ['b', 'c', 'a'],
            // "bcb",
            ['b', 'c', 'b'],
            // "bcc",
            ['b', 'c', 'c'],
            // "caa",
            ['c', 'a', 'a'],
            // "cab",
            ['c', 'a', 'b'],
            // "cac",
            ['c', 'a', 'c'],
            // "cba",
            ['c', 'b', 'a'],
            // "cbb",
            ['c', 'b', 'b'],
            // "cbc",
            ['c', 'b', 'c'],
            // "cca",
            ['c', 'c', 'a'],
            // "ccb",
            ['c', 'c', 'b'],
            // "ccc",
            ['c', 'c', 'c'],
          ];
          expect(result) |> toEqual(expected);
        })
      );
    });
  });

  describe("filtering with ExactlyOne", () => {
    module ApplicativeUtils = MakeApplicativeUtils(ExactlyOneApplicative);
    ApplicativeUtils.(
      test("ExactlyOne is correct", () => {
        let f = a => ExactlyOne.ExactlyOne(Util.isEven(a));
        let list = [4, 5, 6];
        let result = filtering(f, list);
        let expected = ExactlyOne.ExactlyOne([4, 6]);
        expect(ExactlyOne.toString(result))
        |> toEqual(ExactlyOne.toString(expected));
      })
    );
  });

  describe("filtering with Option", () => {
    module ApplicativeUtils = MakeApplicativeUtils(OptionApplicative);
    open ApplicativeUtils;
    test("option is correct (1)", () => {
      let f = a =>
        if (a > 13) {
          None;
        } else {
          Some(a <= 7);
        };
      let list = [4, 5, 6];
      let result = filtering(f, list);
      let expected = Some([4, 5, 6]);
      expect(result) |> toEqual(expected);
    });

    test("option is correct (2)", () => {
      let f = a =>
        if (a > 13) {
          None;
        } else {
          Some(a <= 7);
        };
      let list = [4, 5, 6, 7, 8, 9];
      let result = filtering(f, list);
      let expected = Some([4, 5, 6, 7]);
      expect(result) |> toEqual(expected);
    });

    test("option is correct (2)", () => {
      let f = a =>
        if (a > 13) {
          None;
        } else {
          Some(a <= 7);
        };
      let list = [4, 5, 6, 13, 14];
      let result = filtering(f, list);
      let expected = None;
      expect(result) |> toEqual(expected);
    });
  });

  describe("filtering with List", () => {
    module ApplicativeUtils = MakeApplicativeUtils(ListzApplicative);
    ApplicativeUtils.(
      test("ExactlyOne is correct", () => {
        let f = _ => [true, true];
        let list = [1, 2, 3];
        let result = filtering(f, list);
        let expected = [
          [1, 2, 3],
          [1, 2, 3],
          [1, 2, 3],
          [1, 2, 3],
          [1, 2, 3],
          [1, 2, 3],
          [1, 2, 3],
          [1, 2, 3],
        ];
        expect(result) |> toEqual(expected);
      })
    );
  });
});