open Jest;
open Expect;
open Listz;
open Util;

describe("Listz", () => {
  describe("headOr", () => {
    test("it returns the head", () => {
      let x = 0;
      let xs = [1, 2, 3];
      let result = headOr(x, xs);
      expect(result) |> toBe(1);
    });

    test("it returns the value", () => {
      let x = 0;
      let xs = [];
      let result = headOr(x, xs);
      expect(result) |> toBe(0);
    });
  });

  describe("headOr2", () => {
    test("it returns the head", () => {
      let x = 0;
      let xs = [1, 2, 3];
      let result = headOr2(x, xs);
      expect(result) |> toBe(1);
    });

    test("it returns the value", () => {
      let x = 0;
      let xs = [];
      let result = headOr2(x, xs);
      expect(result) |> toBe(0);
    });
  });

  describe("product", () => {
    test("it handles an empty list", () => {
      let xs = [];
      let result = product(xs);
      expect(result) |> toBe(1);
    });

    test("it produces the product of 1, 2, 3", () => {
      let xs = [1, 2, 3];
      let result = product(xs);
      expect(result) |> toBe(6);
    });

    test("it produces the product of 1, 2, 3, 4", () => {
      let xs = [1, 2, 3, 4];
      let result = product(xs);
      expect(result) |> toBe(24);
    });
  });

  describe("sum", () => {
    test("it handles an empty list", () => {
      let xs = [];
      let result = sum(xs);
      expect(result) |> toBe(0);
    });

    test("it produces the sum of 1, 2, 3", () => {
      let xs = [1, 2, 3];
      let result = sum(xs);
      expect(result) |> toBe(6);
    });

    test("it produces the sum of 1, 2, 3, 4", () => {
      let xs = [1, 2, 3, 4];
      let result = sum(xs);
      expect(result) |> toBe(10);
    });
  });

  describe("length", () => {
    test("it handles an empty list", () => {
      let xs = [];
      let result = length(xs);
      expect(result) |> toBe(0);
    });

    test("it handles a non-empty list", () => {
      let xs = [1, 2, 3];
      let result = length(xs);
      expect(result) |> toBe(3);
    });
  });

  describe("map", () => {
    test("it handles an empty list", () => {
      let xs = [];
      let result = map(x => x * 2, xs);
      expect(result) |> toBe([]);
    });

    test("it handles a non-empty listz", () => {
      let xs = [1, 2, 3];
      let f = x => x * 2;
      let result = map(f, xs);
      expect(result) |> toEqual([2, 4, 6]);
    });
  });

  describe("filter", () => {
    test("it correctly filters a list", () => {
      let xs = [1, 2, 3, 4, 5];
      let result = filter(Util.isEven, xs);
      expect(result) |> toEqual([2, 4]);
    })
  });

  describe("+++", () => {
    test("it correctly appends two lists", () => {
      let xs = [1, 2, 3];
      let ys = [4, 5, 6];
      let result = xs +++ ys;
      expect(result) |> toEqual([1, 2, 3, 4, 5, 6]);
    })
  });

  describe("flatten", () => {
    test("it correctly flattens an array of lists", () => {
      let xs = [1, 2];
      let ys = [3, 4];
      let zs = [5, 6];
      let result = flatten([xs, ys, zs]);
      expect(result) |> toEqual([1, 2, 3, 4, 5, 6]);
    })
  });

  describe("flatMap", () => {
    test("it correctly flatMaps a list", () => {
      let xs = [1, 2, 3];
      let f = x => [x, x + 1, x + 2];
      let result = flatMap(f, xs);
      expect(result) |> toEqual([1, 2, 3, 2, 3, 4, 3, 4, 5]);
    });

    test("it correctly flatMaps a list using compose", () => {
      let xs = [1, 2, 3];
      let f = x => [x, x + 1, x + 2];
      let result = flatMapWithCompose(f, xs);
      expect(result) |> toEqual([1, 2, 3, 2, 3, 4, 3, 4, 5]);
    });
  });

  describe("flattenAgain", () => {
    test("it correctly flattens an array of lists", () => {
      let xs = [1, 2];
      let ys = [3, 4];
      let zs = [5, 6];
      let result = flattenAgain([xs, ys, zs]);
      expect(result) |> toEqual([1, 2, 3, 4, 5, 6]);
    })
  });

  describe("seqOptional", () => {
    test("it correctly handles a list of values", () => {
      let xs = [Some(1), Some(2), Some(3)];
      let result = seqOptional(xs);
      expect(optionToString(result)) |> toEqual("Some(1,2,3,0)");
    });

    test("it correctly handles an empty list", () => {
      let xs = [];
      let result = seqOptional(xs);
      expect(optionToString(result)) |> toEqual("Some(0)");
    });

    test("it correctly handles a None value", () => {
      let xs = [Some(1), None, Some(10)];
      let result = seqOptional(xs);
      expect(optionToString(result)) |> toEqual("None");
    });
  });

  describe("find", () => {
    test("it returns None when no elements found", () => {
      let xs = [1, 3, 5];
      let result = find(isEven, xs);
      expect(optionToString(result)) |> toEqual("None");
    });

    test("it returns None for an empty list", () => {
      let xs = [];
      let result = find(isEven, xs);
      expect(optionToString(result)) |> toEqual("None");
    });

    test("it finds a matching element", () => {
      let xs = [1, 2, 3, 5];
      let result = find(isEven, xs);
      expect(optionToString(result)) |> toEqual("Some(2)");
    });

    test("it finds the first matching element", () => {
      let xs = [1, 2, 3, 4, 5];
      let result = find(isEven, xs);
      expect(optionToString(result)) |> toEqual("Some(2)");
    });
  });
});