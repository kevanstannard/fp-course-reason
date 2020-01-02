open Jest;
open Expect;
open! Validation;

describe("ExactlyOne", () => {
  describe("isError", () => {
    test("isError should indicate an error", () => {
      let x = Error("message");
      let result = isError(x);
      expect(result) |> toBe(true);
    });

    test("isError should indicate not an error", () => {
      let x = Value(123);
      let result = isError(x);
      expect(result) |> toBe(false);
    });
  });

  describe("isValue", () => {
    test("isValue should indicate a value", () => {
      let x = Error("message");
      let result = isValue(x);
      expect(result) |> toBe(false);
    });

    test("isValue should indicate not a value", () => {
      let x = Value(123);
      let result = isValue(x);
      expect(result) |> toBe(true);
    });
  });

  describe("mapValidation", () => {
    test("mapValidation should map an error", () => {
      let x = Error("message");
      let f = n => n + 10;
      let result = x |> mapValidation(f);
      expect(toString(result)) |> toBe("Error(message)");
    });

    test("mapValidation should map a value", () => {
      let x = Value(7);
      let f = n => n + 10;
      let result = x |> mapValidation(f);
      expect(toString(result)) |> toBe("Value(17)");
    });
  });

  describe("bindValidation", () => {
    test("bindValidation should bind an error", () => {
      let f = n => Util.isEven(n) ? Value(n + 10) : Error("odd");
      let x = Error("message");
      let result = x |> bindValidation(f);
      expect(toString(result)) |> toBe("Error(message)");
    });

    test("bindValidation should bind an odd value", () => {
      let f = n => Util.isEven(n) ? Value(n + 10) : Error("odd");
      let x = Value(7);
      let result = x |> bindValidation(f);
      expect(toString(result)) |> toBe("Error(odd)");
    });

    test("bindValidation should bind an even value", () => {
      let f = n => Util.isEven(n) ? Value(n + 10) : Error("odd");
      let x = Value(8);
      let result = x |> bindValidation(f);
      expect(toString(result)) |> toBe("Value(18)");
    });
  });

  describe("valueOr", () => {
    test("valueOr handles an error", () => {
      let x = Error("message");
      let result = valueOr(x, 3);
      expect(result) |> toBe(3);
    });

    test("valueOr handles a value", () => {
      let x = Value(7);
      let result = valueOr(x, 3);
      expect(result) |> toBe(7);
    });
  });

  describe("errorOr", () => {
    test("errorOr handles an error", () => {
      let x = Error("message");
      let result = errorOr(x, "q");
      expect(result) |> toBe("message");
    });

    test("errorOr handles a value", () => {
      let x = Value(7);
      let result = errorOr(x, "q");
      expect(result) |> toBe("q");
    });
  });
});