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
});