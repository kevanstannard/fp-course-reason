open Jest;
open Expect;
open ExactlyOne;

describe("ExactlyOne", () => {
  test("runExactlyOne should extract a value", () => {
    let x = ExactlyOne("hello");
    let result = runExactlyOne(x);
    expect(result) |> toBe("hello");
  });

  test("mapExactlyOne should map a value", () => {
    let x = ExactlyOne(2);
    let f = n => n * 2;
    let result = x |> mapExactlyOne(f);
    expect(toString(result)) |> toBe("ExactlyOne(4)");
  });

  test("bindExactlyOne should bind a value", () => {
    let x = ExactlyOne("World");
    let f = s => ExactlyOne("Hello " ++ s);
    let result = x |> bindExactlyOne(f);
    expect(toString(result)) |> toBe("ExactlyOne(Hello World)");
  });
});