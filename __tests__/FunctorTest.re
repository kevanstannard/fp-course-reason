/*
 open Jest;
 open Expect;
 open Functor.ExactlyOne;

 describe("Functor", () => {
   describe("ExactlyOne", () => {
     ExactlyOne.(
       test("fmap is correct", () => {
         let plusOne = x => x + 1;
         let value = ExactlyOne(2);
         let result = fmap(plusOne, value);
         expect(toString(result)) |> toBe("ExactlyOne(3)");
       })
     )
   })
 });
 */