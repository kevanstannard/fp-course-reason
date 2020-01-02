let isEven: int => bool = [%bs.raw {| n => n % 2 == 0 |}];

let isOdd: int => bool = value => !isEven(value);