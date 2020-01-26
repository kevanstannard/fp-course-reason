/* WITH READER */

module WithReader = {
  open Reader;

  let hi = ask() >>= (name => return("Hi " ++ name));

  let bye = ask() >>= (name => return("Bye " ++ name));

  let hiBye = hi >>= (h => bye >>= (b => return((h, b))));

  let format = map(((h, b)) => h ++ ", " ++ b);

  let makeSuper = local(env => "Super " ++ env);

  let r = hiBye |> format |> makeSuper;

  let start = () => {
    let result = run(r, "Joe");
    Js.log(result);
  };
};

/* WITHOUT READER */

module WithoutReader = {
  let hi = name => "Hi " ++ name;

  let bye = name => "Bye " ++ name;

  let hiBye = name => {
    let h = hi(name);
    let b = bye(name);
    (h, b);
  };

  let format = ((h, b)) => h ++ ", " ++ b;

  let makeSuper = env => "Super " ++ env;

  let r = env => makeSuper(env) |> hiBye |> format;

  let start = () => {
    let result = r("Joe");
    Js.log(result);
  };
};

WithReader.start();
WithoutReader.start();