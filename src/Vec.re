let getX = ((x, _y): (float, float)) => x;
let getY = ((_x, y): (float, float)) => y;
let zero = (0.0, 0.0);

let print = ((x, y)) =>
  print_endline(
    "(" ++ string_of_float(x) ++ "," ++ string_of_float(y) ++ ")",
  );

// 2d Addition
let (+>) = ((x1, y1), (x2, y2)) => (x1 +. x2, y1 +. y2);
// 2d Scalar Multiplication
let ( *> ) = ((x1, y1), factor) => (x1 *. factor, y1 *. factor);

let length = ((x, y)) => sqrt(x *. x +. y *. y);

let cross = ((x1, y1), (x2, y2)) => x1 *. y2 -. y1 *. x2;