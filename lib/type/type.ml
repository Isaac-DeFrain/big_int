(* least significant at head, most significant at tail *)
type t = int list

let t_of_int i = [i]

let rec add_one = function
| [] -> [1]
| h :: tl ->
  if h = max_int then
    h :: add_one tl
  else (h + 1) :: tl

let rec add x y = match (x, y) with
| (_, []) -> x
| ([], _) -> y
| (a :: ta, b :: tb) ->
  if a > 0 && b > 0 then
    if a + b <= 0 then
      (a - max_int + b) :: add (add_one ta) tb
    else (a + b) :: add ta tb
  else
    if a < 0 && b < 0 then
      if a + b >= 0 then
        (a + max_int + b) :: add (add_one ta) tb
      else (a + b) :: add ta tb
    else (a + b) :: add ta tb

let rec sub x y = match (x, y) with
| (_, []) -> x
| ([], _) -> List.map (fun x -> -x) y
| (a :: ta, b :: tb) ->
  if a > 0 && b < 0 then
    if a - b <= 0 then
      (a - max_int + b) :: sub (add_one ta) tb
    else (a - b) :: sub ta tb
  else
    if a < 0 && b > 0 then
      if a - b >= 0 then
        (a + max_int + b) :: sub (add_one ta) tb
      else (a - b) :: sub ta tb
    else (a - b) :: sub ta tb

let mul a b =
  let rec mul_helper acc a = function
  | [] -> acc
  | [0] -> [0]
  | 1 :: tl -> mul_helper (add acc a) a tl
  | h :: tl -> mul_helper (add acc a) a (h - 1 :: tl)
  in
  mul_helper [0] a b

let div _a _b = []

let mod_big _a _b = []
