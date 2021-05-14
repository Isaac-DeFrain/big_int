(* least significant at head, most significant at tail *)
type t = int list

let t_of_int i = [i]

let t_of_list l = l

let zero = [0]

let one = [1]

let compare a b =
  let open List in
  let len_a = length a in
  let len_b = length b in
  if len_a < len_b then -1
  else
    if len_a > len_b then 1
    else
      if len_a = 0 then 0
      else
        let rec cmp_helper len rev_a rev_b =
          if len = 0 then 0
          else
            let msc_a = hd rev_a in
            let msc_b = hd rev_b in
            if msc_a < msc_b then -1
            else
              if msc_a > msc_b then 1
              else cmp_helper (len - 1) (tl rev_a) (tl rev_b)
        in
        cmp_helper len_a (rev a) (rev b)

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
  | 0 :: tl -> mul_helper acc a tl
  | h :: tl -> mul_helper (add acc a) a (h - 1 :: tl)
  in
  mul_helper [0] a b

let div num denom =
  let rec div_helper q n d =
    let cmp = compare n d in
    if cmp < 0 then q
    else
      if cmp = 0 then add q one
      else div_helper (add q one) (sub n d) d
  in
  div_helper zero num denom

let rec mod_big n m =
  let cmp = compare n m in
  if cmp < 0 then n
  else
    if cmp = 0 then zero
    else mod_big (sub n m) m
