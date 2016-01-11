type t = string list list

let newGrid () =
  [
    ["-";"-";"-";"-";"-";"-";"-";"-";"-"];
    ["-";"-";"-";"-";"-";"-";"-";"-";"-"];
    ["-";"-";"-";"-";"-";"-";"-";"-";"-"];
    ["-";"-";"-";"-";"-";"-";"-";"-";"-"];
    ["-";"-";"-";"-";"-";"-";"-";"-";"-"];
    ["-";"-";"-";"-";"-";"-";"-";"-";"-"];
    ["-";"-";"-";"-";"-";"-";"-";"-";"-"];
    ["-";"-";"-";"-";"-";"-";"-";"-";"-"];
    ["-";"-";"-";"-";"-";"-";"-";"-";"-"]
  ]

let rec printLine lst = match lst with
  | [] -> print_endline ""
  | f::s::t::q ->
     print_string (" "^f^" ");
     print_string (" "^s^" ");
     print_string (" "^t^" ");
     print_string " | ";
     printLine q
  | _ -> print_endline "Grid error"

let rec printGrid lst = match lst with
  | [] -> print_endline "";
  | f::s::t::q ->
     print_string " | "; printLine f;
     print_string " | "; printLine s;
     print_string " | "; printLine t;
     print_endline " -------------------------------------";
     printGrid q
  | _ -> print_endline "Grid error"

let print self =
  print_endline " -------------------------------------";
  printGrid self

let getCell self col row =
  let lstCol = List.nth self (row - 1) in
  List.nth lstCol (col - 1)

let replaceCell self col row newCell =
  let rec loopRow lst posRow accRow = match lst with
    | [] -> accRow
    | hr::qr ->
       let rec loopCol lst posCol accCol = match lst with
         | []   -> accCol
         | hc::qc ->
            if posRow = row && posCol = col then loopCol qc (posCol + 1) (accCol@[newCell])
            else loopCol qc (posCol + 1) (accCol@[hc])
       in loopRow qr (posRow + 1) (accRow@[(loopCol hr 1 [])])
  in loopRow self 1 []

let getNestOrigin axis =
  if axis mod 3 = 0 then axis - 3 + 1
  else axis - (axis mod 3) + 1

let getNestNumber col row =
  if col = 1 && row = 1 then 1
  else if col = 4 && row = 1 then 2
  else if col = 7 && row = 1 then 3
  else if col = 1 && row = 4 then 4
  else if col = 4 && row = 4 then 5
  else if col = 7 && row = 4 then 6
  else if col = 1 && row = 7 then 7
  else if col = 4 && row = 7 then 8
  else if col = 7 && row = 7 then 9
  else 0

let isNestWon grid col row =
  if (getCell grid col row = "\027[32m/\027[0m" || getCell grid col row = "\027[31m\\\027[0m")
  then true
  else false

let isNestFull grid col row =
  if getCell grid col row <> "-"
     && getCell grid (col + 1) row <> "-"
     && getCell grid (col + 2) row <> "-"
     && getCell grid col (row + 1) <> "-"
     && getCell grid (col + 1) (row + 1) <> "-"
     && getCell grid (col + 2) (row + 1) <> "-"
     && getCell grid col (row + 2) <> "-"
     && getCell grid (col + 1) (row + 2) <> "-"
     && getCell grid (col + 2) (row + 2) <> "-"
  then true
  else false

let hasNestAlign grid col row s =
  if (s = getCell grid col row
      && s = getCell grid (col + 1) row
      && s = getCell grid (col + 2) row)
     ||
       (s = getCell grid col (row + 1)
        && s = getCell grid (col + 1) (row + 1)
        && s = getCell grid (col + 2) (row + 1))
     ||
       (s = getCell grid col (row + 2)
        && s = getCell grid (col + 1) (row + 2)
        && s = getCell grid (col + 2) (row + 2))
     ||
       (s = getCell grid col row
        && s = getCell grid col (row + 1)
        && s = getCell grid col (row + 2))
     ||
       (s = getCell grid (col + 1) row
        && s = getCell grid (col + 1) (row + 1)
        && s = getCell grid (col + 1) (row + 2))
     ||
       (s = getCell grid (col + 2) row
        && s = getCell grid (col + 2) (row + 1)
        && s = getCell grid (col + 2) (row + 2))
     ||
       (s = getCell grid col row
        && s = getCell grid (col + 1) (row + 1)
        && s = getCell grid (col + 2) (row + 2))
     ||
       (s = getCell grid (col + 2) row
        && s = getCell grid (col + 1) (row + 1)
        && s = getCell grid col (row + 2))
  then true
  else false

let replaceNest grid col row player =
  if Player.getSign player = "\027[32mo\027[0m" then
    begin
      let grid = replaceCell grid col row "\027[32m/\027[0m" in
      let grid = replaceCell grid (col + 1) row "\027[32m¯\027[0m" in
      let grid = replaceCell grid (col + 2) row "\027[32m\\\027[0m" in
      let grid = replaceCell grid col (row + 1) "\027[32m|\027[0m" in
      let grid = replaceCell grid (col + 1) (row + 1) "\027[32m \027[0m" in
      let grid = replaceCell grid (col + 2) (row + 1) "\027[32m|\027[0m" in
      let grid = replaceCell grid col (row + 2) "\027[32m\\\027[0m" in
      let grid = replaceCell grid (col + 1) (row + 2) "\027[32m_\027[0m" in
      let grid = replaceCell grid (col + 2) (row + 2) "\027[32m/\027[0m" in
      grid
    end
  else
    begin
      let grid = replaceCell grid col row "\027[31m\\\027[0m" in
      let grid = replaceCell grid (col + 1) row "\027[31m \027[0m" in
      let grid = replaceCell grid (col + 2) row "\027[31m/\027[0m" in
      let grid = replaceCell grid col (row + 1) "\027[31m \027[0m" in
      let grid = replaceCell grid (col + 1) (row + 1) "\027[31mcol\027[0m" in
      let grid = replaceCell grid (col + 2) (row + 1) "\027[31m \027[0m" in
      let grid = replaceCell grid col (row + 2) "\027[31m/\027[0m" in
      let grid = replaceCell grid (col + 1) (row + 2) "\027[31m \027[0m" in
      let grid = replaceCell grid (col + 2) (row + 2) "\027[31m\\\027[0m" in
      grid
    end

let checkNestVictory grid col row player =
  let nestcol = getNestOrigin col in
  let nestrow = getNestOrigin row in
  let nestNumber = getNestNumber nestcol nestrow in
  if hasNestAlign grid nestcol nestrow (Player.getSign player)
     || isNestFull grid nestcol nestrow then
    begin
      print_endline ((Player.toString player) ^ " has won the grid " ^ (string_of_int nestNumber) ^ " !");
      replaceNest grid nestcol nestrow player
    end
  else grid

let checkVictory grid player =
  let s =
    if (Player.getSign player) = "\027[32mo\027[0m" then "\027[32m/\027[0m"
    else "\027[31m\\\027[0m" in
  if (s = getCell grid 1 1
      && s = getCell grid 4 1
      && s = getCell grid 7 1)
     ||
       (s = getCell grid 1 4
        && s = getCell grid 4 4
        && s = getCell grid 7 4)
     ||
       (s = getCell grid 1 7
        && s = getCell grid 4 7
        && s = getCell grid 7 7)
     ||
       (s = getCell grid 1 1
        && s = getCell grid 1 4
        && s = getCell grid 1 7)
     ||
       (s = getCell grid 4 1
        && s = getCell grid 4 4
        && s = getCell grid 4 7)
     ||
       (s = getCell grid 7 1
        && s = getCell grid 7 4
        && s = getCell grid 7 7)
     ||
       (s = getCell grid 1 1
        && s = getCell grid 4 4
        && s = getCell grid 7 7)
     ||
       (s = getCell grid 7 1
        && s = getCell grid 4 4
        && s = getCell grid 1 7)
  then true
  else false

let checkFull grid =
  if isNestFull grid 1 1
     && isNestFull grid 4 1
     && isNestFull grid 7 1
     && isNestFull grid 1 4
     && isNestFull grid 4 4
     && isNestFull grid 7 4
     && isNestFull grid 1 7
     && isNestFull grid 4 7
     && isNestFull grid 7 7
  then true
  else false
