let rec getUserInput player grid =
  print_string ("[" ^ (Player.toString player) ^ "] turn: ");
  let move = read_line () in
  if String.length move <> 3 || move.[1] <> ' ' || (move.[0] < '1' || move.[0] > '9') || (move.[2] < '1' || move.[2] > '9')
     || Grid.getCell grid ((int_of_char move.[2]) - (int_of_char '0')) ((int_of_char move.[0]) - (int_of_char '0')) <> "-"
  then
    begin
      print_endline "illegal move, please retry";
      print_endline "format \"row column\" with row and column integer between 1 and 9 included";
      getUserInput player grid
    end
  else (((int_of_char move.[2]) - (int_of_char '0')), ((int_of_char move.[0]) - (int_of_char '0')))

let playerMove player grid =
  let move = getUserInput player grid in
  let grid = Grid.replaceCell grid (fst move) (snd move) (Player.getSign player) in
  let grid = Grid.checkNestVictory grid (fst move) (snd move) player in
  Grid.print grid;
  grid

let getPlayerNames () =
  print_string "Player 1 enter your name : ";
  let player1 = Player.newPlayer "\027[32mo\027[0m" (read_line ()) in
  let player2 =
    let rec loop player1 =
      print_string "Player 2 enter your name : ";
      let tmp = (read_line ()) in
      if tmp <> Player.getName player1 then Player.newPlayer "\027[31mx\027[0m" tmp
      else
        begin
          print_endline "You cannot have the same name";
          loop player1
        end
    in loop player1
  in
  (player1, player2)

let newGame () =
  let players = getPlayerNames () in
  let grid = Grid.newGrid () in
  let rec gameLoop grid players =
    let grid = playerMove (fst players) grid in
    if Grid.checkVictory grid (fst players) then
      print_endline ("Game over ! ["^(Player.toString (fst players))^"] win the game !")
    else if Grid.checkFull grid then
      print_endline "Game over ! DRAW"
    else gameLoop grid (snd players, fst players)
  in gameLoop grid players
