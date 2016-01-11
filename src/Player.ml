type t = { sign: string; name: string }

let newPlayer s n = { sign = s; name = n }
let getSign self = self.sign
let getName self = self.name
let toString self = "Player " ^ self.sign ^ ": " ^ self.name
