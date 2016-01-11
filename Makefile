RESULT = tictactictactoe
SOURCES = src/Player.mli src/Grid.mli src/Game.mli src/Player.ml src/Grid.ml src/Game.ml src/main.ml
all: native-code
include OCamlMakefile
