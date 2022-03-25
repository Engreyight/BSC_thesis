execute if score rank memory > piecerank memory run data modify storage assembler:memory memory.children prepend from storage assembler:memory pieces[0]
execute if score rank memory < piecerank memory run data modify storage assembler:memory pieces[0].children prepend from storage assembler:memory memory
execute if score rank memory < piecerank memory run data modify storage assembler:memory memory set from storage assembler:memory pieces[0]
execute if score rank memory < piecerank memory store result score rank memory run data get storage assembler:memory memory.rank
data remove storage assembler:memory pieces[0]
execute store success score havepiece memory store result score piecerank memory run data get storage assembler:memory pieces[0].rank
execute unless score havepiece memory matches 0 run function assembler:library/zip1_loop