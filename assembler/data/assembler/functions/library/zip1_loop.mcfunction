execute store result score rank memory run data get storage assembler:memory memory.rank
execute store result score piecerank memory run data get storage assembler:memory pieces[0].rank
execute if score rank memory > piecerank memory run data modify storage assembler:memory memory.children prepend from storage assembler:memory pieces[0]
execute if score rank memory < piecerank memory run data modify storage assembler:memory pieces[0].children prepend from storage assembler:memory memory
execute if score rank memory < piecerank memory run data modify storage assembler:memory memory set from storage assembler:memory pieces[0]
data remove storage assembler:memory pieces[0]
scoreboard players remove piececount memory 1
execute unless score piececount memory matches 0 run function assembler:library/zip1_loop