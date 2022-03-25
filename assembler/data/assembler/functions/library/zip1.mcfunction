# input: score value memory, storage assembler:memory memory, storage assembler:memory pieces
# output: storage assembler:memory memory
# uses: score rank memory, score havepiece memory

execute store result storage assembler:memory memory.value int 1 run scoreboard players get value memory
execute store success score havepiece memory store result score piecerank memory run data get storage assembler:memory pieces[0].rank
execute unless score havepiece memory matches 0 store result score rank memory run data get storage assembler:memory memory.rank
execute unless score havepiece memory matches 0 unless score havepiece memory matches 0 run function assembler:library/zip1_loop