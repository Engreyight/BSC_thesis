# input: score value memory, score piececount memory, storage assembler:memory memory, storage assembler:memory pieces
# output: storage assembler:memory memory
# uses: score rank memory, score piecerank memory

execute store result storage assembler:memory memory.value int 1 run scoreboard players get value memory
execute unless score piececount memory matches 0 run function assembler:library/zip1_loop