# input: score lower memory, storage assembler:memory memory, storage assembler:memory pieces, score higher memory, storage assembler:memory memory0, storage assembler:memory pieces0, score parity memory
# output: storage assembler:memory memory
# uses: score rank memory, score havepiece memory, storage assembler:memory copy

execute if score parity memory matches 0 run scoreboard players operation value memory = higher memory
execute unless score parity memory matches 0 run scoreboard players operation value memory = lower memory

function assembler:library/zip1
data modify storage assembler:memory copy set from storage assembler:memory memory

execute if score parity memory matches 0 run scoreboard players operation value memory = lower memory
execute unless score parity memory matches 0 run scoreboard players operation value memory = higher memory

data modify storage assembler:memory pieces set from storage assembler:memory pieces0
data modify storage assembler:memory memory set from storage assembler:memory memory0
function assembler:library/zip1

execute store result score piecerank memory run data get storage assembler:memory memory.children[0].rank
scoreboard players remove rank memory 1
execute if score rank memory = piecerank memory run data modify storage assembler:memory memory.children[0] set from storage assembler:memory copy.children[0]
execute if score rank memory > piecerank memory run data modify storage assembler:memory memory.children prepend from storage assembler:memory copy.children[0]
