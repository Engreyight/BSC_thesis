# input: score lower memory, score piececount memory, storage assembler:memory memory, storage assembler:memory pieces, score higher memory, score piececount0 memory, storage assembler:memory memory0, storage assembler:memory pieces0, score parity memory
# output: storage assembler:memory memory
# uses: score rank memory, score piecerank memory, storage assembler:memory copy

execute if score parity memory matches 0 run scoreboard players operation value memory = higher memory
execute unless score parity memory matches 0 run scoreboard players operation value memory = lower memory

function assembler:library/zip1
data modify storage assembler:memory copy set from storage assembler:memory memory

execute if score parity memory matches 0 run scoreboard players operation value memory = lower memory
execute unless score parity memory matches 0 run scoreboard players operation value memory = higher memory

scoreboard players operation piececount memory = piececount0 memory
data modify storage assembler:memory pieces set from storage assembler:memory pieces0
data modify storage assembler:memory memory set from storage assembler:memory memory0
function assembler:library/zip1

data modify storage assembler:memory memory.childcount set from storage assembler:memory copy.childcount
execute if score rank memory < piecerank memory run scoreboard players operation rank memory >< piecerank memory
scoreboard players remove rank memory 1
execute if score rank memory = piecerank memory run data modify storage assembler:memory memory.children[0] set from storage assembler:memory copy.children[0]
execute if score rank memory > piecerank memory run data modify storage assembler:memory memory.children prepend from storage assembler:memory copy.children[0]