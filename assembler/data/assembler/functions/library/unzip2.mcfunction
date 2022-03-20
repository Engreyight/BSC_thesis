# input: score index memory, score readonly memory, storage assembler:memory memory
# output: score lower memory, score piececount memory, storage assembler:memory memory, storage assembler:memory pieces, score higher memory, score piececount0 memory, storage assembler:memory memory0, storage assembler:memory pieces0, score parity memory
# uses: score mod memory, score rank memory, score childcount memory, score newchildcount memory, score childrank memory, score index1 memory, storage assembler:memory copy, score notfound memory

scoreboard players operation parity memory = index memory
scoreboard players operation parity memory %= 2 constants

scoreboard players operation index1 memory = index memory
execute if score parity memory matches 0 run scoreboard players add index1 memory 1
execute unless score parity memory matches 0 run scoreboard players add index memory 1

execute if score readonly memory matches 0 run data modify storage assembler:memory copy set from storage assembler:memory memory
function assembler:library/unzip1

execute if score parity memory matches 0 run scoreboard players operation lower memory = value memory
execute unless score parity memory matches 0 run scoreboard players operation higher memory = value memory

scoreboard players operation index memory = index1 memory
execute if score readonly memory matches 0 run scoreboard players operation piececount0 memory = piececount memory
execute if score readonly memory matches 0 run data modify storage assembler:memory pieces0 set from storage assembler:memory pieces
execute if score readonly memory matches 0 run data modify storage assembler:memory memory0 set from storage assembler:memory memory
execute if score readonly memory matches 0 run data modify storage assembler:memory memory set from storage assembler:memory copy
function assembler:library/unzip1

execute if score parity memory matches 0 run scoreboard players operation higher memory = value memory
execute unless score parity memory matches 0 run scoreboard players operation lower memory = value memory