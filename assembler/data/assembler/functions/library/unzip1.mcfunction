# input: score index memory, score readonly memory, storage assembler:memory memory
# output: score value memory, storage assembler:memory memory, storage assembler:memory pieces
# uses: score mod memory, score rank memory, score childrank memory, storage assembler:memory copy, score found memory

execute unless score readonly memory matches 0 run data modify storage assembler:memory copy set from storage assembler:memory memory
execute if score readonly memory matches 0 run data modify storage assembler:memory pieces set value []
execute unless score readonly memory matches 0 store result score rank memory run data get storage assembler:memory copy.rank
execute if score readonly memory matches 0 store result score rank memory run data get storage assembler:memory memory.rank
execute unless score index memory matches 0 unless score readonly memory matches 0 run function assembler:library/unzip1_loop1_readonly
execute unless score index memory matches 0 if score readonly memory matches 0 run function assembler:library/unzip1_loop1
execute unless score readonly memory matches 0 store result score value memory run data get storage assembler:memory copy.value
execute if score readonly memory matches 0 store result score value memory run data get storage assembler:memory memory.value