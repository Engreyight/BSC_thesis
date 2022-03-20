data modify storage assembler:memory pieces prepend from storage assembler:memory memory.children[0]
scoreboard players add piececount memory 1
data remove storage assembler:memory memory.children[0]
scoreboard players remove childcount memory 1
execute unless score childcount memory matches 0 store result score childrank memory run data get storage assembler:memory memory.children[0].rank
execute unless score childcount memory matches 0 if score childrank memory > rank memory run function assembler:library/unzip1_loop3