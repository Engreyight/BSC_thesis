data modify storage assembler:memory pieces prepend from storage assembler:memory memory.children[0]
data remove storage assembler:memory memory.children[0]
execute store success score found memory store result score childrank memory run data get storage assembler:memory memory.children[0].rank
execute unless score found memory matches 0 if score childrank memory > rank memory run function assembler:library/unzip1_loop3