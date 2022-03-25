function assembler:library/unzip1_loop2
execute store success score found memory store result score childrank memory run data get storage assembler:memory memory.children[0].rank
execute unless score found memory matches 0 if score childrank memory > rank memory run function assembler:library/unzip1_loop3

execute unless score found memory matches 0 if score childrank memory < rank memory run scoreboard players set found memory 0

data modify storage assembler:memory pieces prepend from storage assembler:memory memory

execute if score found memory matches 0 run data modify storage assembler:memory memory set value {"value": 0, "children": []}
execute if score found memory matches 0 store result storage assembler:memory memory.rank int 1 run scoreboard players get rank memory

execute unless score found memory matches 0 run data remove storage assembler:memory pieces[0].children[0]
execute unless score found memory matches 0 run data modify storage assembler:memory memory set from storage assembler:memory memory.children[0]

execute unless score index memory matches 0 run function assembler:library/unzip1_loop1