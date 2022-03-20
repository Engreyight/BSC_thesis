execute store result score rank memory run data get storage assembler:memory memory.rank
function assembler:library/unzip1_loop2
execute store result score childcount memory run data get storage assembler:memory memory.childcount
execute unless score childcount memory matches 0 store result score childrank memory run data get storage assembler:memory memory.children[0].rank
execute unless score childcount memory matches 0 if score childrank memory > rank memory run function assembler:library/unzip1_loop3

scoreboard players set notfound memory 0
execute if score childcount memory matches 0 run scoreboard players set notfound memory 1
execute unless score childcount memory matches 0 if score childrank memory < rank memory run scoreboard players set notfound memory 1

execute unless score notfound memory matches 0 store result score newchildcount memory run data get storage assembler:memory memory.childcount
execute unless score notfound memory matches 0 run scoreboard players add newchildcount memory 1
execute unless score notfound memory matches 0 store result storage assembler:memory memory.childcount int 1 run scoreboard players get newchildcount memory

data modify storage assembler:memory pieces prepend from storage assembler:memory memory
scoreboard players add piececount memory 1

execute unless score notfound memory matches 0 run data modify storage assembler:memory memory set value {"value": 0, "children": [], "childcount": 0}
execute unless score notfound memory matches 0 store result storage assembler:memory memory.rank int 1 run scoreboard players get rank memory

execute if score notfound memory matches 0 run data remove storage assembler:memory pieces[0].children[0]
execute if score notfound memory matches 0 run data modify storage assembler:memory memory set from storage assembler:memory memory.children[0]

execute unless score index memory matches 0 run function assembler:library/unzip1_loop1