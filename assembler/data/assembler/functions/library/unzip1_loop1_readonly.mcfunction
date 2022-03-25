function assembler:library/unzip1_loop2
execute store success score found memory store result score childrank memory run data get storage assembler:memory copy.children[0].rank
execute unless score found memory matches 0 if score childrank memory > rank memory run function assembler:library/unzip1_loop3_readonly

execute unless score found memory matches 0 if score childrank memory < rank memory run scoreboard players set found memory 0

execute if score found memory matches 0 run data modify storage assembler:memory copy set value {"value": 0}

execute unless score found memory matches 0 run data modify storage assembler:memory copy set from storage assembler:memory copy.children[0]
execute unless score found memory matches 0 unless score index memory matches 0 run function assembler:library/unzip1_loop1_readonly