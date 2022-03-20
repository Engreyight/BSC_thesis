execute store result score rank memory run data get storage assembler:memory copy.rank
function assembler:library/unzip1_loop2
execute store result score childcount memory run data get storage assembler:memory copy.childcount
execute unless score childcount memory matches 0 store result score childrank memory run data get storage assembler:memory copy.children[0].rank
execute unless score childcount memory matches 0 if score childrank memory > rank memory run function assembler:library/unzip1_loop3_readonly

execute if score childcount memory matches 0 run data modify storage assembler:memory copy set value {"value": 0}
execute unless score childcount memory matches 0 if score childrank memory < rank memory run data modify storage assembler:memory copy set value {"value": 0}

execute unless score childcount memory matches 0 if score childrank memory = rank memory run data modify storage assembler:memory copy set from storage assembler:memory copy.children[0]
execute unless score childcount memory matches 0 if score childrank memory = rank memory unless score index memory matches 0 run function assembler:library/unzip1_loop1_readonly