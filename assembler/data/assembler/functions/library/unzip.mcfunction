# input: score index memory, score size memory, score readonly memory, storage assembler:memory memory
# output: score mem registers
# uses: score offset memory, score shift memory, score cutoff memory, score copy memory
# plus everything in unzip1/2

scoreboard players operation offset memory = index memory
scoreboard players operation offset memory %= 4 constants
scoreboard players operation index memory /= 4 constants

execute if score size memory matches 4 if score offset memory matches 0 run function assembler:library/unzip1
execute if score size memory matches 4 if score offset memory matches 0 run scoreboard players operation mem registers = value memory

execute if score size memory matches 1 run function assembler:library/unzip_single
execute if score size memory matches 2 unless score offset memory matches 3 run function assembler:library/unzip_single

execute if score size memory matches 2 if score offset memory matches 3 run function assembler:library/unzip_double
execute if score size memory matches 4 unless score offset memory matches 0 run function assembler:library/unzip_double