# input: score mem registers
# output: storage assembler:memory memory
# uses: score offset memory, score shift memory, score cutoff memory, score copy memory
# plus everything in zip1/2

execute if score size memory matches 4 if score offset memory matches 0 run scoreboard players operation value memory = mem registers
execute if score size memory matches 4 if score offset memory matches 0 run function assembler:library/zip1

execute if score size memory matches 1 run function assembler:library/zip_single
execute if score size memory matches 2 unless score offset memory matches 3 run function assembler:library/zip_single

execute if score size memory matches 2 if score offset memory matches 3 run function assembler:library/zip_double
execute if score size memory matches 4 unless score offset memory matches 0 run function assembler:library/zip_double