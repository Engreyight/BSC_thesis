# input: score mem registers
# output: storage assembler:memory memory
# uses: score offset memory, score shift memory, score cutoff memory, score copy memory
# plus everything in zip2

execute if score offset memory matches 3 run scoreboard players operation shift memory = 3B constants
scoreboard players operation copy memory = mem registers
scoreboard players operation mem registers *= shift memory
scoreboard players operation lower memory += mem registers

execute if score size memory matches 2 run scoreboard players operation shift memory = 1B constants
scoreboard players operation copy memory /= cutoff memory
scoreboard players operation copy memory %= shift memory
scoreboard players operation higher memory += copy memory

function assembler:library/zip2