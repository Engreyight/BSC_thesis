# input: score index memory, score size memory, score readonly memory, storage assembler:memory memory
# output: score mem registers
# uses: score offset memory, score shift memory, score cutoff memory, score copy memory
# plus everything in unzip2

function assembler:library/unzip2
scoreboard players operation mem registers = lower memory

execute if score offset memory matches 1 run scoreboard players operation shift memory = 1B constants
execute if score offset memory matches 2 run scoreboard players operation shift memory = 2B constants
execute if score offset memory matches 3 run scoreboard players operation shift memory = 3B constants

execute if score offset memory matches 1 run scoreboard players operation cutoff memory = 3B constants
execute if score offset memory matches 2 run scoreboard players operation cutoff memory = 2B constants
execute if score offset memory matches 3 run scoreboard players operation cutoff memory = 1B constants

scoreboard players operation mem registers /= shift memory
scoreboard players operation mem registers %= cutoff memory

scoreboard players operation copy memory = mem registers
scoreboard players operation copy memory *= shift memory
scoreboard players operation lower memory -= copy memory

execute if score size memory matches 2 run scoreboard players operation shift memory = 1B constants
scoreboard players operation copy memory = higher memory
scoreboard players operation copy memory %= shift memory
scoreboard players operation higher memory -= copy memory
scoreboard players operation copy memory *= cutoff memory
scoreboard players operation mem registers += copy memory
