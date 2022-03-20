# input: score mem registers
# output: storage assembler:memory memory
# uses: score offset memory, score shift memory, score cutoff memory, score copy memory
# plus everything in zip1

scoreboard players operation mem registers %= cutoff memory
scoreboard players operation mem registers *= shift memory
scoreboard players operation value memory += mem registers

function assembler:library/zip1