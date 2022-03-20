scoreboard players remove rank memory 1
scoreboard players operation mod memory = index memory
scoreboard players operation mod memory %= 2 constants
scoreboard players operation index memory /= 2 constants
execute if score mod memory matches 0 run function assembler:library/unzip1_loop2