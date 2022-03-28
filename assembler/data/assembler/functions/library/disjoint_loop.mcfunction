scoreboard players operation mod1 variables = op1 variables
scoreboard players operation mod1 variables %= 2 constants
scoreboard players operation op1 variables /= 2 constants
scoreboard players operation mod2 variables = op2 variables
scoreboard players operation mod2 variables %= 2 constants
scoreboard players operation op2 variables /= 2 constants
execute if score mod1 variables matches 1 if score mod2 variables matches 1 run scoreboard players set disjoint variables 0
execute if score disjoint variables matches 1 unless score op1 variables matches 0 unless score op2 variables matches 0 run function assembler:library/disjoint_loop