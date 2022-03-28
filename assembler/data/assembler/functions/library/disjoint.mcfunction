scoreboard players set disjoint variables 1
execute if score op1 variables matches ..-1 if score op2 variables matches ..-1 run scoreboard players set disjoint variables 0
execute if score disjoint variables matches 1 if score op1 variables matches ..-1 run scoreboard players operation op1 variables += 4B constants
execute if score disjoint variables matches 1 if score op2 variables matches ..-1 run scoreboard players operation op2 variables += 4B constants
execute if score disjoint variables matches 1 unless score op1 variables matches 0 unless score op2 variables matches 0 run function assembler:library/disjoint_loop