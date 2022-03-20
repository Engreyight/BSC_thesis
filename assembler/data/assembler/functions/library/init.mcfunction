scoreboard objectives add memory dummy
scoreboard objectives add registers dummy
scoreboard objectives add constants dummy
execute unless data storage assembler:memory memory run data modify storage assembler:memory memory set value {"value": 0, "children": [], "rank": 31, "childcount": 0}
scoreboard players set 2 constants 2
scoreboard players set 4 constants 4
scoreboard players set 0B constants 1
scoreboard players set 1B constants 256
scoreboard players set 2B constants 65536
scoreboard players set 3B constants 16777216
scoreboard players set 4B constants -2147483648