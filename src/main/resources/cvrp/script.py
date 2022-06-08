import os
from pathlib import Path

directories = [os.curdir +'\src\main\\resources\cvrp\A', os.curdir +'\src\main\\resources\cvrp\B']

for direct in directories:
    files = Path(direct).glob('*.vrp')
    for file in files:
        with open(file, 'r') as f:
            lines = f.readlines()

        lines = [line.lstrip() for line in lines]

        with open(file, 'w') as f:
            f.writelines(lines)