import os
from pathlib import Path

datasetdir = os.curdir +'\src\main\\resources\cvrp\\'
directories = [datasetdir + 'A', datasetdir + 'B', datasetdir + 'P', datasetdir + 'E']

def fileFix():
    for direct in directories:
        files = Path(direct).glob('*.vrp')
        for file in files:
            with open(file, 'r') as f:
                lines = f.readlines()

            lines = [line.lstrip() for line in lines]

            with open(file, 'w') as f:
                f.writelines(lines)

def fileList():
    for direct in directories:
        files = Path(direct).glob('*.vrp')
        for file in files:
            file = str(file).replace("\\", "//")
            file = file[22:]
            file = "    - " + file
            print(file)

fileFix()
fileList()