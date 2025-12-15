import os

listOfFiles = os.listdir("./")

for item in listOfFiles:
    if "File.cbl" in item:
        print(item)

#with open("createFile.cbl", 'r') as file:
#    for line in file:
#        print(line, end="")