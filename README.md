# Sequential File CRUD Operations

## Create
In this code, `addFile` is the create operation.  It is as simple as adding another record to the file.  It prompts the user to add a new model, and it will write it into the file.  It uses a combination of :
- `SELECT OPTIONAL` 
	- So that if the file does not exist, the program will create it


Additionally, there is a `createFile` program that resets the file and pre-writes it with contents
```
911 Turbo
911 Carrera
911 GT3
911 Targa
911 Speedster
```


## Retrieve
There are two Retrieve operations; View All and Search

### `viewFIle`
To show all the contents of a file, we will use the `READ` keyword, and the `OPEN INPUT` mode

```cobol
		OPEN INPUT carsFile
			PERFORM UNTIL eof = 'y'
				READ carsFile
					AT END
						MOVE 'y' TO eof
					NOT AT END
						DISPLAY bufferLine
				END-READ
			END-PERFORM
		CLOSE carsFile
```

- `OPEN INPUT carsFile`
	- This opens a file for reading, and reading only.  You cannot write in this `open` mode
- `PERFORM UNTIL eof = 'y'`
	- This puts encases the `READ` sentence into a loop since it only reads one line/record at a time
- `READ carsFile`
	- Selects which file to read, using the file descriptor
- `AT END MOVE 'y' TO eof`
	- `AT END` tells the program what to do when it reaches EOF (End Of File) where there is no long anything to read.  In this case, we use it to break out of the previously mentioned loop
- `NOT AT END DISPLAY bufferLine`
	- `NOT AT END` is what it will do while it has lines/records to read
	- In this case, we use it to display the `bufferLine`, the current line it reads.
	- `READ`, when used on sequential files, automatically moves line-by-line, but it has to be inside a loop to show all of it

This is essentially like how we print arrays; a linear access
```C
int array[5] = [1,2,3,4,5];
int arrSize = sizeof(array)/sizeof(array[0]);

int ctr;
do {
	printf("%d\n", arr[ctr]);
} while (ctr < arrSize)
```

Though a bit different, you can see the similarities 

## `searchFile`
To search a specific record, we simply add a condition in the previous code and an input

First, we prompt the user to input what they want to search, and store it in variable
```cobol
		DISPLAY "Search for a 911 Model: "
		ACCEPT searchTerm
```

and inside the `NOT AT END` clause:
```cobol
		*> Reset match counter
		   move 0 to match
		  
		  *> Check if the searchTerm is in the current line
		   inspect bufferLine tallying match for all
		   searchTerm(1:function length(function trim(searchTerm)))
		  
		   *> Display when a match is found; supports multple matches
		   if match > 0
			   display "Found: " bufferLine
			   add 1 to matchCount
		   end-if
```
This is a pretty confusing code, but this is a way to allow for partial matches.  For example, the record in the file is `911 Turbo`, the user can only input `Turbo` and it will still return by using a logic of finding the string in a substring (`INSPECT`)

- `INSPECT bufferLine`
	- This checks the `bufferLine`, the current line being read
- `TALLYING match`
	- Stores the number of times it found the substring into variable `match`
- `FOR ALL searchTerm(1:FUNCTION LENGTH(FUNCTION TRIM(searchTerm)))`
	- `FOR ALL searchTerm` - defines what is the substring
	- `searchTerm(1:FUNCTION LENGTH(FUNCTION TRIM(searchTerm)))` - this is a string manipulation in COBOL;
		- When you use `searchTerm(1:n)` whereas n is any number, it reads as "Use only the value of this `searchTerm` from index `1` and the following `n` characters"
		- Take note that COBOL is Base-1, so index 1 refers to the first character
		- for example:
		- ```cobol
				MOVE "HELLO WORLD" TO word
				
				DISPLAY word(1:6)
	      ```
		- The output will be `HELLO`
- Take note the `MOVE 0 TO match`; this is to reset the count each loop, making sure that `match` doesn't
- `ADD 1 TO matchCount` is used as a counter clause
	- The variable will be initialized with a value of 0
	- If no results are found, it will stay 0, and the following clause will be true:
	- ```cobol
			if matchCount = 0
			   display "No matches found for: " searchTerm
		    end-if
	  ```
	  
- So if we read this in English, it reads something like: 
	- "Inspect the string `bufferLine` for all instances of `searchTerm` but without the trailing spaces, then tally the count into `match`.  If `match` is greater than 0, display `bufferLine`"


This can be simplified, but then it becomes strict with exact matches
```COBOL
		IF FUNCTION TRIM(bufferLine) = FUNCTION TRIM(searchTerm)
			DISPLAY bufferLine
		END-IF
```
It only displays the line if it matches the term exactly; not a character off
`FUNCTION TRIM()` makes it so that the trailing and leading spaces are removed

For example: You want to search `911 Turbo`, you have to type it exactly as that

# Update
In updating, it gets more complex, but we only build up from the previous searching algorithm.  One of the quirks of sequential files is that you cannot inherently update in place.  So it involves creating a temporary file, and renaming it into the original

```cobol
		OPEN INPUT originalFile
		OPEN OUTPUT tempFile
```
We open the original as an `INPUT` for reading, and open the temporary file as an `OUTPUT` for writing

So inside the `NOT AT END`, regardless if we use the partial match or the exact match; the general flow is:

```Cobol
		IF currentWord = searchWord
			DISPLAY "Updating: " currentWord
			
			DISPLAY "Enter new word: "
			ACCEPT newWord
			
			MOVE newWord to tempLine
			WRITE tempLine
		ELSE
			MOVE currentWord to tempLine
			WRITE tempLine
		END-IF
```

At it's core, it uses the previous linear search to check if the the  current line is the word you want to update.  If it is, it prompts the user for the new word and writes that to the temporary file instead of the original line.  Otherwise, it just copies the original line

After writing and creating a new temporary file, we must make sure only one file remains. 
So after closing both files, we should: 

```cobol
		*> For Windows
		CALL "SYSTEM" USING "move /Y temp.dat orig.dat" 
		
		*> For Linux
		CALL "SYSTEM" USING "mv temp.dat orig.dat" 
```

It moves the temporary file into the original file.  It does two things in a single commmand
1. It overwrites the content of the original file
2. It deletes the temporary file

