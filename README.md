# Line-Sequential CRUD Operations

## NOTE: 
- This is a code written in GnuCOBOL 3.3 via VS Code.  It may not work when directly copy-pasted into OpenCOBOL.  This is also made for a Windows environment (important for Update and Delete).  I suggest this be used as a reference
- The CRUD Operations are summarized into a menu.  Run `CRUD.exe` to access the operations

---
# Introduction to my code
The following code is a simple CRUD operation demonstration that creates a line sequential record of car owners and their cars.
Ex:
```
Owner: Keisuke          | Car: Mazda RX-7
Owner: Takumi           | Car: Toyota AE86
Owner: Mako             | Car: Nissan Sileighty

```

I've utilized `.dll` modules (or subprograms) whereas the menu program, `CRUD.exe` calls on the modules to isolate each operation, ensuring a cleaner code, and

---
# Declarations
### `ENVIRONTMENT DIVISION`
```cobol
		SELECT OPTIONAL carsFile
			ASSIGN TO "cars.dat"
			ORGANIZATION IS LINE SEQUENTIAL.
```
### `FILE SECTION`
```cobol
		FD carsFile.
			01 carFile-rec.
				02 owner PIC X(16).
				02 carOwned PIC X(32).
```
Whereas the file record is a group named `carFile-rec`
and has members of
- `owner` - an alphanumeric field of 16 characters
- `carOwned` - an alphanumeric  field of 32 characters

---
# Create
There are two Create operations here: `addFile` and `createFile`

## `createFile`
This program is used to reset `cars.dat` into preset values as seen in it's source code:
```cobol
			open output carsFile
                move "Keisuke" to owner
                move "Mazda RX-7" to carOwned
                write carFile-rec

                move spaces to carFile-rec

                move "Takumi" to owner
                move "Toyota AE86" to carOwned
                write carFile-rec

                move space to carFile-rec

                move "Mako" to owner
                move "Nissan Sileighty" to carOwned
                write carFile-rec

            close carsFile
```

Which reflects in the `.dat` file as:
```
Keisuke         Mazda RX-7
Takumi          Toyota AE86
Mako            Nissan Sileighty

```

It simply involves moving values into the file record's members (`owner`, `carOwned`) and writing the file record (`carFile-rec`).  

<fieldset><legend><b>NOTE:</b></legend>
 If you use groups as a file record, you may only use the group name in <code>WRITE</code>.  In this case, our file record is a group with a name of <code>carFile-rec</code> so we use <code>WRITE carFile-rec</code>.  If you try to <code>WRITE [member variable]</code>, it will silently fail and won't write anything on the file
</fieldset>

## `addFile`
In this module, this is where we add another record
It involves a [`LOCAL-STORAGE SECTION`](localStorage%20vs%20workingStorage.md)`group variable called `in-NewRec` whose members are used for input

After the input is accepted, `in-NewRec` is then moved into `carFile-rec` where it will populate the variables.  Then, it will be written

```cobol
		open extend carFile
            move in-NewRec to carFile-rec
            write carFile-rec
            display "New record added!"
        close carFile
```

---
# Retrieve
There are two primary methods of Retrieving data: viewing all and searching specific data,
Same goes for this code; `searchFile`, `viewFile`

## `viewFile`
We now use the `READ` block for opening files, which, as the name suggests, opens the file until it is in the End Of File.  The current record being read is stored inside the `FILE SECTION` declared variable.
- It has to be inside a loop because  while `READ` reads each line (or record), points to the next record, but doesn't automatically display it

In it's core, the `READ` block is essentially an if-else condition

```COBOL
		READ fileDescriptor
			AT END
				*> Code to do when it reaches the End Of File
			NOT AT END
				*> Code to do when it has records to read
		END-READ
```

So let's translate our code into plain English to understand it better:
```COBOL
		perform until eof = "y"
                read carsFile
                     at end
                          move "y" to eof
                     not at end
                          display "Owner: " owner " | Car: " carOwned
                end-read
	    end-perform
```

> *"Read the file `carsFile`.  At the end of file, set the value of `eof` to 'y'.  But if it isn't, print out the current `owner` and their `carOwned` until the value of `eof` is 'y'. "*

## `searchFile`
In searching a record in line sequential, we have to build on our `viewFile` logic with a slight modification.

Instead of showing all records being read, we will a condition that it will *only* print when current record matches the user input (`searchTerm`).  In this case, we'll accept the owner's name as a search term to compare

```cobol
			display "Enter owner to search record: "
		    accept  searchTerm

               open input carsFile
                   perform until eof = 'y'
                       read carsFile
                           at end
                               move 'y' to eof
                           not at end
                             if owner = searchTerm
                               add 1 to matchCount
                               display "Owner: " owner 
                               " | Car: " carOwned
                             end-if
                       end-read
                   end-perform
                   
                   if matchCount = 0
                       display "No matches found for: " searchTerm
                   end-if

```

Additionally, we also included another checker at the end to see if the file read found any matches.  Notice the `ADD 1 TO matchCount` inside `if owner = searchTerm`.  
- `matchCount` is initialize to have a value of 0
- Since we only increment it's value if the `owner` matches the `searchTerm` inputted, it won't change it's value so our check condition  works




# Update
Updating gets a little more tricker when it comes to sequential files

We can simplify it as that *"we cannot direct modify existing records, so we have to create another file, and make the changes during copying instead"*

It's an oversimplification but it's enough to cover it.

<Fieldset><legend>Think of it this way:</legend>
You have a notepad with a list handwritten with a ballpen.  You are not allowed to scratch words out of it.  So if you want to change something, you copy it in another piece of paper completely.  And when you come to the item you want to change, you write the new word instead of the original one.  After that, you label and use your newly written notepad as your main noteoad
</fieldset>

We apply this in our code by declaring another file

```cobol
ENVIRONMENT DIVISION.
	INPUT-OUTPUT SECTION.
		FILE-CONTROL.
			...
			
			SELECT OPTIONAL tempFile
				ASSIGN TO "tempFile.dat"
				ORGANIZATION IS LINE SEQUENTIAL.
		
DATA DIVISION.
	FILE SECTION.
		...
		FD tempFile.
			01 tempFile-rec.
				02 tempOwner PIC X(16).
				02 tempCarOwned PIC X(32).
		
```

Assume that the ellipses (...) are the original file we declared earlier.  
- Notice how it's nearly identical in structure?  
- That's because it may be temporary but it still is a copy of the original so it needs to have the same structure to not cause conflicts with the data 

The logic is built on the previous topic; searching.
But before that, notice how not only we opened two files, instead of just one

```cobol
		OPEN INPUT carFile
		OPEN OUTPUT tempFile
	
			...
		
		CLOSE carFile
		CLOSE tempFile
```

If we remember correctly, `OPEN INPUT` is only for reading a file, while `OPEN OUTPUT` is for writing a new file
Our analogy fits well here.  We only need to copy from the original, so we're only reading from it, and we're creating a new copy on different notepad, so we write on it



After finding the match, we wont only display the current record, but also prompt the user to change a record. 

```cobol
		 if owner = searchKey
			  add 1 to matchCount
			   display "Owner: " owner 
			   " | Car: " carOwned
			   
			   display "Update car: "
			   accept newCar

			   move newCar to tempCarOwned
			   move owner to tempOwner
			   write tempFile-rec
		  else
			   move carFile-rec to tempFile-rec
			   write tempFile-rec
		  end-if
```

If we translate this into plain English, we get: 
> *"If the `owner` matches the `searchKey` that the user wants to change, print the record out.  Also, ask the user what car would they like to change it to.  Then write the new car into the `tempFile` through `tempFile-rec`.  But if it doesn't match, just copy the original record into the tempFile"*

Now that we've essentially rewritten the file from scratch into a new file, we need to relabel it as the original file.  We do this with the line 

```cobol
		CALL "SYSTEM" USING "move /Y temp.dat cars.dat >nul"
		
		*> or for linux:
		CALL "SYSTEM" USING "mv temp.dat cars.dat"
```

- What this does is overwrite `cars.dat` with `temp.dat`
- Whatever `temp.dat` had as it's contents, will be now the content of `cars.dat`
- At the same time, it deletes the `temp.dat`
- For Windows, `/Y` allows the file to be moved without asking for permission
	- `>nul` stops it from outputting 
		```shell
	   1 file(s) moved
	  ```
	  into the terminal

# Delete
The Delete operation is largely similar to Update. 

<fieldset><legend>Analogy:</legend>
If you want to remove an item from your list (that you cannot scratch off of), you have to rewrite it into a new notepad but without that item
</fieldset>

- So modifying the previous logic:
	- when we find a match, instead of prompting the user to input the new word and writing that directly into the new file; we skip writing that record

```cobol
	  if owner = searchKey
		  add 1 to matches
		  display "Deleting:"
		  display "Owner: " owner " | Car: " carOwned
		  continue
	  else
		   move carFile-rec to tempFile-rec
		   write tempFile-rec
	  end-if
```

>*"If the `owner` matches the `searchKey` that the user wants to delete, print that record out.  After that, continue reading the file.  But if it doesn't match, write the record"*

- The keyword `continue` and the lack of `WRITE` in the condition that it matches is what "deletes" a record from the file
  
  - And of course, we retain the relabeling part:
```cobol
		CALL "SYSTEM" USING "move /Y temp.dat cars.dat >nul"
		
		*> or for linux:
		CALL "SYSTEM" USING "mv temp.dat cars.dat"
```

---

<h4 style="display: flex; justify-content: center";>If there are further clarifications to be made, don't hesitate to contact me via Messenger</h4>


