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

> *"Read the file `carsFile`.  At the end of file, set the value of `eof` to 'y'.  But if it isn't, display the current `owner` and their `carOwned` until the value of `eof` is 'y'. "*

