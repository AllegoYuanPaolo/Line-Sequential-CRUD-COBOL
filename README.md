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


