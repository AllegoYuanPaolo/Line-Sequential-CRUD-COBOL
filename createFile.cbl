        IDENTIFICATION DIVISION.
        PROGRAM-ID. createFile.
       environment division.
           input-output section.
               file-control.
                   select optional carsFile
                       assign to "cars.dat"
                       organization is line sequential.
        DATA DIVISION.
           file section.
               fd carsFile.
                   01 carFile-rec.
                      02 owner pic x(16).
                      02 carOwned pic x(32).
            LOCAL-STORAGE SECTION.
        
        PROCEDURE DIVISION.
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
       goback.
 