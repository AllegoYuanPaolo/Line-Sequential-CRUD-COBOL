        IDENTIFICATION DIVISION.
        PROGRAM-ID. viewFile.
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
               01 eof pic x value "n".


       procedure division.
              open input carsFile
              perform until eof = "y"
                read carsFile
                     at end
                          move "y" to eof
                     not at end
                          display "Owner: " owner " | Car: " carOwned
                end-read
              end-perform
              close carsFile
         goback.
