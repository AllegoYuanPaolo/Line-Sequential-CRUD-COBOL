        IDENTIFICATION DIVISION.
        PROGRAM-ID. addFile.
       environment division.
           input-output section.
               file-control.
                   select optional carFile
                       assign to "cars.dat"
                       organization is line sequential.
        DATA DIVISION.
           file section.
               fd carFile.
                   01 carLine pic x(64).       
            LOCAL-STORAGE SECTION.
           01 newCar pic x(64).
        PROCEDURE DIVISION.
        display "Enter new model: "
        accept newCar

        open extend carFile
            move newCar to carLine
            write carLine
        close carFile
       goback.
 