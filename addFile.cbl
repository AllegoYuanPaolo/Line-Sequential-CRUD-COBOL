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
                   01 carFile-rec.
                      02 owner pic x(16).
                      02 carOwned pic x(32).
            LOCAL-STORAGE SECTION.
           01 in-NewRec.
               02 in-Owner pic x(16).
               02 in-CarOwned pic x(16).
        PROCEDURE DIVISION.
        display "Enter owner: "
        accept in-Owner

        display "Enter car: "
        accept in-CarOwned

        open extend carFile
            move in-NewRec to carFile-rec
            write carFile-rec
            display "New record added!"
        close carFile
       goback.
 