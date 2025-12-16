$set sourceformat"free"
        IDENTIFICATION DIVISION.
        PROGRAM-ID. updateFile.
        environment division.
           input-output section.
               file-control. 
                   select optional carFile
                       assign to "cars.dat"
                       organization is line sequential.

                       select optional tempFile
                           assign to "temp.dat"
                           organization is line sequential.
        DATA DIVISION.
           file section.
               fd carFile.
                  01 carFile-rec.
                      02 owner pic x(16).
                      02 carOwned pic x(32).               
               fd tempFile.
                   01 tempFile-rec.
                       02 tempOwner pic x(16).
                       02 tempCarOwned pic x(32).
            LOCAL-STORAGE SECTION.
               01 searchKey pic x(16).
               01 newCar pic x(32).
               01 eof pic x value "n".
               01 match pic 9.
               01 matchCount pic 99 value 0.
        PROCEDURE DIVISION.

           display "Enter Owner to update record: "
           accept searchKey


           open input carFile
           open output tempFile 
           

               perform until eof = 'y'
                   read carFile
                       at end
                           move "y" to eof
                       not at end
                          
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

                   end-read
               end-perform

                 if matchCount = 0
                       display "No matches found for: " searchKey
                   end-if
           
           close carFile
           close tempFile
           
           call "SYSTEM" using "move /Y temp.dat cars.dat >nul"
        
       goback.
 
       