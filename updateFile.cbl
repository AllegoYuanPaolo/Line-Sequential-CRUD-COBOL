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
                   01 carLine pic x(64).

               fd tempFile.
                   01 tempLine  pic x(64).
            LOCAL-STORAGE SECTION.
               01 searchKey pic x(64).
               01 newWord pic x(64).
               01 eof pic x value "n".
               01 match pic 9.
               01 matchCount pic 99 value 0.
        PROCEDURE DIVISION.

           display "Enter model to update: "
           accept searchKey


           open input carFile
           open output tempFile 
           

               perform until eof = 'y'
                   read carFile
                       at end
                           move "y" to eof
                       not at end
                           move 0 to match *> reset match flag

                           *> check for partial match
                           inspect carLine tallying match for all
                               searchKey(1:function length(function trim(searchKey)))
                           
                           *> if there is a match, write the new word
                           *> instead of the original one
                           if match > 0    
                               add 1 to matchCount
                               display "Updating: " carLine
                               
                               display "Enter new model: " no advancing
                               accept newWord

                               move newWord to tempLine
                               write tempLine
                           *> else, just copy the orignal line
                           else 
                               move carLine to tempLine
                               write tempLine
                           end-if
                   end-read
               end-perform

                 if matchCount = 0
                       display "No matches found for: " searchKey
                   end-if
           
           close carFile
           close tempFile
           
           call "SYSTEM" using "move /Y temp.dat cars.dat"
        
       goback.
 
       