$set sourceformat"free"
        IDENTIFICATION DIVISION.
        PROGRAM-ID. searchFile.
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
               01 searchTerm pic x(16).
       
               01 matchCount pic 99 value 0.
           procedure division.
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

               close carsFile

           goback.


            
               