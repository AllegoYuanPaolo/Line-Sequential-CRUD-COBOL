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
                   01 bufferLine pic x(64).
            LOCAL-STORAGE SECTION.
               01 eof pic x value "n".
               01 searchTerm pic x(64).
               01 match pic 9(1).
               01 matchCount pic 99 value 0.
           procedure division.
               display "Search for a 911 model: "
               accept  searchTerm

               open input carsFile
                   perform until eof = 'y'
                       read carsFile
                           at end
                               move 'y' to eof
                           not at end
                              *> Reset match counter
                               move 0 to match
                              
                              *> Check if the searchTerm is in the current line
                               inspect bufferLine tallying match for all
                               searchTerm(1:function length(function trim(searchTerm)))
                              
                               *> Display when a match is found; supports multple matches
                               if match > 0
                                   display "Found: " bufferLine
                                   add 1 to matchCount
                               end-if
                       end-read
                   end-perform
                   
                   if matchCount = 0
                       display "No matches found for: " searchTerm
                   end-if

               close carsFile

           goback.


            
               