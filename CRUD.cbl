        IDENTIFICATION DIVISION.
        PROGRAM-ID. CRUD.
 
        DATA DIVISION.
           
           WORKING-STORAGE SECTION.
               01 exitFlag pic x value 'n'.
               01 choice pic xx.
        PROCEDURE DIVISION.
           perform with test after until exitFlag = 'y'
           call "SYSTEM" using "cls"

               display "[1] - Add Record"
               display "[2] - View All Records"
               display "[3] - Search Record"
               display "[4] - Update Record"
               display "[5] - Delete Record"
               display spaces
               display "[77] - Reset File"
               display "[00] - Exit"
               display "Enter choice >" no advancing
               accept choice
    
               evaluate choice
                   when "00"
                       display "Exiting program..."
                          move 'y' to exitFlag
                   when "1"
                       call "SYSTEM" using "cls"
                       call "addFile"
                       call "SYSTEM" using "pause"
    
                   when "2"
                       call "SYSTEM" using "cls"
                       call "viewFile"
                       call "SYSTEM" using "pause"
    
                   when "3"
                       call "SYSTEM" using "cls"
                       call "searchFile"
                       call "SYSTEM" using "pause"
    
                   when "4"
                       call "SYSTEM" using "cls"
                       call "updateFile"
                       call "SYSTEM" using "pause"
                   
                   when "5"
                       call "SYSTEM" using "cls"
                       call "deleteFile"
                       call "SYSTEM" using "pause"
                   
                    when "77"
                       call "SYSTEM" using "cls"
                       call "createFile"
                       call "SYSTEM" using "pause"
    
                   when other
                       display "   > Invalid option."
                       call "SYSTEM" using "pause"
               end-evaluate

           end-perform
        
       STOP RUN.
 