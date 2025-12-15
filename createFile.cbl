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
                   01 bufferLine pic x(64).
            LOCAL-STORAGE SECTION.
        
        PROCEDURE DIVISION.
           open output carsFile
               write bufferLine from "911 Turbo"
               write bufferLine from "911 Carrera"
               write bufferLine from "911 GT3"
               write bufferLine from "911 Targa"
               write bufferLine from "911 Speedster"
           close carsFile
       goback.
 