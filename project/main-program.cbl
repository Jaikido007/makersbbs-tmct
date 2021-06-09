       IDENTIFICATION DIVISION.
       PROGRAM-ID. main-program.
    
       DATA DIVISION.
          
           
           WORKING-STORAGE SECTION.

           LINKAGE SECTION.
           01 LS-TIME.
               05 LS-YEAR PIC X(4).
               05 LS-MONTH PIC X(2).
               05 LS-DAY PIC X(2).
               05 LS-HOURS-MINS.
                  10 LS-HOURS PIC X(2).
                  10 LS-MINS PIC X(2).

       PROCEDURE DIVISION.
           CALL "server" USING LS-TIME.
           GOBACK.

           
