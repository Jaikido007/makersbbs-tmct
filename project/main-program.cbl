       IDENTIFICATION DIVISION.
       PROGRAM-ID. main-program.

    
       

          

       ENVIRONMENT DIVISION.
           CONFIGURATION SECTION.
           REPOSITORY.
               FUNCTION MESSAGE-CHOICE-TO-NUM
               FUNCTION DISPLAY-MESSAGE-TITLE
               FUNCTION DISPLAY-MESSAGE-BODY
               FUNCTION DISPLAY-MESSAGE-AUTHOR
               FUNCTION DISPLAY-MESSAGE-DATE 
               FUNCTION REPLACE-LETTER.

       PROCEDURE DIVISION.
           CALL "server".
           GOBACK.

           