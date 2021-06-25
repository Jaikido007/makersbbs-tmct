       IDENTIFICATION DIVISION.
       PROGRAM-ID. server.

       ENVIRONMENT DIVISION.
           CONFIGURATION SECTION.
           REPOSITORY.
               FUNCTION REPLACE-LETTER.
               
           INPUT-OUTPUT SECTION.
           FILE-CONTROL.

           SELECT F-WORD-FILE ASSIGN TO "guessing-words.dat"
             ORGANIZATION IS LINE SEQUENTIAL.
           SELECT F-HIGH-SCORES-FILE ASSIGN TO "high-scores.dat"
             ORGANIZATION IS LINE SEQUENTIAL.
           
       DATA DIVISION.
           FILE SECTION.
           FD F-WORD-FILE.
           01 WORD PIC X(20).
           FD F-HIGH-SCORES-FILE.
           01 PLAYER-SCORES.
              05 HIGH-SCORE PIC 99.
              05 PLAYER-NAME PIC X(10).
                      
           WORKING-STORAGE SECTION.
      ******************************************************************
      *******-----VARIABLES RELATED TO LOGIN & MENU SCREEN-----*********
      ******************************************************************
           01 WS-USERNAME PIC X(16).
           01 WS-PASSWORD PIC X(20).
           01 WS-NEW-USER-NAME PIC X(16).
           01 WS-NEW-PASSWORD PIC X(20).
           01 LOGIN-CHOICE PIC X.
           01 MENU-CHOICE PIC X.
           01 ERROR-CHOICE PIC X.
           01 CREATE-CHOICE PIC X.
           01 ACCOUNT-CHOICE PIC X.
           01 WS-LOGIN-CORRECT PIC 9.
           01 WS-ERROR-MSG PIC X(40).
           01 WS-UNAME-UNAVAILABLE PIC 9.
           01 WS-USERCREDITS PIC 9(3).

      ******************************************************************
      ***********-----VARIABLES RELATED TO BANK ACCOUNTS-----***********
      ******************************************************************
           01 BANK-ACCOUNT-CHOICE PIC X.
           01 CARD-NO PIC 9(16).
           01 CARD-EXPIRY PIC 9(4).
           01 CARD-CVV PIC 9(3).
           01 WS-CARD-NO PIC 9(16).
           01 WS-CARD-EXPIRY PIC 9(4).
           01 WS-CARD-CVV PIC 9(3).
      ********************************************
      *----Variables relating to credit store----*
      ********************************************
           01 CREDIT-STORE-CHOICE PIC X.
           01 WS-UPDATE-CREDITS PIC 9(3). 
           01 WS-STORE-CHARGE PIC 9(2).      
      ******************************************************************
      ***********-----VARIABLES RELATED TO GUESSING GAME-----***********
      ******************************************************************
           01 WS-ANSWERWORD PIC X(20).
           01 RANDOMNUMBER PIC 99.
           01 WS-WORD PIC X(20).
           01 WS-GUESSING-CHOICE-WORDS.
               05 WS-GUESSING-CHOICE-WORD OCCURS 213 TIMES
               DESCENDING KEY IS WS-GUESSING-WORDS-WORD
               INDEXED BY WORD-IDX.
                   10 WS-GUESSING-WORDS-WORD PIC X(20).
           01 WS-GUESS-CHOICE PIC X.
      ******************************************************************
      *********-----VARIABLES RELATED TO HIGH SCORE SCREEN----**********
      ******************************************************************
           01 WS-HIGH-SCORE-CHOICE PIC X.
           01 WS-HIGH-SCORE PIC 99.
           01 WS-HIGH-SCORES.  
              05 WS-TABLE-HIGH-SCORE OCCURS 100 TIMES     
              ASCENDING KEY IS WS-SCORE
              INDEXED BY SCORE-IDX.
                  10 WS-SCORE PIC 99.
                  10 WS-NAME PIC X(10).
      ******************************************************************
      ********-----VARIABLES RELATED TO CHECKING GUESSES-----***********
      ****************************************************************** 
           01 WS-LETTERS-LEFT PIC 99.
           01 WS-GUESSES-LEFT PIC 99.          
      ******************************************************************
      **********-----VARIABLES RELATED TO WINNING & LOSING-----*********
      ******************************************************************
           01 WS-GUESSING-LOSING-CHOICE PIC X.
           01 WS-GUESSING-WINNING-CHOICE PIC X.
           01 WS-WORD-LENGTH PIC 99.

           01 COUNTER UNSIGNED-INT.
           01 OFFSET UNSIGNED-INT.
           
           01 WS-FILE-IS-ENDED         PIC 9 VALUE ZERO.
           01 MSG-MENU-CHOICE          PIC XXX.
           01 GAMES-MENU-CHOICE        PIC X.
           01 WS-COUNTER               PIC 99.
           01 NUM-FILE-LINES           PIC 999.
           01 ID-NUM                   PIC 999 VALUE 1.
           01 WS-DATETIME              PIC X(21).
           01 WS-FORMATTED-DT.
             05 WS-FORMATTED-DTE-TME.
               15 WS-FORMATTED-YEAR    PIC  X(4). 
               15 FILLER               PIC X VALUE "-".
               15 WS-FORMATTED-MONTH   PIC  X(2).
               15 FILLER               PIC X VALUE "-".
               15 WS-FORMATTED-DY      PIC  X(2).
               15 FILLER               PIC X VALUE "-".
               15 WS-FORMATTED-HOUR    PIC  X(2).
               15 FILLER               PIC X VALUE ":".
               15 WS-FORMATTED-MINS    PIC  X(2).
               15 FILLER               PIC X VALUE ":".
               15 WS-FORMATTED-SEC     PIC  X(2).
               15 FILLER               PIC X VALUE ":".
               15 WS-FORMATTED-MS      PIC  X(2).
                   
           01 WS-LIST-TABLE.
               05 WS-LIST-ENTRY OCCURS 10 TO 999 TIMES DEPENDING ON 
                 NUM-FILE-LINES.
                   10 LIST-ID          PIC XXX.
                   10 LIST-TITLE       PIC X(50).
                   10 LIST-CONTENT     PIC X(300).
                   10 LIST-USERNAME    PIC X(16).        
           01 WS-CONTENT-DISPLAY.
               05 LS-PART-1            PIC X(60).
               05 LS-PART-2            PIC X(60).
               05 LS-PART-3            PIC X(60).
               05 LS-PART-4            PIC X(60).
               05 LS-PART-5            PIC X(60).
           

           01 MSG-SELECT               PIC 999.
           01 MSG-VIEW-CHOICE          PIC X.
           01 MSG-WRITE-CHOICE         PIC X.
           
           01 NEW-MESSAGE.
             05 WS-TITLE               PIC X(50).
             05 WS-CONTENT             PIC X(300).
             05 WS-MSG-AUTHOR            PIC X(16).

      ******************************************************************
      ******************-----COMMENT SYSTEM VARIABLES-----**************
      ******************************************************************

           01 NUM-COMMENTS PIC 9999.

           01 COMMENT-TABLE.
               05 COM-ENTRY OCCURS 1 TO 9999 TIMES 
               DEPENDING ON NUM-COMMENTS.
                  *>  10 TEMP-ID PIC 999.
                   10 COM-AUTHOR PIC X(16).
                   10 COM-DATE PIC X(21).
                   10 COM-COMMENT PIC X(50).

           01 COM-INDEX PIC 9999 VALUE 1.

           01 COM-SCRN-CHOICE PIC X.

      ******************************************************************
      ***********************-----TIME VARIABLES----********************
      ******************************************************************
           01 WS-TIME.
               05 WS-YEAR PIC X(4).
               05 WS-MONTH PIC X(2).
               05 WS-DAY PIC X(2).
               05 WS-HOURS-MINS.
                   10 WS-HOURS PIC X(2).
                   10 WS-MINS PIC X(2).
             
      ******************************************************************
           
           LINKAGE SECTION.
           01 LS-COUNTER UNSIGNED-INT.
           01 LS-NUM UNSIGNED-INT.
           01 LS-MESSAGE PIC X(60). 

      ****************************************************************** 

           SCREEN SECTION.
           01 LOGIN-SCREEN
               BACKGROUND-COLOR IS 1.
               05 BLANK SCREEN.
             *>    LOGIN HEADER
               05 LINE 1 COL 1  VALUE "   :                              
      -    "                                                           "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 1 COL 2 PIC X(2) USING WS-FORMATTED-HOUR 
               FOREGROUND-COLOR IS 7 REVERSE-VIDEO.
               05 LINE 1 COL 5 PIC X(2) USING WS-FORMATTED-MINS
               FOREGROUND-COLOR IS 7 REVERSE-VIDEO.
              *>    LOGIN FOOTER
              05 LINE 43 COL 1 VALUE "                                 
      -    "                                                           "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 44 COL 1 VALUE "     (L) Log-in     (C) Create an
      -    "account     (Q) Quit                                "                                 
                FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 45 COL 1 VALUE "                                 
      -    "                                                           "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 46 COL 1 VALUE "                                 
      -    "                                                           "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
              *>    FRIENDFACE LOGO ASCII ART
               05 LINE 14 COL 34 VALUE " ________________________"
                   FOREGROUND-COLOR IS 7.
               05 LINE 15 COL 35 VALUE "|FFFFFFFFFFFFFFFFFFFFFF|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 16 COL 35 VALUE "|FFFFFFFFFFFFF_____FFFF|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 17 COL 35 VALUE "|FFFFFFFFFFFF__FFFFFFFF|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 18 COL 35 VALUE "|FFFFFFFFFFFF__FFFFFFFF|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 19 COL 35 VALUE "|FFFFFFFFFFFF__FFFFFFFF|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 20 COL 35 VALUE "|FFFFFFFFF________FFFFF|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 21 COL 35 VALUE "|FFFFFFFFFFFF__FFFFFFFF|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 22 COL 35 VALUE "|FFFFFFFFFFFF__FFFFFFFF|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 23 COL 35 VALUE "|FFFFFFFFFFFF__FFFFFFFF|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 24 COL 35 VALUE "|FFFFFFFFFFFF__FFFFFFFF|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 25 COL 35 VALUE "|FFFFFFFFFFFFFFFFFFFFFF|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 26 COL 34 VALUE " ------------------------"
                   FOREGROUND-COLOR IS 7.
        *>    FRIENDFACE TEXT ASCII ART  
               05 LINE 31 COL 25 VALUE "______    _                ______
      -        "_" FOREGROUND-COLOR IS 7.
               05 LINE 32 COL 25 VALUE "|  ___|  (_)              | |  __
      -        "_| " FOREGROUND-COLOR IS 7.
               05 LINE 33 COL 25 VALUE "| |_ _ __ _  ___ _ __   __| | |_ 
      -        "__ _  ___ ___" FOREGROUND-COLOR IS 7.
               05 LINE 34 COL 25 VALUE "|  _| '__| |/ _ \ '_ \ / _` |  _/
      -        " _` |/ __/ _ \" FOREGROUND-COLOR IS 7.
               05 LINE 35 COL 25 VALUE "| | | |  | |  __/ | | | (_| | || 
      -        "(_| | (_|  __/" FOREGROUND-COLOR IS 7.
               05 LINE 36 COL 25 VALUE "\_| |_|  |_|\___|_| |_|\__,_\_| \
      -        "__,_|\___\___|" FOREGROUND-COLOR IS 7.
              *>    LOGIN OPTION POSITIONING
               05 LINE 42 COLUMN 6 VALUE "Option: ".
               05 LOGIN-CHOICE-FIELD LINE 42 COLUMN 14 PIC X
                  USING LOGIN-CHOICE.

           01 SIGN-IN-SCREEN
             BACKGROUND-COLOR IS 1.
             05 BLANK SCREEN.
        *>    SIGN IN HEADER
               05 LINE 1 COL 1  VALUE "   :                              
      -    "                                                           "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 1 COL 2 PIC X(2) USING WS-FORMATTED-HOUR 
               FOREGROUND-COLOR IS 7 REVERSE-VIDEO.
               05 LINE 1 COL 5 PIC X(2) USING WS-FORMATTED-MINS
               FOREGROUND-COLOR IS 7 REVERSE-VIDEO.
        *>    SIGN IN FOOTER
               05 LINE 43 COL 1 VALUE "                                 
      -    "                                                           "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 44 COL 1 VALUE "                                 
      -    "      (c)FriendFace                                        "                                 
                FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 45 COL 1 VALUE "                  Join FriendFace                               
      -    " as a VIP member to unlock all its features!              "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 46 COL 1 VALUE "                                 
      -    "                                                           "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.     
        *>    FRIENDFACE LOGO ASCII ART
               05 LINE 14 COL 34 VALUE " ________________________"
                   FOREGROUND-COLOR IS 7.
               05 LINE 15 COL 35 VALUE "|FFFFFFFFFFFFFFFFFFFFFF|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 16 COL 35 VALUE "|FFFFFFFFFFFFF_____FFFF|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 17 COL 35 VALUE "|FFFFFFFFFFFF__FFFFFFFF|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 18 COL 35 VALUE "|FFFFFFFFFFFF__FFFFFFFF|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 19 COL 35 VALUE "|FFFFFFFFFFFF__FFFFFFFF|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 20 COL 35 VALUE "|FFFFFFFFF________FFFFF|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 21 COL 35 VALUE "|FFFFFFFFFFFF__FFFFFFFF|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 22 COL 35 VALUE "|FFFFFFFFFFFF__FFFFFFFF|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 23 COL 35 VALUE "|FFFFFFFFFFFF__FFFFFFFF|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 24 COL 35 VALUE "|FFFFFFFFFFFF__FFFFFFFF|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 25 COL 35 VALUE "|FFFFFFFFFFFFFFFFFFFFFF|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 26 COL 34 VALUE " ------------------------"
                   FOREGROUND-COLOR IS 7.
        *>    SIGN IN BODY
             05 LINE 30 COLUMN 37 VALUE "Enter your username:".
             05 WS-USERNAME-FIELD LINE 32 COLUMN 37 PIC X(16)
                USING WS-USERNAME. 
             05 LINE 32 COLUMN 53 VALUE "____".
             05 LINE 34 COLUMN 37 VALUE "Enter your password:".
             05 WS-PASSWORD-FIELD LINE 36 COLUMN 37 PIC X(20)
                USING WS-PASSWORD.       

           01 ERROR-SCREEN
             BACKGROUND-COLOR IS 1.
             05 BLANK SCREEN.
        *>    ERROR HEADER
             05 LINE 1 COL 1  VALUE "   :                              
      -    "                                                           "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 1 COL 2 PIC X(2) USING WS-FORMATTED-HOUR 
             FOREGROUND-COLOR IS 7 REVERSE-VIDEO.
             05 LINE 1 COL 5 PIC X(2) USING WS-FORMATTED-MINS
             FOREGROUND-COLOR IS 7 REVERSE-VIDEO.
             05 LINE 1 COL 90 USING WS-USERCREDITS
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
        *>    ERROR FOOTER
               05 LINE 43 COL 1 VALUE "                                 
      -    "                                                           "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 44 COL 1 VALUE "     (L) Back to Log-in     (C) C
      -    "reate an account     (Q) Quit                              "                                 
                FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 45 COL 1 VALUE "                                 
      -    "                                                           "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 46 COL 1 VALUE "                                 
      -    "                                                           "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.

                05 LINE 30 COLUMN 32 PIC X(40) USING WS-ERROR-MSG.
        *>    FRIENDFACE LOGO ASCII ART
               05 LINE 14 COL 34 VALUE " ________________________"
                   FOREGROUND-COLOR IS 7.
               05 LINE 15 COL 35 VALUE "|FFFFFFFFFFFFFFFFFFFFFF|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 16 COL 35 VALUE "|FFFFFFFFFFFFF_____FFFF|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 17 COL 35 VALUE "|FFFFFFFFFFFF__FFFFFFFF|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 18 COL 35 VALUE "|FFFFFFFFFFFF__FFFFFFFF|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 19 COL 35 VALUE "|FFFFFFFFFFFF__FFFFFFFF|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 20 COL 35 VALUE "|FFFFFFFFF________FFFFF|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 21 COL 35 VALUE "|FFFFFFFFFFFF__FFFFFFFF|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 22 COL 35 VALUE "|FFFFFFFFFFFF__FFFFFFFF|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 23 COL 35 VALUE "|FFFFFFFFFFFF__FFFFFFFF|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 24 COL 35 VALUE "|FFFFFFFFFFFF__FFFFFFFF|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 25 COL 35 VALUE "|FFFFFFFFFFFFFFFFFFFFFF|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 26 COL 34 VALUE " ------------------------"
                   FOREGROUND-COLOR IS 7.
        *>    ERROR OPTION POSITIONING
               05 LINE 42 COLUMN 6 VALUE "Option: ".
               05 ERROR-CHOICE-FIELD LINE 42 COLUMN 14 PIC X
                  USING ERROR-CHOICE.

           01 CREATE-AN-ACCOUNT-SCREEN
               BACKGROUND-COLOR IS 01.
                05 BLANK SCREEN.
        *>    CREATE ACCOUNT HEADER
               05 LINE 1 COL 1  VALUE "   :                              
      -    "                                                           "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 1 COL 2 PIC X(2) USING WS-FORMATTED-HOUR 
               FOREGROUND-COLOR IS 7 REVERSE-VIDEO.
               05 LINE 1 COL 5 PIC X(2) USING WS-FORMATTED-MINS
               FOREGROUND-COLOR IS 7 REVERSE-VIDEO.
        *>    CREATE ACCOUNT FOOTER
               05 LINE 43 COL 1 VALUE "                                 
      -    "                                                           "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 44 COL 1 VALUE "     (S) Submit     (D) Discard  
      -    "   (G) Go back     (Q) Quit                                "                                 
                FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 45 COL 1 VALUE "                                 
      -    "                                                           "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 46 COL 1 VALUE "                                 
      -    "                                                           "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO. 
        *>    FIRENDFACE LOGO ASCII ART
               05 LINE 14 COL 34 VALUE " ________________________"
                   FOREGROUND-COLOR IS 7.
               05 LINE 15 COL 35 VALUE "|FFFFFFFFFFFFFFFFFFFFFF|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 16 COL 35 VALUE "|FFFFFFFFFFFFF_____FFFF|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 17 COL 35 VALUE "|FFFFFFFFFFFF__FFFFFFFF|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 18 COL 35 VALUE "|FFFFFFFFFFFF__FFFFFFFF|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 19 COL 35 VALUE "|FFFFFFFFFFFF__FFFFFFFF|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 20 COL 35 VALUE "|FFFFFFFFF________FFFFF|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 21 COL 35 VALUE "|FFFFFFFFFFFF__FFFFFFFF|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 22 COL 35 VALUE "|FFFFFFFFFFFF__FFFFFFFF|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 23 COL 35 VALUE "|FFFFFFFFFFFF__FFFFFFFF|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 24 COL 35 VALUE "|FFFFFFFFFFFF__FFFFFFFF|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 25 COL 35 VALUE "|FFFFFFFFFFFFFFFFFFFFFF|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 26 COL 34 VALUE " ------------------------"
                   FOREGROUND-COLOR IS 7.

             05 LINE 30 COLUMN 37 VALUE "Create your account".
             05 LINE 32 COLUMN 37 VALUE " Enter a username: ".
             05 WS-NEW-USER-NAME-FIELD LINE 34 COLUMN 38 PIC X(16)
                USING WS-NEW-USER-NAME.
             05 LINE 34 COLUMN 54 VALUE "____".
             05 LINE 36 COLUMN 37 VALUE " Enter a password: ".
             05 LINE 37 COLUMN 36 VALUE " (max 20 characters)".
             05 WS-NEW-PASSWORD-FIELD LINE 39 COLUMN 38 PIC X(20)
                USING WS-NEW-PASSWORD.

        *>    CREATE ACCOUNT OPTION POSITIONING
               05 LINE 42 COLUMN 6 VALUE "Option: ".
               05 CREATE-CHOICE-FIELD LINE 42 COLUMN 14 PIC X
                  USING CREATE-CHOICE. 

           01 USER-ACCOUNT-SCREEN
             BACKGROUND-COLOR IS 1.
             05 BLANK SCREEN.
        *>    USER ACCOUNT HEADER
             05 LINE 1 COL 1  VALUE "   :                              
      -    "                                                         "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 1 COL 2 PIC X(2) USING WS-FORMATTED-HOUR 
             FOREGROUND-COLOR IS 7 REVERSE-VIDEO.
             05 LINE 1 COL 5 PIC X(2) USING WS-FORMATTED-MINS
             FOREGROUND-COLOR IS 7 REVERSE-VIDEO.
             05 LINE 1 COL 81 VALUE "CREDITS: "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 1 COL 90 USING WS-USERCREDITS
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
        *>    USER ACCOUNT FOOTER
               05 LINE 43 COL 1 VALUE "                                 
      -    "                                                           "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 44 COL 1 VALUE "     (B) Bank account      (G) Go       
      -    " back     (Q) Quit                                         "                                 
                FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 45 COL 1 VALUE "                                 
      -    "                                                           "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 46 COL 1 VALUE "                                 
      -    "                                                           "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO. 
             05 LINE  6 COL 10 VALUE "Hi, ".
             05 LINE  6 COL 14 PIC X(16) USING WS-USERNAME.
             05 LINE  8 COL 10 VALUE "Available Credits: ".
             05 LINE  8 COL 30 PIC 9(3) USING WS-USERCREDITS.
        *>    USER ACCOUNT OPTION POSITIONING
               05 LINE 42 COLUMN 6 VALUE "Option: ".
             05 ACCOUNT-CHOICE-FIELD LINE 42 COL 14 PIC X
                USING ACCOUNT-CHOICE.

           01 BANK-DETAILS-SCREEN
             BACKGROUND-COLOR IS 1.
             05 BLANK SCREEN.
        *>    BANK DETAILS HEADER
               05 LINE 1 COL 1  VALUE "   :                              
      -    "                                                           "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 1 COL 2 PIC X(2) USING WS-FORMATTED-HOUR 
               FOREGROUND-COLOR IS 7 REVERSE-VIDEO.
               05 LINE 1 COL 5 PIC X(2) USING WS-FORMATTED-MINS
               FOREGROUND-COLOR IS 7 REVERSE-VIDEO.
             05 LINE 1 COL 90 USING WS-USERCREDITS
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
        *>    BANK DETAILS FOOTER
             05 LINE 43 COL 1 VALUE "                                 
      -    "                                                           "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 44 COL 1 VALUE "     (S) Submit     (D) Discard  
      -    "   (G) Go back     (Q) Quit                                "                                 
                FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 45 COL 1 VALUE "                                 
      -    "                                                           "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 46 COL 1 VALUE "                                 
      -    "                                                           "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO. 
           *>    BANK DETAILS BODY
             05 LINE 6 COLUMN 10 VALUE "ADD A BANK CARD".
             05 LINE 8 COLUMN 10 VALUE "Enter Card Number: ".
             05 CARD-NO-FIELD LINE 10 COLUMN 10 PIC 9(16)
                USING CARD-NO.
             05 LINE 12 COLUMN 10 VALUE "Enter Expiry Date: ".
             05 CARD-EXPIRY-FIELD LINE 14 COLUMN 10 PIC 9(4)
                USING CARD-EXPIRY.
             05 LINE 16 COLUMN 10 VALUE "Enter CVV Number: ".
             05 CARD-CVV-FIELD LINE 18 COLUMN 10 PIC 9(3)
                USING CARD-CVV.
        *>    BANK DETAILS OPTION POSITIONING
               05 LINE 42 COLUMN 6 VALUE "Option: ".
             05 BANK-ACCOUNT-CHOICE-FIELD LINE 42 COLUMN 14 PIC X
                USING BANK-ACCOUNT-CHOICE.

           01 CREDIT-STORE-SCREEN
             BACKGROUND-COLOR IS 1.
             05 BLANK SCREEN.
        *>    CREDIT STORE HEADER
             05 LINE 1 COL 1  VALUE "   :                              
      -    "                                                         "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 1 COL 2 PIC X(2) USING WS-FORMATTED-HOUR 
             FOREGROUND-COLOR IS 7 REVERSE-VIDEO.
             05 LINE 1 COL 5 PIC X(2) USING WS-FORMATTED-MINS
             FOREGROUND-COLOR IS 7 REVERSE-VIDEO.
             05 LINE 1 COL 81 VALUE "CREDITS: "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 1 COL 90 USING WS-USERCREDITS
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
        *>    CREDIT STORE FOOTER
             05 LINE 43 COL 1 VALUE "                                 
      -    "                                                           "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 44 COL 1 VALUE "     (1) 100 Credits     (2) 200 Cr  
      -    "edits     (3) 300 Credits                                  "                                 
                FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 45 COL 1 VALUE "     (G) Go back         (Q) Quit                                 
      -    "                                                           "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 46 COL 1 VALUE "                                 
      -    "                                                           "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO. 
        *>    CREDIT STORE BODY
             05 LINE  4 COL 10 VALUE "FriendFace" UNDERLINE.
             05 LINE 6 COLUMN 10 VALUE "WELCOME TO THE CREDIT STORE".
        *>    CREDIT STORE OPTION POSITIONING
               05 LINE 42 COLUMN 6 VALUE "Option: ".
             05 CREDIT-STORE-CHOICE-FIELD LINE 42 COLUMN 14 PIC X
                USING CREDIT-STORE-CHOICE.        

           01 MENU-SCREEN
             BACKGROUND-COLOR IS 1.
             05 BLANK SCREEN.
        *>    MENU HEADER
             05 LINE 1 COL 1  VALUE "   :                              
      -    "                                                         "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 1 COL 2 PIC X(2) USING WS-FORMATTED-HOUR 
             FOREGROUND-COLOR IS 7 REVERSE-VIDEO.
             05 LINE 1 COL 5 PIC X(2) USING WS-FORMATTED-MINS
             FOREGROUND-COLOR IS 7 REVERSE-VIDEO.
             05 LINE 1 COL 81 VALUE "CREDITS: "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 1 COL 90 USING WS-USERCREDITS
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
        *>    MENU FOOTER
             05 LINE 43 COL 1 VALUE "                                 
      -    "                                                           "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 44 COL 1 VALUE "     (M) Messages     (F) Fun & g
      -    "ames     (C) Credit store     (A) Account details          "                                 
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 45 COL 1 VALUE "     (L) Logout       (Q) Quit                                 
      -    "                                                           "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 46 COL 1 VALUE "                                 
      -    "                                                           "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
        *>    MENU BODY
           
             05 LINE  4 COL 10 VALUE "FriendFace" UNDERLINE.
             05 LINE  6 COL 10 VALUE "Hi, ".
             05 LINE  6 COL 14 PIC X(16) USING WS-USERNAME.
             05 LINE  8 COL 10 VALUE "Welcome to FriendFace - The latest
      -      " and greatest social media platform!".  
             05 LINE  9 COL 10 VALUE "Feel free to:".
             05 LINE 11 COL 24 VALUE "* " FOREGROUND-COLOR IS 7.
             05 LINE 11 COL 26 VALUE "Read our message board.".
             05 LINE 12 COL 24 VALUE "* " FOREGROUND-COLOR IS 7.
             05 LINE 12 COL 26 VALUE "Play a few games.".
             05 LINE 13 COL 24 VALUE "* " FOREGROUND-COLOR IS 7.
             05 LINE 13 COL 26 VALUE "Leave a message of your own.". 
             05 LINE 14 COL 24 VALUE "* " FOREGROUND-COLOR IS 7.
             05 LINE 14 COL 26 VALUE "Most importantly. HAVE FUN!". 
        *>    MENU OPTION POSITIONING
               05 LINE 42 COLUMN 6 VALUE "Option: ".
               05 MENU-CHOICE-FIELD LINE 42 COLUMN 14 PIC X
                  USING MENU-CHOICE.
           
           01 MSG-MENU-SCREEN
             BACKGROUND-COLOR IS 1.
             05 BLANK SCREEN.
        *>    MSG MENU HEADER
               05 LINE 1 COL 1  VALUE "   :                              
      -    "                                                           "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 1 COL 2 PIC X(2) USING WS-FORMATTED-HOUR 
               FOREGROUND-COLOR IS 7 REVERSE-VIDEO.
               05 LINE 1 COL 5 PIC X(2) USING WS-FORMATTED-MINS
               FOREGROUND-COLOR IS 7 REVERSE-VIDEO.
        *>    MSG MENU FOOTER
               05 LINE 43 COL 1 VALUE "                                 
      -    "                                                           "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 44 COL 1 VALUE "    ( ) Read message by number   
      -    "  (W) Write message    (N) Next page    (P) Previous page  "                                                              
                FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 45 COL 1 VALUE "    (C) Credit store                        
      -    "  (G) Go back          (Q) Quit                            "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 46 COL 1 VALUE "                                 
      -    "                                                           "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
         *>    MSG MENU BODY
             05 LINE  4 COL 10 VALUE "FriendFace" UNDERLINE.

             05 LINE 8 COL 1 VALUE "
      -      "BULLETIN BOARD                                   "  
             FOREGROUND-COLOR IS 7.

             05 LINE 10 COL 8 VALUE "                                   
      -    "                                           "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 11 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 11 COL 10 PIC XXX     USING LIST-ID(ID-NUM)
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 11 COL 13 VALUE "  "
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 11 COL 14 PIC X(50)   USING LIST-TITLE(ID-NUM)
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 11 COL 64 VALUE "                    "
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 11 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 12 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 12 COL 10 PIC XXX     USING LIST-ID(ID-NUM + 1)
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 12 COL 13 VALUE "  "
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 12 COL 14 PIC X(50)   USING LIST-TITLE(ID-NUM + 1)
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 12 COL 64 VALUE "                    "
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 12 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 13 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 13 COL 10 PIC XXX     USING LIST-ID(ID-NUM + 2)
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 13 COL 13 VALUE "  "
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 13 COL 14 PIC X(50)   USING LIST-TITLE(ID-NUM + 2)
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 13 COL 64 VALUE "                    "
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 13 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 14 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 14 COL 10 PIC XXX     USING LIST-ID(ID-NUM + 3)
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 14 COL 13 VALUE "  "
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 14 COL 14 PIC X(50)   USING LIST-TITLE(ID-NUM + 3)
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 14 COL 64 VALUE "                    "
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 14 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 15 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 15 COL 10 PIC XXX     USING LIST-ID(ID-NUM + 4)
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 15 COL 13 VALUE "  "
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 15 COL 14 PIC X(50)   USING LIST-TITLE(ID-NUM + 4)
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 15 COL 64 VALUE "                    "
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 15 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 16 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 16 COL 10 PIC XXX     USING LIST-ID(ID-NUM + 5)
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 16 COL 13 VALUE "  "
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 16 COL 14 PIC X(50)   USING LIST-TITLE(ID-NUM + 5)
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 16 COL 64 VALUE "                    "
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 16 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 17 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 17 COL 10 PIC XXX     USING LIST-ID(ID-NUM + 6)
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 17 COL 13 VALUE "  "
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 17 COL 14 PIC X(50)   USING LIST-TITLE(ID-NUM + 6)
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 17 COL 64 VALUE "                    "
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 17 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 18 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 18 COL 10 PIC XXX     USING LIST-ID(ID-NUM + 7)
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 18 COL 13 VALUE "  "
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 18 COL 14 PIC X(50)   USING LIST-TITLE(ID-NUM + 7)
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 18 COL 64 VALUE "                    "
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 18 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 19 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 19 COL 10 PIC XXX     USING LIST-ID(ID-NUM + 8)
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 19 COL 13 VALUE "  "
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 19 COL 14 PIC X(50)   USING LIST-TITLE(ID-NUM + 8)
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 19 COL 64 VALUE "                    "
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 19 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 20 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 20 COL 10 PIC XXX     USING LIST-ID(ID-NUM + 9)
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 20 COL 13 VALUE "  "
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 20 COL 14 PIC X(50)   USING LIST-TITLE(ID-NUM + 9)
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 20 COL 64 VALUE "                    "
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 20 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 21 COL 8 VALUE "                                   
      -    "                                           "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.

             05 LINE 23 COL 1 VALUE "
      -      "RECENT MESSAGES                                   " 
             FOREGROUND-COLOR IS 7.
        *>    MSG MENU OPTION POSITIONING
               05 LINE 42 COLUMN 6 VALUE "Option: ".
               05 MSG-MENU-CHOICE-FIELD LINE 42 COLUMN 14 PIC XX
                  USING MSG-MENU-CHOICE.

           01 MESSAGE-VIEW-SCREEN
               BACKGROUND-COLOR IS 01.
                05 BLANK SCREEN.
        *>    MESSAGE VIEW HEADER
               05 LINE 1 COL 1  VALUE "   :                              
      -    "                                                           "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 1 COL 2 PIC X(2) USING WS-FORMATTED-HOUR 
               FOREGROUND-COLOR IS 7 REVERSE-VIDEO.
               05 LINE 1 COL 5 PIC X(2) USING WS-FORMATTED-MINS
               FOREGROUND-COLOR IS 7 REVERSE-VIDEO.
        *>    MESSAGE VIEW FOOTER
               05 LINE 43 COL 1 VALUE "                                 
      -    "                                                           "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 44 COL 1 VALUE "     (N) Next page     (P) Previo
      -    "us page     (C) Comments section                           "                                
                FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 45 COL 1 VALUE "     (G) Go back       (Q) Quit                                 
      -    "                                                           "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 46 COL 1 VALUE "                                 
      -    "                                                           "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
        *>    MESSAGE VIEW BODY
             05 LINE  4 COL 10 VALUE "FriendFace" UNDERLINE.
      
             05 LINE 8 COL 10 VALUE "-----------------------------------
      -      "---------------------" FOREGROUND-COLOR IS 7.

             05 LINE 9 COL 10 VALUE "---------------------BULLETIN BOARD
      -      "---------------------" FOREGROUND-COLOR 
             IS 7.
             05 LINE 10 COL 10 VALUE "----------------------------------
      -      "----------------------" FOREGROUND-COLOR IS 7.

             05 LINE 12 COL 10 VALUE "Title: ".
             05 LINE 12 COL 19 PIC X(50) USING LIST-TITLE(MSG-SELECT).
             05 LINE 14 COL 10 VALUE "Message: ".
             05 LINE 14 COL 19 PIC X(60) USING LS-PART-1.
             05 LINE 15 COL 19 PIC X(60) USING LS-PART-2.
             05 LINE 16 COL 19 PIC X(60) USING LS-PART-3.
             05 LINE 17 COL 19 PIC X(60) USING LS-PART-4.
             05 LINE 18 COL 19 PIC X(60) USING LS-PART-5.
             05 LINE 20 COL 10 VALUE "Author: ".
             05 LINE 20 COL 19 PIC X(16) 
                USING LIST-USERNAME(MSG-SELECT).

             05 LINE 22 COL 10 VALUE "----------------------------------
      -      "---------------------" FOREGROUND-COLOR IS 7.
             05 LINE 23 COL 10 VALUE "---------------------CHOSEN MESSAG
      -      "E--------------------" FOREGROUND-COLOR IS 7.
             05 LINE 24 COL 10 VALUE "----------------------------------
      -      "---------------------" FOREGROUND-COLOR IS 7.
        *>    MESSAGE VIEW OPTION POSITIONING
               05 LINE 42 COLUMN 6 VALUE "Option: ".
               05 MSG-VIEW-CHOICE-FIELD LINE 42 COLUMN 14 PIC X
                  USING MSG-VIEW-CHOICE.

           01 WRITE-MSG-SCREEN
               BACKGROUND-COLOR IS 01.
               05 BLANK SCREEN.
        *>    WRITE MESSAGE HEADER
               05 LINE 1 COL 1  VALUE "   :                              
      -    "                                                           "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 1 COL 2 PIC X(2) USING WS-FORMATTED-HOUR 
               FOREGROUND-COLOR IS 7 REVERSE-VIDEO.
               05 LINE 1 COL 5 PIC X(2) USING WS-FORMATTED-MINS
               FOREGROUND-COLOR IS 7 REVERSE-VIDEO.
        *>    WRITE MESSAGE FOOTER
               05 LINE 43 COL 1 VALUE "                                 
      -    "                                                           "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 44 COL 1 VALUE "     (S) Submit message     (D) D
      -    "iscard message     (Q) Quit                                "                                 
                FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 45 COL 1 VALUE "                                 
      -    "                                                           "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 46 COL 1 VALUE "                                 
      -    "                                                           "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
        *>    WRITE MESSAGE BODY
             05 LINE  4 COL 10 VALUE "FriendFace" UNDERLINE.
      
             05 LINE 8 COL 10 VALUE "-----------------------------------
      -      "---------------------" FOREGROUND-COLOR IS 7.

             05 LINE 9 COL 10 VALUE "---------------------BULLETIN BOARD
      -      "---------------------" FOREGROUND-COLOR 
             IS 7.
             05 LINE 10 COL 10 VALUE "----------------------------------
      -      "----------------------" FOREGROUND-COLOR IS 7.

             05 LINE 12 COL 10 VALUE "TITLE:   ".
             05 WS-TITLE-FIELD LINE 12 COL 18 PIC X(50) USING WS-TITLE.
             05 LINE 14 COL 10 VALUE "MESSAGE: ".
             05 LINE-1-FIELD LINE 16 COL 10 PIC X(60)  USING LS-PART-1.
             05 LINE-2-FIELD LINE 17 COL 10 PIC X(60)  USING LS-PART-2.
             05 LINE-3-FIELD LINE 18 COL 10 PIC X(60)  USING LS-PART-3.
             05 LINE-4-FIELD LINE 19 COL 10 PIC X(60)  USING LS-PART-4.
             05 LINE-5-FIELD LINE 20 COL 10 PIC X(60)  USING LS-PART-5. 

                   05 LINE 22 COL 10 VALUE "----------------------------------
      -      "---------------------" FOREGROUND-COLOR IS 7.
             05 LINE 23 COL 10 VALUE "---------------------LEAVE A MESSA
      -      "GE-------------------" FOREGROUND-COLOR IS 7.
             05 LINE 24 COL 10 VALUE "----------------------------------
      -      "---------------------" FOREGROUND-COLOR IS 7.
        *>    WRITE MESSAGE OPTION POSITIONING
               05 LINE 42 COLUMN 6 VALUE "Option: ".
               05 MSG-WRITE-CHOICE-FIELD LINE 42 COLUMN 14 PIC X
                  USING MSG-WRITE-CHOICE.
      ******************************************************************
      *****************-----COMMENTS SCREEN SECTION--------*************
      ******************************************************************

           01 COMMENT-SCREEN
             BACKGROUND-COLOR IS 1.
             05 BLANK SCREEN.
        *>    COMMENT SECTION HEADER
             05 LINE 1 COL 1  VALUE "   :                              
      -    "                                                           "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 1 COL 2 PIC X(2) USING WS-FORMATTED-HOUR 
             FOREGROUND-COLOR IS 7 REVERSE-VIDEO.
             05 LINE 1 COL 5 PIC X(2) USING WS-FORMATTED-MINS
             FOREGROUND-COLOR IS 7 REVERSE-VIDEO.
        *>    COMMENT SECTION FOOTER
            05 LINE 43 COL 1 VALUE "                                 
      -    "                                                           "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 44 COL 1 VALUE "     (N) Next page     (P) Previous
      -    " page     (G) Go back     (Q) Quit                         "                                 
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 45 COL 1 VALUE "                                 
      -    "                                                           "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 46 COL 1 VALUE "                                 
      -    "                                                           "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 3 COL 10 VALUE "Comments for the message titled: ".
             05 LINE 3 COL 43 PIC X(50) USING LIST-TITLE(MSG-SELECT)
             FOREGROUND-COLOR IS 2, HIGHLIGHT.
        *>    COMMENT SECTION BODY
             05 LINE 7 COL 10 VALUE "Comment:" UNDERLINE.
             05 LINE 7 COL 72 VALUE "Date posted:" UNDERLINE.
            *>    1st COMMENT
             05 LINE 9 COL 72 PIC X(21) USING COM-DATE(COM-INDEX).
             05 LINE 9 COL 10 PIC X(50) USING COM-COMMENT(COM-INDEX)
             FOREGROUND-COLOR IS 2.
             05 LINE 10 COL 10 PIC X(16) USING COM-AUTHOR(COM-INDEX).
             *>    2nd COMMENT
             05 LINE 12 COL 72 PIC X(21) 
             USING COM-DATE(COM-INDEX + 1).
             05 LINE 12 COL 10 PIC X(50) 
             USING COM-COMMENT(COM-INDEX + 1)
             FOREGROUND-COLOR IS 2.
             05 LINE 13 COL 10 PIC X(16) 
             USING COM-AUTHOR(COM-INDEX + 1).
             *>    3rd COMMENT
             05 LINE 15 COL 72 PIC X(21) 
             USING COM-DATE(COM-INDEX + 2).
             05 LINE 15 COL 10 PIC X(50) 
             USING COM-COMMENT(COM-INDEX + 2)
             FOREGROUND-COLOR IS 2.
             05 LINE 16 COL 10 PIC X(16) 
             USING COM-AUTHOR(COM-INDEX + 2).
             *>    4th COMMENT
             05 LINE 18 COL 72 PIC X(21) 
             USING COM-DATE(COM-INDEX + 3).
             05 LINE 18 COL 10 PIC X(50) 
             USING COM-COMMENT(COM-INDEX + 3)
             FOREGROUND-COLOR IS 2.
             05 LINE 19 COL 10 PIC X(16) 
             USING COM-AUTHOR(COM-INDEX + 3).
             *>    5th COMMENT
             05 LINE 21 COL 72 PIC X(21) 
             USING COM-DATE(COM-INDEX + 4).
             05 LINE 21 COL 10 PIC X(50) 
             USING COM-COMMENT(COM-INDEX + 4)
             FOREGROUND-COLOR IS 2.
             05 LINE 22 COL 10 PIC X(16) 
             USING COM-AUTHOR(COM-INDEX + 4).
        *>    COMMENT SECTION OPTION POSITIONING
             05 LINE 42 COLUMN 6 VALUE "Option: ".
             05 COM-SCRN-CHOICE-FIELD LINE 42 COL 14 PIC X USING
               COM-SCRN-CHOICE.   
      ******************************************************************
      *******************-----GAMES MENU SECTION----********************
      ******************************************************************
                       
           01 GAMES-MENU-SCREEN
               BACKGROUND-COLOR IS 1.
               05 BLANK SCREEN.
        *>    GAMES MENU HEADER
               05 LINE 1 COL 1  VALUE "   :                              
      -    "                                                           "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 1 COL 2 PIC X(2) USING WS-FORMATTED-HOUR 
               FOREGROUND-COLOR IS 7 REVERSE-VIDEO.
               05 LINE 1 COL 5 PIC X(2) USING WS-FORMATTED-MINS
               FOREGROUND-COLOR IS 7 REVERSE-VIDEO.
        *>    GAMES MENU FOOTER
               05 LINE 43 COL 1 VALUE "                                 
      -    "                                                           "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 44 COL 1 VALUE "     (W) Word guessing game     (                    
      -    "C) Credit Store                                            "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 45 COL 1 VALUE "     (G) Go back                (
      -    "Q) Quit                                                    "                                         
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 46 COL 1 VALUE "                                 
      -    "                                                           "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
        *>    GAMES MENU OPTION POSITIONING
               05 LINE 42 COLUMN 6 VALUE "Option: ".
               05 GAMES-MENU-CHOICE-FIELD LINE 42 COLUMN 14 PIC X
                  USING GAMES-MENU-CHOICE.

       01 WORD-GUESSING-SCREEN
               BACKGROUND-COLOR IS 1.
               05 BLANK SCREEN.
        *>    WORD GUESSING GAME HEADER
               05 LINE 1 COL 1  VALUE "   :                              
      -    "                                                           "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 1 COL 2 PIC X(2) USING WS-FORMATTED-HOUR 
               FOREGROUND-COLOR IS 7 REVERSE-VIDEO.
               05 LINE 1 COL 5 PIC X(2) USING WS-FORMATTED-MINS
               FOREGROUND-COLOR IS 7 REVERSE-VIDEO.
        *>    WORD GUESSING GAME FOOTER
               05 LINE 43 COL 1 VALUE "                                 
      -    "                                                           "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 44 COL 1 VALUE "     (W) Word guessing game     (                    
      -    "C) Credit Store                                            "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 45 COL 1 VALUE "     (G) Go back                (
      -    "Q) Quit                                                    "                                         
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 46 COL 1 VALUE "                                 
      -    "                                                           "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
        *>    WORD GUESSING GAME BODY
             05 LINE 2 COLUMN 37 VALUE "Word Guessing Game" UNDERLINE.
             05 LINE 14 COLUMN 10 VALUE "Guess this word: ".
             05 LINE 16 COLUMN 10 PIC X(20) USING WS-WORD.
             05 LINE 18 COLUMN 10 VALUE "Guesses left: ".
             05 LINE 18 COLUMN 40 PIC 99 USING WS-GUESSES-LEFT.
             05 LINE 20 COLUMN 10 VALUE "( ) Enter a letter to guess".
             05 LINE 21 COLUMN 10 VALUE "(!) Quit game".
        *>    WORD GUESSING GAME OPTION POSITIONING
               05 LINE 42 COLUMN 6 VALUE "Option: ".
   
           01 IN-GAME-SCREEN
               BACKGROUND-COLOR IS 1.
               05 BLANK SCREEN.
        *>    IN GAME HEADER 
               05 LINE 1 COL 1  VALUE "   :                              
      -    "                                                           "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 1 COL 2 PIC X(2) USING WS-FORMATTED-HOUR 
               FOREGROUND-COLOR IS 7 REVERSE-VIDEO.
               05 LINE 1 COL 5 PIC X(2) USING WS-FORMATTED-MINS
               FOREGROUND-COLOR IS 7 REVERSE-VIDEO.
         *>    IN GAME FOOTER
               05 LINE 43 COL 1 VALUE "                                 
      -    "                                                           "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 44 COL 1 VALUE "     ( ) Enter a letter to guess                    
      -    "    (!) Quit game                                          "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 45 COL 1 VALUE "     
      -    "                                                           "                                         
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 46 COL 1 VALUE "                                 
      -    "                                                           "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
        *>    IN GAME BODY
             05 LINE 3 COLUMN 37 VALUE "Word Guessing Game"
             UNDERLINE, FOREGROUND-COLOR IS 7.
             05 LINE 14 COLUMN 10 VALUE "Guess this word: "
             FOREGROUND-COLOR IS 7.
             05 LINE 16 COLUMN 10 PIC X(20) USING WS-WORD
               FOREGROUND-COLOR IS 6.
             05 LINE 18 COLUMN 10 VALUE "Guesses left: "
             FOREGROUND-COLOR IS 7.
             05 LINE 18 COLUMN 40 PIC 99 USING WS-GUESSES-LEFT
             FOREGROUND-COLOR IS 7.
        *>    IN GAME OPTION POSITIONING
             05 LINE 42 COLUMN 6 VALUE "Option: ".
             05 WS-GUESS-CHOICE-FIELD LINE 42 COLUMN 14 PIC X
               USING WS-GUESS-CHOICE   FOREGROUND-COLOR IS 0.

           01 WORD-GUESSING-LOSE-SCREEN
             BACKGROUND-COLOR IS 4.
             05 BLANK SCREEN.
        *>    LOSE GAME HEADER
               05 LINE 1 COL 1  VALUE "   :                              
      -    "                                                           "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 1 COL 2 PIC X(2) USING WS-FORMATTED-HOUR 
               FOREGROUND-COLOR IS 7 REVERSE-VIDEO.
               05 LINE 1 COL 5 PIC X(2) USING WS-FORMATTED-MINS
               FOREGROUND-COLOR IS 7 REVERSE-VIDEO.
        *>    LOSE GAME FOOTER
               05 LINE 43 COL 1 VALUE "                                 
      -    "                                                           "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 44 COL 1 VALUE "     (P) Play again     (H) High                        
      -    " scores     (Q) Quit game                                  "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 45 COL 1 VALUE " 
      -    "                                                           "                                         
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 46 COL 1 VALUE "                                 
      -    "                                                           "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
        *>    LOSE GAME BODY
             05 LINE 2 COLUMN 37 VALUE "Word Guessing Game".
             05 LINE 14 COLUMN 10 VALUE "You lost!".
             05 LINE 16 COLUMN 10 PIC X(20) USING WS-WORD.
             05 LINE 18 COLUMN 10 VALUE "Guesses left: ".
             05 LINE 18 COLUMN 40 PIC 99 USING WS-GUESSES-LEFT.      
        *>    LOSE GAME OPTION POSITIONING
             05 LINE 42 COLUMN 6 VALUE "Option: "
             FOREGROUND-COLOR IS 7.
             05 WS-GUESSING-LOSING-CHOICE-FIELD LINE 42 COLUMN 14 PIC X
               USING WS-GUESSING-LOSING-CHOICE.

           01 WORD-GUESSING-WINNING-SCREEN
             BACKGROUND-COLOR IS 2.
             05 BLANK SCREEN.
        *>    WIN GAME HEADER
               05 LINE 1 COL 1  VALUE "   :                              
      -    "                                                           "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 1 COL 2 PIC X(2) USING WS-FORMATTED-HOUR 
               FOREGROUND-COLOR IS 7 REVERSE-VIDEO.
               05 LINE 1 COL 5 PIC X(2) USING WS-FORMATTED-MINS
               FOREGROUND-COLOR IS 7 REVERSE-VIDEO.
        *>    WIN GAME FOOTER
               05 LINE 43 COL 1 VALUE "                                 
      -    "                                                           "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 44 COL 1 VALUE "     (P) Play again     (H) High                        
      -    " scores     (Q) Quit game                                  "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 45 COL 1 VALUE " 
      -    "                                                           "                                         
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 46 COL 1 VALUE "                                 
      -    "                                                           "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
        *>    WIN GAME BODY
             05 LINE 2 COLUMN 37 VALUE "Word Guessing Game" UNDERLINE,
               FOREGROUND-COLOR IS 0.
             05 LINE 14 COLUMN 10 VALUE "You guessed the word!"
               FOREGROUND-COLOR IS 0.
             05 LINE 16 COLUMN 10 PIC X(20) USING WS-ANSWERWORD
               FOREGROUND-COLOR IS 0.
             05 LINE 18 COLUMN 10 PIC 99 USING WS-GUESSES-LEFT
               FOREGROUND-COLOR IS 0.
             05 LINE 20 COLUMN 10 VALUE "You scored: "
               FOREGROUND-COLOR IS 0.
             05 LINE 20 COLUMN 22 PIC 99 USING WS-HIGH-SCORE
               FOREGROUND-COLOR IS 0.
        *>    WIN GAME OPTION POSITIONING
             05 LINE 42 COLUMN 6 VALUE "Option: "
             FOREGROUND-COLOR IS 0.
             05 WS-GUESSING-WINNING-CHOICE-FIELD LINE 42 COLUMN 14 PIC X
               USING WS-GUESSING-WINNING-CHOICE
               FOREGROUND-COLOR IS 0.

           01 HIGH-SCORE-SCREEN
             BACKGROUND-COLOR IS 1.
             05 BLANK SCREEN.
        *>    HIGH SCORE HEADER

        *>    HIGH SCORE FOOTER 
               05 LINE 43 COL 1 VALUE "                                 
      -    "                                                           "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 44 COL 1 VALUE "     (G) Go back     (Q) Quit                        
      -    "                                                           "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 45 COL 1 VALUE " 
      -    "                                                           "                                         
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 46 COL 1 VALUE "                                 
      -    "                                                           "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
        *>    HIGH SCORE BODY
             05 LINE 2 COLUMN 40 VALUE "Word Guessing Game" UNDERLINE,
             FOREGROUND-COLOR IS 7.
             05 LINE 6 COLUMN 43 VALUE "High Scores:"
             HIGHLIGHT, FOREGROUND-COLOR IS 7.
             05 LINE 7 COLUMN 40 VALUE "*****************"
             FOREGROUND-COLOR IS 2.
             05 LINE 8 COLUMN 40 VALUE "*" FOREGROUND-COLOR IS 2.
             05 LINE 8 COLUMN 42 PIC XX USING WS-SCORE(1)
             FOREGROUND-COLOR IS 7.
             05 LINE 8 COLUMN 46 PIC X(10) USING WS-NAME(1)
             FOREGROUND-COLOR IS 7.
             05 LINE 8 COLUMN 56 VALUE "*" FOREGROUND-COLOR IS 2.
             05 LINE 9 COLUMN 40 VALUE "*****************" 
             FOREGROUND-COLOR IS 2.
             05 LINE 10 COLUMN 40 VALUE "*" FOREGROUND-COLOR IS 7.
             05 LINE 10 COLUMN 42 PIC XX USING WS-SCORE(2)
             FOREGROUND-COLOR IS 7.
             05 LINE 10 COLUMN 46 PIC X(10) USING WS-NAME(2)
             FOREGROUND-COLOR IS 7.
             05 LINE 10 COLUMN 56 VALUE "*" FOREGROUND-COLOR IS 7.
             05 LINE 11 COLUMN 40 VALUE "*" FOREGROUND-COLOR IS 7.
             05 LINE 11 COLUMN 56 VALUE "*" FOREGROUND-COLOR IS 7.
             05 LINE 12 COLUMN 40 VALUE "*" FOREGROUND-COLOR IS 7.
             05 LINE 12 COLUMN 42 PIC XX USING WS-SCORE(3)
             FOREGROUND-COLOR IS 7.
             05 LINE 12 COLUMN 46 PIC X(10) USING WS-NAME(3)
             FOREGROUND-COLOR IS 7.
             05 LINE 12 COLUMN 56 VALUE "*" FOREGROUND-COLOR IS 7.
             05 LINE 13 COLUMN 40 VALUE "*" FOREGROUND-COLOR IS 7.
             05 LINE 13 COLUMN 56 VALUE "*" FOREGROUND-COLOR IS 7.
             05 LINE 14 COLUMN 40 VALUE "*" FOREGROUND-COLOR IS 7.
             05 LINE 14 COLUMN 42 PIC XX USING WS-SCORE(4)
             FOREGROUND-COLOR IS 7.
             05 LINE 14 COLUMN 46 PIC X(10) USING WS-NAME(4)
             FOREGROUND-COLOR IS 7.
             05 LINE 14 COLUMN 56 VALUE "*" FOREGROUND-COLOR IS 7.
             05 LINE 15 COLUMN 40 VALUE "*" FOREGROUND-COLOR IS 7.
             05 LINE 15 COLUMN 56 VALUE "*" FOREGROUND-COLOR IS 7.
             05 LINE 16 COLUMN 40 VALUE "*" FOREGROUND-COLOR IS 7.
             05 LINE 16 COLUMN 42 PIC XX USING WS-SCORE(5)
             FOREGROUND-COLOR IS 7.
             05 LINE 16 COLUMN 46 PIC X(10) USING WS-NAME(5)
             FOREGROUND-COLOR IS 7.
             05 LINE 16 COLUMN 56 VALUE "*" FOREGROUND-COLOR IS 7.
             05 LINE 17 COLUMN 40 VALUE "*****************" 
             FOREGROUND-COLOR IS 7.
        *>    HIGH SCORE OPTION POSITIONING
               05 LINE 42 COLUMN 6 VALUE "Option: ".
               05 WS-HIGH-SCORE-CHOICE-FIELD LINE 42 COLUMN 14 PIC X
                  USING WS-HIGH-SCORE-CHOICE.

      ************************END OF SCREEN SECTION********************* 
           
       PROCEDURE DIVISION.
      ******************************************************************
      *-----**************LOGIN / SIGN-IN/UP SECTION-----***************
      ******************************************************************
        0110-DISPLAY-LOGIN.
           PERFORM 0200-TIME-AND-DATE.
           INITIALIZE LOGIN-CHOICE. 
           DISPLAY LOGIN-SCREEN.
           ACCEPT LOGIN-CHOICE-FIELD.
           IF LOGIN-CHOICE = "l" OR "L" THEN 
               PERFORM 0111-SIGN-IN 
           ELSE IF LOGIN-CHOICE = "c" OR "C" THEN 
               PERFORM 0112-SIGN-UP
           ELSE IF LOGIN-CHOICE = "q" OR "Q" THEN 
               STOP RUN
           ELSE 
               PERFORM 0120-DISPLAY-MENU
           END-IF.

       0111-SIGN-IN.
           PERFORM 0200-TIME-AND-DATE.
           INITIALIZE WS-USERNAME.
           INITIALIZE WS-PASSWORD.
           DISPLAY SIGN-IN-SCREEN.
           ACCEPT WS-USERNAME-FIELD.
           ACCEPT WS-PASSWORD-FIELD.
           
           CALL "sign-in" USING WS-USERNAME, WS-PASSWORD, 
           WS-LOGIN-CORRECT.

           IF WS-LOGIN-CORRECT = 1 THEN
               PERFORM 0120-DISPLAY-MENU 
           ELSE 
               MOVE "Incorrect Username or Password" TO WS-ERROR-MSG
                   PERFORM 0114-ERROR-PAGE 
           END-IF. 

       0112-SIGN-UP.
           PERFORM 0200-TIME-AND-DATE.
           INITIALIZE WS-NEW-USER-NAME.
           INITIALIZE WS-NEW-PASSWORD.
           INITIALIZE CREATE-CHOICE
           DISPLAY CREATE-AN-ACCOUNT-SCREEN.
           ACCEPT WS-NEW-USER-NAME-FIELD.
           ACCEPT WS-NEW-PASSWORD-FIELD.
           ACCEPT CREATE-CHOICE-FIELD.
           
           IF CREATE-CHOICE = "q" OR "Q" THEN 
               PERFORM 0110-DISPLAY-LOGIN   
           ELSE IF CREATE-CHOICE = "s" OR "S" THEN
               PERFORM 0113-SIGN-UP-CHECK
           END-IF.       

       0113-SIGN-UP-CHECK.
           
           IF WS-NEW-USER-NAME = " "
               MOVE "Invalid Username Try Another" TO WS-ERROR-MSG
               PERFORM 0114-ERROR-PAGE
           ELSE IF WS-NEW-PASSWORD = " "
               MOVE "Invalid Password Try Another" TO WS-ERROR-MSG
               PERFORM 0114-ERROR-PAGE
           END-IF.    
           
           CALL "sign-up-check" USING WS-NEW-USER-NAME 
               WS-UNAME-UNAVAILABLE.

           IF WS-UNAME-UNAVAILABLE = 1 THEN
               MOVE "Username Taken" TO WS-ERROR-MSG
               PERFORM 0114-ERROR-PAGE
           ELSE
               CALL "sign-up" USING WS-NEW-USER-NAME WS-NEW-PASSWORD
               PERFORM 0111-SIGN-IN
           END-IF.

       0114-ERROR-PAGE.
           PERFORM 0200-TIME-AND-DATE.
           PERFORM 0250-CREDIT-TOTAL.
           INITIALIZE ERROR-CHOICE.
           DISPLAY ERROR-SCREEN.
           ACCEPT ERROR-CHOICE-FIELD.
           IF ERROR-CHOICE = "l" OR "L" THEN 
               PERFORM 0111-SIGN-IN
           ELSE IF ERROR-CHOICE = "c" OR "C" THEN 
               PERFORM 0112-SIGN-UP 
           ELSE 
               PERFORM 0114-ERROR-PAGE 
           END-IF.
      ****************************************************************** 
      ********-----DISPLAY MENU COMES AFTER SUCCESFUL LOGIN----*********
      ******************************************************************
       0120-DISPLAY-MENU.
           PERFORM 0200-TIME-AND-DATE.
           PERFORM 0250-CREDIT-TOTAL.
           INITIALIZE MENU-CHOICE.
           DISPLAY MENU-SCREEN.
           ACCEPT MENU-CHOICE-FIELD.

           IF MENU-CHOICE =        "q" or "Q" THEN
             STOP RUN
           ELSE IF MENU-CHOICE =   "l" or "L" THEN
             PERFORM 0110-DISPLAY-LOGIN
           ELSE IF MENU-CHOICE =   "m" or "M" THEN
             PERFORM 0130-MSG-MENU
           ELSE IF MENU-CHOICE =   "f" or "F" THEN
             PERFORM 0160-GAMES-MENU
           ELSE IF MENU-CHOICE =   "a" or "A" THEN
             PERFORM 0122-USER-ACCOUNT-MENU
           ELSE IF MENU-CHOICE =   "c" or "C" THEN
               PERFORM 0128-CREDIT-STORE 
           END-IF.

           PERFORM 0120-DISPLAY-MENU.

       0122-USER-ACCOUNT-MENU.
           PERFORM 0200-TIME-AND-DATE.
           PERFORM 0250-CREDIT-TOTAL.
           INITIALIZE ACCOUNT-CHOICE.
           DISPLAY USER-ACCOUNT-SCREEN.
           ACCEPT ACCOUNT-CHOICE-FIELD.

           IF ACCOUNT-CHOICE =     "q" or "Q" THEN
               STOP RUN
           ELSE IF ACCOUNT-CHOICE = "b" or "B" THEN
               PERFORM 0125-BANK-DETAILS  
           ELSE IF ACCOUNT-CHOICE = "g" or "G" THEN
               PERFORM 0128-CREDIT-STORE  
           ELSE IF ACCOUNT-CHOICE = "c" or "C" THEN
               PERFORM 0128-CREDIT-STORE 
           END-IF.

           PERFORM 0122-USER-ACCOUNT-MENU.

      ******************************************************************   
      ********************----BANK DETAILS SECTIONS----*****************
      ******************************************************************
           
       0125-BANK-DETAILS.    
           PERFORM 0200-TIME-AND-DATE.
           PERFORM 0250-CREDIT-TOTAL.
           INITIALIZE CARD-NO.
           INITIALIZE CARD-EXPIRY.
           INITIALIZE CARD-CVV.
           INITIALIZE BANK-ACCOUNT-CHOICE.
           DISPLAY BANK-DETAILS-SCREEN.

           ACCEPT CARD-NO-FIELD.
           ACCEPT CARD-EXPIRY-FIELD.
           ACCEPT CARD-CVV-FIELD.
           ACCEPT BANK-ACCOUNT-CHOICE-FIELD.

           IF BANK-ACCOUNT-CHOICE = "s" or "S" then
               PERFORM 0127-UPDATE-BANK-DETAILS
           ELSE IF BANK-ACCOUNT-CHOICE = "d" or "D" then
               PERFORM 0125-BANK-DETAILS
           ELSE IF BANK-ACCOUNT-CHOICE = "q" or "Q" THEN
               STOP RUN
           ELSE IF BANK-ACCOUNT-CHOICE = "g" or "G" THEN
               PERFORM 0122-USER-ACCOUNT-MENU
           END-IF.  

       0127-UPDATE-BANK-DETAILS.
           MOVE CARD-NO TO WS-CARD-NO.
           MOVE CARD-EXPIRY TO WS-CARD-EXPIRY.
           MOVE CARD-CVV TO WS-CARD-CVV.

           CALL "bank-details" USING WS-USERNAME, WS-CARD-NO,
           WS-CARD-EXPIRY, WS-CARD-CVV.
           
           PERFORM 0122-USER-ACCOUNT-MENU.

      ******************************************************************   
      *****************----CREDIT STORE SECTION----*********************
      ******************************************************************
       0128-CREDIT-STORE.
           MOVE 0 TO WS-UPDATE-CREDITS.
           
           PERFORM 0200-TIME-AND-DATE.
           PERFORM 0250-CREDIT-TOTAL.
           
           INITIALIZE CREDIT-STORE-CHOICE.
           DISPLAY CREDIT-STORE-SCREEN.

           ACCEPT CREDIT-STORE-CHOICE-FIELD.
           
           IF CREDIT-STORE-CHOICE = "1" THEN
               MOVE 100 TO WS-UPDATE-CREDITS
               MOVE 10 TO WS-STORE-CHARGE
               PERFORM 0129-ADD-CREDITS
           ELSE IF CREDIT-STORE-CHOICE = "2" THEN
               MOVE 200 TO WS-UPDATE-CREDITS
               MOVE 20 TO WS-STORE-CHARGE
               PERFORM 0129-ADD-CREDITS
           ELSE IF CREDIT-STORE-CHOICE = "3" THEN
               MOVE 300 TO WS-UPDATE-CREDITS
               MOVE 30 TO WS-STORE-CHARGE 
               PERFORM 0129-ADD-CREDITS
           ELSE IF CREDIT-STORE-CHOICE = "g" OR "G" THEN
              PERFORM 0128-CREDIT-STORE  
           ELSE IF CREDIT-STORE-CHOICE = "q" OR "Q" THEN
              STOP RUN  
           END-IF.
       
       0129-ADD-CREDITS.
           CALL "add-credits" USING WS-USERNAME, WS-UPDATE-CREDITS.
           
           PERFORM 0300-TRANSACTIONS.

      ******************************************************************
      *********-----MESSAGE SECTION FOR READ/WRITE/COMMENT----**********
      ******************************************************************
       0130-MSG-MENU.
           PERFORM 0250-CREDIT-TOTAL.
           PERFORM 0200-TIME-AND-DATE.
           CALL "number-of-file-lines" USING NUM-FILE-LINES.
           CALL "get-list-page-alt" USING NUM-FILE-LINES WS-LIST-TABLE.
           *> CALL "id-sort" USING WS-LIST-TABLE. <*
           INITIALIZE MSG-MENU-CHOICE.
           DISPLAY MSG-MENU-SCREEN.
           ACCEPT MSG-MENU-CHOICE-FIELD.
           MOVE MSG-MENU-CHOICE TO MSG-SELECT.
         
           IF MSG-SELECT > 0 THEN
             PERFORM 0140-MESSAGE-VIEW
           END-IF. 
           IF MSG-MENU-CHOICE =        "g" OR "G" THEN
               PERFORM 0120-DISPLAY-MENU
           ELSE IF MSG-MENU-CHOICE =   "n" OR "N" THEN
             COMPUTE ID-NUM = ID-NUM + 10
               IF ID-NUM IS GREATER THAN OR EQUAL TO NUM-FILE-LINES
                 COMPUTE ID-NUM = ID-NUM - 10
                 PERFORM 0130-MSG-MENU
               ELSE
                   PERFORM 0130-MSG-MENU
               END-IF               
               
           ELSE IF MSG-MENU-CHOICE =       "p" OR "P" THEN
             COMPUTE ID-NUM = ID-NUM - 10
               
               IF ID-NUM IS LESS THAN 10
                   MOVE 1 TO ID-NUM
                    PERFORM 0130-MSG-MENU
               ELSE
                    PERFORM 0130-MSG-MENU
               END-IF
           ELSE IF MSG-MENU-CHOICE =       "w" OR "W"
             PERFORM 0150-MESSAGE-WRITE
              
           ELSE IF MSG-MENU-CHOICE =       "q" OR "Q" THEN
              STOP RUN  
           END-IF.

           PERFORM 0130-MSG-MENU.

       0140-MESSAGE-VIEW. 
           PERFORM 0200-TIME-AND-DATE.  
           PERFORM 0250-CREDIT-TOTAL.
           CALL "number-of-file-lines" USING NUM-FILE-LINES.
           CALL "get-list-page-alt" USING NUM-FILE-LINES WS-LIST-TABLE.
           *> CALL "id-sort" USING WS-LIST-TABLE. <*        
           MOVE LIST-CONTENT(MSG-SELECT) TO WS-CONTENT-DISPLAY.
           INITIALIZE MSG-VIEW-CHOICE.
           DISPLAY MESSAGE-VIEW-SCREEN.
           ACCEPT MSG-VIEW-CHOICE-FIELD.

           IF MSG-VIEW-CHOICE =        "n" OR "N" THEN
             COMPUTE MSG-SELECT = MSG-SELECT + 1
               IF MSG-SELECT IS GREATER THAN OR EQUAL TO NUM-FILE-LINES
                 COMPUTE MSG-SELECT = MSG-SELECT - 1
                 PERFORM 0140-MESSAGE-VIEW
               ELSE
                   PERFORM 0140-MESSAGE-VIEW
               END-IF                
           ELSE IF MSG-VIEW-CHOICE =   "p" OR "P" THEN
             COMPUTE MSG-SELECT = MSG-SELECT - 1
               IF MSG-SELECT IS LESS THAN 1
                 COMPUTE MSG-SELECT = MSG-SELECT + 1
               ELSE
                   PERFORM 0140-MESSAGE-VIEW
               END-IF 
           ELSE IF MSG-VIEW-CHOICE =   "g" OR "G" THEN
               PERFORM 0130-MSG-MENU
           ELSE IF MSG-VIEW-CHOICE =   "q" OR "Q" THEN
              STOP RUN  
           END-IF.

           IF MSG-VIEW-CHOICE = "c" OR "C"
             PERFORM 0151-COMMENT-SCREEN
           END-IF 
           .
           
           PERFORM 0140-MESSAGE-VIEW. 

       0150-MESSAGE-WRITE.
           PERFORM 0200-TIME-AND-DATE.
           PERFORM 0250-CREDIT-TOTAL.
           INITIALIZE WS-TITLE.
           INITIALIZE LS-PART-1.
           INITIALIZE LS-PART-2.
           INITIALIZE LS-PART-3.
           INITIALIZE LS-PART-4.
           INITIALIZE LS-PART-5.
           INITIALIZE MSG-WRITE-CHOICE.
           DISPLAY WRITE-MSG-SCREEN.
           
           ACCEPT WS-TITLE-FIELD.
           ACCEPT LINE-1-FIELD.
           ACCEPT LINE-2-FIELD.
           ACCEPT LINE-3-FIELD.
           ACCEPT LINE-4-FIELD.
           ACCEPT LINE-5-FIELD.
           ACCEPT MSG-WRITE-CHOICE-FIELD.

           PERFORM UNTIL MSG-WRITE-CHOICE-FIELD = "d" OR "D" OR "s"
             OR "S"

             ACCEPT MSG-WRITE-CHOICE-FIELD

           END-PERFORM.

           IF MSG-WRITE-CHOICE-FIELD = "d" OR "D" THEN
               PERFORM 0130-MSG-MENU
           END-IF.

           IF MSG-WRITE-CHOICE-FIELD = "s" OR "S" THEN 
              MOVE WS-CONTENT-DISPLAY TO WS-CONTENT
              MOVE WS-USERNAME TO WS-MSG-AUTHOR

                IF WS-TITLE-FIELD NOT = SPACE AND LOW-VALUE THEN
                  CALL "post-message" USING NEW-MESSAGE
                  PERFORM 0130-MSG-MENU
                END-IF    
           END-IF.

           PERFORM 0120-DISPLAY-MENU.

       0151-COMMENT-SCREEN.
           PERFORM 0280-CURRENT-TIME.
           PERFORM 0250-CREDIT-TOTAL.
           CALL "num-comments" USING NUM-COMMENTS.
           CALL "get-comment" USING COMMENT-TABLE MSG-SELECT.

           IF COM-INDEX < 5
             MOVE 1 TO COM-INDEX
           END-IF
           .
           
           INITIALIZE COM-SCRN-CHOICE.
           DISPLAY COMMENT-SCREEN.
           ACCEPT COM-SCRN-CHOICE-FIELD.
       
           IF COM-SCRN-CHOICE-FIELD = "n" OR "N" THEN
             ADD 5 TO COM-INDEX
             IF COM-COMMENT(COM-INDEX) = SPACES
               SUBTRACT 5 FROM COM-INDEX
               PERFORM 0151-COMMENT-SCREEN
             ELSE
               PERFORM 0151-COMMENT-SCREEN
             END-IF
           END-IF
           .

           IF COM-SCRN-CHOICE-FIELD = "p" OR "P" THEN
             SUBTRACT 5 FROM COM-INDEX
             PERFORM 0151-COMMENT-SCREEN
           END-IF
           .

           IF COM-SCRN-CHOICE-FIELD = "g" OR "G" THEN
             PERFORM 0140-MESSAGE-VIEW
           ELSE IF COM-SCRN-CHOICE-FIELD = "q" OR "Q" THEN
             STOP RUN
           END-IF
           .

           PERFORM 0151-COMMENT-SCREEN.
      ******************************************************************
      ***************-----FUN AND GAMES SECTION----*********************
      ******************************************************************
       0160-GAMES-MENU.
           PERFORM 0200-TIME-AND-DATE.
           PERFORM 0250-CREDIT-TOTAL.
           INITIALIZE GAMES-MENU-CHOICE.
           DISPLAY GAMES-MENU-SCREEN.
           ACCEPT GAMES-MENU-CHOICE-FIELD
           IF GAMES-MENU-CHOICE =      "q" or "Q" THEN
               STOP RUN
           ELSE IF GAMES-MENU-CHOICE = "g" or "G" THEN
               PERFORM 0120-DISPLAY-MENU   
           ELSE IF GAMES-MENU-CHOICE = "w" or "W" THEN
               PERFORM 0210-DISPLAY-GUESSING-GAME 
           END-IF.

           PERFORM 0160-GAMES-MENU.
      ******************************************************************
      ******************-----TIME/DATE SECTION----**********************
      ******************************************************************
       0200-TIME-AND-DATE.
           MOVE FUNCTION CURRENT-DATE TO WS-DATETIME. 
           MOVE WS-DATETIME(1:4)  TO WS-FORMATTED-YEAR.
           MOVE WS-DATETIME(5:2)  TO WS-FORMATTED-MONTH.
           MOVE WS-DATETIME(7:2)  TO WS-FORMATTED-DY.
           MOVE WS-DATETIME(9:2)  TO WS-FORMATTED-HOUR.
           MOVE WS-DATETIME(11:2) TO WS-FORMATTED-MINS.
           MOVE WS-DATETIME(13:2) TO WS-FORMATTED-SEC.
           MOVE WS-DATETIME(15:2) TO WS-FORMATTED-MS.
      ******************************************************************
      ****************----WORD GUESSING GAME SECTION----****************
      ******************************************************************
       0210-DISPLAY-GUESSING-GAME.
           PERFORM 0200-TIME-AND-DATE.
           PERFORM 0250-CREDIT-TOTAL.
           MOVE 15 TO WS-GUESSES-LEFT.
           SET WORD-IDX TO 0.
           OPEN INPUT F-WORD-FILE.
           MOVE 0 TO WS-FILE-IS-ENDED.
           PERFORM UNTIL WS-FILE-IS-ENDED = 1
               READ F-WORD-FILE
                   NOT AT END
                       ADD 1 TO WORD-IDX
                       MOVE WORD TO WS-GUESSING-WORDS-WORD(WORD-IDX)
                   AT END
                       MOVE 1 TO WS-FILE-IS-ENDED
               END-READ
           END-PERFORM.
           CLOSE F-WORD-FILE.
           MOVE FUNCTION CURRENT-DATE(14:3) TO RANDOMNUMBER.
           MOVE WS-GUESSING-WORDS-WORD(RANDOMNUMBER) TO WS-WORD.
           MOVE WS-WORD TO WS-ANSWERWORD.
           MOVE REPLACE-LETTER(WS-WORD) TO WS-WORD. 
           DISPLAY WORD-GUESSING-SCREEN.
           MOVE 1 TO COUNTER.
           PERFORM UNTIL COUNTER = 20
             IF "*" EQUALS WS-WORD(COUNTER:1) 
              THEN ADD 1 TO WS-WORD-LENGTH
             END-IF
             ADD 1 TO COUNTER
           END-PERFORM.
           PERFORM 0220-IN-GAME-SCREEN.
          
       0220-IN-GAME-SCREEN.
           PERFORM 0200-TIME-AND-DATE.
           PERFORM 0250-CREDIT-TOTAL.
           INITIALIZE WS-GUESS-CHOICE.
           DISPLAY IN-GAME-SCREEN.
           ACCEPT WS-GUESS-CHOICE-FIELD.
           IF WS-GUESS-CHOICE = "!" THEN 
               PERFORM 0120-DISPLAY-MENU
           ELSE
               PERFORM 0230-CHECK-GUESS
           END-IF.
           
       0230-CHECK-GUESS.
           MOVE 1 TO COUNTER.
           PERFORM UNTIL COUNTER = 20
                 IF WS-GUESS-CHOICE = WS-ANSWERWORD(COUNTER:1) THEN
                   MOVE WS-GUESS-CHOICE TO WS-WORD(COUNTER:1) 
                 END-IF
                 ADD 1 TO COUNTER     
           END-PERFORM.
           SUBTRACT 1 FROM WS-GUESSES-LEFT.
           MOVE 1 TO COUNTER.
           MOVE 0 TO WS-LETTERS-LEFT.
           PERFORM UNTIL COUNTER = 20
             IF "*" EQUALS WS-WORD(COUNTER:1) 
              THEN ADD 1 TO WS-LETTERS-LEFT
             END-IF
             ADD 1 TO COUNTER
           END-PERFORM.
             IF WS-LETTERS-LEFT = 0
              THEN 
              PERFORM 0240-WINNING-SCREEN
             ELSE IF WS-GUESSES-LEFT = 0
              THEN 
              PERFORM 0250-LOSING-SCREEN
             ELSE
              PERFORM 0220-IN-GAME-SCREEN
             END-IF.
           
       0240-WINNING-SCREEN.
           PERFORM 0200-TIME-AND-DATE.
           PERFORM 0250-CREDIT-TOTAL.
           INITIALIZE WS-GUESSING-WINNING-CHOICE.
           COMPUTE WS-HIGH-SCORE = WS-GUESSES-LEFT * WS-GUESSES-LEFT + 5
           .
           DISPLAY WORD-GUESSING-WINNING-SCREEN.
           OPEN EXTEND F-HIGH-SCORES-FILE
               MOVE WS-HIGH-SCORE TO HIGH-SCORE
               MOVE WS-USERNAME TO PLAYER-NAME
               WRITE PLAYER-SCORES 
               END-WRITE.
           CLOSE F-HIGH-SCORES-FILE.
           ACCEPT WS-GUESSING-WINNING-CHOICE-FIELD.
           IF WS-GUESSING-WINNING-CHOICE = "p" OR "P"
               THEN PERFORM 0210-DISPLAY-GUESSING-GAME
           ELSE IF WS-GUESSING-WINNING-CHOICE = "h" OR "H"
             THEN PERFORM 0260-HIGH-SCORE-TABLE
           ELSE IF WS-GUESSING-WINNING-CHOICE = "!" OR "q" OR "Q"
             THEN PERFORM 0120-DISPLAY-MENU
           ELSE
             PERFORM 0240-WINNING-SCREEN
           END-IF.

       0250-LOSING-SCREEN.
           PERFORM 0280-CURRENT-TIME.
           PERFORM 0250-CREDIT-TOTAL.
           INITIALIZE WS-GUESSING-LOSING-CHOICE.
           DISPLAY WORD-GUESSING-LOSE-SCREEN.
           ACCEPT WS-GUESSING-LOSING-CHOICE-FIELD.
           IF WS-GUESSING-LOSING-CHOICE = "p" OR "P"
               THEN PERFORM 0210-DISPLAY-GUESSING-GAME
           ELSE IF WS-GUESSING-LOSING-CHOICE = "h" OR "H"
             THEN PERFORM 0260-HIGH-SCORE-TABLE
           ELSE IF WS-GUESSING-LOSING-CHOICE = "!" OR "q" OR "Q"
             THEN PERFORM 0120-DISPLAY-MENU
           ELSE
             PERFORM 0250-LOSING-SCREEN
           END-IF.

       0260-HIGH-SCORE-TABLE.
           SET COUNTER TO 0.
           OPEN INPUT F-HIGH-SCORES-FILE.
           MOVE 0 TO WS-FILE-IS-ENDED.
           PERFORM UNTIL WS-FILE-IS-ENDED = 1
               READ F-HIGH-SCORES-FILE
                   NOT AT END
                       ADD 1 TO COUNTER
                       MOVE HIGH-SCORE TO WS-SCORE(COUNTER)
                       MOVE PLAYER-NAME TO WS-NAME(COUNTER)
                   AT END 
                       MOVE 1 TO WS-FILE-IS-ENDED
               END-READ 
           END-PERFORM.
           CLOSE F-HIGH-SCORES-FILE.
           PERFORM 0270-HIGH-SCORE-SCREEN.
           
       0270-HIGH-SCORE-SCREEN.
           PERFORM 0280-CURRENT-TIME.
           PERFORM 0250-CREDIT-TOTAL.
           INITIALIZE WS-HIGH-SCORE-CHOICE.
           SORT WS-TABLE-HIGH-SCORE ON DESCENDING WS-SCORE.
           DISPLAY HIGH-SCORE-SCREEN.
           ACCEPT WS-HIGH-SCORE-CHOICE-FIELD.
           IF WS-HIGH-SCORE-CHOICE = "g" OR "G"
             PERFORM 0120-DISPLAY-MENU
           ELSE IF WS-HIGH-SCORE-CHOICE = "q" OR "Q"
             STOP RUN
           ELSE 
               PERFORM 0270-HIGH-SCORE-SCREEN
           END-IF.

       0280-CURRENT-TIME.
           MOVE FUNCTION CURRENT-DATE TO WS-TIME.
      ******************************************************************
      **************-----COMMENTS ON MESSAGES SECTION-----**************
      ******************************************************************
       
       0250-CREDIT-TOTAL.
           CALL 'find-credits' USING WS-USERNAME, WS-USERCREDITS.

      ******************************************************************
      ******************----TRANSACTION LOG SECTION----*****************
      ******************************************************************
       0300-TRANSACTIONS.
           PERFORM 0200-TIME-AND-DATE.
           PERFORM 0250-CREDIT-TOTAL.
           CALL 'transactions' USING WS-FORMATTED-DT, WS-USERNAME,
           WS-STORE-CHARGE.
           PERFORM 0122-USER-ACCOUNT-MENU.
           
       