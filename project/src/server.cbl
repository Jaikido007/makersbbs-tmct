       IDENTIFICATION DIVISION.
       PROGRAM-ID. server.
       ENVIRONMENT DIVISION.
           CONFIGURATION SECTION.
           REPOSITORY.
               FUNCTION REPLACE-LETTER.
               
           INPUT-OUTPUT SECTION.
           FILE-CONTROL.
        *>    WORD GUSSING GAME FILE CONTROL
           SELECT F-WORD-FILE ASSIGN TO "guessing-words.dat"
             ORGANIZATION IS LINE SEQUENTIAL.
           SELECT F-HIGH-SCORES-FILE ASSIGN TO "high-scores.dat"
             ORGANIZATION IS LINE SEQUENTIAL.
        *>   TIC-TAC-TOE FILE CONTROL   
           SELECT FD-WINMASKS ASSIGN TO "PLACEMENT.DAT"
             ORGANIZATION IS LINE SEQUENTIAL.
           
       DATA DIVISION.
           FILE SECTION.
        *>    WORD GUESSING GAME SECTION
           FD F-WORD-FILE.
           01 WORD                             PIC X(20).
           FD F-HIGH-SCORES-FILE.
           01 PLAYER-SCORES.
              05 HIGH-SCORE                    PIC 99.
              05 PLAYER-NAME                   PIC X(10).
        *>  TIC-TAC-TOE F-Section
           FD FD-WINMASKS.
           01 FD-WINMASK                       PIC X(9).
                      
           WORKING-STORAGE SECTION.
      ******************************************************************
      *******-----VARIABLES RELATED TO LOGIN & MENU SCREEN-----*********
      ******************************************************************
           01 WS-USERNAME                      PIC X(16).
           01 WS-PASSWORD                      PIC X(20).
           01 WS-NEW-USER-NAME                 PIC X(16).
           01 WS-NEW-PASSWORD                  PIC X(20).
           01 LOGIN-CHOICE                     PIC X.
           01 MENU-CHOICE                      PIC X.
           01 ERROR-CHOICE                     PIC X.
           01 CREATE-CHOICE                    PIC X.
           01 ACCOUNT-CHOICE                   PIC X.
           01 WS-LOGIN-CORRECT                 PIC 9.
           01 WS-ERROR-MSG                     PIC X(40).
           01 WS-UNAME-UNAVAILABLE             PIC 9.
           01 WS-USERCREDITS                   PIC 9(3).
           01 WS-USERACCOUNTLEVEL              PIC X(3).
           01 WS-UPDATE-PASSWORD               PIC X(20).
           01 UPDATE-PASSWORD-CHOICE           PIC X.
      ******************************************************************
      ***********-----VARIABLES RELATED TO BANK ACCOUNTS-----***********
      ******************************************************************
           01 BANK-ACCOUNT-CHOICE              PIC X.
           01 CARD-NO                          PIC 9(16).
           01 CARD-EXPIRY                      PIC 9(4).
           01 CARD-CVV                         PIC 9(3).
           01 WS-CARD-NO                       PIC 9(16).
           01 WS-CARD-EXPIRY                   PIC 9(4).
           01 WS-CARD-CVV                      PIC 9(3).
           01 WS-ON-FILE                       PIC X.
           01 WS-BNK-DTLS-PRESENT              PIC X(3).
           01 WS-CARD-EXP                      PIC 9(4).
      ******************************************************************
      **************----VARIABLES RELATING TO CREDIT STORE----**********
      ******************************************************************
           01 CREDIT-STORE-CHOICE              PIC X.
           01 WS-UPDATE-CREDITS                PIC 9(3). 
           01 WS-STORE-CHARGE                  PIC 99.
           01 WS-BALANCE-AVAILABLE             PIC X.
           01 CREDIT-ERROR-CHOICE              PIC X. 
      ******************************************************************
      ********-----VARIABLES RELATED TO WORD GUESSING GAME-----*********
      ******************************************************************
           01 WS-ANSWERWORD                    PIC X(10).
           01 RANDOMNUMBER                     PIC 99.
           01 WS-WORD                          PIC X(10).
           01 WS-GUESSING-CHOICE-WORDS.
             05 WS-GUESSING-CHOICE-WORD        OCCURS 213 TIMES
             DESCENDING KEY IS WS-GUESSING-WORDS-WORD
             INDEXED BY WORD-IDX.
               10 WS-GUESSING-WORDS-WORD       PIC X(20).
           01 WS-GUESS-CHOICE                  PIC X.
           01 WS-GTW-COL                       PIC 9 VALUE 4.
      *********-----VARIABLES RELATED TO HIGH SCORE SCREEN----**********
           01 WS-HIGH-SCORE-CHOICE             PIC X.
           01 WS-HIGH-SCORE                    PIC 99.
           01 WS-HIGH-SCORES.  
              05 WS-TABLE-HIGH-SCORE           OCCURS 100 TIMES     
              ASCENDING KEY IS WS-SCORE
              INDEXED BY SCORE-IDX.
                  10 WS-SCORE                  PIC 99.
                  10 WS-NAME                   PIC X(10).
      ********-----VARIABLES RELATED TO CHECKING GUESSES-----***********
           01 WS-LETTERS-LEFT                  PIC 99.
           01 WS-GUESSES-LEFT                  PIC 99.          
      **********-----VARIABLES RELATED TO WINNING & LOSING-----*********
           01 WS-GUESSING-LOSING-CHOICE        PIC X.
           01 WS-GUESSING-WINNING-CHOICE       PIC X.
           01 WS-WORD-LENGTH                   PIC 99.
      ******************************************************************
           01 COUNTER                          UNSIGNED-INT.
           01 OFFSET                           UNSIGNED-INT.
           
           01 WS-FILE-IS-ENDED                 PIC 9 VALUE ZERO.
           01 MSG-MENU-CHOICE                  PIC XXX.
           01 GAMES-MENU-CHOICE                PIC X.
           01 NUM-OF-MESSAGES                   PIC 999.
           01 ID-NUM                           PIC 999 VALUE 1.
           01 WS-DATETIME                      PIC X(21).
           01 WS-FORMATTED-DT.
             05 WS-FORMATTED-DTE-TME.
               15 WS-FORMATTED-YEAR            PIC  X(4). 
               15 FILLER                       PIC X VALUE "-".
               15 WS-FORMATTED-MONTH           PIC  X(2).
               15 FILLER                       PIC X VALUE "-".
               15 WS-FORMATTED-DY              PIC  X(2).
               15 FILLER                       PIC X VALUE "-".
               15 WS-FORMATTED-HOUR            PIC  X(2).
               15 FILLER                       PIC X VALUE ":".
               15 WS-FORMATTED-MINS            PIC  X(2).
               15 FILLER                       PIC X VALUE ":".
               15 WS-FORMATTED-SEC             PIC  X(2).
               15 FILLER                       PIC X VALUE ":".
               15 WS-FORMATTED-MS              PIC  X(2).
                   
           01 WS-LIST-TABLE.
             05 WS-LIST-ENTRY                  OCCURS 10 TO 999 TIMES 
             DEPENDING ON NUM-OF-MESSAGES.
               10 LIST-ID                      PIC XXX.
               10 LIST-TITLE                   PIC X(50).
               10 LIST-CONTENT                 PIC X(300).
               10 LIST-USERNAME                PIC X(16).
               10 LIST-DATE                    PIC X(10).
           01 WS-CONTENT-DISPLAY.
             05 LS-PART-1                    PIC X(60).
             05 LS-PART-2                    PIC X(60).
             05 LS-PART-3                    PIC X(60).
             05 LS-PART-4                    PIC X(60).
             05 LS-PART-5                    PIC X(60).
           
           01 MSG-SELECT                       PIC 999.
           01 MSG-VIEW-CHOICE                  PIC X.
           01 MSG-WRITE-CHOICE                 PIC X.
           
           01 NEW-MESSAGE.
             05 WS-TITLE                       PIC X(50).
             05 WS-CONTENT                     PIC X(300).
             05 WS-MSG-AUTHOR                  PIC X(16).
             05 WS-POST-DATE                   PIC X(10).
      ******************************************************************
      ******************-----SPONSORED POSTS VARIABLES******************
      ******************************************************************
           01 SPONSORED-POSTS-TABLE.
             05 SP-ENTRY                       OCCURS 2 TIMES
             ASCENDING KEY IS SP-TITLE
             INDEXED BY MSG-IDX.
               10 SP-TITLE                     PIC X(50).
               10 SP-CONTENT                   PIC X(300).
               10 SP-USERNAME                  PIC X(16).
           01 WS-SP-TABLE-COUNTER              PIC 9.
           01 WS-SP-CONTENT-DISPLAY.
               05 SP-PART-1                    PIC X(60).
               05 SP-PART-2                    PIC X(60).
               05 SP-PART-3                    PIC X(60).
               05 SP-PART-4                    PIC X(60).
               05 SP-PART-5                    PIC X(60).
           01 SP-MSG-VIEW-CHOICE               PIC X. 
           01 SP-MSG-SELECT                    PIC 999.
           01 SP-MSG-MENU-CHOICE               PIC XXX.
           01 WS-SP-COUNTER                    PIC 9.
           01 SP-ERROR-CHOICE                  PIC X.
      ******************************************************************
      ******************-----TIC-TAC-TOE VARIABLES**********************
      ******************************************************************
           01 WS-PLAYER                        PIC A(1).
               88 HUMAN-PLAYER         VALUE "X".
               88 COMPUTER-PLAYER      VALUE "O".
           01 WS-STATE                         PIC A(5).
               88 GAME-OVER            VALUES "WIN", "LOSE", "STALE".
           01 WS-MOVE-OUTCOME                  PIC A(5).
               88 MOVE-COMPLETE        VALUES "WIN", "LOSE", "FAIL".
           01 WS-MASK-DETECTED                 PIC 9.
               88 WIN-DETECTED         VALUES 3, 4, 5, 6, 7, 8, 9.
           01 WS-COMPUTER-MOVED                PIC 9.
               88 COMPUTER-MOVED       VALUE 1.
           01 WS-EOF                           PIC 9.
               88 EOF                  VALUE 1.
           01 WS-SWAP-PLAYERS                  PIC 9.
               88 SWAP-PLAYERS         VALUE 1.
           01 WS-NEXT-MOVE                     PIC XX.
               88 FINISHED-PLAYING     VALUES "N", "n".
           01 WS-GAME-GRID.
             05 WS-GAME-GRID-ROW               OCCURS 3 TIMES.
               10 WS-GAME-GRID-COL             OCCURS 3 TIMES.
                   15 WS-CELL                  PIC X.
           01 WS-COLOR-GREEN                   PIC 9 VALUE 2.
           01 WS-COLOR-BLUE                    PIC 9 VALUE 1.
           01 WS-COLOR-WHITE                   PIC 9 VALUE 7.
           01 WS-COLOR-CYAN                    PIC 9 VALUE 3.
           01 WS-COLOR-RED                     PIC 9 VALUE 4.
           01 WS-BG-COLOR                      PIC 9 VALUE 1.
           01 WS-FG-COLOR                      PIC 9 VALUE 1.
           01 WS-FG-CELL                       PIC 9.
           01 WS-FG                            PIC 9.
           01 WS-BG                            PIC 9.
           01 WS-COL                           PIC 9.
           01 WS-ROW                           PIC 9.
           01 WS-WINS                          PIC 99.
           01 WS-MOVES                         PIC 99.
           01 WS-GAMES                         PIC 99.
           01 WS-COMPUTER-MOVE                 PIC 9.
           01 WS-DETECT-LOOP-COUNT             PIC 9.
           01 WS-OANDXMESSAGE                  PIC X(28).
           01 WS-INSTRUCTION                   PIC X(16).
           01 WS-FLAT-GAME-GRID                PIC X(9).
      ******************************************************************
      ****************----NUMBER GUESSING GAME VARIABLES*----****************
      ******************************************************************
           01 SEED                             PIC 9(8).
           01 GUESS-INPUT                      PIC XX.
           01 GUESS                            PIC 99.
           01 ANSWER                           PIC 99.
           01 TOTAL-GUESSES                    PIC 99.
           01 WS-RANDOM-NUM-MSG                PIC X(34).
           01 WS-GTN-BG-COLOR                  PIC 9.
           01 WS-GTN-FG-COLOR                  PIC 9 VALUE 7. 
      ******************************************************************
      ******************-----COMMENT SYSTEM VARIABLES-----**************
      ******************************************************************
           01 NUM-COMMENTS                     PIC 9(4).
           01 COMMENT-TABLE.
             05 COM-ENTRY                      OCCURS 1 TO 9999 TIMES 
             DEPENDING ON NUM-COMMENTS.
        *>     10 TEMP-ID PIC 999.
               10 COM-AUTHOR                   PIC X(16).
               10 COM-DATE                     PIC X(10).
               10 COM-COMMENT                  PIC X(50).
           01 COM-INDEX                        PIC 9(4) VALUE 1.
           01 COM-SCRN-CHOICE                  PIC X.
      ******************************************************************
      ****************-----COMMENT WRITING VARIABLES-----***************
      ******************************************************************           
           01 POST-COMMENT-CHOICE              PIC X.
           
           01 POST-COM-TBL.
             05 POST-COMMENT-AUTHOR            PIC X(16).
             05 POST-COMMENT-DATE              PIC X(10).
             05 WRITE-COMMENT                  PIC X(50).
      ******************************************************************
      **************-----COMMENT COUNTING VARIABLES-----****************
      ******************************************************************
           01 COMMENT-TOTAL-TABLE.
             05 COM-TOTAL-ENTRY                OCCURS 1 TO 999 TIMES 
             DEPENDING ON NUM-OF-MESSAGES.
               10 SUM-COMMENTS                 PIC Z(4).
   
      ******************************************************************
      ***********************-----TIME VARIABLES----********************
      ******************************************************************
           01 WS-TIME.
             05 WS-YEAR                        PIC X(4).
             05 WS-MONTH                       PIC XX.
             05 WS-DAY                         PIC XX.
             05 WS-HOURS-MINS.
               10 WS-HOURS                     PIC XX.
               10 WS-MINS                      PIC XX.             
      ******************************************************************
           LINKAGE SECTION.
           01 LS-COUNTER                       UNSIGNED-INT.
           01 LS-NUM                           UNSIGNED-INT.
           01 LS-MESSAGE                       PIC X(60). 
      ****************************************************************** 
           SCREEN SECTION.
           01 LOGIN-SCREEN
               BACKGROUND-COLOR IS 1.
             05 BLANK SCREEN.
        *>    LOGIN HEADER
             05 LINE 1 COL 1  VALUE "   :                              
      -    "                                                         "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 1 COL 2 PIC X(2) USING WS-FORMATTED-HOUR 
             FOREGROUND-COLOR IS 7 REVERSE-VIDEO.
             05 LINE 1 COL 5 PIC X(2) USING WS-FORMATTED-MINS
             FOREGROUND-COLOR IS 7 REVERSE-VIDEO.
        *>    LOGIN FOOTER
             05 LINE 43 COL 1 VALUE "                                 
      -    "                                                          "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 44 COL 1 VALUE "     (L) Log-in     (C) Create an a
      -    "ccount                                                    "                                 
              FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 45 COL 1 VALUE "     (Q) Quit                                 
      -    "                                                          "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 46 COL 1 VALUE "                                 
      -    "                                                          "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
        *>    FRIENDFACE LOGO ASCII ART
             05 LINE 14 COL 34 VALUE " ________________________"
             FOREGROUND-COLOR IS 7.
             05 LINE 15 COL 35 VALUE "|FfFfFfFfFfFfFfFfFfFfFf|"
             FOREGROUND-COLOR IS 7.
             05 LINE 16 COL 35 VALUE "|FfFfFfFfFfFfF_____FfFf|"
             FOREGROUND-COLOR IS 7.
             05 LINE 17 COL 35 VALUE "|FfFfFfFfFfFf__FfFfFfFf|"
             FOREGROUND-COLOR IS 7.
             05 LINE 18 COL 35 VALUE "|FfFfFfFfFfFf__FfFfFfFf|"
             FOREGROUND-COLOR IS 7.
             05 LINE 19 COL 35 VALUE "|FfFfFfFfFfFf__FfFfFfFf|"
             FOREGROUND-COLOR IS 7.
             05 LINE 20 COL 35 VALUE "|FfFfFfFfF________FfFfF|"
             FOREGROUND-COLOR IS 7.
             05 LINE 21 COL 35 VALUE "|FfFfFfFfFfFf__FfFfFfFf|"
             FOREGROUND-COLOR IS 7.
             05 LINE 22 COL 35 VALUE "|FfFfFfFfFfFf__FfFfFfFf|"
             FOREGROUND-COLOR IS 7.
             05 LINE 23 COL 35 VALUE "|FfFfFfFfFfFf__FfFfFfFf|"
             FOREGROUND-COLOR IS 7.
             05 LINE 24 COL 35 VALUE "|FfFfFfFfFfFf__FfFfFfFf|"
             FOREGROUND-COLOR IS 7.
             05 LINE 25 COL 35 VALUE "|FfFfFfFfFfFfFfFfFfFfFf|"
             FOREGROUND-COLOR IS 7.
             05 LINE 26 COL 34 VALUE " ------------------------"
             FOREGROUND-COLOR IS 7.
        *>    FRIENDFACE TEXT ASCII ART  
             05 LINE 31 COL 25 VALUE "______    _                _______
      -    "" FOREGROUND-COLOR IS 7.
             05 LINE 32 COL 25 VALUE "|  ___|  (_)              | |  ___
      -      "| " FOREGROUND-COLOR IS 7.
             05 LINE 33 COL 25 VALUE "| |_ _ __ _  ___ _ __   __| | |_ _
      -      "_ _  ___ ___" FOREGROUND-COLOR IS 7.
             05 LINE 34 COL 25 VALUE "|  _| '__| |/ _ \ '_ \ / _` |  _/
      -      "_` |/ __/ _ \" FOREGROUND-COLOR IS 7.
             05 LINE 35 COL 25 VALUE "| | | |  | |  __/ | | | (_| | || (
      -      "_| | (_|  __/" FOREGROUND-COLOR IS 7.
             05 LINE 36 COL 25 VALUE "\_| |_|  |_|\___|_| |_|\__,_\_| \_
      -      "_,_|\___\___|" FOREGROUND-COLOR IS 7.
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
      -    " as a VIP member to unlock all its features!               "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 46 COL 1 VALUE "                                 
      -    "                                                           "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.     
        *>    FRIENDFACE LOGO ASCII ART
               05 LINE 14 COL 34 VALUE " ________________________"
                   FOREGROUND-COLOR IS 7.
               05 LINE 15 COL 35 VALUE "|FfFfFfFfFfFfFfFfFfFfFF|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 16 COL 35 VALUE "|FfFfFfFfFfFfF_____FfFf|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 17 COL 35 VALUE "|FfFfFfFfFfFf__FfFfFfFf|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 18 COL 35 VALUE "|FfFfFfFfFfFf__FfFfFfFf|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 19 COL 35 VALUE "|FfFfFfFfFfFf__FfFfFfFf|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 20 COL 35 VALUE "|FfFfFfFfF________FfFfF|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 21 COL 35 VALUE "|FfFfFfFfFfFf__FfFfFfFf|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 22 COL 35 VALUE "|FfFfFfFfFfFf__FfFfFfFf|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 23 COL 35 VALUE "|FfFfFfFfFfFf__FfFfFfFf|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 24 COL 35 VALUE "|FfFfFfFfFfFf__FfFfFfFf|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 25 COL 35 VALUE "|FfFfFfFfFfFfFfFfFfFfFF|"
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
      -    "                                                         "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 1 COL 2 PIC X(2) USING WS-FORMATTED-HOUR 
             FOREGROUND-COLOR IS 7 REVERSE-VIDEO.
             05 LINE 1 COL 5 PIC X(2) USING WS-FORMATTED-MINS
             FOREGROUND-COLOR IS 7 REVERSE-VIDEO.
           
        *>    ERROR FOOTER
               05 LINE 43 COL 1 VALUE "                                 
      -    "                                                           "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 44 COL 1 VALUE "     (L) Back to Log-in     (C) C
      -    "reate an account                                           "                                 
                FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 45 COL 1 VALUE "     (Q) Quit                                  
      -    "                                                           "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 46 COL 1 VALUE "                                 
      -    "                                                           "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
                05 LINE 30 COLUMN 32 PIC X(40) USING WS-ERROR-MSG.
        *>    FRIENDFACE LOGO ASCII ART
               05 LINE 14 COL 34 VALUE " ________________________"
                   FOREGROUND-COLOR IS 7.
               05 LINE 15 COL 35 VALUE "|FfFfFfFfFfFfFfFfFfFfFf|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 16 COL 35 VALUE "|FfFfFfFfFfFfF_____FfFf|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 17 COL 35 VALUE "|FfFfFfFfFfFf__FfFfFfFf|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 18 COL 35 VALUE "|FfFfFfFfFfFf__FfFfFfFf|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 19 COL 35 VALUE "|FfFfFfFfFfFf__FfFfFfFf|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 20 COL 35 VALUE "|FfFfFfFfF________FfFfF|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 21 COL 35 VALUE "|FfFfFfFfFfFf__FfFfFfFf|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 22 COL 35 VALUE "|FfFfFfFfFfFf__FfFfFfFf|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 23 COL 35 VALUE "|FfFfFfFfFfFf__FfFfFfFf|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 24 COL 35 VALUE "|FfFfFfFfFfFf__FfFfFfFf|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 25 COL 35 VALUE "|FfFfFfFfFfFfFfFfFfFfFf|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 26 COL 34 VALUE " ------------------------"
                   FOREGROUND-COLOR IS 7.
        *>    ERROR OPTION POSITIONING
               05 LINE 42 COLUMN 6 VALUE "Option: ".
               05 ERROR-CHOICE-FIELD LINE 42 COLUMN 14 PIC X
                  USING ERROR-CHOICE.

           01 COMMENT-ERROR-SCREEN
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
             05 LINE 1 COL 90 PIC X(3) USING WS-USERCREDITS
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
        *>    ERROR FOOTER
               05 LINE 43 COL 1 VALUE "                                 
      -    "                                                           "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 44 COL 1 VALUE "     (C) Credit store     (A) Acc
      -    "ount options                      "                                 
                FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 45 COL 1 VALUE "     (G) Back             (Q) Qui                                 
      -    "t                                                          "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 46 COL 1 VALUE "                                 
      -    "                                                           "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
                05 LINE 30 COLUMN 32 PIC X(40) USING WS-ERROR-MSG.
        *>    FRIENDFACE LOGO ASCII ART
               05 LINE 14 COL 34 VALUE " ________________________"
                   FOREGROUND-COLOR IS 7.
               05 LINE 15 COL 35 VALUE "|FfFfFfFfFfFfFfFfFfFfFf|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 16 COL 35 VALUE "|FfFfFfFfFfFfF_____FfFf|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 17 COL 35 VALUE "|FfFfFfFfFfFf__FfFfFfFf|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 18 COL 35 VALUE "|FfFfFfFfFfFf__FfFfFfFf|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 19 COL 35 VALUE "|FfFfFfFfFfFf__FfFfFfFf|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 20 COL 35 VALUE "|FfFfFfFfF________FfFfF|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 21 COL 35 VALUE "|FfFfFfFfFfFf__FfFfFfFf|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 22 COL 35 VALUE "|FfFfFfFfFfFf__FfFfFfFf|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 23 COL 35 VALUE "|FfFfFfFfFfFf__FfFfFfFf|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 24 COL 35 VALUE "|FfFfFfFfFfFf__FfFfFfFf|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 25 COL 35 VALUE "|FfFfFfFfFfFfFfFfFfFfFf|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 26 COL 34 VALUE " ------------------------"
                   FOREGROUND-COLOR IS 7.
        *>    ERROR OPTION POSITIONING
               05 LINE 42 COLUMN 6 VALUE "Option: ".
               05 C-ERROR-CHOICE-FIELD LINE 42 COLUMN 14 PIC X
                  USING ERROR-CHOICE.

           01 CREDIT-ERROR-SCREEN
             BACKGROUND-COLOR IS 1.
             05 BLANK SCREEN.
        *>    ERROR HEADER
             05 LINE 1 COL 1  VALUE "   :                              
      -    "                                                         "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 1 COL 2 PIC X(2) USING WS-FORMATTED-HOUR 
             FOREGROUND-COLOR IS 7 REVERSE-VIDEO.
             05 LINE 1 COL 5 PIC X(2) USING WS-FORMATTED-MINS
             FOREGROUND-COLOR IS 7 REVERSE-VIDEO.
             05 LINE 1 COL 80 VALUE "CREDITS: "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 1 COL 89 PIC X(3) USING WS-USERCREDITS
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
        *>    ERROR FOOTER
               05 LINE 43 COL 1 VALUE "                                 
      -    "                                                           "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 44 COL 1 VALUE "     (C) Credit Store     (A) Acc
      -    "ount details                                               "                                 
                FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 45 COL 1 VALUE "     (M) Main menu        (Q) Qui                                 
      -    "t                                                          "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 46 COL 1 VALUE "                                 
      -    "                                                           "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
                05 LINE 30 COLUMN 32 PIC X(40) USING WS-ERROR-MSG.
        *>    FRIENDFACE LOGO ASCII ART
               05 LINE 14 COL 34 VALUE " ________________________"
                   FOREGROUND-COLOR IS 7.
               05 LINE 15 COL 35 VALUE "|FfFfFfFfFfFfFfFfFfFfFf|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 16 COL 35 VALUE "|FfFfFfFfFfFfF_____FfFf|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 17 COL 35 VALUE "|FfFfFfFfFfFf__FfFfFfFf|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 18 COL 35 VALUE "|FfFfFfFfFfFf__FfFfFfFf|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 19 COL 35 VALUE "|FfFfFfFfFfFf__FfFfFfFf|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 20 COL 35 VALUE "|FfFfFfFfF________FfFfF|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 21 COL 35 VALUE "|FfFfFfFfFfFf__FfFfFfFf|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 22 COL 35 VALUE "|FfFfFfFfFfFf__FfFfFfFf|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 23 COL 35 VALUE "|FfFfFfFfFfFf__FfFfFfFf|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 24 COL 35 VALUE "|FfFfFfFfFfFf__FfFfFfFf|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 25 COL 35 VALUE "|FfFfFfFfFfFfFfFfFfFfFf|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 26 COL 34 VALUE " ------------------------"
                   FOREGROUND-COLOR IS 7.
        *>    ERROR OPTION POSITIONING
               05 LINE 42 COLUMN 6 VALUE "Option: ".
               05 CREDIT-ERROR-CHOICE-FIELD LINE 42 COLUMN 14 PIC X
                  USING CREDIT-ERROR-CHOICE. 

           01 SP-ERROR-SCREEN
             BACKGROUND-COLOR IS 1.
             05 BLANK SCREEN.
        *>    ERROR HEADER
             05 LINE 1 COL 1  VALUE "   :                              
      -    "                                                         "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 1 COL 2 PIC X(2) USING WS-FORMATTED-HOUR 
             FOREGROUND-COLOR IS 7 REVERSE-VIDEO.
             05 LINE 1 COL 5 PIC X(2) USING WS-FORMATTED-MINS
             FOREGROUND-COLOR IS 7 REVERSE-VIDEO.
             05 LINE 1 COL 89 PIC X(3) USING WS-USERCREDITS
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
        *>    ERROR FOOTER
               05 LINE 43 COL 1 VALUE "                                 
      -    "                                                           "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 44 COL 1 VALUE "     (G) Go Back
      -    "                                                           "                                 
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
               05 LINE 15 COL 35 VALUE "|FfFfFfFfFfFfFfFfFfFfFf|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 16 COL 35 VALUE "|FfFfFfFfFfFfF_____FfFf|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 17 COL 35 VALUE "|FfFfFfFfFfFf__FfFfFfFf|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 18 COL 35 VALUE "|FfFfFfFfFfFf__FfFfFfFf|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 19 COL 35 VALUE "|FfFfFfFfFfFf__FfFfFfFf|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 20 COL 35 VALUE "|FfFfFfFfF________FfFfF|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 21 COL 35 VALUE "|FfFfFfFfFfFf__FfFfFfFf|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 22 COL 35 VALUE "|FfFfFfFfFfFf__FfFfFfFf|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 23 COL 35 VALUE "|FfFfFfFfFfFf__FfFfFfFf|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 24 COL 35 VALUE "|FfFfFfFfFfFf__FfFfFfFf|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 25 COL 35 VALUE "|FfFfFfFfFfFfFfFfFfFfFf|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 26 COL 34 VALUE " ------------------------"
                   FOREGROUND-COLOR IS 7.
        *>    ERROR OPTION POSITIONING
               05 LINE 42 COLUMN 6 VALUE "Option: ".
               05 SP-ERROR-CHOICE-FIELD LINE 42 COLUMN 14 PIC X
                  USING SP-ERROR-CHOICE. 

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
      -    "                                                           "                                 
                FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 45 COL 1 VALUE "     (G) Go back    (Q) Quit                                 
      -    "                                                           "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 46 COL 1 VALUE "                                 
      -    "                                                           "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO. 
        *>    FIRENDFACE LOGO ASCII ART
               05 LINE 14 COL 34 VALUE " ________________________"
                   FOREGROUND-COLOR IS 7.
               05 LINE 15 COL 35 VALUE "|FfFfFfFfFfFfFfFfFfFfFf|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 16 COL 35 VALUE "|FfFfFfFfFfFfF_____FfFf|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 17 COL 35 VALUE "|FfFfFfFfFfFf__FfFfFfFf|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 18 COL 35 VALUE "|FfFfFfFfFfFf__FfFfFfFf|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 19 COL 35 VALUE "|FfFfFfFfFfFf__FfFfFfFf|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 20 COL 35 VALUE "|FfFfFfFfF________FfFfF|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 21 COL 35 VALUE "|FfFfFfFfFfFf__FfFfFfFf|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 22 COL 35 VALUE "|FfFfFfFfFfFf__FfFfFfFf|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 23 COL 35 VALUE "|FfFfFfFfFfFf__FfFfFfFf|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 24 COL 35 VALUE "|FfFfFfFfFfFf__FfFfFfFf|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 25 COL 35 VALUE "|FfFfFfFfFfFfFfFfFfFfFf|"
                   FOREGROUND-COLOR IS 7.
               05 LINE 26 COL 34 VALUE " ------------------------"
                   FOREGROUND-COLOR IS 7.
             05 LINE 30 COLUMN 37 VALUE "Create your account".
             05 LINE 32 COLUMN 37 VALUE " Enter a username: ".
             05 WS-NEW-USER-NAME-FIELD LINE 34 COLUMN 37 PIC X(16)
                USING WS-NEW-USER-NAME.
             05 LINE 34 COLUMN 52 VALUE "____".
             05 LINE 36 COLUMN 37 VALUE " Enter a password: ".
             05 LINE 37 COLUMN 36 VALUE " (max 20 characters)".
             05 WS-NEW-PASSWORD-FIELD LINE 39 COLUMN 37 PIC X(20)
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
             05 LINE 1 COL 80 VALUE "CREDITS: "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 1 COL 89 PIC 9(3) USING WS-USERCREDITS
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
        *>    USER ACCOUNT FOOTER
               05 LINE 43 COL 1 VALUE "                                 
      -    "                                                           "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 44 COL 1 VALUE "     (B) Add a bank card      (P)
      -    " Change password                                           "                                   
                FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 45 COL 1 VALUE "     (G) Go back              (Q)                                 
      -    " Quit                                                      "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 46 COL 1 VALUE "                                 
      -    "                                                           "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO. 
        *>    USER ACCOUNT BODY
             05 LINE  4 COL 10 VALUE "FriendFace" UNDERLINE.

             05 LINE  9 COL 8 VALUE "                                ACC          
      -    "OUNT DETAILS                               "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 10 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 10 COL 10 VALUE "                            Userna                       
      -    "me:                                       "
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 10 COL 48 PIC X(16) USING WS-USERNAME
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 10 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.

             05 LINE 11 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 11 COL 10 VALUE "                                              
      -    "                                         "
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 11 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.

             05 LINE 12 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 12 COL 10 VALUE "                       Account Lev                   
      -    "el:                                       "
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 12 COL 48 PIC X(3) USING WS-USERACCOUNTLEVEL
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 12 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.

             05 LINE 13 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 13 COL 10 VALUE "                                              
      -    "                                         "
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 13 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.

             05 LINE 14 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 14 COL 10 VALUE "                   Available Credi               
      -    "ts:                                      "
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 14 COL 48 PIC 9(3) USING WS-USERCREDITS
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 14 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.

             05 LINE 15 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 15 COL 10 VALUE "                                   
      -    "                                          "
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 15 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.

             05 LINE 16 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.

             05 LINE 16 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 16 COL 10 VALUE "                   Bank card on fi               
      -    "le:                                      "
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 16 COL 48 PIC X(3) USING WS-BNK-DTLS-PRESENT
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 16 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.

             05 LINE 17 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 17 COL 10 VALUE "                                              
      -    "                                         "
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 17 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.

             05 LINE 18 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 18 COL 10 VALUE "                Your card expires             
      -    "on:                                      "
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 18 COL 48 PIC 9(4) USING WS-CARD-EXP
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 18 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.

             05 LINE 19 COL 8 VALUE "                                   
      -    "                                           "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.           
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
             05 LINE 1 COL 89 PIC 9(3) USING WS-USERCREDITS
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
        *>    BANK DETAILS FOOTER
             05 LINE 43 COL 1 VALUE "                                 
      -    "                                                         "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 44 COL 1 VALUE "     (S) Submit     (D) Discard  
      -    "                                                         "                                 
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 45 COL 1 VALUE "     (G) Go back    (Q) Quit                                 
      -    "                                                         "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 46 COL 1 VALUE "                                 
      -    "                                                         "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO. 
        *>    BANK DETAILS BODY
            05 LINE  4 COL 10 VALUE "FriendFace" UNDERLINE.
             05 LINE  9 COL 8 VALUE "                               ADD                  
      -    "A BANK CARD                                "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 10 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 10 COL 10 VALUE "                                  
      -    "                                         "
             FOREGROUND-COLOR IS 2, REVERSE-VIDEO.
             05 LINE 10 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 11 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 11 COL 10 VALUE "               Enter Card Number:
      -    " " FOREGROUND-COLOR IS 2, REVERSE-VIDEO.
             05 CARD-NO-FIELD LINE 11 COLUMN 44 PIC 9(16)
                USING CARD-NO   FOREGROUND-COLOR IS 2, REVERSE-VIDEO.
             05 LINE 11 COL 60 VALUE "                        "
             FOREGROUND-COLOR IS 2, REVERSE-VIDEO.
             05 LINE 11 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 12 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 12 COL 10 VALUE "                                  
      -    "                                         "
             FOREGROUND-COLOR IS 2, REVERSE-VIDEO.
             05 LINE 12 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 13 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 13 COL 10 VALUE "               Enter Expiry Date:
      -    " " FOREGROUND-COLOR IS 2, REVERSE-VIDEO.
             05 CARD-EXPIRY-FIELD LINE 13 COLUMN 44 PIC 9(4)
             USING CARD-EXPIRY FOREGROUND-COLOR IS 2, REVERSE-VIDEO.
             05 LINE 13 COL 48 VALUE "                                  
      -    "  " FOREGROUND-COLOR IS 2, REVERSE-VIDEO.
             05 LINE 13 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 14 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 14 COL 10 VALUE "               Enter CVV Number:  
      -    " " FOREGROUND-COLOR IS 2, REVERSE-VIDEO.
             05 CARD-CVV-FIELD LINE 14 COLUMN 44 PIC 9(3)
                USING CARD-CVV FOREGROUND-COLOR IS 2, REVERSE-VIDEO.
             05 LINE 14 COL 47 VALUE "                                  
      -    "   " FOREGROUND-COLOR IS 2, REVERSE-VIDEO.
             05 LINE 14 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 15 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 15 COL 10 VALUE  "                                  
      -    "                                         "
             FOREGROUND-COLOR IS 2, REVERSE-VIDEO.
             05 LINE 15 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 16 COL 8 VALUE "                                   
      -    "                                           "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
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
             05 LINE 1 COL 80 VALUE "CREDITS: "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 1 COL 89 PIC 9(3) USING WS-USERCREDITS
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
        *>    CREDIT STORE FOOTER
             05 LINE 43 COL 1 VALUE "                                 
      -    "                                                         "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 44 COL 1 VALUE "  (1) 10 Credits    (2) 25 Credits  
      -    "   (3) 50 Credits   (4) 100 CREDITS   (V) VIP Upgrade    "                                 
                FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 45 COL 1 VALUE "  (G) Go back       (Q) Quit                                 
      -    "                                                         "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 46 COL 1 VALUE "                                 
      -    "                                                         "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO. 
        *>    CREDIT STORE BODY
             05 LINE  4 COL 10 VALUE "FriendFace" UNDERLINE.
             05 LINE 6 COLUMN 35 VALUE "WELCOME TO THE CREDIT STORE".
             05 LINE  9 COL 8 VALUE "                                 
      -    "CREDITS                                    "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 10 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 10 COL 10 VALUE "                                   
      -    "                                          "
             FOREGROUND-COLOR IS 2, REVERSE-VIDEO.
             05 LINE 10 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 11 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 11 COL 10 VALUE "                            £10 :                  
      -    "  10 CREDITS                              "
             FOREGROUND-COLOR IS 2, REVERSE-VIDEO.
             05 LINE 11 COL 85 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 12 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 12 COL 10 VALUE "                            £20 :                  
      -    "  25 CREDITS                              "
             FOREGROUND-COLOR IS 2, REVERSE-VIDEO.
             05 LINE 12 COL 85 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 13 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 13 COL 10 VALUE "                            £35 :                  
      -    "  50 CREDITS                              "
             FOREGROUND-COLOR IS 2, REVERSE-VIDEO.
             05 LINE 13 COL 85 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 14 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 14 COL 10 VALUE "                            £60 :                  
      -    " 100 CREDITS                              "
             FOREGROUND-COLOR IS 2, REVERSE-VIDEO.
             05 LINE 14 COL 85 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 15 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 15 COL 10 VALUE "                                   
      -    "                                          "
             FOREGROUND-COLOR IS 2, REVERSE-VIDEO.
             05 LINE 15 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 16 COL 8 VALUE "                                   
      -    "                                           "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.

             05 LINE 20 COL 8 VALUE "                           LIFETIME     
      -    " VIP MEMBERSHIP                            "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 21 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 21 COL 10 VALUE "                                   
      -    "                                          "
             FOREGROUND-COLOR IS 6, REVERSE-VIDEO.
             05 LINE 21 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 22 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 22 COL 10 VALUE "                    100 CREDITS :                  
      -    "VIP UPGRADE                             "
             FOREGROUND-COLOR IS 6, REVERSE-VIDEO.
             05 LINE 22 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.

             05 LINE 23 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 23 COL 10 VALUE "                                              
      -    "                                         "
             FOREGROUND-COLOR IS 6, REVERSE-VIDEO.
             05 LINE 23 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.

             05 LINE 24 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 24 COL 10 VALUE " VIP's get: FREE message posts, FR  
      -    "EE comments & FREE play of Tic-Tac-Toe! "
             FOREGROUND-COLOR IS 6, REVERSE-VIDEO.
             05 LINE 24 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.

             05 LINE 25 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 25 COL 10 VALUE "                                              
      -    "                                         "
             FOREGROUND-COLOR IS 6, REVERSE-VIDEO.
             05 LINE 25 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.

             05 LINE 26 COL 8 VALUE "                                   
      -    "                                           "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
        *>    CREDIT STORE OPTION POSITIONING
               05 LINE 42 COLUMN 6 VALUE "Option: ".
             05 CREDIT-STORE-CHOICE-FIELD LINE 42 COLUMN 14 PIC X
                USING CREDIT-STORE-CHOICE.
                
           01 UPDATE-PASSWORD-SCREEN
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
             05 LINE 1 COL 89 PIC 9(3) USING WS-USERCREDITS
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
        *>    BANK DETAILS FOOTER
             05 LINE 43 COL 1 VALUE "                                 
      -    "                                                           "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 44 COL 1 VALUE "     (S) Submit     (D) Discard  
      -    "                                                           "                                 
                FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 45 COL 1 VALUE "   (G) Go back    (Q) Quit                                  
      -    "                                                           "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 46 COL 1 VALUE "                                 
      -    "                                                           "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO. 
           *>    UPDATE PASSWORD BODY
             05 LINE 6 COLUMN 10 VALUE "UPDATE PASSWORD".
             05 LINE 8 COLUMN 10 VALUE "ENTER NEW PASSWORD: ".
             05 WS-UPDATE-PASSWORD-FIELD LINE 10 COLUMN 10 PIC X(20)
                USING WS-UPDATE-PASSWORD.
          
        *>    UPDATE PASSWORD OPTION POSITIONING
               05 LINE 42 COLUMN 6 VALUE "Option: ".
             05 UPDATE-PASSWORD-CHOICE-FIELD LINE 42 COLUMN 14 PIC X
                USING UPDATE-PASSWORD-CHOICE.  

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
             05 LINE 1 COL 80 VALUE "CREDITS: "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 1 COL 89 PIC 9(3) USING WS-USERCREDITS
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
        *>    MENU FOOTER
             05 LINE 43 COL 1 VALUE "                                 
      -    "                                                         "
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
             05 LINE 4 COL 34 VALUE "WELCOME TO FRIENDFACE"                                     
             FOREGROUND-COLOR IS 7, UNDERLINE. 

             05 LINE  6 COL 40 VALUE "Hi, ".
             05 LINE  6 COL 44 PIC X(16) USING WS-USERNAME.
        *>    MESSSAGE MENU BOX
             05 LINE  9 COL  8 VALUE "  "               
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE  9 COL 10 VALUE "         MESSAGES           " 
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE  9 COL 38 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 10 COL  8 VALUE "  "               
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 10 COL 10 VALUE " READ THE BULLETIN BOARD    "
             FOREGROUND-COLOR IS 2, REVERSE-VIDEO. 
             05 LINE 10 COL 38 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 11 COL  8 VALUE "  "               
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 11 COL 10 VALUE " LEAVE A COMMENT            " 
             FOREGROUND-COLOR IS 2, REVERSE-VIDEO.
             05 LINE 11 COL 38 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 12 COL  8 VALUE "  "               
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 12 COL 10 VALUE " WRITE A MESSAGE            "  
             FOREGROUND-COLOR IS 2, REVERSE-VIDEO.
             05 LINE 12 COL 38 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 13 COL  8 VALUE "  "               
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 13 COL 10 VALUE " POST AN ADVERT             " 
             FOREGROUND-COLOR IS 2, REVERSE-VIDEO.
             05 LINE 13 COL 38 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 14 COL  8 VALUE "  "               
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 14 COL 10 VALUE "                            " 
             FOREGROUND-COLOR IS 2, REVERSE-VIDEO.
             05 LINE 14 COL 38 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 15 COL  8 VALUE "  "               
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 15 COL 10 VALUE "                            " 
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 15 COL 38 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
           
        *>    CREDIT MENU BOX
             05 LINE 21 COL  8 VALUE "  "               
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 21 COL 10 VALUE "       CREDIT STORE         " 
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 21 COL 38 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 22 COL  8 VALUE "  "               
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 22 COL 10 VALUE " BUY CREDITS                "
             FOREGROUND-COLOR IS 6, REVERSE-VIDEO. 
             05 LINE 22 COL 38 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 23 COL  8 VALUE "  "               
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 23 COL 10 VALUE " BECOME A VIP MEMBER         " 
             FOREGROUND-COLOR IS 6, REVERSE-VIDEO.
             05 LINE 23 COL 38 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 24 COL  8 VALUE "  "               
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 24 COL 10 VALUE "                            "  
             FOREGROUND-COLOR IS 6, REVERSE-VIDEO.
             05 LINE 24 COL 38 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 25 COL  8 VALUE "  "               
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 25 COL 10 VALUE "                            "
             FOREGROUND-COLOR IS 6, REVERSE-VIDEO.
             05 LINE 25 COL 38 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 26 COL  8 VALUE "  "               
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 26 COL 10 VALUE "                            " 
             FOREGROUND-COLOR IS 6, REVERSE-VIDEO.
             05 LINE 26 COL 38 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 27 COL  8 VALUE "  "               
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 27 COL 10 VALUE "                            " 
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 27 COL 38 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.

        *>    FUN & GAMES MENU BOX
             05 LINE  9 COL 50 VALUE "  "               
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE  9 COL 52 VALUE "        FUN & GAMES         " 
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE  9 COL 80 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 10 COL 50 VALUE "  "               
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 10 COL 52 VALUE " PLAY GAMES                 "
             FOREGROUND-COLOR IS 5, REVERSE-VIDEO. 
             05 LINE 10 COL 80 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 11 COL 50 VALUE "  "               
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 11 COL 52 VALUE " WIN CREDITS                " 
             FOREGROUND-COLOR IS 5, REVERSE-VIDEO.
             05 LINE 11 COL 80 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 12 COL 50 VALUE "  "               
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 12 COL 52 VALUE " TOP THE SCOREBOARD         "  
             FOREGROUND-COLOR IS 5, REVERSE-VIDEO.
             05 LINE 12 COL 80 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 13 COL 50 VALUE "  "               
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 13 COL 52 VALUE "                            "  
             FOREGROUND-COLOR IS 5, REVERSE-VIDEO.
             05 LINE 13 COL 80 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 14 COL 50 VALUE "  "               
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 14 COL 52 VALUE "                            "  
             FOREGROUND-COLOR IS 5, REVERSE-VIDEO.
             05 LINE 14 COL 80 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 15 COL 50 VALUE "  "               
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 15 COL 52 VALUE "                            " 
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 15 COL 80 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.

        *>    ACCOUNT MENU BOX
             05 LINE 21 COL 50 VALUE "  "               
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 21 COL 52 VALUE "      ACCOUNT DETAILS       " 
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 21 COL 80 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 22 COL 50 VALUE "  "               
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 22 COL 52 VALUE " CHANGE YOUR PASSWORD       "
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO. 
             05 LINE 22 COL 80 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 23 COL 50 VALUE "  "               
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 23 COL 52 VALUE " ADD A BANK CARD            " 
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 23 COL 80 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 24 COL 50 VALUE "  "               
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 24 COL 52 VALUE " UPDATE YOUR BANK CARD      "  
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 24 COL 80 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 25 COL 50 VALUE "  "               
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 25 COL 52 VALUE " VIEW AVAILABLE CREDITS      "
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 25 COL 80 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 26 COL 50 VALUE "  "               
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 26 COL 52 VALUE "                             " 
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 26 COL 80 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 27 COL 50 VALUE "  "               
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 27 COL 52 VALUE "                            " 
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 27 COL 80 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.

        *>    MENU OPTION POSITIONING
               05 LINE 42 COLUMN 6 VALUE "Option: ".
               05 MENU-CHOICE-FIELD LINE 42 COLUMN 14 PIC X
                  USING MENU-CHOICE.
           
           01 MSG-MENU-SCREEN
             BACKGROUND-COLOR IS 1.
             05 BLANK SCREEN.
        *>    MSG MENU HEADER
             05 LINE 1 COL 1  VALUE "   :                              
      -    "                                                         "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 1 COL 2 PIC X(2) USING WS-FORMATTED-HOUR 
             FOREGROUND-COLOR IS 7 REVERSE-VIDEO.
             05 LINE 1 COL 5 PIC X(2) USING WS-FORMATTED-MINS
             FOREGROUND-COLOR IS 7 REVERSE-VIDEO.
             05 LINE 1 COL 80 VALUE "CREDITS: "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 1 COL 89 PIC 9(3) USING WS-USERCREDITS
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
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
             05 LINE 4 COL 40 VALUE "BULLETIN BOARD"                                     
             FOREGROUND-COLOR IS 7, UNDERLINE.
             05 LINE  9 COL 8 VALUE "                               SPON                 
      -    "SORED MESSAGES                             "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.

             05 LINE 10 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 10 COL 10 VALUE "                                   
      -    "                                        "
             FOREGROUND-COLOR IS 6, REVERSE-VIDEO.
             05 LINE 10 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.

             05 LINE 11 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 11 COL 10 VALUE "S1                                   
      -    "                                      "
             FOREGROUND-COLOR IS 6, REVERSE-VIDEO.
             05 LINE 11 COL 14 PIC X(50) USING SP-TITLE(1) 
             FOREGROUND-COLOR IS 6, REVERSE-VIDEO.
             05 LINE 11 COL 64 VALUE "                    "
             FOREGROUND-COLOR IS 6, REVERSE-VIDEO.
             05 LINE 11 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.

             05 LINE 12 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 12 COL 10 VALUE "                                   
      -    "                                        "
             FOREGROUND-COLOR IS 6, REVERSE-VIDEO.
             05 LINE 12 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.

             05 LINE 13 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 13 COL 10 VALUE "S2                                   
      -    "                                      "
             FOREGROUND-COLOR IS 6, REVERSE-VIDEO.
             05 LINE 13 COL 14 PIC X(50) USING SP-TITLE(2) 
             FOREGROUND-COLOR IS 6, REVERSE-VIDEO.
             05 LINE 13 COL 64 VALUE "                    "
             FOREGROUND-COLOR IS 6, REVERSE-VIDEO.
             05 LINE 13 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.

             05 LINE 14 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 14 COL 10 VALUE "                                   
      -    "                                        "
             FOREGROUND-COLOR IS 6, REVERSE-VIDEO.
             05 LINE 14 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.

             05 LINE 15 COL 8 VALUE "  No. TITLE                                        
      -    "                             No. COMMENTS  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 16 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 16 COL 10 PIC XXX     USING LIST-ID(ID-NUM)
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 16 COL 13 VALUE "  "
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 16 COL 14 PIC X(50)   USING LIST-TITLE(ID-NUM)
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 16 COL 64 VALUE "                "
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 16 COL 80 PIC Z(4)    USING SUM-COMMENTS(ID-NUM)
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
           
             05 LINE 16 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.

             05 LINE 17 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 17 COL 10 PIC XXX     USING LIST-ID(ID-NUM + 1)
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 17 COL 13 VALUE "  "
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 17 COL 14 PIC X(50)   USING LIST-TITLE(ID-NUM + 1)
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 17 COL 64 VALUE "                "
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 17 COL 80 PIC Z(4)   USING SUM-COMMENTS(ID-NUM + 1)
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 17 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.

             05 LINE 18 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 18 COL 10 PIC XXX     USING LIST-ID(ID-NUM + 2)
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 18 COL 13 VALUE "  "
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 18 COL 14 PIC X(50)   USING LIST-TITLE(ID-NUM + 2)
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 18 COL 64 VALUE "                "
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 18 COL 80 PIC Z(4)  USING SUM-COMMENTS(ID-NUM + 2)
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 18 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.

             05 LINE 19 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 19 COL 10 PIC XXX     USING LIST-ID(ID-NUM + 3)
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 19 COL 13 VALUE "  "
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 19 COL 14 PIC X(50)   USING LIST-TITLE(ID-NUM + 3)
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 19 COL 64 VALUE "                "
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 19 COL 80 PIC Z(4)  USING SUM-COMMENTS(ID-NUM + 3)
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.           
             05 LINE 19 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.

             05 LINE 20 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 20 COL 10 PIC XXX     USING LIST-ID(ID-NUM + 4)
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 20 COL 13 VALUE "  "
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 20 COL 14 PIC X(50)   USING LIST-TITLE(ID-NUM + 4)
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 20 COL 64 VALUE "                "
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 20 COL 80 PIC Z(4)  USING SUM-COMMENTS(ID-NUM + 4)
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 20 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.

             05 LINE 21 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 21 COL 10 PIC XXX     USING LIST-ID(ID-NUM + 5)
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 21 COL 13 VALUE "  "
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 21 COL 14 PIC X(50)   USING LIST-TITLE(ID-NUM + 5)
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 21 COL 64 VALUE "                "
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 21 COL 80 PIC Z(4)  USING SUM-COMMENTS(ID-NUM + 5)
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 21 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.

             05 LINE 22 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 22 COL 10 PIC XXX     USING LIST-ID(ID-NUM + 6)
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 22 COL 13 VALUE "  "
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 22 COL 14 PIC X(50)   USING LIST-TITLE(ID-NUM + 6)
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 22 COL 64 VALUE "                "
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 22 COL 80 PIC Z(4)  USING SUM-COMMENTS(ID-NUM + 6)
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 22 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.

             05 LINE 23 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 23 COL 10 PIC XXX     USING LIST-ID(ID-NUM + 7)
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 23 COL 13 VALUE "  "
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 23 COL 14 PIC X(50)   USING LIST-TITLE(ID-NUM + 7)
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 23 COL 64 VALUE "                "
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 23 COL 80 PIC Z(4)  USING SUM-COMMENTS(ID-NUM + 7)
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 23 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.

             05 LINE 24 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 24 COL 10 PIC XXX     USING LIST-ID(ID-NUM + 8)
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 24 COL 13 VALUE "  "
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 24 COL 14 PIC X(50)   USING LIST-TITLE(ID-NUM + 8)
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 24 COL 64 VALUE "                "
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 24 COL 80 PIC Z(4)  USING SUM-COMMENTS(ID-NUM + 8)
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 24 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.

             05 LINE 25 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 25 COL 10 PIC XXX     USING LIST-ID(ID-NUM + 9)
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 25 COL 13 VALUE "  "
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 25 COL 14 PIC X(50)   USING LIST-TITLE(ID-NUM + 9)
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 25 COL 64 VALUE "                "
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 25 COL 80 PIC Z(4)  USING SUM-COMMENTS(ID-NUM + 9)
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 25 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.

             05 LINE 26 COL 8 VALUE  "                               REC                 
      -    "ENT MESSAGES                                "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
        *>    MSG MENU OPTION POSITIONING
               05 LINE 42 COLUMN 6 VALUE "Option: ".
               05 MSG-MENU-CHOICE-FIELD LINE 42 COLUMN 14 PIC XX
                  USING MSG-MENU-CHOICE.

           01 MESSAGE-VIEW-SCREEN
               BACKGROUND-COLOR IS 01.
                05 BLANK SCREEN.
        *>    MESSAGE VIEW HEADER
             05 LINE 1 COL 1  VALUE "   :                              
      -    "                                                         "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 1 COL 2 PIC X(2) USING WS-FORMATTED-HOUR 
             FOREGROUND-COLOR IS 7 REVERSE-VIDEO.
             05 LINE 1 COL 5 PIC X(2) USING WS-FORMATTED-MINS
             FOREGROUND-COLOR IS 7 REVERSE-VIDEO.
             05 LINE 1 COL 80 VALUE "CREDITS: "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 1 COL 89 PIC 9(3) USING WS-USERCREDITS
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
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
             05 LINE 4 COL 40 VALUE "BULLETIN BOARD"                                     
             FOREGROUND-COLOR IS 7, UNDERLINE.
                  
             05 LINE  9 COL 8 VALUE "                              YOUR                 
      -    "CHOSEN MESSAGE    Date posted:             "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE  9 COL 74 PIC X(10) USING LIST-DATE(MSG-SELECT)
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 10 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 10 COL 10 VALUE "Title:   "
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 10 COL 19 PIC X(50) USING LIST-TITLE(MSG-SELECT)
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 10 COL 69 VALUE "               "
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 10 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 11 COL 8 VALUE "  "   
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.                                  
             05 LINE 11 COL 10 VALUE "                                   
      -    "                                        "   
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 11 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 12 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 12 COL 10 VALUE "Message: "
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 12 COL 19 PIC X(60) USING LS-PART-1
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 12 COL 69 VALUE "               "
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 12 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 13 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 13 COL 10 VALUE "         "
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 13 COL 19 PIC X(60) USING LS-PART-2
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 13 COL 69 VALUE "               "
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 13 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 14 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 14 COL 10 VALUE "         "
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 14 COL 19 PIC X(60) USING LS-PART-3
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 14 COL 69 VALUE "               "
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 14 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 15 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 15 COL 10 VALUE "         "
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 15 COL 19 PIC X(60) USING LS-PART-4
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 15 COL 69 VALUE "               "
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 15 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 16 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 16 COL 10 VALUE "         "
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 16 COL 19 PIC X(60) USING LS-PART-5
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 16 COL 69 VALUE "               "
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 16 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 17 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 17 COL 10 VALUE "                                   
      -    "                                        "
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 17 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 18 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 18 COL 10 VALUE "                                   
      -    "                                        "  
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 18 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 19 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 19 COL 10 VALUE "Author:  "
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 19 COL 19 PIC X(16) USING LIST-USERNAME(MSG-SELECT)
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 19 COL 35 VALUE "                                  
      -      "               " FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 19 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 20 COL 8 VALUE "                                   
      -    "                        No. Comments:      "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 20 COL 80 PIC Z(4) USING SUM-COMMENTS(MSG-SELECT)
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
        *>    MESSAGE VIEW OPTION POSITIONING
               05 LINE 42 COLUMN 6 VALUE "Option: ".
               05 MSG-VIEW-CHOICE-FIELD LINE 42 COLUMN 14 PIC X
                  USING MSG-VIEW-CHOICE.

           01 SP-MESSAGE-VIEW-SCREEN
               BACKGROUND-COLOR IS 01.
                05 BLANK SCREEN.
        *>    SPONSORED MESSAGE VIEW HEADER
              05 LINE 1 COL 1  VALUE "   :                              
      -    "                                                          "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 1 COL 2 PIC X(2) USING WS-FORMATTED-HOUR 
             FOREGROUND-COLOR IS 7 REVERSE-VIDEO.
             05 LINE 1 COL 5 PIC X(2) USING WS-FORMATTED-MINS
             FOREGROUND-COLOR IS 7 REVERSE-VIDEO.
             05 LINE 1 COL 80 VALUE "CREDITS: "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 1 COL 89 PIC 9(3) USING WS-USERCREDITS
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
        *>    SPONSORED MESSAGE VIEW FOOTER
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
        *>    SPONSORED MESSAGE VIEW BODY
             05 LINE  4 COL 10 VALUE "FriendFace" UNDERLINE.
             05 LINE 4 COL 40 VALUE "BULLETIN BOARD"                                     
             FOREGROUND-COLOR IS 7, UNDERLINE.
             05 LINE  9 COL 8 VALUE "                              SPONS                
      -    "ORED MESSAGE                               "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE  9 COL 74 VALUE " "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 10 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 10 COL 10 VALUE "Title:   "
             FOREGROUND-COLOR IS 6, REVERSE-VIDEO.
             05 LINE 10 COL 19 PIC X(50) USING SP-TITLE(SP-MSG-SELECT)
             FOREGROUND-COLOR IS 6, REVERSE-VIDEO.
             05 LINE 10 COL 69 VALUE "               "
             FOREGROUND-COLOR IS 6, REVERSE-VIDEO.
             05 LINE 10 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 11 COL 8 VALUE "  "   
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.                                  
             05 LINE 11 COL 10 VALUE "                                   
      -    "                                        "   
             FOREGROUND-COLOR IS 6, REVERSE-VIDEO.
             05 LINE 11 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 12 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 12 COL 10 VALUE "Message: "
             FOREGROUND-COLOR IS 6, REVERSE-VIDEO.
             05 LINE 12 COL 19 PIC X(60) USING SP-PART-1
             FOREGROUND-COLOR IS 6, REVERSE-VIDEO.
             05 LINE 12 COL 79 VALUE "     "
             FOREGROUND-COLOR IS 6, REVERSE-VIDEO.
             05 LINE 12 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 13 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 13 COL 10 VALUE "         "
             FOREGROUND-COLOR IS 6, REVERSE-VIDEO.
             05 LINE 13 COL 19 PIC X(60) USING SP-PART-2
             FOREGROUND-COLOR IS 6, REVERSE-VIDEO.
             05 LINE 13 COL 79 VALUE "     "
             FOREGROUND-COLOR IS 6, REVERSE-VIDEO.
             05 LINE 13 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 14 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 14 COL 10 VALUE "         "
             FOREGROUND-COLOR IS 6, REVERSE-VIDEO.
             05 LINE 14 COL 19 PIC X(60) USING SP-PART-3
             FOREGROUND-COLOR IS 6, REVERSE-VIDEO.
             05 LINE 14 COL 79 VALUE "     "
             FOREGROUND-COLOR IS 6, REVERSE-VIDEO.
             05 LINE 14 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 15 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 15 COL 10 VALUE "         "
             FOREGROUND-COLOR IS 6, REVERSE-VIDEO.
             05 LINE 15 COL 19 PIC X(60) USING SP-PART-4
             FOREGROUND-COLOR IS 6, REVERSE-VIDEO.
             05 LINE 15 COL 79 VALUE "     "
             FOREGROUND-COLOR IS 6, REVERSE-VIDEO.
             05 LINE 15 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 16 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 16 COL 10 VALUE "         "
             FOREGROUND-COLOR IS 6, REVERSE-VIDEO.
             05 LINE 16 COL 19 PIC X(60) USING SP-PART-5
             FOREGROUND-COLOR IS 6, REVERSE-VIDEO.
             05 LINE 16 COL 79 VALUE "     "
             FOREGROUND-COLOR IS 6, REVERSE-VIDEO.
             05 LINE 16 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 17 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 17 COL 10 VALUE "                                   
      -    "                                        "
             FOREGROUND-COLOR IS 6, REVERSE-VIDEO.
             05 LINE 17 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 18 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 18 COL 10 VALUE "                                   
      -    "                                        "  
             FOREGROUND-COLOR IS 6, REVERSE-VIDEO.
             05 LINE 18 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 19 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 19 COL 10 VALUE "Author:  "
             FOREGROUND-COLOR IS 6, REVERSE-VIDEO.
             05 LINE 19 COL 19 PIC X(16) 
             USING SP-USERNAME(SP-MSG-SELECT)
             FOREGROUND-COLOR IS 6, REVERSE-VIDEO.
             05 LINE 19 COL 35 VALUE "                                  
      -      "               " FOREGROUND-COLOR IS 6, REVERSE-VIDEO.
             05 LINE 19 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 20 COL 8 VALUE "                                   
      -    "                                           "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.   

             05 LINE 25 COL 8 VALUE "                                   
      -    "                                           "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 26 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 26 COL 10 VALUE "                                   
      -    "                                          "
             FOREGROUND-COLOR IS 2, REVERSE-VIDEO.
             05 LINE 26 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 27 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 27 COL 10 VALUE "            Leave a sponsored post                  
      -    " for just 10 Credits!                     "
             FOREGROUND-COLOR IS 2, REVERSE-VIDEO.
             05 LINE 27 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.

             05 LINE 28 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 28 COL 10 VALUE "            Sponsored posts expire                  
      -    " at midnight!                             "
             FOREGROUND-COLOR IS 2, REVERSE-VIDEO.
             05 LINE 28 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.

             05 LINE 29 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 29 COL 10 VALUE "                                              
      -    "                                         "
             FOREGROUND-COLOR IS 2, REVERSE-VIDEO.
             05 LINE 29 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.

             05 LINE 30 COL 8 VALUE "                                   
      -    "                                           "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.        
        *>    SPONSORED MESSAGE VIEW OPTION POSITIONING
               05 LINE 42 COLUMN 6 VALUE "Option: ".
               05 SP-MSG-VIEW-CHOICE-FIELD LINE 42 COLUMN 14 PIC X
                  USING SP-MSG-VIEW-CHOICE. 

           01 WRITE-MSG-SCREEN
               BACKGROUND-COLOR IS 01.
               05 BLANK SCREEN.
        *>    WRITE MESSAGE HEADER
             05 LINE 1 COL 1  VALUE "   :                              
      -    "                                                         "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 1 COL 2 PIC X(2) USING WS-FORMATTED-HOUR 
             FOREGROUND-COLOR IS 7 REVERSE-VIDEO.
             05 LINE 1 COL 5 PIC X(2) USING WS-FORMATTED-MINS
             FOREGROUND-COLOR IS 7 REVERSE-VIDEO.
             05 LINE 1 COL 80 VALUE "CREDITS: "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 1 COL 89 PIC 9(3) USING WS-USERCREDITS
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
        *>    WRITE MESSAGE FOOTER
               05 LINE 43 COL 1 VALUE "                                 
      -    "                                                           "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 44 COL 1 VALUE "     (P) Post message     (D) Dis
      -    "card message     (S) Sponsor message                       "                                 
                FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 45 COL 1 VALUE "     (G) Go back          (Q) Qui                                 
      -    "t                                                          "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 46 COL 1 VALUE "                                 
      -    "                                                           "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
        *>    WRITE MESSAGE BODY
             05 LINE  4 COL 10 VALUE "FriendFace" UNDERLINE.
             05 LINE 4 COL 40 VALUE "BULLETIN BOARD"                                     
             FOREGROUND-COLOR IS 7, UNDERLINE.
             05 LINE  9 COL 8 VALUE "                               POST                 
      -    " A MESSAGE                                 "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 10 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 10 COL 10 VALUE "Title:   "
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 WS-TITLE-FIELD LINE 10 COL 19 PIC X(50) USING WS-TITLE
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 10 COL 19 PIC X(50) USING LIST-TITLE(MSG-SELECT)
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 10 COL 69 VALUE "__________     "
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 10 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 11 COL 8 VALUE "  "   
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.                                  
             05 LINE 11 COL 10 VALUE "                                   
      -    "                                        "   
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 11 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 12 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 12 COL 10 VALUE "Message: "
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE-1-FIELD LINE 12 COL 19 PIC X(60) USING LS-PART-1
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 12 COL 69 VALUE "               "
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 12 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 13 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 13 COL 10 VALUE "         "
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE-2-FIELD LINE 13 COL 19 PIC X(60) USING LS-PART-2
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 13 COL 69 VALUE "               "
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 13 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 14 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 14 COL 10 VALUE "         "
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE-3-FIELD LINE 14 COL 19 PIC X(60) USING LS-PART-3
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 14 COL 69 VALUE "               "
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 14 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 15 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 15 COL 10 VALUE "         "
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE-4-FIELD LINE 15 COL 19 PIC X(60) USING LS-PART-4
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 15 COL 69 VALUE "               "
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 15 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 16 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 16 COL 10 VALUE "         "
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE-5-FIELD LINE 16 COL 19 PIC X(60) USING LS-PART-5
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 16 COL 69 VALUE "               "
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 16 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 17 COL 8 VALUE "                                   
      -    "                                           "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO. 
             05 LINE 19 COL 33 VALUE "Make your post a sponsored post"                                    
             FOREGROUND-COLOR IS 7.
             05 LINE 20 COL 38 VALUE "for just "                                    
             FOREGROUND-COLOR IS 7.
             05 LINE 20 COL 47 VALUE "10 credits!"
             FOREGROUND-COLOR IS 2.
             05 LINE 21 COL 37 VALUE "*limited availability*"                                    
             FOREGROUND-COLOR IS 6, UNDERLINE, BLINK.
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
      -    "                                                         "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 1 COL 2 PIC X(2) USING WS-FORMATTED-HOUR 
             FOREGROUND-COLOR IS 7 REVERSE-VIDEO.
             05 LINE 1 COL 5 PIC X(2) USING WS-FORMATTED-MINS
             FOREGROUND-COLOR IS 7 REVERSE-VIDEO.
             05 LINE 1 COL 80 VALUE "CREDITS: "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 1 COL 89 PIC 9(3) USING WS-USERCREDITS
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
        *>    COMMENT SECTION FOOTER
            05 LINE 43 COL 1 VALUE "                                 
      -    "                                                         "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 44 COL 1 VALUE "     (N) Next page     (P) Previous
      -    " page     (C) Comment                                      "                                 
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 45 COL 1 VALUE "     (G) Go back       (Q) Quit                                 
      -    "                                                         "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 46 COL 1 VALUE "                                 
      -    "                                                         "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 7 COL 10 VALUE "Comments for the message titled: ".
             05 LINE 7 COL 43 PIC X(50) USING LIST-TITLE(MSG-SELECT)
             FOREGROUND-COLOR IS 2, HIGHLIGHT.
        *>    COMMENT SECTION BODY
             05 LINE  4 COL 10 VALUE "FriendFace" UNDERLINE.
             05 LINE  4 COL 40 VALUE "BULLETIN BOARD"                                     
             FOREGROUND-COLOR IS 7, UNDERLINE.
            *>    1st COMMENT
             05 LINE  9 COL 6 VALUE "                                  
      -    "       " FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 9 COL 48 PIC X(21) USING COM-DATE(COM-INDEX)
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 9 COL 60 VALUE "          "
             FOREGROUND-COLOR IS 1, REVERSE-VIDEO.  
             05 LINE 10 COL 6 VALUE "  " 
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 10 COL 8 VALUE "                                  
      -    "                " FOREGROUND-COLOR IS 2, REVERSE-VIDEO.
             05 LINE 10 COL 58 VALUE "  " 
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO. 
             05 LINE 11 COL 6 VALUE "  " 
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO. 
             05 LINE 11 COL 8 PIC X(50) USING COM-COMMENT(COM-INDEX)
             FOREGROUND-COLOR IS 2, HIGHLIGHT, REVERSE-VIDEO.
             05 LINE 11 COL 58 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 12 COL 6 VALUE "  " 
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 12 COL 8 VALUE "                                  
      -    "                " FOREGROUND-COLOR IS 2, REVERSE-VIDEO. 
             05 LINE 12 COL 58 VALUE "  " 
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.  
             05 LINE 13 COL 6 VALUE "                                                    
      -    "                   " FOREGROUND-COLOR IS 7, REVERSE-VIDEO. 
             05 LINE 13 COL 8 PIC X(16) USING COM-AUTHOR(COM-INDEX)
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.  
            *>    2nd COMMENT
             05 LINE 15 COL 33 VALUE "                                  
      -    "        " FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 15 COL 75 PIC X(21) USING COM-DATE(COM-INDEX + 1)
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE15 COL 87 VALUE "          "
             FOREGROUND-COLOR IS 1, REVERSE-VIDEO.   
             05 LINE 16 COL 33 VALUE "  " 
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 16 COL 35 VALUE "                                  
      -    "                " FOREGROUND-COLOR IS 6, REVERSE-VIDEO.
             05 LINE 16 COL 85 VALUE "  " 
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO. 
             05 LINE 17 COL 33 VALUE "  " 
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO. 
             05 LINE 17 COL 35 PIC X(50) 
             USING COM-COMMENT(COM-INDEX + 1)
             FOREGROUND-COLOR IS 6, HIGHLIGHT, REVERSE-VIDEO.
             05 LINE 17 COL 85 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 18 COL 33 VALUE "  " 
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 18 COL 35 VALUE "                                  
      -    "                " FOREGROUND-COLOR IS 6, REVERSE-VIDEO. 
             05 LINE 18 COL 85 VALUE "  " 
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.  
             05 LINE 19 COL 33 VALUE "                                                    
      -    "                    " FOREGROUND-COLOR IS 7, REVERSE-VIDEO. 
             05 LINE 19 COL 35 PIC X(16) USING COM-AUTHOR(COM-INDEX + 1)
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO. 
            *>    3rd COMMENT
             05 LINE 21 COL 6 VALUE "                                  
      -    "       " FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 21 COL 48 PIC X(21) USING COM-DATE(COM-INDEX + 2)
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 21 COL 60 VALUE "          "
             FOREGROUND-COLOR IS 1, REVERSE-VIDEO.  
             05 LINE 22 COL 6 VALUE "  " 
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 22 COL 8 VALUE "                                  
      -    "                " FOREGROUND-COLOR IS 2, REVERSE-VIDEO.
             05 LINE 22 COL 58 VALUE "  " 
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO. 
             05 LINE 23 COL 6 VALUE "  " 
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO. 
             05 LINE 23 COL 8 PIC X(50) USING COM-COMMENT(COM-INDEX + 2)
             FOREGROUND-COLOR IS 2, HIGHLIGHT, REVERSE-VIDEO.
             05 LINE 23 COL 58 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 24 COL 6 VALUE "  " 
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 24 COL 8 VALUE "                                  
      -    "                " FOREGROUND-COLOR IS 2, REVERSE-VIDEO. 
             05 LINE 24 COL 58 VALUE "  " 
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.  
             05 LINE 25 COL 6 VALUE "                                                    
      -    "                   " FOREGROUND-COLOR IS 7, REVERSE-VIDEO. 
             05 LINE 25 COL 8 PIC X(16) USING COM-AUTHOR(COM-INDEX + 2)
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
            *>    4th COMMENT 
             05 LINE 27 COL 33 VALUE "                                  
      -    "        " FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 27 COL 75 PIC X(21) USING COM-DATE(COM-INDEX + 3)
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 27 COL 87 VALUE "          "
             FOREGROUND-COLOR IS 1, REVERSE-VIDEO.   
             05 LINE 28 COL 33 VALUE "  " 
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 28 COL 35 VALUE "                                  
      -    "                " FOREGROUND-COLOR IS 6, REVERSE-VIDEO.
             05 LINE 28 COL 85 VALUE "  " 
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO. 
             05 LINE 29 COL 33 VALUE "  " 
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO. 
             05 LINE 29 COL 35 PIC X(50) 
             USING COM-COMMENT(COM-INDEX + 3)
             FOREGROUND-COLOR IS 6, HIGHLIGHT, REVERSE-VIDEO.
             05 LINE 29 COL 85 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 30 COL 33 VALUE "  " 
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 30 COL 35 VALUE "                                  
      -    "                " FOREGROUND-COLOR IS 6, REVERSE-VIDEO. 
             05 LINE 30 COL 85 VALUE "  " 
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.  
             05 LINE 31 COL 33 VALUE "                                                    
      -    "                    " FOREGROUND-COLOR IS 7, REVERSE-VIDEO. 
             05 LINE 31 COL 35 PIC X(16) USING COM-AUTHOR(COM-INDEX + 3)
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO. 
            *>  *>    5th COMMENT
             05 LINE 33 COL 6 VALUE "                                  
      -    "       " FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 33 COL 48 PIC X(21) USING COM-DATE(COM-INDEX + 4)
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 33 COL 60 VALUE "          "
             FOREGROUND-COLOR IS 1, REVERSE-VIDEO.  
             05 LINE 34 COL 6 VALUE "  " 
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 34 COL 8 VALUE "                                  
      -    "                " FOREGROUND-COLOR IS 2, REVERSE-VIDEO.
             05 LINE 34 COL 58 VALUE "  " 
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO. 
             05 LINE 35 COL 6 VALUE "  " 
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO. 
             05 LINE 35 COL 8 PIC X(50) USING COM-COMMENT(COM-INDEX + 4)
             FOREGROUND-COLOR IS 2, HIGHLIGHT, REVERSE-VIDEO.
             05 LINE 35 COL 58 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 36 COL 6 VALUE "  " 
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 36 COL 8 VALUE "                                  
      -    "                " FOREGROUND-COLOR IS 2, REVERSE-VIDEO. 
             05 LINE 36 COL 58 VALUE "  " 
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.  
             05 LINE 37 COL 6 VALUE "                                                    
      -    "                   " FOREGROUND-COLOR IS 7, REVERSE-VIDEO. 
             05 LINE 37 COL 8 PIC X(16) USING COM-AUTHOR(COM-INDEX + 4)
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
        *>    COMMENT SECTION OPTION POSITIONING
             05 LINE 42 COLUMN 6 VALUE "Option: ".
             05 COM-SCRN-CHOICE-FIELD LINE 42 COL 14 PIC X USING
               COM-SCRN-CHOICE. 

           01 WRITE-COMMENT-SCREEN
               BACKGROUND-COLOR IS 01.
               05 BLANK SCREEN.
        *>    WRITE COMMENT HEADER
             05 LINE 1 COL 1  VALUE "Type your comment here(50):                              
      -    "                                                         "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 1 COL 2 PIC X(2) USING WS-FORMATTED-HOUR 
             FOREGROUND-COLOR IS 7 REVERSE-VIDEO.
             05 LINE 1 COL 5 PIC X(2) USING WS-FORMATTED-MINS
             FOREGROUND-COLOR IS 7 REVERSE-VIDEO.
             05 LINE 1 COL 80 VALUE "CREDITS: "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 1 COL 89 PIC 9(3) USING WS-USERCREDITS
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
        *>    WRITE COMMENT FOOTER
               05 LINE 43 COL 1 VALUE "                                 
      -    "                                                           "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 44 COL 1 VALUE "     (P) Post comment     (D) Dis
      -    "card comment                                               "                                 
                FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 45 COL 1 VALUE "     (Q) Quit             (Q) Qui                              
      -    "t                                                          "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 46 COL 1 VALUE "                                 
      -    "                                                           "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
        *>    WRITE COMMENT BODY
             05 LINE  4 COL 10 VALUE "FriendFace" UNDERLINE.
             05 LINE 4 COL 40 VALUE "BULLETIN BOARD"                                     
             FOREGROUND-COLOR IS 7, UNDERLINE.
             05 LINE  9 COL 8 VALUE "                               POST                 
      -    " A COMMENT                                 "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 10 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.                                 
             05 LINE 10 COL 10 VALUE "                                   
      -    "                                        "   
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 10 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 11 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 11 COL 10 VALUE "Comment: "
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 WRITE-COMMENT-FIELD LINE 11 COL 19 PIC X(50) 
             USING WRITE-COMMENT FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 11 COL 69 VALUE "               "
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 11 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 12 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.                                 
             05 LINE 12 COL 10 VALUE "                                   
      -    "                                        "   
             FOREGROUND-COLOR IS 3, REVERSE-VIDEO.
             05 LINE 12 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 13 COL 8 VALUE "                                   
      -    "                                           "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO. 
          
        *>    WRITE COMMENT OPTION POSITIONING
               05 LINE 42 COLUMN 6 VALUE "Option: ".
               05 POST-COMMENT-CHOICE-FIELD LINE 42 COLUMN 14 PIC X
                  USING POST-COMMENT-CHOICE.  
      ******************************************************************
      *******************-----GAMES MENU SECTION----********************
      ******************************************************************           
           01 GAMES-MENU-SCREEN
               BACKGROUND-COLOR IS 1.
               05 BLANK SCREEN.
        *>    GAMES MENU HEADER
             05 LINE 1 COL 1  VALUE "   :                              
      -    "                                                         "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 1 COL 2 PIC X(2) USING WS-FORMATTED-HOUR 
             FOREGROUND-COLOR IS 7 REVERSE-VIDEO.
             05 LINE 1 COL 5 PIC X(2) USING WS-FORMATTED-MINS
             FOREGROUND-COLOR IS 7 REVERSE-VIDEO.
             05 LINE 1 COL 80 VALUE "CREDITS: "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 1 COL 89 PIC 9(3) USING WS-USERCREDITS
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
        *>    GAMES MENU FOOTER
               05 LINE 43 COL 1 VALUE "                                 
      -    "                                                           "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 44 COL 1 VALUE "     (1) Guess the number     (2)
      -    " Guess the word    (3) Tic-Tac-Toe                         "                                            "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 45 COL 1 VALUE "     (C) Credit Store         (G)
      -    " Go back           (Q) Quit                                "                                         
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 46 COL 1 VALUE "                                 
      -    "                                                           "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
        *>    GAMES MENU BODY
             05 LINE  4 COL 10 VALUE "FriendFace" UNDERLINE.
             05 LINE 4 COL 40 VALUE "GAMES MENU"                                     
             FOREGROUND-COLOR IS 7, UNDERLINE.
             05 LINE 12 COL 8 VALUE "                                   
      -    "                                         "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 12 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 13 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 13 COL 10 VALUE "                                   
      -    "                                          "
             FOREGROUND-COLOR IS 5, REVERSE-VIDEO.
             05 LINE 13 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 14 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 14 COL 10 VALUE "                          1: GUESS                
      -    " THE NUMBER                             "
             FOREGROUND-COLOR IS 5, REVERSE-VIDEO.
             05 LINE 14 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 15 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 15 COL 10 VALUE "                                   
      -    "                                        "
             FOREGROUND-COLOR IS 5, REVERSE-VIDEO.
             05 LINE 15 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
        *>    GAME 2 POSITION
             05 LINE 16 COL 8 VALUE "                                   
      -    "                                           "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 17 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 17 COL 10 VALUE "                                   
      -    "                                          "
             FOREGROUND-COLOR IS 6, REVERSE-VIDEO.
             05 LINE 17 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 18 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 18 COL 10 VALUE "                          2: GUESS                
      -    " THE WORD                               "
             FOREGROUND-COLOR IS 6, REVERSE-VIDEO.
             05 LINE 18 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 19 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 19 COL 10 VALUE "                                   
      -    "                                        "
             FOREGROUND-COLOR IS 6, REVERSE-VIDEO.
             05 LINE 19 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 20 COL 8 VALUE "                                   
      -    "                                           "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
        *>    GAME 3 POSITION
             05 LINE 21 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 21 COL 10 VALUE "                                   
      -    "                                          "
             FOREGROUND-COLOR IS 2, REVERSE-VIDEO.
             05 LINE 21 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 22 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 22 COL 10 VALUE "                          3: TIC-T                
      -    "AC-TOE                                  "
             FOREGROUND-COLOR IS 2, REVERSE-VIDEO.
             05 LINE 22 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 23 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 23 COL 10 VALUE "                                   
      -    "                                        "
             FOREGROUND-COLOR IS 2, REVERSE-VIDEO.
             05 LINE 23 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 24 COL 8 VALUE "                                   
      -    "                                           "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
        *>    GAMES MENU OPTION POSITIONING
               05 LINE 42 COLUMN 6 VALUE "Option: ".
               05 GAMES-MENU-CHOICE-FIELD LINE 42 COLUMN 14 PIC X
                  USING GAMES-MENU-CHOICE.

       01 WORD-GUESSING-SCREEN
             BACKGROUND-COLOR IS 1.
             05 BLANK SCREEN.
        *>   WORD GUESSING GAME HEADER
             05 LINE 1 COL 1  VALUE "   :                              
      -    "                                                         "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 1 COL 2 PIC X(2) USING WS-FORMATTED-HOUR 
             FOREGROUND-COLOR IS 7 REVERSE-VIDEO.
             05 LINE 1 COL 5 PIC X(2) USING WS-FORMATTED-MINS
             FOREGROUND-COLOR IS 7 REVERSE-VIDEO.
             05 LINE 1 COL 80 VALUE "CREDITS: "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 1 COL 89 PIC 9(3) USING WS-USERCREDITS
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
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
             05 LINE  4 COL 10 VALUE "FriendFace" UNDERLINE.
             05 LINE 4 COL 40 VALUE "GUESS THE WORD"                                     
             FOREGROUND-COLOR IS 7, UNDERLINE.
             05 LINE 12 COL 8 VALUE "                                   
      -    "                        "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 12 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 13 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 13 COL 10 VALUE "                                   
      -    "                                          "
             FOREGROUND-COLOR IS 5, REVERSE-VIDEO.
             05 LINE 13 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 14 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 14 COL 10 VALUE "Guess this word: "
             FOREGROUND-COLOR IS 5, REVERSE-VIDEO.
             05 LINE 14 COL 27 PIC X(10) USING WS-WORD
             FOREGROUND-COLOR IS 5, REVERSE-VIDEO.
             05 LINE 14 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 15 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 15 COL 10 VALUE "                                   
      -    "                                        "
             FOREGROUND-COLOR IS 5, REVERSE-VIDEO.
             05 LINE 15 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 16 COL 8 VALUE "                                   
      -    "                                           "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
           
             05 LINE 17 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 17 COL 10 VALUE "                                   
      -    "                                          "
             FOREGROUND-COLOR IS 6, REVERSE-VIDEO.
             05 LINE 17 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 18 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 18 COL 10 VALUE "Guesses left: "                          "
             FOREGROUND-COLOR IS 6, REVERSE-VIDEO.
             05 LINE 18 COL 24 PIC 99 USING WS-GUESSES-LEFT                           "
             FOREGROUND-COLOR IS 6, REVERSE-VIDEO.
             05 LINE 18 COL 60 VALUE "Reveal Word: "     
             FOREGROUND-COLOR IS 6, REVERSE-VIDEO.
             05 LINE 18 COLUMN 73 PIC X(10) USING WS-ANSWERWORD
             FOREGROUND-COLOR IS 1, REVERSE-VIDEO. 
             05 LINE 18 COL 83 VALUE " "
             FOREGROUND-COLOR IS 6, REVERSE-VIDEO.             
             05 LINE 18 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 19 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 19 COL 10 VALUE "                                   
      -    "                                        "
             FOREGROUND-COLOR IS 6, REVERSE-VIDEO.
             05 LINE 19 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 20 COL 8 VALUE "                                   
      -    "                                           "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.        
        *>    WORD GUESSING GAME OPTION POSITIONING
               05 LINE 42 COLUMN 6 VALUE "Option: ".
   
           01 IN-GAME-SCREEN
             BACKGROUND-COLOR IS 1.
             05 BLANK SCREEN.
        *>   WORD GUESSING IN GAME HEADER
             05 LINE 1 COL 1  VALUE "   :                              
      -    "                                                         "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 1 COL 2 PIC X(2) USING WS-FORMATTED-HOUR 
             FOREGROUND-COLOR IS 7 REVERSE-VIDEO.
             05 LINE 1 COL 5 PIC X(2) USING WS-FORMATTED-MINS
             FOREGROUND-COLOR IS 7 REVERSE-VIDEO.
             05 LINE 1 COL 80 VALUE "CREDITS: "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 1 COL 89 PIC 9(3) USING WS-USERCREDITS
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
        *>    WORD GUESSING IN GAME FOOTER
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
        *>    WORD GUESSING IN GAME BODY
             05 LINE  4 COL 10 VALUE "FriendFace" UNDERLINE.
             05 LINE 4 COL 40 VALUE "GUESS THE WORD"                                     
             FOREGROUND-COLOR IS 7, UNDERLINE.
             05 LINE 12 COL 8 VALUE "                                   
      -    "                                         "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 12 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 13 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 13 COL 10 VALUE "                                   
      -    "                                          "
             FOREGROUND-COLOR IS 5, REVERSE-VIDEO.
             05 LINE 13 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 14 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 14 COL 10 VALUE "Guess this word: "
             FOREGROUND-COLOR IS 5, REVERSE-VIDEO.
             05 LINE 14 COL 27 PIC X(10) USING WS-WORD
             FOREGROUND-COLOR IS 5, REVERSE-VIDEO.
             05 LINE 14 COL 37 VALUE "                                  
      -    "             "
             FOREGROUND-COLOR IS 5, REVERSE-VIDEO.
             05 LINE 14 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 15 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 15 COL 10 VALUE "                                   
      -    "                                        "
             FOREGROUND-COLOR IS 5, REVERSE-VIDEO.
             05 LINE 15 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 16 COL 8 VALUE "                                   
      -    "                                           "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
           
             05 LINE 17 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 17 COL 10 VALUE "                                   
      -    "                                          "
             FOREGROUND-COLOR IS 6, REVERSE-VIDEO.
             05 LINE 17 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 18 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 18 COL 10 VALUE "Guesses left: "                          "
             FOREGROUND-COLOR IS 6, REVERSE-VIDEO.
             05 LINE 18 COL 24 PIC 99 USING WS-GUESSES-LEFT                           "
             FOREGROUND-COLOR IS 6, REVERSE-VIDEO.
             05 LINE 18 COL 26 VALUE "                                 
      -    " " FOREGROUND-COLOR IS 6, REVERSE-VIDEO.   
             05 LINE 18 COL 60 VALUE "Reveal Word: "     
             FOREGROUND-COLOR IS 6, REVERSE-VIDEO.
             05 LINE 18 COLUMN 73 PIC X(10) USING WS-ANSWERWORD
             FOREGROUND-COLOR IS 1, REVERSE-VIDEO. 
             05 LINE 18 COL 83 VALUE " "
             FOREGROUND-COLOR IS 6, REVERSE-VIDEO.             
             05 LINE 18 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 19 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 19 COL 10 VALUE "                                   
      -    "                                        "
             FOREGROUND-COLOR IS 6, REVERSE-VIDEO.
             05 LINE 19 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 20 COL 8 VALUE "                                   
      -    "                                           "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.          
        *>    WORD GUESSING GAME OPTION POSITIONING
               05 LINE 42 COLUMN 6 VALUE "Option: ".
             05 WS-GUESS-CHOICE-FIELD LINE 42 COLUMN 14 PIC X
               USING WS-GUESS-CHOICE   FOREGROUND-COLOR IS 0.
           01 WORD-GUESSING-LOSE-SCREEN
             BACKGROUND-COLOR IS 4.
             05 BLANK SCREEN.
        *>    LOSE GAME HEADER
             05 LINE 1 COL 1  VALUE "   :                              
      -    "                                                         "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 1 COL 2 PIC X(2) USING WS-FORMATTED-HOUR 
             FOREGROUND-COLOR IS 7 REVERSE-VIDEO.
             05 LINE 1 COL 5 PIC X(2) USING WS-FORMATTED-MINS
             FOREGROUND-COLOR IS 7 REVERSE-VIDEO.
             05 LINE 1 COL 80 VALUE "CREDITS: "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 1 COL 89 PIC 9(3) USING WS-USERCREDITS
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
        *>    LOSE GAME FOOTER
               05 LINE 43 COL 1 VALUE "                                 
      -    "                                                           "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 44 COL 1 VALUE "     (P) Play again     (H) High                        
      -    "scores     (R) Reveal word (costs 5 credits)               "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 45 COL 1 VALUE "     (Q) Quit game  
      -    "                                                           "                                         
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 46 COL 1 VALUE "                                 
      -    "                                                           "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
        *>    LOSE GAME BODY
             05 LINE  4 COL 10 VALUE "FriendFace"
             FOREGROUND-COLOR IS 0, UNDERLINE.
             05 LINE  4 COL 40 VALUE "GUESS THE WORD"                                     
             FOREGROUND-COLOR IS 0, UNDERLINE.
             05 LINE 12 COL 8 VALUE "                                   
      -    "                                         "
             FOREGROUND-COLOR IS 0, REVERSE-VIDEO.
             05 LINE 12 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 0, REVERSE-VIDEO.
             05 LINE 13 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 0, REVERSE-VIDEO.
             05 LINE 13 COL 10 VALUE "                                   
      -    "                                        "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 13 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 0, REVERSE-VIDEO.
             05 LINE 14 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 0, REVERSE-VIDEO.
             05 LINE 14 COL 10 VALUE "You failed to guess the word: "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 14 COLUMN 40 PIC X(10) USING WS-WORD
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 14 COL 50 VALUE "                                  
      -    "" FOREGROUND-COLOR IS 7, REVERSE-VIDEO. 
             05 LINE 14 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 0, REVERSE-VIDEO.           
             05 LINE 15 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 0, REVERSE-VIDEO.
             05 LINE 15 COL 10 VALUE "                                   
      -    "                                        "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 15 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 0, REVERSE-VIDEO.
             05 LINE 16 COL 8 VALUE "                                   
      -    "                                           "
             FOREGROUND-COLOR IS 0, REVERSE-VIDEO.
             05 LINE 16 COL 42 VALUE "YOU LOST!"
             FOREGROUND-COLOR IS 0, REVERSE-VIDEO, BLINK.
           
             05 LINE 17 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 0, REVERSE-VIDEO.
             05 LINE 17 COL 10 VALUE "                                   
      -    "                                          "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 17 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 0, REVERSE-VIDEO.
             05 LINE 18 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 0, REVERSE-VIDEO.
             05 LINE 18 COL 10 VALUE "Guesses left: "                          "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 18 COL 24 PIC 99 USING WS-GUESSES-LEFT                           "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 18 COL 26 VALUE "                                 
      -    " " FOREGROUND-COLOR IS 7, REVERSE-VIDEO. 
             05 LINE 18 COL 60 VALUE "Reveal Word: "     
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 18 COLUMN 73 PIC X(10) USING WS-ANSWERWORD
             FOREGROUND-COLOR IS WS-GTW-COL, REVERSE-VIDEO. 
             05 LINE 18 COL 83 VALUE " "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.             
             05 LINE 18 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 0, REVERSE-VIDEO. 
           
             05 LINE 19 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 0, REVERSE-VIDEO.
             05 LINE 19 COL 10 VALUE "                                   
      -    "                                        "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 19 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 0, REVERSE-VIDEO.
             05 LINE 20 COL 8 VALUE "                                   
      -    "                                           "
             FOREGROUND-COLOR IS 0, REVERSE-VIDEO.     
        *>    LOSE GAME OPTION POSITIONING
             05 LINE 42 COLUMN 6 VALUE "Option: "
             FOREGROUND-COLOR IS 0.
             05 WS-GUESSING-LOSING-CHOICE-FIELD LINE 42 COLUMN 14 PIC X
               USING WS-GUESSING-LOSING-CHOICE   FOREGROUND-COLOR IS 0.
           
           01 WORD-GUESSING-WINNING-SCREEN
             BACKGROUND-COLOR IS 2.
             05 BLANK SCREEN.
        *>    WIN GAME HEADER
             05 LINE 1 COL 1  VALUE "   :                              
      -    "                                                         "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 1 COL 2 PIC X(2) USING WS-FORMATTED-HOUR 
             FOREGROUND-COLOR IS 7 REVERSE-VIDEO.
             05 LINE 1 COL 5 PIC X(2) USING WS-FORMATTED-MINS
             FOREGROUND-COLOR IS 7 REVERSE-VIDEO.
             05 LINE 1 COL 80 VALUE "CREDITS: "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 1 COL 89 PIC 9(3) USING WS-USERCREDITS
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
        *>    WIN GAME FOOTER
               05 LINE 43 COL 1 VALUE "                                 
      -    "                                                           "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 44 COL 1 VALUE "     (P) Play again     (H) High                        
      -    " scores                                                    "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 45 COL 1 VALUE "     (Q) Quit game 
      -    "                                                           "                                         
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 46 COL 1 VALUE "                                 
      -    "                                                           "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
        *>    WIN GAME BODY
             05 LINE  4 COL 10 VALUE "FriendFace"
             FOREGROUND-COLOR IS 0, UNDERLINE.
             05 LINE 4 COL 40 VALUE "GUESS THE WORD"                                     
             FOREGROUND-COLOR IS 0, UNDERLINE.
             05 LINE 12 COL 8 VALUE "                                   
      -    "                                         "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 12 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 13 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 13 COL 10 VALUE "                                   
      -    "                                        "
             FOREGROUND-COLOR IS 0, REVERSE-VIDEO.
             05 LINE 13 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 14 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 14 COL 10 VALUE "You guessed the word: "
             FOREGROUND-COLOR IS 0, REVERSE-VIDEO.
             05 LINE 14 COLUMN 32 PIC X(10) USING WS-ANSWERWORD
             FOREGROUND-COLOR IS 0, REVERSE-VIDEO.
             05 LINE 14 COL 42 VALUE "                                  
      -    "        " FOREGROUND-COLOR IS 0, REVERSE-VIDEO.  
             05 LINE 14 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 15 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 15 COL 10 VALUE "                                   
      -    "                                        "
             FOREGROUND-COLOR IS 0, REVERSE-VIDEO.
             05 LINE 15 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 16 COL 8 VALUE "                                   
      -    "                                           "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 16 COL 42 VALUE "YOU WON 10 CREDITS!"
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO, BLINK.
           
             05 LINE 17 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 17 COL 10 VALUE "                                   
      -    "                                          "
             FOREGROUND-COLOR IS 0, REVERSE-VIDEO.
             05 LINE 17 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 18 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 18 COL 10 VALUE "You scored: "                          "
             FOREGROUND-COLOR IS 0, REVERSE-VIDEO.
             05 LINE 18 COL 22 PIC 99 USING WS-HIGH-SCORE                           "
             FOREGROUND-COLOR IS 0, REVERSE-VIDEO.
             05 LINE 18 COL 24 VALUE "                                 
      -    "                          "
             FOREGROUND-COLOR IS 0, REVERSE-VIDEO.  
             05 LINE 18 COL 84 VALUE "  "           
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 19 COL 8 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 19 COL 10 VALUE "                                   
      -    "                                        "
             FOREGROUND-COLOR IS 0, REVERSE-VIDEO.
             05 LINE 19 COL 84 VALUE "  "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 20 COL 8 VALUE "                                   
      -    "                                           "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO. 
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
             05 LINE 1 COL 1  VALUE "   :                              
      -    "                                                         "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 1 COL 2 PIC X(2) USING WS-FORMATTED-HOUR 
             FOREGROUND-COLOR IS 7 REVERSE-VIDEO.
             05 LINE 1 COL 5 PIC X(2) USING WS-FORMATTED-MINS
             FOREGROUND-COLOR IS 7 REVERSE-VIDEO.
             05 LINE 1 COL 80 VALUE "CREDITS: "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 1 COL 89 PIC 9(3) USING WS-USERCREDITS
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
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
             05 LINE  4 COL 10 VALUE "FriendFace"
             FOREGROUND-COLOR IS 7, UNDERLINE.
             05 LINE 4 COL 40 VALUE "GUESS THE WORD"                                     
             FOREGROUND-COLOR IS 7, UNDERLINE.
             05 LINE 6 COLUMN 41 VALUE "Scoreboard:"
             HIGHLIGHT, FOREGROUND-COLOR IS 7.
        *>    TOP SCORER POSITION
             05 LINE 8 COLUMN 38 VALUE "    TOP SCORER     "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 9 COLUMN 38 VALUE "  " FOREGROUND-COLOR IS 7,
             REVERSE-VIDEO.
             05 LINE 9 COLUMN 40 VALUE " " FOREGROUND-COLOR IS 2,
             REVERSE-VIDEO.
             05 LINE 9 COLUMN 41 PIC XX USING WS-SCORE(1)
             FOREGROUND-COLOR IS 2, REVERSE-VIDEO, BLINK.
             05 LINE 9 COLUMN 43 VALUE "  " FOREGROUND-COLOR IS 2,
             REVERSE-VIDEO.
             05 LINE 9 COLUMN 45 PIC X(10) USING WS-NAME(1)
             FOREGROUND-COLOR IS 2, REVERSE-VIDEO, BLINK.
             05 LINE 9 COLUMN 55 VALUE "  " FOREGROUND-COLOR IS 7,
             REVERSE-VIDEO.
             05 LINE 10 COLUMN 38 VALUE "    HIGH SCORERS   " 
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
        *>    SCORER 2
             05 LINE 11 COLUMN 38 VALUE "  " FOREGROUND-COLOR IS 7,
             REVERSE-VIDEO.
             05 LINE 11 COLUMN 40 VALUE " " FOREGROUND-COLOR IS 6,
             REVERSE-VIDEO.
             05 LINE 11 COLUMN 41 PIC XX USING WS-SCORE(2)
             FOREGROUND-COLOR IS 6, REVERSE-VIDEO.
             05 LINE 11 COLUMN 43 VALUE "  " FOREGROUND-COLOR IS 6,
             REVERSE-VIDEO.
             05 LINE 11 COLUMN 45 PIC X(10) USING WS-NAME(2)
             FOREGROUND-COLOR IS 6, REVERSE-VIDEO.
             05 LINE 11 COLUMN 55 VALUE "  " FOREGROUND-COLOR IS 7,
             REVERSE-VIDEO.
             05 LINE 12 COLUMN 38 VALUE "  " FOREGROUND-COLOR IS 7,
             REVERSE-VIDEO.
             05 LINE 12 COLUMN 40 VALUE "               " 
             FOREGROUND-COLOR IS 6, REVERSE-VIDEO.
             05 LINE 12 COLUMN 55 VALUE "  " FOREGROUND-COLOR IS 7,
             REVERSE-VIDEO.
            *>    SCORER 3
             05 LINE 13 COLUMN 38 VALUE "  " FOREGROUND-COLOR IS 7,
             REVERSE-VIDEO.
             05 LINE 13 COLUMN 40 VALUE " " FOREGROUND-COLOR IS 6,
             REVERSE-VIDEO.
             05 LINE 13 COLUMN 41 PIC XX USING WS-SCORE(3)
             FOREGROUND-COLOR IS 6, REVERSE-VIDEO.
             05 LINE 13 COLUMN 43 VALUE "  " FOREGROUND-COLOR IS 6,
             REVERSE-VIDEO.
             05 LINE 13 COLUMN 45 PIC X(10) USING WS-NAME(3)
             FOREGROUND-COLOR IS 6, REVERSE-VIDEO.
             05 LINE 13 COLUMN 55 VALUE "  " FOREGROUND-COLOR IS 7,
             REVERSE-VIDEO.
             05 LINE 14 COLUMN 38 VALUE "  " FOREGROUND-COLOR IS 7,
             REVERSE-VIDEO.
             05 LINE 14 COLUMN 40 VALUE "               " 
             FOREGROUND-COLOR IS 6, REVERSE-VIDEO.
             05 LINE 14 COLUMN 55 VALUE "  " FOREGROUND-COLOR IS 7,
             REVERSE-VIDEO.
            *>    SCORER 4
             05 LINE 15 COLUMN 38 VALUE "  " FOREGROUND-COLOR IS 7,
             REVERSE-VIDEO.
             05 LINE 15 COLUMN 40 VALUE " " FOREGROUND-COLOR IS 6,
             REVERSE-VIDEO.
             05 LINE 15 COLUMN 41 PIC XX USING WS-SCORE(4)
             FOREGROUND-COLOR IS 6, REVERSE-VIDEO.
             05 LINE 15 COLUMN 43 VALUE "  " FOREGROUND-COLOR IS 6,
             REVERSE-VIDEO.
             05 LINE 15 COLUMN 45 PIC X(10) USING WS-NAME(4)
             FOREGROUND-COLOR IS 6, REVERSE-VIDEO.
             05 LINE 15 COLUMN 55 VALUE "  " FOREGROUND-COLOR IS 7,
             REVERSE-VIDEO.
             05 LINE 16 COLUMN 38 VALUE "  " FOREGROUND-COLOR IS 7,
             REVERSE-VIDEO.
             05 LINE 16 COLUMN 40 VALUE "               " 
             FOREGROUND-COLOR IS 6, REVERSE-VIDEO.
             05 LINE 16 COLUMN 55 VALUE "  " FOREGROUND-COLOR IS 7,
             REVERSE-VIDEO.
            *>    SCORER 5
             05 LINE 17 COLUMN 38 VALUE "  " FOREGROUND-COLOR IS 7,
             REVERSE-VIDEO.
             05 LINE 17 COLUMN 40 VALUE " " FOREGROUND-COLOR IS 6,
             REVERSE-VIDEO.
             05 LINE 17 COLUMN 41 PIC XX USING WS-SCORE(5)
             FOREGROUND-COLOR IS 6, REVERSE-VIDEO.
             05 LINE 17 COLUMN 43 VALUE "  " FOREGROUND-COLOR IS 6,
             REVERSE-VIDEO.
             05 LINE 17 COLUMN 45 PIC X(10) USING WS-NAME(5)
             FOREGROUND-COLOR IS 6, REVERSE-VIDEO.
             05 LINE 17 COLUMN 55 VALUE "  " FOREGROUND-COLOR IS 7,
             REVERSE-VIDEO.
             05 LINE 18 COLUMN 38 VALUE "  " FOREGROUND-COLOR IS 7,
             REVERSE-VIDEO.
             05 LINE 18 COLUMN 40 VALUE "               " 
             FOREGROUND-COLOR IS 6, REVERSE-VIDEO.
             05 LINE 18 COLUMN 55 VALUE "  " FOREGROUND-COLOR IS 7,
             REVERSE-VIDEO.
            *>    SCORER 6
             05 LINE 19 COLUMN 38 VALUE "  " FOREGROUND-COLOR IS 7,
             REVERSE-VIDEO.
             05 LINE 19 COLUMN 40 VALUE " " FOREGROUND-COLOR IS 6,
             REVERSE-VIDEO.
             05 LINE 19 COLUMN 41 PIC XX USING WS-SCORE(6)
             FOREGROUND-COLOR IS 6, REVERSE-VIDEO.
             05 LINE 19 COLUMN 43 VALUE "  " FOREGROUND-COLOR IS 6,
             REVERSE-VIDEO.
             05 LINE 19 COLUMN 45 PIC X(10) USING WS-NAME(6)
             FOREGROUND-COLOR IS 6, REVERSE-VIDEO.
             05 LINE 19 COLUMN 55 VALUE "  " FOREGROUND-COLOR IS 7,
             REVERSE-VIDEO.
             05 LINE 20 COLUMN 38 VALUE "  " FOREGROUND-COLOR IS 7,
             REVERSE-VIDEO.
             05 LINE 20 COLUMN 40 VALUE "               " 
             FOREGROUND-COLOR IS 6, REVERSE-VIDEO.
             05 LINE 20 COLUMN 55 VALUE "  " FOREGROUND-COLOR IS 7,
             REVERSE-VIDEO.
            *>    SCORER 7
             05 LINE 21 COLUMN 38 VALUE "  " FOREGROUND-COLOR IS 7,
             REVERSE-VIDEO.
             05 LINE 21 COLUMN 40 VALUE " " FOREGROUND-COLOR IS 6,
             REVERSE-VIDEO.
             05 LINE 21 COLUMN 41 PIC XX USING WS-SCORE(7)
             FOREGROUND-COLOR IS 6, REVERSE-VIDEO.
             05 LINE 21 COLUMN 43 VALUE "  " FOREGROUND-COLOR IS 6,
             REVERSE-VIDEO.
             05 LINE 21 COLUMN 45 PIC X(10) USING WS-NAME(7)
             FOREGROUND-COLOR IS 6, REVERSE-VIDEO.
             05 LINE 21 COLUMN 55 VALUE "  " FOREGROUND-COLOR IS 7,
             REVERSE-VIDEO.
             05 LINE 22 COLUMN 38 VALUE "  " FOREGROUND-COLOR IS 7,
             REVERSE-VIDEO.
             05 LINE 22 COLUMN 40 VALUE "               " 
             FOREGROUND-COLOR IS 6, REVERSE-VIDEO.
             05 LINE 22 COLUMN 55 VALUE "  " FOREGROUND-COLOR IS 7,
             REVERSE-VIDEO.
            *>    SCORER 8
             05 LINE 23 COLUMN 38 VALUE "  " FOREGROUND-COLOR IS 7,
             REVERSE-VIDEO.
             05 LINE 23 COLUMN 40 VALUE " " FOREGROUND-COLOR IS 6,
             REVERSE-VIDEO.
             05 LINE 23 COLUMN 41 PIC XX USING WS-SCORE(8)
             FOREGROUND-COLOR IS 6, REVERSE-VIDEO.
             05 LINE 23 COLUMN 43 VALUE "  " FOREGROUND-COLOR IS 6,
             REVERSE-VIDEO.
             05 LINE 23 COLUMN 45 PIC X(10) USING WS-NAME(8)
             FOREGROUND-COLOR IS 6, REVERSE-VIDEO.
             05 LINE 23 COLUMN 55 VALUE "  " FOREGROUND-COLOR IS 7,
             REVERSE-VIDEO.
             05 LINE 24 COLUMN 38 VALUE "  " FOREGROUND-COLOR IS 7,
             REVERSE-VIDEO.
             05 LINE 24 COLUMN 40 VALUE "               " 
             FOREGROUND-COLOR IS 6, REVERSE-VIDEO.
             05 LINE 24 COLUMN 55 VALUE "  " FOREGROUND-COLOR IS 7,
             REVERSE-VIDEO.
            *>    SCORER 9
             05 LINE 25 COLUMN 38 VALUE "  " FOREGROUND-COLOR IS 7,
             REVERSE-VIDEO.
             05 LINE 25 COLUMN 40 VALUE " " FOREGROUND-COLOR IS 6,
             REVERSE-VIDEO.
             05 LINE 25 COLUMN 41 PIC XX USING WS-SCORE(9)
             FOREGROUND-COLOR IS 6, REVERSE-VIDEO.
             05 LINE 25 COLUMN 43 VALUE "  " FOREGROUND-COLOR IS 6,
             REVERSE-VIDEO.
             05 LINE 25 COLUMN 45 PIC X(10) USING WS-NAME(9)
             FOREGROUND-COLOR IS 6, REVERSE-VIDEO.
             05 LINE 25 COLUMN 55 VALUE "  " FOREGROUND-COLOR IS 7,
             REVERSE-VIDEO.
             05 LINE 26 COLUMN 38 VALUE "  " FOREGROUND-COLOR IS 7,
             REVERSE-VIDEO.
             05 LINE 26 COLUMN 40 VALUE "               " 
             FOREGROUND-COLOR IS 6, REVERSE-VIDEO.
             05 LINE 26 COLUMN 55 VALUE "  " FOREGROUND-COLOR IS 7,
             REVERSE-VIDEO.
            *>    SCORER 10
             05 LINE 27 COLUMN 38 VALUE "  " FOREGROUND-COLOR IS 7,
             REVERSE-VIDEO.
             05 LINE 27 COLUMN 40 VALUE " " FOREGROUND-COLOR IS 6,
             REVERSE-VIDEO.
             05 LINE 27 COLUMN 41 PIC XX USING WS-SCORE(10)
             FOREGROUND-COLOR IS 6, REVERSE-VIDEO.
             05 LINE 27 COLUMN 43 VALUE "  " FOREGROUND-COLOR IS 6,
             REVERSE-VIDEO.
             05 LINE 27 COLUMN 45 PIC X(10) USING WS-NAME(10)
             FOREGROUND-COLOR IS 6, REVERSE-VIDEO.
             05 LINE 27 COLUMN 55 VALUE "  " FOREGROUND-COLOR IS 7,
             REVERSE-VIDEO.
             05 LINE 28 COLUMN 38 VALUE "                   " 
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
        *>    HIGH SCORE OPTION POSITIONING
               05 LINE 42 COLUMN 6 VALUE "Option: ".
               05 WS-HIGH-SCORE-CHOICE-FIELD LINE 42 COLUMN 14 PIC X
                  USING WS-HIGH-SCORE-CHOICE.

           01 TIC-TAC-TOE-SCREEN
             BACKGROUND-COLOR IS WS-BG-COLOR.
             05 BLANK SCREEN.
       *>    TIC-TAC-TOE HEADER
             05 LINE 1 COL 1  VALUE "   :                              
      -    "                                                         "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 1 COL 2 PIC X(2) USING WS-FORMATTED-HOUR 
             FOREGROUND-COLOR IS 7 REVERSE-VIDEO.
             05 LINE 1 COL 5 PIC X(2) USING WS-FORMATTED-MINS
             FOREGROUND-COLOR IS 7 REVERSE-VIDEO.
             05 LINE 1 COL 80 VALUE "CREDITS: "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 1 COL 89 PIC 9(3) USING WS-USERCREDITS
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
        *>    TIC-TAC-TOE FOOTER
               05 LINE 43 COL 1 VALUE "                                 
      -    "                                                           "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 44 COL 1 VALUE "     Make your move: e.g (A1) top                        
      -    " left (C3) bottom right                                    "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 45 COL 1 VALUE "     When prompted: (Y) Play agai  
      -    "n (N) Quit game                                            "                                         
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 46 COL 1 VALUE "                                 
      -    "                                                           "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
        *>    TIC-TAC-TOE BODY
             05 LINE  4 COL 10 VALUE "FriendFace"
             FOREGROUND-COLOR IS 7, UNDERLINE.
             05 LINE 4 COL 40 VALUE "TIC-TAC-TOE"                                     
             FOREGROUND-COLOR IS 7, UNDERLINE.          
               05 LINE 14 COLUMN 35 VALUE IS "   +---+---+---+   "
                   BACKGROUND-COLOR WS-BG FOREGROUND-COLOR WS-FG.
               05 LINE 15 COLUMN 35 VALUE IS " A |   |   |   |   "
                   BACKGROUND-COLOR WS-BG FOREGROUND-COLOR WS-FG.
               05 LINE 16 COLUMN 35 VALUE IS "   +---+---+---+   "
                   BACKGROUND-COLOR WS-BG FOREGROUND-COLOR WS-FG.
               05 LINE 17 COLUMN 35 VALUE IS " B |   |   |   |   "
                   BACKGROUND-COLOR WS-BG FOREGROUND-COLOR WS-FG.
               05 LINE 18 COLUMN 35 VALUE IS "   +---+---+---+   "
                   BACKGROUND-COLOR WS-BG FOREGROUND-COLOR WS-FG.
               05 LINE 19 COLUMN 35 VALUE IS " C |   |   |   |   "
                   BACKGROUND-COLOR WS-BG FOREGROUND-COLOR WS-FG.
               05 LINE 20 COLUMN 35 VALUE IS "   +---+---+---+   "
                   BACKGROUND-COLOR WS-BG FOREGROUND-COLOR WS-FG.
               05 LINE 21 COLUMN 35 VALUE IS "     1   2   3     "
                   BACKGROUND-COLOR WS-BG FOREGROUND-COLOR WS-FG.
               05 LINE 15 COLUMN 40 PIC A(1) FROM WS-CELL(1,1)
                   BACKGROUND-COLOR WS-BG FOREGROUND-COLOR WS-FG-CELL.
               05 LINE 15 COLUMN 44 PIC A(1) FROM WS-CELL(1,2)
                   BACKGROUND-COLOR WS-BG FOREGROUND-COLOR WS-FG-CELL.
               05 LINE 15 COLUMN 48 PIC A(1) FROM WS-CELL(1,3)
                   BACKGROUND-COLOR WS-BG FOREGROUND-COLOR WS-FG-CELL.
               05 LINE 17 COLUMN 40 PIC A(1) FROM WS-CELL(2,1)
                   BACKGROUND-COLOR WS-BG FOREGROUND-COLOR WS-FG-CELL.
               05 LINE 17 COLUMN 44 PIC A(1) FROM WS-CELL(2,2)
                   BACKGROUND-COLOR WS-BG FOREGROUND-COLOR WS-FG-CELL.
               05 LINE 17 COLUMN 48 PIC A(1) FROM WS-CELL(2,3)
                   BACKGROUND-COLOR WS-BG FOREGROUND-COLOR WS-FG-CELL.
               05 LINE 19 COLUMN 40 PIC A(1) FROM WS-CELL(3,1)
                   BACKGROUND-COLOR WS-BG FOREGROUND-COLOR WS-FG-CELL.
               05 LINE 19 COLUMN 44 PIC A(1) FROM WS-CELL(3,2)
                   BACKGROUND-COLOR WS-BG FOREGROUND-COLOR WS-FG-CELL.
               05 LINE 19 COLUMN 48 PIC A(1) FROM WS-CELL(3,3)
                   BACKGROUND-COLOR WS-BG FOREGROUND-COLOR WS-FG-CELL.
               05 LINE 23 COLUMN 30 VALUE IS "Message: "
                   FOREGROUND-COLOR IS 6.
                   05 MSG PIC X(128) FROM WS-OANDXMESSAGE.
               05 LINE 25 COLUMN 30 PIC X(16) FROM WS-INSTRUCTION.
                   05 WS-NEXT-MOVE-FIELD PIC X(2) USING WS-NEXT-MOVE.
               05 LINE 27 COLUMN 30 VALUE IS "Stats: "
                   FOREGROUND-COLOR IS 6.
               05 LINE 28 COLUMN 30 VALUE IS "Moves played = "
                   FOREGROUND-COLOR IS 2.
                   05 MOVES PIC 9 FROM WS-MOVES.
               05 LINE 29 COLUMN 30 VALUE IS "Games won = "
                   FOREGROUND-COLOR IS 5.
                   05 WINS PIC 9(2) FROM WS-WINS.
               05 LINE 29 COLUMN 44 VALUE IS "/".
                   05 GAMES PIC 9(2) FROM WS-GAMES. 
           
           01 GUESS-THE-NUMBER-GAME-SCREEN
             BACKGROUND-COLOR IS WS-BG-COLOR.
             05 BLANK SCREEN.
        *>   NUMBER GUESSING GAME HEADER
             05 LINE 1 COL 1  VALUE "   :                              
      -    "                                                         "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 1 COL 2 PIC X(2) USING WS-FORMATTED-HOUR 
             FOREGROUND-COLOR IS 7 REVERSE-VIDEO.
             05 LINE 1 COL 5 PIC X(2) USING WS-FORMATTED-MINS
             FOREGROUND-COLOR IS 7 REVERSE-VIDEO.
             05 LINE 1 COL 80 VALUE "CREDITS: "
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
             05 LINE 1 COL 89 PIC 9(3) USING WS-USERCREDITS
             FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
        *>    NUMBER GUESSING GAME FOOTER
               05 LINE 43 COL 1 VALUE "                                 
      -    "                                                           "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 44 COL 1 VALUE "     Guess a number between 1 and                     
      -    " 10 in 3 goes or less to win 10 Creidts!                   "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 45 COL 1 VALUE "                    
      -    "                                                           "                                         
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 46 COL 1 VALUE "                                 
      -    "                                                           "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO. 
        *>    NUMBER GUESSING GAME BODY
             05 LINE  4 COL 10 VALUE "FriendFace"
             FOREGROUND-COLOR IS WS-FG-COLOR, UNDERLINE.
             05 LINE 4 COL 40 VALUE "GUESS THE NUMBER"                                     
             FOREGROUND-COLOR IS WS-FG-COLOR, UNDERLINE.
             
             05 LINE 13 COL 8 VALUE "                       GUESS A NUMB                 
      -    "ER BETWEEN 1 AND 10!                       "
             FOREGROUND-COLOR IS WS-GTN-FG-COLOR, REVERSE-VIDEO.
             05 LINE 13 COL 84 VALUE "  "
             FOREGROUND-COLOR IS WS-GTN-FG-COLOR, REVERSE-VIDEO.

             05 LINE 14 COL 8 VALUE "  "
             FOREGROUND-COLOR IS WS-GTN-FG-COLOR, REVERSE-VIDEO.
             05 LINE 14 COL 10 VALUE "Message:       "
             FOREGROUND-COLOR IS WS-GTN-BG-COLOR, REVERSE-VIDEO.
             05 MSG PIC X(34) FROM WS-RANDOM-NUM-MSG
             FOREGROUND-COLOR IS WS-GTN-BG-COLOR, REVERSE-VIDEO.
             05 LINE 14 COL 59 VALUE "                         "
             FOREGROUND-COLOR IS WS-GTN-BG-COLOR, REVERSE-VIDEO.  
             05 LINE 14 COL 84 VALUE "  "
             FOREGROUND-COLOR IS WS-GTN-FG-COLOR, REVERSE-VIDEO.

             05 LINE 15 COL 8 VALUE "  "
             FOREGROUND-COLOR IS WS-GTN-FG-COLOR, REVERSE-VIDEO.
             05 LINE 15 COL 10 VALUE "                                   
      -    "                                        "
             FOREGROUND-COLOR IS WS-GTN-BG-COLOR, REVERSE-VIDEO.
             05 LINE 15 COL 10 VALUE "Guess: "
             FOREGROUND-COLOR IS WS-GTN-BG-COLOR, REVERSE-VIDEO.
             05 GUESS-INPUT-FIELD LINE 15 COLUMN 25 PIC XX 
             USING GUESS-INPUT FOREGROUND-COLOR IS WS-GTN-BG-COLOR, 
             REVERSE-VIDEO.           
             05 LINE 15 COL 84 VALUE "  "
             FOREGROUND-COLOR IS WS-GTN-FG-COLOR, REVERSE-VIDEO.

             05 LINE 16 COL 8 VALUE "                                   
      -    "STATS                                      "
             FOREGROUND-COLOR IS WS-GTN-FG-COLOR, REVERSE-VIDEO.

             05 LINE 17 COL 8 VALUE "  "
             FOREGROUND-COLOR IS WS-GTN-FG-COLOR, REVERSE-VIDEO.
             05 LINE 17 COL 10 VALUE "Total Guesses: "
             FOREGROUND-COLOR IS WS-GTN-BG-COLOR, REVERSE-VIDEO.
             05 GUESSES PIC 99 FROM TOTAL-GUESSES 
             FOREGROUND-COLOR IS WS-GTN-BG-COLOR, REVERSE-VIDEO.
             05 LINE 17 COL 27 VALUE "                                  
      -    "                       " FOREGROUND-COLOR IS 
             WS-GTN-BG-COLOR, 
             REVERSE-VIDEO.  
             05 LINE 17 COL 84 VALUE "  "
             FOREGROUND-COLOR IS WS-GTN-FG-COLOR, REVERSE-VIDEO.

             05 LINE 18 COL 8 VALUE "  "
             FOREGROUND-COLOR IS WS-GTN-FG-COLOR, REVERSE-VIDEO.
             05 LINE 18 COL 10 VALUE "                                   
      -    "                                          "
             FOREGROUND-COLOR IS WS-GTN-FG-COLOR, REVERSE-VIDEO.
               
                        
      ************************END OF SCREEN SECTION********************* 
           
       PROCEDURE DIVISION.
      ******************************************************************
      *-----**************LOGIN / SIGN-IN/UP SECTION-----***************
      ******************************************************************
       0100-DISPLAY-LOGIN.
           PERFORM 0200-TIME-AND-DATE.
           INITIALIZE LOGIN-CHOICE. 
           DISPLAY LOGIN-SCREEN.
           ACCEPT LOGIN-CHOICE-FIELD.
           IF LOGIN-CHOICE = "l" OR "L" THEN 
               PERFORM 0101-SIGN-IN 
           ELSE IF LOGIN-CHOICE = "c" OR "C" THEN 
               PERFORM 0102-SIGN-UP
           ELSE IF LOGIN-CHOICE = "q" OR "Q" THEN 
               STOP RUN
           ELSE 
               PERFORM 0110-DISPLAY-MENU
           END-IF.

       0101-SIGN-IN.
           PERFORM 0200-TIME-AND-DATE.
           INITIALIZE WS-USERNAME.
           INITIALIZE WS-PASSWORD.
           DISPLAY SIGN-IN-SCREEN.
           ACCEPT WS-USERNAME-FIELD.
           ACCEPT WS-PASSWORD-FIELD.
           
           CALL "sign-in" USING WS-USERNAME, WS-PASSWORD, 
           WS-LOGIN-CORRECT.
           IF WS-LOGIN-CORRECT = 1 THEN
               PERFORM 0110-DISPLAY-MENU 
           ELSE 
               MOVE "Incorrect Username or Password!" 
               TO WS-ERROR-MSG
               PERFORM 0109-ERROR-PAGE 
           END-IF. 

       0102-SIGN-UP.
           PERFORM 0200-TIME-AND-DATE.
           INITIALIZE WS-NEW-USER-NAME.
           INITIALIZE WS-NEW-PASSWORD.
           INITIALIZE CREATE-CHOICE
           DISPLAY CREATE-AN-ACCOUNT-SCREEN.
           ACCEPT WS-NEW-USER-NAME-FIELD.
           ACCEPT WS-NEW-PASSWORD-FIELD.
           ACCEPT CREATE-CHOICE-FIELD.
           
           IF CREATE-CHOICE = "q" OR "Q" THEN 
               PERFORM 0100-DISPLAY-LOGIN   
           ELSE IF CREATE-CHOICE = "s" OR "S" THEN
               PERFORM 0103-SIGN-UP-CHECK
           ELSE IF CREATE-CHOICE = "d" OR "D" OR "g" OR "G" THEN
               PERFORM 0101-SIGN-IN
           END-IF.  

           PERFORM 0102-SIGN-UP. 

       0103-SIGN-UP-CHECK.
           
           IF WS-NEW-USER-NAME = " "
               MOVE "Invalid Username! Try again." TO WS-ERROR-MSG
               PERFORM 0109-ERROR-PAGE
           ELSE IF WS-NEW-PASSWORD = " "
               MOVE "Invalid Password! Try again." TO WS-ERROR-MSG
               PERFORM 0109-ERROR-PAGE
           END-IF.    
           
           CALL "sign-up-check" USING WS-NEW-USER-NAME 
               WS-UNAME-UNAVAILABLE.
           IF WS-UNAME-UNAVAILABLE = 1 THEN
               MOVE "Username Taken" TO WS-ERROR-MSG
               PERFORM 0109-ERROR-PAGE
           ELSE
               CALL "sign-up" USING WS-NEW-USER-NAME WS-NEW-PASSWORD
               PERFORM 0101-SIGN-IN
           END-IF.

       0109-ERROR-PAGE.
           PERFORM 0200-TIME-AND-DATE.
           PERFORM 0132-CREDIT-TOTAL.
           INITIALIZE ERROR-CHOICE.
           DISPLAY ERROR-SCREEN.
           ACCEPT ERROR-CHOICE-FIELD.
           IF ERROR-CHOICE = "l" OR "L" THEN 
               PERFORM 0101-SIGN-IN
           ELSE IF ERROR-CHOICE = "c" OR "C" THEN 
               PERFORM 0102-SIGN-UP
           ELSE IF ERROR-CHOICE = "q" OR "Q" THEN 
               STOP RUN
           ELSE 
               PERFORM 0109-ERROR-PAGE 
           END-IF.
      ****************************************************************** 
      ********-----DISPLAY MENU COMES AFTER SUCCESFUL LOGIN----*********
      ******************************************************************
       0110-DISPLAY-MENU.
           PERFORM 0200-TIME-AND-DATE.
           PERFORM 0132-CREDIT-TOTAL.
          *>  Reset message pagination idx
           MOVE 1 TO ID-NUM.
           INITIALIZE MENU-CHOICE.
           DISPLAY MENU-SCREEN.
           ACCEPT MENU-CHOICE-FIELD.
           IF MENU-CHOICE =        "q" or "Q" THEN
             STOP RUN
           ELSE IF MENU-CHOICE =   "l" or "L" THEN
             PERFORM 0100-DISPLAY-LOGIN
           ELSE IF MENU-CHOICE =   "m" or "M" THEN
             PERFORM 0140-MESSAGE-MENU
           ELSE IF MENU-CHOICE =   "f" or "F" THEN
             PERFORM 0400-GAMES-MENU
           ELSE IF MENU-CHOICE =   "a" or "A" THEN
             PERFORM 0125-USER-ACCOUNT-MENU
           ELSE IF MENU-CHOICE =   "c" or "C" THEN
               PERFORM 0130-CREDIT-STORE 
           END-IF.
           PERFORM 0110-DISPLAY-MENU.      
      ******************************************************************   
      ********************----ACCOUNT/BANK SECTION----******************
      ******************************************************************
       0120-BANK-DETAILS.    
           PERFORM 0200-TIME-AND-DATE.
           PERFORM 0132-CREDIT-TOTAL.
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
               PERFORM 0121-UPDATE-BANK-DETAILS
           ELSE IF BANK-ACCOUNT-CHOICE = "d" or "D" then
               PERFORM 0120-BANK-DETAILS
           ELSE IF BANK-ACCOUNT-CHOICE = "q" or "Q" THEN
               STOP RUN
           ELSE IF BANK-ACCOUNT-CHOICE = "g" or "G" THEN
               PERFORM 0125-USER-ACCOUNT-MENU
           END-IF.  

       0121-UPDATE-BANK-DETAILS.
           MOVE CARD-NO TO WS-CARD-NO.
           MOVE CARD-EXPIRY TO WS-CARD-EXPIRY.
           MOVE CARD-CVV TO WS-CARD-CVV.
           CALL "bank-details" USING WS-USERNAME, WS-CARD-NO,
           WS-CARD-EXPIRY, WS-CARD-CVV.
           
           PERFORM 0125-USER-ACCOUNT-MENU.

       0125-USER-ACCOUNT-MENU.
           PERFORM 0200-TIME-AND-DATE.
           PERFORM 0132-CREDIT-TOTAL.
           PERFORM 0126-CHECK-ACCOUNT-STATUS.
           PERFORM 0600-CHECK-BANK-DETAILS-PRESENT.
           PERFORM 0620-GET-EXPIRY-DATE.
           INITIALIZE ACCOUNT-CHOICE.
           DISPLAY USER-ACCOUNT-SCREEN.
           ACCEPT ACCOUNT-CHOICE-FIELD.
           IF ACCOUNT-CHOICE =     "q" or "Q" THEN
               STOP RUN
           ELSE IF ACCOUNT-CHOICE = "b" or "B" THEN
               PERFORM 0120-BANK-DETAILS  
           ELSE IF ACCOUNT-CHOICE = "g" or "G" THEN
               PERFORM 0130-CREDIT-STORE
           ELSE IF ACCOUNT-CHOICE = "p" or "P" THEN
               PERFORM 0127-UPDATE-PASSWORD 
           END-IF.

           PERFORM 0125-USER-ACCOUNT-MENU.

       0126-CHECK-ACCOUNT-STATUS.
           CALL "account-status-check" USING WS-USERNAME, 
           WS-USERACCOUNTLEVEL. 

       0127-UPDATE-PASSWORD.
           PERFORM 0200-TIME-AND-DATE.
           PERFORM 0132-CREDIT-TOTAL.
           INITIALIZE WS-UPDATE-PASSWORD.
           INITIALIZE UPDATE-PASSWORD-CHOICE.
           DISPLAY UPDATE-PASSWORD-SCREEN.
           ACCEPT WS-UPDATE-PASSWORD-FIELD.
           ACCEPT UPDATE-PASSWORD-CHOICE-FIELD.
           IF UPDATE-PASSWORD-CHOICE = "s" OR "S"
               IF WS-UPDATE-PASSWORD = " "
                 MOVE "Invalid Password Try Another" TO WS-ERROR-MSG
                 PERFORM 0109-ERROR-PAGE
               ELSE
                 CALL "update-password" USING WS-USERNAME, 
                 WS-UPDATE-PASSWORD
                 PERFORM 0125-USER-ACCOUNT-MENU
               END-IF  
           ELSE IF UPDATE-PASSWORD-CHOICE = "D" OR "d"
               PERFORM 0127-UPDATE-PASSWORD
           ELSE IF UPDATE-PASSWORD-CHOICE = "G" OR "g"
               PERFORM 0125-USER-ACCOUNT-MENU
           ELSE
               PERFORM 0127-UPDATE-PASSWORD
           END-IF.  

      ******************************************************************   
      **********************----CREDITS SECTION----*********************
      ******************************************************************
       0130-CREDIT-STORE.
           MOVE 0 TO WS-UPDATE-CREDITS.
           
           PERFORM 0200-TIME-AND-DATE.
           PERFORM 0132-CREDIT-TOTAL.
           PERFORM 0600-CHECK-BANK-DETAILS-PRESENT.
           
           INITIALIZE CREDIT-STORE-CHOICE.
           DISPLAY CREDIT-STORE-SCREEN.
           ACCEPT CREDIT-STORE-CHOICE-FIELD.

           IF WS-BNK-DTLS-PRESENT = "YES" THEN
           
               IF CREDIT-STORE-CHOICE = "1" THEN
                   MOVE 10 TO WS-UPDATE-CREDITS
                   MOVE 10 TO WS-STORE-CHARGE
                   PERFORM 0131-ADD-CREDITS
       
               ELSE IF CREDIT-STORE-CHOICE = "2" THEN
                   MOVE 25 TO WS-UPDATE-CREDITS
                   MOVE 20 TO WS-STORE-CHARGE
                   PERFORM 0131-ADD-CREDITS
       
               ELSE IF CREDIT-STORE-CHOICE = "3" THEN
                   MOVE 50 TO WS-UPDATE-CREDITS
                   MOVE 35 TO WS-STORE-CHARGE 
                   PERFORM 0131-ADD-CREDITS

               ELSE IF CREDIT-STORE-CHOICE = "4" THEN
                   MOVE 100 TO WS-UPDATE-CREDITS
                   MOVE 60 TO WS-STORE-CHARGE 
                   PERFORM 0131-ADD-CREDITS    

               ELSE IF CREDIT-STORE-CHOICE = "v" or "V" THEN
                   PERFORM 0135-VIP-ACCOUNT
       
               ELSE IF CREDIT-STORE-CHOICE = "g" OR "G" THEN
                  PERFORM 0110-DISPLAY-MENU  
               ELSE IF CREDIT-STORE-CHOICE = "q" OR "Q" THEN
                  STOP RUN  
               END-IF

           ELSE IF WS-BNK-DTLS-PRESENT = "NO" THEN
               MOVE "NO BANK DETAILS, PLEASE ADD TO BUY" 
               TO WS-ERROR-MSG
               PERFORM 0136-CREDIT-ERROR-PAGE
           END-IF.         
       
       0131-ADD-CREDITS.
           CALL "add-credits" USING WS-USERNAME, WS-UPDATE-CREDITS.
           
           PERFORM 0300-TRANSACTIONS.

       0132-CREDIT-TOTAL.
           CALL 'find-credits' USING WS-USERNAME, WS-USERCREDITS.

       0133-CHECK-CREDIT-BALANCE.
           MOVE "N" TO WS-BALANCE-AVAILABLE.
           PERFORM 0132-CREDIT-TOTAL.
           IF WS-UPDATE-CREDITS <= WS-USERCREDITS
               MOVE "Y" TO WS-BALANCE-AVAILABLE
           END-IF.

       0135-VIP-ACCOUNT.
           MOVE 0   TO WS-UPDATE-CREDITS.
           MOVE 100 TO WS-UPDATE-CREDITS.
           PERFORM 0133-CHECK-CREDIT-BALANCE.
           
           IF WS-BALANCE-AVAILABLE = "Y" THEN
               CALL "subtract-credits" USING WS-USERNAME, 
               WS-UPDATE-CREDITS
               CALL "account-status" USING WS-USERNAME
           ELSE IF WS-BALANCE-AVAILABLE = "N" THEN
               MOVE "Insufficient Credits" TO WS-ERROR-MSG
               PERFORM 0136-CREDIT-ERROR-PAGE
           END-IF.

       0136-CREDIT-ERROR-PAGE.
           PERFORM 0200-TIME-AND-DATE.
           PERFORM 0132-CREDIT-TOTAL.
           INITIALIZE CREDIT-ERROR-CHOICE.
           DISPLAY CREDIT-ERROR-SCREEN.
           ACCEPT CREDIT-ERROR-CHOICE-FIELD.
           IF CREDIT-ERROR-CHOICE = "c" OR "C" THEN 
               PERFORM 0130-CREDIT-STORE
           ELSE IF CREDIT-ERROR-CHOICE = "a" OR "A" THEN 
               PERFORM 0125-USER-ACCOUNT-MENU
           ELSE IF CREDIT-ERROR-CHOICE = "q" OR "Q" THEN 
               STOP RUN
           ELSE IF CREDIT-ERROR-CHOICE-FIELD = "m" OR "M" THEN
               PERFORM 0110-DISPLAY-MENU
           ELSE 
               PERFORM 0136-CREDIT-ERROR-PAGE 
           END-IF.    
      ******************************************************************
      *******************-----MESSAGE SECTION----***********************
      ******************************************************************
       0140-MESSAGE-MENU.
           PERFORM 0126-CHECK-ACCOUNT-STATUS.
           PERFORM 0132-CREDIT-TOTAL.
           PERFORM 0200-TIME-AND-DATE.

           CALL "number-of-messages" USING NUM-OF-MESSAGES.
           CALL "list-all-messages" USING WS-LIST-TABLE.
           CALL 'count-comments-posted' USING COMMENT-TOTAL-TABLE.
           MOVE " " TO SP-ENTRY(1).
           MOVE " " TO SP-ENTRY(2).
           CALL "get-sponsored-posts" USING WS-FORMATTED-DT,
           SPONSORED-POSTS-TABLE, WS-SP-TABLE-COUNTER.

           INITIALIZE MSG-MENU-CHOICE.
           DISPLAY MSG-MENU-SCREEN.
           ACCEPT MSG-MENU-CHOICE-FIELD.
           MOVE MSG-MENU-CHOICE TO MSG-SELECT.
         
           IF MSG-SELECT > 0 AND NOT > NUM-OF-MESSAGES THEN
               PERFORM 0141-MESSAGE-VIEW
           END-IF. 

           IF MSG-MENU-CHOICE =        "g" OR "G" THEN
               PERFORM 0110-DISPLAY-MENU
           ELSE IF MSG-MENU-CHOICE =   "n" OR "N" THEN
             COMPUTE ID-NUM = ID-NUM + 10
               IF ID-NUM IS GREATER THAN OR EQUAL TO NUM-OF-MESSAGES
                 COMPUTE ID-NUM = ID-NUM - 10
                 PERFORM 0140-MESSAGE-MENU
               ELSE
                   PERFORM 0140-MESSAGE-MENU
               END-IF               
               
           ELSE IF MSG-MENU-CHOICE =       "p" OR "P" THEN
             COMPUTE ID-NUM = ID-NUM - 10
               
               IF ID-NUM IS LESS THAN 10
                   MOVE 1 TO ID-NUM
                    PERFORM 0140-MESSAGE-MENU
               ELSE
                    PERFORM 0140-MESSAGE-MENU
               END-IF
             
           ELSE IF MSG-MENU-CHOICE =       "c" OR "C"
             PERFORM 0130-CREDIT-STORE  
           ELSE IF MSG-MENU-CHOICE =       "q" OR "Q" THEN
              STOP RUN  
           END-IF.

           IF MSG-MENU-CHOICE = "s1" OR "S1" OR "s2" OR "S2" THEN
               PERFORM 0145-SP-MESSAGE-VIEW
           END-IF.

           IF MSG-MENU-CHOICE =       "w" OR "W"
             IF WS-USERACCOUNTLEVEL = "STD" AND WS-USERCREDITS = 0
               MOVE "Insufficient Credits" TO WS-ERROR-MSG
               PERFORM 0136-CREDIT-ERROR-PAGE
             END-IF  
               PERFORM 0142-MESSAGE-WRITE       
           END-IF

           PERFORM 0140-MESSAGE-MENU.

       0141-MESSAGE-VIEW. 
           PERFORM 0200-TIME-AND-DATE.  
           PERFORM 0132-CREDIT-TOTAL.
           CALL "number-of-messages" USING NUM-OF-MESSAGES.
           CALL "list-all-messages" USING WS-LIST-TABLE.
           CALL "count-comments-posted" USING COMMENT-TOTAL-TABLE.
           MOVE LIST-CONTENT(MSG-SELECT) TO WS-CONTENT-DISPLAY.
           INITIALIZE MSG-VIEW-CHOICE.
           DISPLAY MESSAGE-VIEW-SCREEN.
           ACCEPT MSG-VIEW-CHOICE-FIELD.
           IF MSG-VIEW-CHOICE =        "n" OR "N" THEN
             COMPUTE MSG-SELECT = MSG-SELECT + 1
               IF MSG-SELECT IS GREATER THAN OR EQUAL TO NUM-OF-MESSAGES
                 COMPUTE MSG-SELECT = MSG-SELECT - 1
                 PERFORM 0141-MESSAGE-VIEW
               ELSE
                   PERFORM 0141-MESSAGE-VIEW
               END-IF                
           ELSE IF MSG-VIEW-CHOICE =   "p" OR "P" THEN
             COMPUTE MSG-SELECT = MSG-SELECT - 1
               IF MSG-SELECT IS LESS THAN 1
                 COMPUTE MSG-SELECT = MSG-SELECT + 1
               ELSE
                   PERFORM 0141-MESSAGE-VIEW
               END-IF 
           ELSE IF MSG-VIEW-CHOICE =   "g" OR "G" THEN
               PERFORM 0140-MESSAGE-MENU
           ELSE IF MSG-VIEW-CHOICE =   "q" OR "Q" THEN
              STOP RUN  
           END-IF.

           IF MSG-VIEW-CHOICE = "c" OR "C"
             MOVE 1 TO COM-INDEX
             PERFORM 0143-COMMENT-SCREEN
           END-IF 
           .
           
           PERFORM 0141-MESSAGE-VIEW. 

       0142-MESSAGE-WRITE.
           PERFORM 0126-CHECK-ACCOUNT-STATUS.
           PERFORM 0200-TIME-AND-DATE.
           PERFORM 0132-CREDIT-TOTAL.
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
             OR "S" OR "p" OR "P" OR "q" OR "Q"
             ACCEPT MSG-WRITE-CHOICE-FIELD
           END-PERFORM.

           IF MSG-WRITE-CHOICE-FIELD = "d" OR "D" THEN
               PERFORM 0140-MESSAGE-MENU 
           END-IF.

           IF MSG-WRITE-CHOICE-FIELD = "p" OR "P" THEN 
              MOVE WS-CONTENT-DISPLAY TO WS-CONTENT
              MOVE WS-USERNAME TO WS-MSG-AUTHOR
              MOVE WS-FORMATTED-DTE-TME TO WS-POST-DATE
                IF WS-TITLE-FIELD NOT = SPACE AND LOW-VALUE THEN
                   IF WS-USERACCOUNTLEVEL = "STD"
                     MOVE 0 TO WS-UPDATE-CREDITS
                     MOVE 1 TO WS-UPDATE-CREDITS
                     PERFORM 0133-CHECK-CREDIT-BALANCE
           
                     IF WS-BALANCE-AVAILABLE = "Y" THEN
                       CALL "subtract-credits" USING WS-USERNAME, 
                           WS-UPDATE-CREDITS
                     ELSE
                       MOVE "Insufficient Credits" TO WS-ERROR-MSG
                       PERFORM 0136-CREDIT-ERROR-PAGE
                     END-IF 
                   END-IF
                   CALL "post-message" USING NEW-MESSAGE
                  PERFORM 0140-MESSAGE-MENU
                END-IF  
           END-IF.

           IF MSG-WRITE-CHOICE-FIELD = "s" OR "S" THEN 
              PERFORM 0147-SP-COUNTER
              IF WS-SP-COUNTER = 0 OR 1 THEN
                MOVE WS-CONTENT-DISPLAY TO WS-CONTENT
                MOVE WS-USERNAME TO WS-MSG-AUTHOR
                IF WS-TITLE-FIELD NOT = SPACE AND LOW-VALUE THEN
                    PERFORM 0146-SPONSORED-MESSAGES
                END-IF
              ELSE 
                MOVE "NO SPONSORED POSTS LEFT TODAY" TO WS-ERROR-MSG
                PERFORM 0148-SP-ERROR-SCREEN
              END-IF  
           END-IF.

           IF MSG-WRITE-CHOICE-FIELD = "q" OR "Q" THEN
             STOP RUN
           END-IF.
           PERFORM 0110-DISPLAY-MENU.

       0143-COMMENT-SCREEN.
           PERFORM 0126-CHECK-ACCOUNT-STATUS.
           PERFORM 0201-CURRENT-DATE.
           PERFORM 0132-CREDIT-TOTAL.
           CALL "num-comments" USING NUM-COMMENTS.
           CALL "get-comment" USING COMMENT-TABLE MSG-SELECT.
           IF COM-INDEX < 5
             MOVE 1 TO COM-INDEX
           END-IF.
           
           INITIALIZE COM-SCRN-CHOICE.
           DISPLAY COMMENT-SCREEN.
           ACCEPT COM-SCRN-CHOICE-FIELD.
       
           IF COM-SCRN-CHOICE-FIELD = "n" OR "N" THEN
             ADD 5 TO COM-INDEX
             IF COM-COMMENT(COM-INDEX) = SPACES
               SUBTRACT 5 FROM COM-INDEX
               PERFORM 0143-COMMENT-SCREEN
             ELSE
               PERFORM 0143-COMMENT-SCREEN
             END-IF
           END-IF.

           IF COM-SCRN-CHOICE-FIELD = "p" OR "P" THEN
             SUBTRACT 5 FROM COM-INDEX
             PERFORM 0143-COMMENT-SCREEN
           END-IF.

           IF COM-SCRN-CHOICE-FIELD = "g" OR "G" THEN
             PERFORM 0141-MESSAGE-VIEW
           ELSE IF COM-SCRN-CHOICE-FIELD = "q" OR "Q" THEN
             STOP RUN
           END-IF.

           IF COM-SCRN-CHOICE-FIELD = "c" OR "C"
             IF WS-USERACCOUNTLEVEL = 'VIP'
               PERFORM 0144-COMMENT-WRITE
             ELSE
               MOVE 'Upgrade account to comment' TO 
               WS-ERROR-MSG
               PERFORM 0156-COMMENT-ERROR
             END-IF
           END-IF
           PERFORM 0143-COMMENT-SCREEN.

       0144-COMMENT-WRITE.
           PERFORM 0200-TIME-AND-DATE.
           PERFORM 0132-CREDIT-TOTAL.
           INITIALIZE WRITE-COMMENT.
           INITIALIZE POST-COMMENT-CHOICE.
           DISPLAY WRITE-COMMENT-SCREEN.
           
            
           ACCEPT WRITE-COMMENT-FIELD.
           ACCEPT POST-COMMENT-CHOICE-FIELD.
           PERFORM UNTIL POST-COMMENT-CHOICE-FIELD = "d" OR "D" OR "p"
               OR "P" OR "q" OR "Q"
             ACCEPT POST-COMMENT-CHOICE-FIELD
           END-PERFORM.

           IF POST-COMMENT-CHOICE-FIELD = "d" OR "D" THEN
               PERFORM 0143-COMMENT-SCREEN
           END-IF.

           IF POST-COMMENT-CHOICE-FIELD = "p" OR "P" THEN 
              MOVE WS-USERNAME TO POST-COMMENT-AUTHOR
              MOVE WS-FORMATTED-DTE-TME TO POST-COMMENT-DATE
              IF WRITE-COMMENT NOT = SPACE
                CALL "post-comment" USING MSG-SELECT, POST-COM-TBL
                PERFORM 0143-COMMENT-SCREEN
              END-IF    
           END-IF.
           IF POST-COMMENT-CHOICE-FIELD = "q" OR "Q" THEN
             STOP RUN
           END-IF.

           PERFORM 0143-COMMENT-SCREEN.

       0145-SP-MESSAGE-VIEW.   
           PERFORM 0200-TIME-AND-DATE.  
           PERFORM 0132-CREDIT-TOTAL.
           IF MSG-MENU-CHOICE = "S1" or "s1"
               MOVE 1 TO SP-MSG-SELECT
           ELSE IF MSG-MENU-CHOICE = "S2" OR "s2"
               MOVE 2 TO SP-MSG-SELECT
           END-IF.

           MOVE SP-CONTENT(SP-MSG-SELECT) TO WS-SP-CONTENT-DISPLAY.
           INITIALIZE SP-MSG-VIEW-CHOICE.
           DISPLAY SP-MESSAGE-VIEW-SCREEN.
           ACCEPT SP-MSG-VIEW-CHOICE-FIELD.
           IF SP-MSG-VIEW-CHOICE =   "g" OR "G" THEN
               PERFORM 0140-MESSAGE-MENU  
           END-IF.    

       0146-SPONSORED-MESSAGES.
           PERFORM 0200-TIME-AND-DATE.
           PERFORM 0126-CHECK-ACCOUNT-STATUS.
           MOVE 0 TO WS-UPDATE-CREDITS.
           MOVE 10 TO WS-UPDATE-CREDITS.
           PERFORM 0133-CHECK-CREDIT-BALANCE.
           IF WS-USERACCOUNTLEVEL = "VIP" THEN
               IF WS-BALANCE-AVAILABLE = "Y" THEN
                   CALL "subtract-credits" USING WS-USERNAME, 
                   WS-UPDATE-CREDITS
                   CALL "post-sponsored-message" USING WS-FORMATTED-DT,
                   NEW-MESSAGE
               ELSE IF WS-BALANCE-AVAILABLE = "N" THEN
                   MOVE "Insufficent Credits" TO WS-ERROR-MSG
                   PERFORM 0136-CREDIT-ERROR-PAGE
               END-IF 
           ELSE IF WS-USERACCOUNTLEVEL = "STD" THEN
               MOVE "UPGRADE ACCOUNT TO SPONSOR POSTS" TO WS-ERROR-MSG
               PERFORM 0136-CREDIT-ERROR-PAGE
           END-IF.  

           PERFORM 0140-MESSAGE-MENU.

       0147-SP-COUNTER.
           PERFORM 0200-TIME-AND-DATE.
           MOVE 0 TO WS-SP-COUNTER.
           CALL "sponsored-posts-counter" USING WS-FORMATTED-DT, 
           WS-SP-COUNTER.

       0148-SP-ERROR-SCREEN.
           PERFORM 0200-TIME-AND-DATE.
           PERFORM 0132-CREDIT-TOTAL.
           INITIALIZE SP-ERROR-CHOICE.
           DISPLAY SP-ERROR-SCREEN.
           ACCEPT SP-ERROR-CHOICE-FIELD.
           
           IF SP-ERROR-CHOICE = "g" OR "G" THEN 
               PERFORM 0140-MESSAGE-MENU
           ELSE 
               PERFORM 0148-SP-ERROR-SCREEN
           END-IF.      
      ******************************************************************
      ************************-----ERROR SECTION----********************
      ******************************************************************
       0156-COMMENT-ERROR.
           PERFORM 0200-TIME-AND-DATE.
           PERFORM 0132-CREDIT-TOTAL.
           INITIALIZE ERROR-CHOICE.
           DISPLAY COMMENT-ERROR-SCREEN.
           ACCEPT C-ERROR-CHOICE-FIELD.
           IF ERROR-CHOICE = 'C' OR 'c' THEN
             PERFORM 0130-CREDIT-STORE
           ELSE IF ERROR-CHOICE = 'A' OR 'a' THEN
             PERFORM 0125-USER-ACCOUNT-MENU
           ELSE IF ERROR-CHOICE = 'G' OR 'g' THEN
             PERFORM 0143-COMMENT-SCREEN
           END-IF.
           PERFORM 0156-COMMENT-ERROR.
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

       0201-CURRENT-DATE.
           MOVE FUNCTION CURRENT-DATE TO WS-TIME.
      ******************************************************************
      ******************----TRANSACTION LOG SECTION----*****************
      ******************************************************************
       0300-TRANSACTIONS.
           PERFORM 0200-TIME-AND-DATE.
           PERFORM 0132-CREDIT-TOTAL.
           CALL 'transactions' USING WS-FORMATTED-DT, WS-USERNAME,
           WS-STORE-CHARGE.
           PERFORM 0125-USER-ACCOUNT-MENU.
      ******************************************************************
      ***************-----FUN AND GAMES SECTION----*********************
      ******************************************************************
       0400-GAMES-MENU.
           PERFORM 0200-TIME-AND-DATE.
           PERFORM 0132-CREDIT-TOTAL.
           PERFORM 0126-CHECK-ACCOUNT-STATUS.
           INITIALIZE GAMES-MENU-CHOICE.
           DISPLAY GAMES-MENU-SCREEN.
           ACCEPT GAMES-MENU-CHOICE-FIELD
           IF GAMES-MENU-CHOICE =      "q" or "Q" THEN
               STOP RUN
           ELSE IF GAMES-MENU-CHOICE = "g" or "G" THEN
               PERFORM 0110-DISPLAY-MENU 
           END-IF.  
           
           IF GAMES-MENU-CHOICE = "1" THEN
           MOVE 0 TO WS-UPDATE-CREDITS
           MOVE 5 TO WS-UPDATE-CREDITS
           PERFORM 0133-CHECK-CREDIT-BALANCE           
               IF WS-BALANCE-AVAILABLE = "Y" THEN
                   CALL "subtract-credits" USING WS-USERNAME, 
                   WS-UPDATE-CREDITS
                   CALL "account-status" USING WS-USERNAME
                   PERFORM 0430-GUESS-THE-NUMBER-GAME
               ELSE IF WS-BALANCE-AVAILABLE = "N" THEN
                MOVE "Insufficient Credits" TO WS-ERROR-MSG
                   PERFORM 0136-CREDIT-ERROR-PAGE
               END-IF
           END-IF. 

           IF GAMES-MENU-CHOICE = "2" THEN
           MOVE 0 TO WS-UPDATE-CREDITS
           MOVE 5 TO WS-UPDATE-CREDITS
           PERFORM 0133-CHECK-CREDIT-BALANCE          
               IF WS-BALANCE-AVAILABLE = "Y" THEN
                   CALL "subtract-credits" USING WS-USERNAME, 
                   WS-UPDATE-CREDITS
                   CALL "account-status" USING WS-USERNAME
                   PERFORM 0410-GUESS-THE-WORD-GAME
               ELSE IF WS-BALANCE-AVAILABLE = "N" THEN
                MOVE "Insufficient Credits" TO WS-ERROR-MSG
                   PERFORM 0136-CREDIT-ERROR-PAGE
               END-IF
           END-IF. 
           
           

           IF GAMES-MENU-CHOICE = "3" THEN           
               IF WS-USERACCOUNTLEVEL = "STD" THEN
               MOVE 0 TO WS-UPDATE-CREDITS
               MOVE 5 TO WS-UPDATE-CREDITS
               PERFORM 0133-CHECK-CREDIT-BALANCE           
                   IF WS-BALANCE-AVAILABLE = "Y" THEN
                       CALL "subtract-credits" USING WS-USERNAME, 
                       WS-UPDATE-CREDITS
                       PERFORM 0420-TIC-TAC-TOE 
                   ELSE IF WS-BALANCE-AVAILABLE = "N" THEN
                    MOVE "Insufficient Credits" TO WS-ERROR-MSG
                       PERFORM 0136-CREDIT-ERROR-PAGE
               END-IF
           END-IF.
           
           IF GAMES-MENU-CHOICE = "3" THEN  
               IF WS-USERACCOUNTLEVEL = "VIP" THEN
                   PERFORM 0420-TIC-TAC-TOE
               END-IF
           END-IF.
           

           PERFORM 0400-GAMES-MENU.
      
      ******************************************************************
      ****************----WORD GUESSING GAME SECTION----****************
      ******************************************************************
       0410-GUESS-THE-WORD-GAME.
           SET WS-GTW-COL TO 4
           PERFORM 0200-TIME-AND-DATE.
           PERFORM 0132-CREDIT-TOTAL.
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
           PERFORM UNTIL COUNTER = 10
             IF "*" EQUALS WS-WORD(COUNTER:1) 
              THEN ADD 1 TO WS-WORD-LENGTH
             END-IF
             ADD 1 TO COUNTER
           END-PERFORM.
           PERFORM 0411-IN_GAME-SCREEN.
          
       0411-IN_GAME-SCREEN.
           PERFORM 0200-TIME-AND-DATE.
           PERFORM 0132-CREDIT-TOTAL.
           INITIALIZE WS-GUESS-CHOICE.
           DISPLAY IN-GAME-SCREEN.
           ACCEPT WS-GUESS-CHOICE-FIELD.
           IF WS-GUESS-CHOICE = "!" THEN 
               PERFORM 0110-DISPLAY-MENU
           ELSE
               PERFORM 0412-CHECK-GUESS
           END-IF.
           
       0412-CHECK-GUESS.
           MOVE 1 TO COUNTER.
           PERFORM UNTIL COUNTER = 10
                 IF WS-GUESS-CHOICE = WS-ANSWERWORD(COUNTER:1) THEN
                   MOVE WS-GUESS-CHOICE TO WS-WORD(COUNTER:1) 
                 END-IF
                 ADD 1 TO COUNTER     
           END-PERFORM.
           SUBTRACT 1 FROM WS-GUESSES-LEFT.
           MOVE 1 TO COUNTER.
           MOVE 0 TO WS-LETTERS-LEFT.
           PERFORM UNTIL COUNTER = 10
             IF "*" EQUALS WS-WORD(COUNTER:1) 
              THEN ADD 1 TO WS-LETTERS-LEFT
             END-IF
             ADD 1 TO COUNTER
           END-PERFORM.
             IF WS-LETTERS-LEFT = 0
              THEN
               MOVE 0  TO WS-UPDATE-CREDITS
               MOVE 10 TO WS-UPDATE-CREDITS
               CALL "add-credits" USING WS-USERNAME, WS-UPDATE-CREDITS
              PERFORM 0413-WINNING-SCREEN
             ELSE IF WS-GUESSES-LEFT = 0
              THEN 
              PERFORM 0414-LOSING-SCREEN
             ELSE
              PERFORM 0411-IN_GAME-SCREEN
             END-IF.
           
       0413-WINNING-SCREEN.
           PERFORM 0200-TIME-AND-DATE.
           PERFORM 0132-CREDIT-TOTAL.
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
               THEN PERFORM 0410-GUESS-THE-WORD-GAME
           ELSE IF WS-GUESSING-WINNING-CHOICE = "h" OR "H"
             THEN PERFORM 0416-HIGH-SCORE-TABLE
           ELSE IF WS-GUESSING-WINNING-CHOICE = "!" OR "q" OR "Q"
             THEN PERFORM 0110-DISPLAY-MENU
           ELSE
             PERFORM 0413-WINNING-SCREEN
           END-IF.

       0414-LOSING-SCREEN.
           PERFORM 0201-CURRENT-DATE.
           PERFORM 0132-CREDIT-TOTAL.
           INITIALIZE WS-GUESSING-LOSING-CHOICE.
           DISPLAY WORD-GUESSING-LOSE-SCREEN.
           ACCEPT WS-GUESSING-LOSING-CHOICE-FIELD.
           IF WS-GUESSING-LOSING-CHOICE = "r" OR "R"
           MOVE 7 TO WS-GTW-COL
           MOVE 0 TO WS-UPDATE-CREDITS
           MOVE 5 TO WS-UPDATE-CREDITS
           PERFORM 0133-CHECK-CREDIT-BALANCE          
               IF WS-BALANCE-AVAILABLE = "Y" THEN
                   CALL "subtract-credits" USING WS-USERNAME, 
                   WS-UPDATE-CREDITS
                   CALL "account-status" USING WS-USERNAME
                   PERFORM 0414-LOSING-SCREEN
               ELSE IF WS-BALANCE-AVAILABLE = "N" THEN
                MOVE "Insufficient Credits" TO WS-ERROR-MSG
                   PERFORM 0136-CREDIT-ERROR-PAGE
               END-IF
           END-IF.

           IF WS-GUESSING-LOSING-CHOICE = "p" OR "P"
               THEN PERFORM 0410-GUESS-THE-WORD-GAME
           ELSE IF WS-GUESSING-LOSING-CHOICE = "h" OR "H"
             THEN PERFORM 0416-HIGH-SCORE-TABLE
           ELSE IF WS-GUESSING-LOSING-CHOICE = "!" OR "q" OR "Q"
             THEN PERFORM 0110-DISPLAY-MENU
           ELSE
             PERFORM 0414-LOSING-SCREEN
           END-IF.
           
       0415-HIGH-SCORE-SCREEN.
           PERFORM 0201-CURRENT-DATE.
           PERFORM 0132-CREDIT-TOTAL.
           INITIALIZE WS-HIGH-SCORE-CHOICE.
           SORT WS-TABLE-HIGH-SCORE ON DESCENDING WS-SCORE.
           DISPLAY HIGH-SCORE-SCREEN.
           ACCEPT WS-HIGH-SCORE-CHOICE-FIELD.
           IF WS-HIGH-SCORE-CHOICE = "g" OR "G"
             PERFORM 0400-GAMES-MENU
           ELSE IF WS-HIGH-SCORE-CHOICE = "q" OR "Q"
             STOP RUN
           ELSE 
               PERFORM 0415-HIGH-SCORE-SCREEN
           END-IF.

       0416-HIGH-SCORE-TABLE.
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
           PERFORM 0415-HIGH-SCORE-SCREEN.
      ******************************************************************
      ****************----TIC-TAC-TOE GAME SECTION----******************
      ******************************************************************
       0420-TIC-TAC-TOE.
           PERFORM 0201-CURRENT-DATE.
           PERFORM 0132-CREDIT-TOTAL.
           MOVE "X" TO WS-PLAYER
           PERFORM GAME-LOOP-PARAGRAPH
               WITH TEST AFTER UNTIL FINISHED-PLAYING
           PERFORM 0400-GAMES-MENU.

           GAME-LOOP-PARAGRAPH.
               INITIALIZE WS-GAME-GRID
               INITIALIZE WS-STATE
               INITIALIZE WS-MOVES
               MOVE WS-COLOR-BLUE  TO WS-BG-COLOR 
               
               MOVE "Make a move like 'A2'" TO WS-OANDXMESSAGE
               PERFORM GAME-FRAME-PARAGRAPH
                   WITH TEST AFTER UNTIL GAME-OVER
               ADD 1 TO WS-GAMES END-ADD
               EVALUATE WS-STATE

               WHEN "WIN"
                   ADD 1 TO WS-WINS END-ADD
                   MOVE WS-COLOR-GREEN TO WS-BG-COLOR 
                   MOVE WS-COLOR-BLUE  TO WS-FG
                   MOVE WS-COLOR-BLUE  TO WS-FG-CELL
                   MOVE WS-COLOR-GREEN TO WS-BG

               WHEN "STALE"
                   MOVE WS-COLOR-BLUE  TO WS-BG-COLOR 
                   MOVE WS-COLOR-BLUE  TO WS-FG
                   MOVE WS-COLOR-BLUE  TO WS-FG-CELL
                   MOVE WS-COLOR-BLUE  TO WS-BG

               WHEN "LOSE"
                   MOVE WS-COLOR-RED   TO WS-BG-COLOR 
                   MOVE WS-COLOR-BLUE  TO WS-FG
                   MOVE WS-COLOR-BLUE  TO WS-FG-CELL
                   MOVE WS-COLOR-RED   TO WS-BG  

               WHEN OTHER
                   MOVE WS-COLOR-BLUE  TO WS-BG-COLOR
                   MOVE WS-COLOR-BLUE  TO WS-FG
                   MOVE WS-COLOR-BLUE  TO WS-FG-CELL
                   MOVE WS-COLOR-BLUE   TO WS-BG
               END-EVALUATE
               MOVE "One more (y/n)? " TO WS-INSTRUCTION
               MOVE "y" TO WS-NEXT-MOVE
               DISPLAY TIC-TAC-TOE-SCREEN
               ACCEPT WS-NEXT-MOVE-FIELD.

           GAME-FRAME-PARAGRAPH.
               MOVE "Move to square: " TO WS-INSTRUCTION
               MOVE WS-COLOR-GREEN     TO WS-FG
               MOVE WS-COLOR-WHITE     TO WS-FG-CELL
               MOVE WS-COLOR-BLUE      TO WS-BG
               INITIALIZE WS-MOVE-OUTCOME

               IF COMPUTER-PLAYER
                   INITIALIZE WS-COMPUTER-MOVED
                   PERFORM UNTIL COMPUTER-MOVED
                       COMPUTE WS-ROW = FUNCTION RANDOM * 3 + 1
                       END-COMPUTE
                       COMPUTE WS-COL = FUNCTION RANDOM * 3 + 1
                       END-COMPUTE
                       IF WS-CELL(WS-ROW,WS-COL) IS EQUAL TO " "
                       THEN
                           SET WS-COMPUTER-MOVED TO 1
                           MOVE WS-PLAYER TO WS-CELL(WS-ROW,WS-COL)
                       END-IF
                   END-PERFORM

               ELSE
                   INITIALIZE WS-NEXT-MOVE
                   DISPLAY TIC-TAC-TOE-SCREEN
                   ACCEPT WS-NEXT-MOVE-FIELD
                   EVALUATE FUNCTION UPPER-CASE(WS-NEXT-MOVE(1:1))
                       WHEN "A" SET WS-ROW TO 1
                       WHEN "B" SET WS-ROW TO 2
                       WHEN "C" SET WS-ROW TO 3
                       WHEN OTHER MOVE "FAIL" TO WS-MOVE-OUTCOME
                   END-EVALUATE
                   SET WS-COL TO WS-NEXT-MOVE(2:1)

                   IF
                       WS-MOVE-OUTCOME IS NOT EQUAL TO "FAIL"
                       AND WS-COL IS GREATER THAN 0
                       AND WS-COL IS LESS THAN 4
                       AND WS-CELL(WS-ROW,WS-COL) = " "
                   THEN
                       MOVE WS-PLAYER TO WS-CELL(WS-ROW,WS-COL)
                   ELSE
                       MOVE "FAIL" TO WS-MOVE-OUTCOME
                   END-IF
               END-IF
               MOVE WS-GAME-GRID TO WS-FLAT-GAME-GRID

               IF HUMAN-PLAYER
                   INSPECT WS-FLAT-GAME-GRID REPLACING ALL "X" BY "1"
                   INSPECT WS-FLAT-GAME-GRID REPLACING ALL "O" BY "0"
               ELSE
                   INSPECT WS-FLAT-GAME-GRID REPLACING ALL "X" BY "0"
                   INSPECT WS-FLAT-GAME-GRID REPLACING ALL "O" BY "1"
               END-IF

               INSPECT WS-FLAT-GAME-GRID REPLACING ALL " " BY "0"
               INITIALIZE WS-EOF
               OPEN INPUT FD-WINMASKS
               PERFORM UNTIL EOF OR MOVE-COMPLETE
                   READ FD-WINMASKS NEXT RECORD
                       AT END
                           SET WS-EOF TO 1
                       NOT AT END
                           PERFORM VALIDATE-WIN-PARAGRAPH
                   END-READ
               END-PERFORM
               CLOSE FD-WINMASKS
               IF NOT MOVE-COMPLETE AND WS-MOVES IS EQUAL TO 8
                   MOVE "STALE" TO WS-MOVE-OUTCOME
               END-IF

               INITIALIZE WS-SWAP-PLAYERS
               EVALUATE WS-MOVE-OUTCOME

               WHEN "WIN"
                   MOVE "WINNER!"    TO WS-OANDXMESSAGE
                   MOVE "WIN" TO WS-STATE
                   SET WS-SWAP-PLAYERS     TO 1
               WHEN "LOSE"
                   MOVE "LOSER!"   TO WS-OANDXMESSAGE
                   MOVE "LOSE" TO WS-STATE
                   SET WS-SWAP-PLAYERS     TO 1
               WHEN "STALE"
                   MOVE "IT'S A DRAW!" TO WS-OANDXMESSAGE
                   MOVE "STALE" TO WS-STATE
               WHEN "FAIL"
                   MOVE "Invalid move..." TO WS-OANDXMESSAGE
               WHEN OTHER
                   MOVE "Enter a move" TO WS-OANDXMESSAGE
                   SET WS-SWAP-PLAYERS TO 1
                   ADD 1 TO WS-MOVES END-ADD
               END-EVALUATE

               IF SWAP-PLAYERS
                   IF HUMAN-PLAYER
                       MOVE "O" TO WS-PLAYER
                   ELSE
                       MOVE "X" TO WS-PLAYER
                   END-IF
               END-IF.

           VALIDATE-WIN-PARAGRAPH.
               INITIALIZE WS-MASK-DETECTED
               SET WS-DETECT-LOOP-COUNT TO 1
               PERFORM 9 TIMES

                   IF
                       FD-WINMASK(WS-DETECT-LOOP-COUNT:1)
                       IS EQUAL TO
                       WS-FLAT-GAME-GRID(WS-DETECT-LOOP-COUNT:1)
                       AND IS EQUAL TO 1
                   THEN
                       ADD 1 TO WS-MASK-DETECTED END-ADD
                   END-IF
                   ADD 1 TO WS-DETECT-LOOP-COUNT END-ADD
               END-PERFORM

               IF WIN-DETECTED
                   IF HUMAN-PLAYER
                       MOVE "WIN" TO WS-MOVE-OUTCOME
                   ELSE
                       MOVE "LOSE" TO WS-MOVE-OUTCOME
                   END-IF
               END-IF.
      ******************************************************************
      ****************----NUMBER GUESSING GAME SECTION----**************
      ******************************************************************
       0430-GUESS-THE-NUMBER-GAME.
           PERFORM 0200-TIME-AND-DATE.
           PERFORM 0132-CREDIT-TOTAL.
           PERFORM INITIALIZE-RANDOM-NUM-GAME.
           INITIALIZE-RANDOM-NUM-GAME.
           MOVE WS-COLOR-BLUE TO WS-BG-COLOR.
           MOVE 7 TO WS-FG-COLOR.
           MOVE 5 TO WS-GTN-BG-COLOR.
           MOVE 0 TO TOTAL-GUESSES.
           DISPLAY GUESS-THE-NUMBER-GAME-SCREEN.
           ACCEPT SEED FROM TIME
           COMPUTE ANSWER =
               FUNCTION REM(FUNCTION RANDOM(SEED) * 1000, 10) + 1
           MOVE "Guess a number!" TO WS-RANDOM-NUM-MSG
           PERFORM GAME-LOOP.

           GAME-LOOP.
           INITIALIZE GUESS-INPUT.
           DISPLAY GUESS-THE-NUMBER-GAME-SCREEN END-DISPLAY
           ACCEPT GUESS-INPUT-FIELD
           MOVE GUESS-INPUT TO GUESS.
           ADD 1 TO TOTAL-GUESSES.

           IF GUESS = ANSWER
               MOVE "You Won 10 Credits! Go again?(Y/N)"
               TO WS-RANDOM-NUM-MSG
               MOVE 0  TO WS-UPDATE-CREDITS
               MOVE 10 TO WS-UPDATE-CREDITS
               CALL "add-credits" USING WS-USERNAME, WS-UPDATE-CREDITS
               MOVE 0 TO WS-FG-COLOR
               MOVE WS-COLOR-GREEN TO WS-BG-COLOR
               MOVE 0 TO WS-GTN-BG-COLOR
               GO TO WIN-LOOP
           END-IF.

           IF TOTAL-GUESSES > 2
               MOVE "You Lose! Try again?(Y/N)"
               TO WS-RANDOM-NUM-MSG
               MOVE 7 TO WS-FG-COLOR
               MOVE WS-COLOR-RED TO WS-BG-COLOR
               MOVE 0 TO WS-GTN-FG-COLOR
               MOVE 7 TO WS-GTN-BG-COLOR
               GO TO WIN-LOOP
           END-IF.

           IF GUESS > ANSWER
               MOVE "Too high! Guess again."
               TO WS-RANDOM-NUM-MSG
               GO TO GAME-LOOP
           ELSE IF GUESS < ANSWER
               MOVE "Too low! Guess again."
               TO WS-RANDOM-NUM-MSG
               GO TO GAME-LOOP
           END-IF.

           WIN-LOOP.
           INITIALIZE GUESS-INPUT.
           DISPLAY GUESS-THE-NUMBER-GAME-SCREEN END-DISPLAY
           ACCEPT GUESS-INPUT-FIELD
           IF GUESS-INPUT = "y" OR "Y"
               MOVE 0 TO WS-UPDATE-CREDITS
               MOVE 5 TO WS-UPDATE-CREDITS
               PERFORM 0133-CHECK-CREDIT-BALANCE
               IF WS-BALANCE-AVAILABLE = "Y" THEN
                    CALL "subtract-credits" USING WS-USERNAME,
                    WS-UPDATE-CREDITS
                    PERFORM 0430-GUESS-THE-NUMBER-GAME
                ELSE IF WS-BALANCE-AVAILABLE = "N" THEN
                    MOVE "Insufficient Credits" TO WS-ERROR-MSG
                    PERFORM 0136-CREDIT-ERROR-PAGE
                END-IF
           GO TO INITIALIZE-RANDOM-NUM-GAME
           END-IF.
           IF GUESS-INPUT = "n" OR "N"
               PERFORM 0400-GAMES-MENU
           ELSE
               MOVE "INVALID ENTRY! Enter Y or N"
               TO WS-RANDOM-NUM-MSG
               GO TO WIN-LOOP
           END-IF.


       0600-CHECK-BANK-DETAILS-PRESENT.

           CALL "check-bank-details-present" USING WS-USERNAME, 
           WS-ON-FILE.

           IF WS-ON-FILE = "Y" THEN
               MOVE "YES" TO WS-BNK-DTLS-PRESENT
           ELSE IF WS-ON-FILE = "N" THEN
               MOVE "NO" TO WS-BNK-DTLS-PRESENT
           END-IF.         

       0620-GET-EXPIRY-DATE.
           
           PERFORM 0600-CHECK-BANK-DETAILS-PRESENT.

           IF WS-BNK-DTLS-PRESENT = "YES"
               CALL "get-expiry-date" USING WS-USERNAME, WS-CARD-EXP
           ELSE
               MOVE "0000" TO WS-CARD-EXP    
           END-IF.  
