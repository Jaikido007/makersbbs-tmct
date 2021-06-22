       IDENTIFICATION DIVISION.
       PROGRAM-ID. server.

       ENVIRONMENT DIVISION.
           CONFIGURATION SECTION.
           REPOSITORY.
               FUNCTION REPLACE-LETTER.
               
           INPUT-OUTPUT SECTION.
           FILE-CONTROL.

           SELECT F-WORD-FILE ASSIGN TO 'guessing-words.dat'
             ORGANIZATION IS LINE SEQUENTIAL.
           SELECT F-HIGH-SCORES-FILE ASSIGN TO 'high-scores.dat'
             ORGANIZATION IS LINE SEQUENTIAL.
           SELECT F-USERS-FILE ASSIGN TO 'users.dat'
             ORGANIZATION IS LINE SEQUENTIAL.
           
       DATA DIVISION.
           FILE SECTION.
           FD F-WORD-FILE.
           01 WORD PIC X(20).
           FD F-HIGH-SCORES-FILE.
           01 PLAYER-SCORES.
              05 HIGH-SCORE PIC 99.
              05 PLAYER-NAME PIC X(10).
           FD F-USERS-FILE.
           01 USERS.
              05 USERNAME PIC X(16).
              05 USER-PASSWORD PIC X(20).   
                      
           WORKING-STORAGE SECTION.
      ****************************************************
      *----Variables related to login and menu screen----*
      ****************************************************
           01 USER-NAME PIC X(16).
           01 WS-PASSWORD PIC X(20).
           01 NEW-USER-NAME PIC X(16).
           01 NEW-PASSWORD PIC X(20).
           01 LOGIN-CHOICE PIC X.
           01 MENU-CHOICE PIC X.
           01 ERROR-CHOICE PIC X.
           01 CREATE-CHOICE PIC X.
           01 WS-USERS.
               05 WS-USER OCCURS 100 TIMES
               ASCENDING KEY IS WS-UNAME
               INDEXED BY USER-IDX.
                   10 WS-UNAME PIC X(16).
                   10 WS-PWORD PIC X(20).
           01 WS-FOUND PIC 9.
           01 WS-IDX UNSIGNED-INT. 
      ********************************************
      *----Variables related to guessing game----*
      ********************************************
           01 WS-ANSWERWORD PIC X(20).
           01 RANDOMNUMBER PIC 99.
           01 WS-WORD PIC X(20).
           01 WS-GUESSING-CHOICE-WORDS.
               05 WS-GUESSING-CHOICE-WORD OCCURS 213 TIMES
               DESCENDING KEY IS WS-GUESSING-WORDS-WORD
               INDEXED BY WORD-IDX.
                   10 WS-GUESSING-WORDS-WORD PIC X(20).
           01 WS-GUESS-CHOICE PIC X.
      ************************************************
      *----Variables related to high score screen----*
      ************************************************
           01 WS-HIGH-SCORE-CHOICE PIC X.
           01 WS-HIGH-SCORE PIC 99.
           01 WS-HIGH-SCORES.  
              05 WS-TABLE-HIGH-SCORE OCCURS 100 TIMES     
              ASCENDING KEY IS WS-SCORE
              INDEXED BY SCORE-IDX.
                  10 WS-SCORE PIC 99.
                  10 WS-NAME PIC X(10).
      ***********************************************
      *----Variables related to checking guesses----*
      ***********************************************  
           01 WS-LETTERS-LEFT PIC 99.
           01 WS-GUESSES-LEFT PIC 99.          
      *************************************************
      *----Variables related to winning and losing----*
      *************************************************
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
               15 FILLER               PIC X VALUE '-'.
               15 WS-FORMATTED-MONTH   PIC  X(2).
               15 FILLER               PIC X VALUE '-'.
               15 WS-FORMATTED-DY      PIC  X(2).
               15 FILLER               PIC X VALUE '-'.
               15 WS-FORMATTED-HOUR    PIC  X(2).
               15 FILLER               PIC X VALUE ':'.
               15 WS-FORMATTED-MINS    PIC  X(2).
               15 FILLER               PIC X VALUE ':'.
               15 WS-FORMATTED-SEC     PIC  X(2).
               15 FILLER               PIC X VALUE ':'.
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
             05 WS-USERNAME            PIC X(16).
      **************
      *----Time----*
      **************
           01 WS-TIME.
               05 WS-YEAR PIC X(4).
               05 WS-MONTH PIC X(2).
               05 WS-DAY PIC X(2).
               05 WS-HOURS-MINS.
                   10 WS-HOURS PIC X(2).
                   10 WS-MINS PIC X(2).
           
           LINKAGE SECTION.
           01 LS-COUNTER UNSIGNED-INT.
           01 LS-NUM UNSIGNED-INT.
           01 LS-MESSAGE PIC X(60).  

           SCREEN SECTION.
           01 LOGIN-SCREEN
               BACKGROUND-COLOR IS 1.
               05 BLANK SCREEN.
           *>upper border
               05 LINE 1 COL 1  VALUE "   :                              
      -    "                                                           "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 1 COL 2 PIC X(2) USING WS-FORMATTED-HOUR 
               FOREGROUND-COLOR IS 7 REVERSE-VIDEO.
               05 LINE 1 COL 5 PIC X(2) USING WS-FORMATTED-MINS
               FOREGROUND-COLOR IS 7 REVERSE-VIDEO.
           *>bottom border
               05 LINE 43 COL 1 VALUE "                                 
      -    "                                                           "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 44 COL 1 VALUE "     (L) Go to Log-in     (C) Cre
      -    "ate an account     (Q) Quit                                "                                 
                FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
               05 LINE 45 COL 1 VALUE "                                 
      -    "                                                           "
               FOREGROUND-COLOR IS 7, REVERSE-VIDEO.
           *> general code

          
               05 LINE 31 COL 5 VALUE "______    _                ______
      -        "_" FOREGROUND-COLOR IS 7.
               05 LINE 32 COL 5 VALUE "|  ___|  (_)              | |  __
      -        "_| " FOREGROUND-COLOR IS 7.
               05 LINE 33 COL 5 VALUE "| |_ _ __ _  ___ _ __   __| | |_ 
      -        "__ _  ___ ___" FOREGROUND-COLOR IS 7.
               05 LINE 34 COL 5 VALUE "|  _| '__| |/ _ \ '_ \ / _` |  _/
      -        " _` |/ __/ _ \" FOREGROUND-COLOR IS 7.
               05 LINE 35 COL 5 VALUE "| | | |  | |  __/ | | | (_| | || 
      -        "(_| | (_|  __/" FOREGROUND-COLOR IS 7.
               05 LINE 36 COL 5 VALUE "\_| |_|  |_|\___|_| |_|\__,_\_| \
      -        "__,_|\___\___|" FOREGROUND-COLOR IS 7.

               


               05 LINE 42 COLUMN 6 VALUE "Pick: ".
               05 LOGIN-CHOICE-FIELD LINE 42 COLUMN 12 PIC X
                  USING LOGIN-CHOICE.

           01 SIGN-IN-SCREEN
             BACKGROUND-COLOR IS 0.
             05 BLANK SCREEN.
             05 LINE 2 COL 2 PIC X(2) USING WS-FORMATTED-HOUR.
             05 LINE 2 COL 4 VALUE ":".
             05 LINE 2 COL 5 PIC X(2) USING WS-FORMATTED-MINS.  
             05 LINE 4 COL 12 VALUE "MAKERS BBS" UNDERLINE, BLINK
             HIGHLIGHT, FOREGROUND-COLOR IS 3.
             05 LINE 6 COLUMN 10 VALUE "Enter your username:".
             05 USER-NAME-FIELD LINE 7 COLUMN 10 PIC X(16)
                USING USER-NAME.   
             05 LINE 8 COLUMN 10 VALUE "Enter your password:".
             05 PASSWORD-FIELD LINE 9 COLUMN 10 PIC X(20)
                USING WS-PASSWORD.    

           01 ERROR-SCREEN
             BACKGROUND-COLOR IS 0.
             05 BLANK SCREEN.
             05 LINE 2 COL 2 PIC X(2) USING WS-FORMATTED-HOUR.
             05 LINE 2 COL 4 VALUE ":".
             05 LINE 2 COL 5 PIC X(2) USING WS-FORMATTED-MINS.  
             05 LINE 4 COL 12 VALUE "MAKERS BBS" UNDERLINE, BLINK
             HIGHLIGHT, FOREGROUND-COLOR IS 3.
             05 LINE 6 COLUMN 10 VALUE "Incorrect Username or Password".
             05 LINE 7 COLUMN 10 VALUE "(L) Back to Log-in.".
             05 LINE 8 COLUMN 10 VALUE "(C) Create an account.".
             05 LINE 10 COLUMN 10 VALUE "Pick: ".
             05 ERROR-CHOICE-FIELD LINE 10 COLUMN 16 PIC X
                USING ERROR-CHOICE.

           01 CREATE-AN-ACCOUNT-SCREEN
             BACKGROUND-COLOR IS 0.
             05 BLANK SCREEN.
             05 LINE 2 COL 2 PIC X(2) USING WS-FORMATTED-HOUR.
             05 LINE 2 COL 4 VALUE ":".
             05 LINE 2 COL 5 PIC X(2) USING WS-FORMATTED-MINS.  
             05 LINE 4 COL 12 VALUE "MAKERS BBS" UNDERLINE, BLINK
             HIGHLIGHT, FOREGROUND-COLOR IS 3.
             05 LINE 6 COLUMN 10 VALUE "Create your account".
             05 LINE 8 COLUMN 10 VALUE "Enter a username:".
             05 NEW-USER-NAME-FIELD LINE 8 COLUMN 10 PIC X(10)
                USING NEW-USER-NAME.
             05 LINE 10 COLUMN 10 VALUE "Enter a password: ".
             05 LINE 10 COLUMN 29 VALUE "(max 20 characters)".
             05 NEW-PASSWORD-FIELD LINE 11 COLUMN 10 PIC X(20)
                USING NEW-PASSWORD.
             05 LINE 12 COLUMN 10 VALUE "(S) Submit".
             05 LINE 13 COLUMN 10 VALUE "(Q) Go Back".
             05 LINE 17 COLUMN 10 VALUE "Pick: ".
             05 CREATE-CHOICE-FIELD LINE 17 COLUMN 16 PIC X
                USING CREATE-CHOICE.        

           01 MENU-SCREEN
             BACKGROUND-COLOR IS 0.
             05 BLANK SCREEN.
             05 LINE  1 FOREGROUND-COLOR IS 7 REVERSE-VIDEO.
             05 LINE  2 COL 2 PIC X(2) USING WS-FORMATTED-HOUR.
             05 LINE  2 COL 4 VALUE ":".
             05 LINE  2 COL 5 PIC X(2) USING WS-FORMATTED-MINS.  
             05 LINE  4 COL 10 VALUE "MAKERS BBS" UNDERLINE.
             05 LINE  6 COL 10 VALUE "Hi, ".
             05 LINE  6 COL 14 PIC X(16) USING USER-NAME.
             05 LINE  8 COL 10 VALUE "Welcome to COBOL The Barbarian's s
      -      "tate oF the art Bulletin Board.".  
             05 LINE  9 COL 10 VALUE "Feel free to:".
             05 LINE 10 COL 24 VALUE "* " FOREGROUND-COLOR IS 2.
             05 LINE 10 COL 26 VALUE "Read our message board.".
             05 LINE 11 COL 24 VALUE "* " FOREGROUND-COLOR IS 5.
             05 LINE 11 COL 26 VALUE "Play a few games.".
             05 LINE 12 COL 24 VALUE "* " FOREGROUND-COLOR IS 2.
             05 LINE 12 COL 26 VALUE "Leave a message of your own.". 
             05 LINE 13 COL 24 VALUE "* " FOREGROUND-COLOR IS 5.
             05 LINE 13 COL 26 VALUE "Most importantly. HAVE FUN!". 

             05 LINE 19 COL 24 VALUE "(M) Messages    "
                REVERSE-VIDEO HIGHLIGHT FOREGROUND-COLOR IS 2.
             05 LINE 19 COL 42 VALUE "(F) Fun & games "
                REVERSE-VIDEO, HIGHLIGHT FOREGROUND-COLOR IS 5.
             05 LINE 21 COL 24 VALUE "(L) Logout      "
                REVERSE-VIDEO , HIGHLIGHT.            
             05 LINE 21 COL 42 VALUE "(Q) Quit        "
                REVERSE-VIDEO, HIGHLIGHT.  
             05 LINE 23 COL 24 VALUE "Pick: ".
             05 MENU-CHOICE-FIELD LINE 23 COL 30 PIC X
                USING MENU-CHOICE.    
           
           01 MSG-MENU-SCREEN
             BACKGROUND-COLOR IS 0.
             05 BLANK SCREEN.
             05 LINE  2 COL 2 PIC X(2) USING WS-FORMATTED-HOUR.
             05 LINE  2 COL 4 VALUE ":".
             05 LINE  2 COL 5 PIC X(2) USING WS-FORMATTED-MINS.
             05 LINE  4 COL 10 VALUE "MAKERS BBS" UNDERLINE.

             05 LINE 8 COL 10 VALUE "-----------------------------------
      -      "---------------------" FOREGROUND-COLOR IS 3.

             05 LINE 9 COL 10 VALUE "*********************BULLETIN BOARD
      -      "*********************" BLINK, HIGHLIGHT, FOREGROUND-COLOR 
             IS 2.
             05 LINE 10 COL 10 VALUE "-----------------------------------
      -      "---------------------" FOREGROUND-COLOR IS 3.

             05 LINE 11 COL 10 PIC XXX     USING LIST-ID(ID-NUM).
             05 LINE 11 COL 14 PIC X(50)   USING LIST-TITLE(ID-NUM).
             05 LINE 12 COL 10 PIC XXX     USING LIST-ID(ID-NUM + 1).
             05 LINE 12 COL 14 PIC X(50)   USING LIST-TITLE(ID-NUM + 1).
             05 LINE 13 COL 10 PIC XXX     USING LIST-ID(ID-NUM + 2).
             05 LINE 13 COL 14 PIC X(50)   USING LIST-TITLE(ID-NUM + 2).
             05 LINE 14 COL 10 PIC XXX     USING LIST-ID(ID-NUM + 3).
             05 LINE 14 COL 14 PIC X(50)   USING LIST-TITLE(ID-NUM + 3).
             05 LINE 15 COL 10 PIC XXX     USING LIST-ID(ID-NUM + 4).
             05 LINE 15 COL 14 PIC X(50)   USING LIST-TITLE(ID-NUM + 4).
             05 LINE 16 COL 10 PIC XXX     USING LIST-ID(ID-NUM + 5).
             05 LINE 16 COL 14 PIC X(50)   USING LIST-TITLE(ID-NUM + 5).
             05 LINE 17 COL 10 PIC XXX     USING LIST-ID(ID-NUM + 6).
             05 LINE 17 COL 14 PIC X(50)   USING LIST-TITLE(ID-NUM + 6).
             05 LINE 18 COL 10 PIC XXX     USING LIST-ID(ID-NUM + 7).
             05 LINE 18 COL 14 PIC X(50)   USING LIST-TITLE(ID-NUM + 7).
             05 LINE 19 COL 10 PIC XXX     USING LIST-ID(ID-NUM + 8).
             05 LINE 19 COL 14 PIC X(50)   USING LIST-TITLE(ID-NUM + 8).
             05 LINE 20 COL 10 PIC XXX     USING LIST-ID(ID-NUM + 9).
             05 LINE 20 COL 14 PIC X(50)   USING LIST-TITLE(ID-NUM + 9).

             05 LINE 21 COL 10 VALUE "----------------------------------
      -      "---------------------" FOREGROUND-COLOR IS 3.
             05 LINE 22 COL 10 VALUE "*********************RECENT MESSAG
      -      "ES*******************" FOREGROUND-COLOR IS 2.
             05 LINE 23 COL 10 VALUE "----------------------------------
      -      "---------------------" FOREGROUND-COLOR IS 3.

             05 LINE 25 COL 24 VALUE "( ) Read Message by Number "
             REVERSE-VIDEO HIGHLIGHT FOREGROUND-COLOR IS 2.  
             05 LINE 27 COL 24 VALUE "(W) Write your own message "
             REVERSE-VIDEO HIGHLIGHT FOREGROUND-COLOR IS 2.               
             05 LINE 29 COL 18 VALUE "(N) Next Page     "
             REVERSE-VIDEO, HIGHLIGHT FOREGROUND-COLOR IS 6.  
             05 LINE 29 COL 41 VALUE "(P) Previous Page "
             REVERSE-VIDEO, HIGHLIGHT FOREGROUND-COLOR IS 6. 
             05 LINE 31 COL 18 VALUE "(G) Go back       "
             REVERSE-VIDEO, HIGHLIGHT.
             05 LINE 31 COL 41 VALUE "(Q) Quit          "
             REVERSE-VIDEO, HIGHLIGHT.
             05 LINE 33 COL 18 VALUE "Pick: ".
             05 MSG-MENU-CHOICE-FIELD LINE 33 COL 24 PIC XXX
                USING MSG-MENU-CHOICE.

           01 MESSAGE-VIEW-SCREEN
             BACKGROUND-COLOR IS 0.
             05 BLANK SCREEN.
             05 LINE  2 COL 2 PIC X(2) USING WS-FORMATTED-HOUR.
             05 LINE  2 COL 4 VALUE ":".
             05 LINE  2 COL 5 PIC X(2) USING WS-FORMATTED-MINS.
             05 LINE  4 COL 10 VALUE "MAKERS BBS" UNDERLINE.
      
             05 LINE 8 COL 10 VALUE "-----------------------------------
      -      "---------------------" FOREGROUND-COLOR IS 3.

             05 LINE 9 COL 10 VALUE "*********************BULLETIN BOARD
      -      "*********************" BLINK, HIGHLIGHT, FOREGROUND-COLOR 
             IS 2.
             05 LINE 10 COL 10 VALUE "-----------------------------------
      -      "---------------------" FOREGROUND-COLOR IS 3.

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
      -      "---------------------" FOREGROUND-COLOR IS 3.
             05 LINE 23 COL 10 VALUE "*********************CHOSEN MESSAG
      -      "E********************" FOREGROUND-COLOR IS 2.
             05 LINE 24 COL 10 VALUE "----------------------------------
      -      "---------------------" FOREGROUND-COLOR IS 3.

             05 LINE 29 COL 18 VALUE "(N) Next Page     "
             REVERSE-VIDEO, HIGHLIGHT FOREGROUND-COLOR IS 6.
             05 LINE 29 COL 41 VALUE "(P) Previous Page "
             REVERSE-VIDEO, HIGHLIGHT FOREGROUND-COLOR IS 6.
             05 LINE 31 COL 18 VALUE "(G) Go back       "
             REVERSE-VIDEO, HIGHLIGHT.
             05 LINE 31 COL 41 VALUE "(Q) Quit          "
             REVERSE-VIDEO, HIGHLIGHT.
             05 LINE 33 COL 18 VALUE "Pick: ".
             05 MSG-VIEW-CHOICE-FIELD LINE 33 COL 24 PIC XXX
                USING MSG-VIEW-CHOICE.

           01 WRITE-MSG-SCREEN
             BACKGROUND-COLOR IS 0.
             05 BLANK SCREEN.
             05 LINE  2 COL 2 PIC X(2) USING WS-FORMATTED-HOUR.
             05 LINE  2 COL 4 VALUE ":".
             05 LINE  2 COL 5 PIC X(2) USING WS-FORMATTED-MINS.

             05 LINE  4 COL 10 VALUE "MAKERS BBS" UNDERLINE.
            05 LINE  8 COL 10 VALUE "-----------------------------------
      -      "---------------------" FOREGROUND-COLOR IS 3.

             05 LINE 9 COL 10 VALUE "*********************BULLETIN BOARD
      -      "*********************" BLINK, HIGHLIGHT, FOREGROUND-COLOR 
             IS 2.
             05 LINE 10 COL 10 VALUE "----------------------------------
      -      "---------------------" FOREGROUND-COLOR IS 3.

             05 LINE 12 COL 10 VALUE "TITLE:   ".
             05 WS-TITLE-FIELD LINE 11 COL 18 PIC X(50) USING WS-TITLE.
             05 LINE 14 COL 10 VALUE "MESSAGE: ".
             05 LINE-1-FIELD LINE 16 COL 10 PIC X(60)  USING LS-PART-1.
             05 LINE-2-FIELD LINE 17 COL 10 PIC X(60)  USING LS-PART-2.
             05 LINE-3-FIELD LINE 18 COL 10 PIC X(60)  USING LS-PART-3.
             05 LINE-4-FIELD LINE 19 COL 10 PIC X(60)  USING LS-PART-4.
             05 LINE-5-FIELD LINE 20 COL 10 PIC X(60)  USING LS-PART-5. 

             05 LINE 22 COL 10 VALUE "----------------------------------
      -      "---------------------" FOREGROUND-COLOR IS 3.
             05 LINE 23 COL 10 VALUE "*********************LEAVE A MESSA
      -      "GE*******************" FOREGROUND-COLOR IS 2.
             05 LINE 24 COL 10 VALUE "----------------------------------
      -      "---------------------" FOREGROUND-COLOR IS 3.
             05 LINE 26 COL 10 VALUE "(s) Submit Message".
             05 LINE 27 COL 10 VALUE "(d) Discard Message".
             05 LINE 29 COL 10 VALUE "Choice:".
             05 MSG-WRITE-CHOICE-FIELD 
               LINE 29 COL 18 PIC X USING MSG-WRITE-CHOICE.
               
           
           01 GAMES-MENU-SCREEN
             BACKGROUND-COLOR IS 0.
             05 BLANK SCREEN.
             05 LINE  2 COL 2 PIC X(2) USING WS-FORMATTED-HOUR.
             05 LINE  2 COL 4 VALUE ":".
             05 LINE  2 COL 5 PIC X(2) USING WS-FORMATTED-MINS.

             05 LINE 30 COL 21 VALUE "(W) Guess The Word" 
             REVERSE-VIDEO, HIGHLIGHT FOREGROUND-COLOR IS 5.
             05 LINE 36 COL 18 VALUE "(G) Go back "
             REVERSE-VIDEO, HIGHLIGHT.
             05 LINE 36 COL 32 VALUE "(Q) Quit    "
             REVERSE-VIDEO, HIGHLIGHT.
             05 LINE 38 COL 18 VALUE "Pick: ".
             05 GAMES-MENU-CHOICE-FIELD LINE 38 COL 24 PIC X
                USING GAMES-MENU-CHOICE.  

      ******************************************************************
      *----WORD GUESSING GAME SCREEN SECTION----************************
      ******************************************************************             
       01 WORD-GUESSING-SCREEN
               BACKGROUND-COLOR IS 0.
             05 BLANK SCREEN.
             05 LINE 2 COLUMN 10 VALUE "Teenage Mutant Ninja Cobol".
             05 LINE 2 COLUMN 37 VALUE "Turtles Guessing Game".
             05 LINE 18 COLUMN 10 VALUE "Guess this word: ".
             05 LINE 20 COLUMN 10 PIC X(20) USING WS-WORD.
             05 LINE 22 COLUMN 10 VALUE "Guesses left: ".
             05 LINE 22 COLUMN 40 PIC 99 USING WS-GUESSES-LEFT.
             05 LINE 24 COLUMN 10 VALUE "( ) Enter a letter to guess".
             05 LINE 25 COLUMN 10 VALUE "(!) Quit game".
             05 LINE 26 COLUMN 10 VALUE "Pick: ".
   
           01 IN-GAME-SCREEN
           BACKGROUND-COLOR IS 0.
             05 BLANK SCREEN.
             05 LINE 2 COLUMN 10 VALUE "Teenage Mutant Ninja Cobol".
             05 LINE 2 COLUMN 37 VALUE "Turtles Guessing Game".
             05 LINE 34 COLUMN 10 VALUE "Guess this word: ".
             05 LINE 36 COLUMN 10 PIC X(20) USING WS-WORD.
             05 LINE 38 COLUMN 10 VALUE "Guesses left: ".
             05 LINE 38 COLUMN 40 PIC 99 USING WS-GUESSES-LEFT.
             05 LINE 40 COLUMN 10 VALUE "( ) Enter a letter to guess".
             05 LINE 41 COLUMN 10 VALUE "(!) Quit game".
             05 LINE 42 COLUMN 10 VALUE "Pick: ".
             05 WS-GUESS-CHOICE-FIELD LINE 42 COLUMN 16 PIC X
               USING WS-GUESS-CHOICE.

           01 WORD-GUESSING-LOSE-SCREEN
             BACKGROUND-COLOR IS 4.
             05 BLANK SCREEN.
             05 LINE 2 COLUMN 10 VALUE "Teenage Mutant Ninja Cobol".
             05 LINE 2 COLUMN 37 VALUE "Turtles Guessing Game".
             05 LINE 34 COLUMN 10 VALUE "You lost!".
             05 LINE 36 COLUMN 10 PIC X(20) USING WS-WORD.
             05 LINE 38 COLUMN 10 VALUE "Guesses left: ".
             05 LINE 38 COLUMN 40 PIC 99 USING WS-GUESSES-LEFT.
             05 LINE 39 COLUMN 10 VALUE "(p) Play again".
             05 LINE 40 COLUMN 10 VALUE "(h) See high scores".
             05 LINE 41 COLUMN 10 VALUE "(!) Quit game".
             05 LINE 42 COLUMN 10 VALUE "Pick: ".
             05 WS-GUESSING-CHOICE-LOSE-FIELD LINE 42 COLUMN 16 PIC X
               USING WS-GUESSING-LOSING-CHOICE.

           01 WORD-GUESSING-WINNING-SCREEN
             BACKGROUND-COLOR IS 2.
             05 BLANK SCREEN.
             05 LINE 2 COLUMN 10 VALUE "Teenage Mutant Ninja Cobol".
             05 LINE 2 COLUMN 37 VALUE "Turtles Guessing Game".
             05 LINE 34 COLUMN 10 VALUE "You guessed the word!".
             05 LINE 36 COLUMN 10 PIC X(20) USING WS-ANSWERWORD.
             05 LINE 38 COLUMN 10 PIC 99 USING WS-GUESSES-LEFT.
             05 LINE 40 COLUMN 10 VALUE "You scored: ".
             05 LINE 40 COLUMN 22 PIC 99 USING WS-HIGH-SCORE.
             05 LINE 42 COLUMN 10 VALUE "(p) Play Again".
             05 LINE 43 COLUMN 10 VALUE "(h) See High Scores".
             05 LINE 44 COLUMN 10 VALUE "(!) Quit game".
             05 LINE 45 COLUMN 10 VALUE "Pick: ".
             05 WS-GUESSING-CHOICE-WINNING-FIELD LINE 45 COLUMN 16 PIC X
               USING WS-GUESSING-WINNING-CHOICE.

           01 HIGH-SCORE-SCREEN
           BACKGROUND-COLOR IS 2.
             05 BLANK SCREEN.
             05 LINE 2 COLUMN 10 VALUE "Teenage Mutant Ninja Cobol".
             05 LINE 2 COLUMN 37 VALUE "Turtles Guessing Game".
             05 LINE 34 COLUMN 10 VALUE "High Scores:".
             05 LINE 36 COLUMN 10 PIC XX USING WS-SCORE(1).
             05 LINE 36 COLUMN 14 PIC X(10) USING WS-NAME(1).
             05 LINE 38 COLUMN 10 PIC XX USING WS-SCORE(2).
             05 LINE 38 COLUMN 14 PIC X(10) USING WS-NAME(2).
             05 LINE 40 COLUMN 10 PIC XX USING WS-SCORE(3).
             05 LINE 40 COLUMN 14 PIC X(10) USING WS-NAME(3).
             05 LINE 42 COLUMN 10 VALUE "(b) Go back".
             05 LINE 44 COLUMN 10 VALUE "Pick: ".
             05 WS-HIGH-SCORE-FIELD LINE 44 COLUMN 16 PIC X
               USING WS-HIGH-SCORE-CHOICE.


       PROCEDURE DIVISION.
      ************************************
      *----LOGIN / SIGN-IN/UP SECTION----*
      ************************************
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
               PERFORM 0110-DISPLAY-LOGIN
           END-IF.

       0111-SIGN-IN.
           SET COUNTER TO 0.
           OPEN INPUT F-USERS-FILE.
           MOVE 0 TO WS-FILE-IS-ENDED.
           PERFORM UNTIL WS-FILE-IS-ENDED = 1
               READ F-USERS-FILE
                   NOT AT END
                       ADD 1 TO COUNTER
                       MOVE USERNAME TO WS-UNAME(COUNTER)
                       MOVE USER-PASSWORD TO WS-PWORD(COUNTER)
                   AT END 
                       MOVE 1 TO WS-FILE-IS-ENDED
               END-READ 
           END-PERFORM.

           CLOSE F-USERS-FILE.
           INITIALIZE USER-NAME.
           INITIALIZE WS-PASSWORD.
           DISPLAY SIGN-IN-SCREEN.

           ACCEPT USER-NAME-FIELD.
           ACCEPT PASSWORD-FIELD.
           MOVE 0 TO WS-FOUND.
           MOVE 1 TO WS-IDX.
           ADD 1 TO COUNTER.
           PERFORM UNTIL WS-IDX = COUNTER
               IF USER-NAME = WS-UNAME(WS-IDX) AND 
               WS-PASSWORD = WS-PWORD(WS-IDX) THEN
                   MOVE 1 TO WS-FOUND 
               END-IF
               ADD 1 TO WS-IDX 
           END-PERFORM.

           IF WS-FOUND = 1 THEN
               PERFORM 0120-DISPLAY-MENU 
           ELSE 
               PERFORM 0113-ERROR-PAGE 
           END-IF. 

       0112-SIGN-UP.
           INITIALIZE NEW-USER-NAME.
           INITIALIZE NEW-PASSWORD.
           INITIALIZE CREATE-CHOICE
           DISPLAY CREATE-AN-ACCOUNT-SCREEN.
           ACCEPT NEW-USER-NAME-FIELD.
           ACCEPT NEW-PASSWORD-FIELD.
           ACCEPT CREATE-CHOICE-FIELD.
           IF CREATE-CHOICE = "q" OR "Q" THEN 
               PERFORM 0110-DISPLAY-LOGIN
           ELSE IF CREATE-CHOICE = "s" THEN 
               OPEN EXTEND F-USERS-FILE
               MOVE NEW-USER-NAME TO USERNAME
               MOVE NEW-PASSWORD TO USER-PASSWORD
               WRITE USERS
               END-WRITE               
           END-IF.
           CLOSE F-USERS-FILE.
           PERFORM 0111-SIGN-IN.

       0113-ERROR-PAGE.
           INITIALIZE ERROR-CHOICE.
           DISPLAY ERROR-SCREEN.
           ACCEPT ERROR-CHOICE-FIELD.
           IF ERROR-CHOICE = "l" OR "L" THEN 
               PERFORM 0111-SIGN-IN
           ELSE IF ERROR-CHOICE = "c" OR "C" THEN 
               PERFORM 0112-SIGN-UP 
           ELSE 
               PERFORM 0113-ERROR-PAGE 
           END-IF.
      **************************************************     
      *----DISPLAY MENU COMES AFTER SUCCESFUL LOGIN----*
      **************************************************
       0120-DISPLAY-MENU.
           PERFORM 0200-TIME-AND-DATE.
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
           END-IF.

           PERFORM 0120-DISPLAY-MENU.
      ************************************************
      *----MESSAGE SECTION FOR READ/WRITE/COMMENT----*
      ************************************************
       0130-MSG-MENU.
           PERFORM 0200-TIME-AND-DATE.
           CALL 'number-of-file-lines' USING NUM-FILE-LINES.
           CALL 'get-list-page-alt' USING NUM-FILE-LINES WS-LIST-TABLE.
           *> CALL 'id-sort' USING WS-LIST-TABLE. <*
           INITIALIZE MSG-MENU-CHOICE.
           DISPLAY MSG-MENU-SCREEN.
           ACCEPT MSG-MENU-CHOICE-FIELD.
           MOVE MSG-MENU-CHOICE TO MSG-SELECT.
         
           IF MSG-SELECT > 0 THEN
             PERFORM 0140-MESSAGE-VIEW
           END-IF. 
           IF MSG-MENU-CHOICE =        "g" OR 'G' THEN
               PERFORM 0120-DISPLAY-MENU
           ELSE IF MSG-MENU-CHOICE =   "n" OR 'N' THEN
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
           CALL 'number-of-file-lines' USING NUM-FILE-LINES.
           CALL 'get-list-page-alt' USING NUM-FILE-LINES WS-LIST-TABLE.
           *> CALL 'id-sort' USING WS-LIST-TABLE. <*        
           MOVE LIST-CONTENT(MSG-SELECT) TO WS-CONTENT-DISPLAY.
           INITIALIZE MSG-VIEW-CHOICE.
           DISPLAY MESSAGE-VIEW-SCREEN.
           ACCEPT MSG-VIEW-CHOICE-FIELD.

           IF MSG-VIEW-CHOICE =        "n" OR 'N' THEN
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
           
           PERFORM 0140-MESSAGE-VIEW. 

       0150-MESSAGE-WRITE.
           PERFORM 0200-TIME-AND-DATE.
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

           IF MSG-WRITE-CHOICE-FIELD = "d" OR "D" THEN
             PERFORM 0130-MSG-MENU
           END-IF.

           IF MSG-WRITE-CHOICE-FIELD = "s" OR "S" THEN 
              MOVE WS-CONTENT-DISPLAY TO WS-CONTENT
              MOVE USER-NAME TO WS-USERNAME

             IF WS-TITLE-FIELD NOT = SPACE AND LOW-VALUE THEN
               CALL 'post-message' USING NEW-MESSAGE
               PERFORM 0130-MSG-MENU
             END-IF
           END-IF.
           
           

           PERFORM 0120-DISPLAY-MENU.
      *******************************
      *----FUN AND GAMES SECTION----*
      *******************************
       0160-GAMES-MENU.
           PERFORM 0200-TIME-AND-DATE.
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
      ***************************
      *----TIME/DATE SECTION----*
      ***************************
       0200-TIME-AND-DATE.
           MOVE FUNCTION CURRENT-DATE TO WS-DATETIME. 
           MOVE WS-DATETIME(1:4)  TO WS-FORMATTED-YEAR.
           MOVE WS-DATETIME(5:2)  TO WS-FORMATTED-MONTH.
           MOVE WS-DATETIME(7:2)  TO WS-FORMATTED-DY.
           MOVE WS-DATETIME(9:2)  TO WS-FORMATTED-HOUR.
           MOVE WS-DATETIME(11:2) TO WS-FORMATTED-MINS.
           MOVE WS-DATETIME(13:2) TO WS-FORMATTED-SEC.
           MOVE WS-DATETIME(15:2) TO WS-FORMATTED-MS.
      ************************************
      *----WORD GUESSING GAME SECTION----*
      ************************************
       0210-DISPLAY-GUESSING-GAME.
           PERFORM 0200-TIME-AND-DATE.
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
             IF '*' EQUALS WS-WORD(COUNTER:1) 
              THEN ADD 1 TO WS-WORD-LENGTH
             END-IF
             ADD 1 TO COUNTER
           END-PERFORM.
           PERFORM 0220-IN-GAME-SCREEN.
          
       0220-IN-GAME-SCREEN.
           PERFORM 0200-TIME-AND-DATE.
           INITIALIZE WS-GUESS-CHOICE.
           DISPLAY IN-GAME-SCREEN.
           ACCEPT WS-GUESS-CHOICE-FIELD.
           IF WS-GUESS-CHOICE = '!' THEN 
               PERFORM 0120-DISPLAY-MENU
           ELSE
               PERFORM 0230-CHECK-GUESS
           END-IF.
           
       0230-CHECK-GUESS.
           MOVE 1 TO COUNTER.
           PERFORM UNTIL COUNTER = 20
                 IF WS-GUESS-CHOICE = WS-ANSWERWORD(COUNTER:1) 
                 THEN
                      MOVE WS-GUESS-CHOICE TO WS-WORD(COUNTER:1) 
                 END-IF
                 ADD 1 TO COUNTER     
           END-PERFORM.
           SUBTRACT 1 FROM WS-GUESSES-LEFT.
           MOVE 1 TO COUNTER.
           MOVE 0 TO WS-LETTERS-LEFT.
           PERFORM UNTIL COUNTER = 20
             IF '*' EQUALS WS-WORD(COUNTER:1) 
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
           INITIALIZE WS-GUESSING-WINNING-CHOICE.
           COMPUTE WS-HIGH-SCORE = WS-LETTERS-LEFT * WS-GUESSES-LEFT.
           DISPLAY WORD-GUESSING-WINNING-SCREEN.
           OPEN EXTEND F-HIGH-SCORES-FILE
               MOVE WS-HIGH-SCORE TO HIGH-SCORE
               MOVE USER-NAME TO PLAYER-NAME
               WRITE PLAYER-SCORES 
               END-WRITE.
           CLOSE F-HIGH-SCORES-FILE.
           ACCEPT WS-GUESSING-WINNING-CHOICE.
           IF WS-GUESSING-WINNING-CHOICE = 'p'
               THEN PERFORM 0210-DISPLAY-GUESSING-GAME
           ELSE IF WS-GUESSING-WINNING-CHOICE = 'h'
             THEN PERFORM 0260-HIGH-SCORE-TABLE
           ELSE IF WS-GUESSING-WINNING-CHOICE = '!'
             THEN PERFORM 0120-DISPLAY-MENU
           ELSE
             PERFORM 0240-WINNING-SCREEN
           END-IF.

       0250-LOSING-SCREEN.
           PERFORM 0280-CURRENT-TIME
           INITIALIZE WS-GUESSING-LOSING-CHOICE.
           DISPLAY WORD-GUESSING-LOSE-SCREEN.
           ACCEPT WS-GUESSING-LOSING-CHOICE.
           IF WS-GUESSING-LOSING-CHOICE = 'p'
               THEN PERFORM 0210-DISPLAY-GUESSING-GAME
           ELSE IF WS-GUESSING-LOSING-CHOICE = 'h'
             THEN PERFORM 0260-HIGH-SCORE-TABLE
           ELSE IF WS-GUESSING-LOSING-CHOICE = '!'
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
           PERFORM 0280-CURRENT-TIME
           INITIALIZE WS-HIGH-SCORE-CHOICE.
           SORT WS-TABLE-HIGH-SCORE ON DESCENDING WS-SCORE.
           DISPLAY HIGH-SCORE-SCREEN.
           ACCEPT WS-HIGH-SCORE-CHOICE.
           IF WS-HIGH-SCORE-CHOICE = "b" OR "B"
             PERFORM 0120-DISPLAY-MENU
           ELSE 
               PERFORM 0270-HIGH-SCORE-SCREEN
           END-IF.

       0280-CURRENT-TIME.
           MOVE FUNCTION CURRENT-DATE TO WS-TIME.

      *******************
      *----PONG GAME----*
      *******************   
       
           *>0230 THRU 0290

      *************************
      *----CREDITS SECTION----*
      ************************* 

           *> 0300 THRU 0399
       