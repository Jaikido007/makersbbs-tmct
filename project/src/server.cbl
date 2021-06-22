       IDENTIFICATION DIVISION.
       PROGRAM-ID. server.

       ENVIRONMENT DIVISION.
           CONFIGURATION SECTION.
           REPOSITORY.
               FUNCTION MESSAGE-CHOICE-TO-NUM
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
              05 USERNAME PIC X(10).
              05 USER-PASSWORD PIC X(20).   
                      
           WORKING-STORAGE SECTION.
      *     Variables related to login and menu screen
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
      
      *    Variables related to guessing game
           01 WS-ANSWERWORD PIC X(20).
           01 RANDOMNUMBER PIC 99.
           01 WS-WORD PIC X(20).
           01 WS-GUESSING-CHOICE-WORDS.
               05 WS-GUESSING-CHOICE-WORD OCCURS 213 TIMES
               DESCENDING KEY IS WS-GUESSING-WORDS-WORD
               INDEXED BY WORD-IDX.
                   10 WS-GUESSING-WORDS-WORD PIC X(20).
           01 WS-GUESS-CHOICE PIC X.

      *    Variables related to high score screen
           01 WS-HIGH-SCORE-CHOICE PIC X.
           01 WS-HIGH-SCORE PIC 99.
           01 WS-HIGH-SCORES.  
              05 WS-TABLE-HIGH-SCORE OCCURS 100 TIMES     
              ASCENDING KEY IS WS-SCORE
              INDEXED BY SCORE-IDX.
                  10 WS-SCORE PIC 99.
                  10 WS-NAME PIC X(10).

      *    Variables related to checking guesses  
           01 WS-LETTERS-LEFT PIC 99.
           01 WS-GUESSES-LEFT PIC 99.          

      *    Variables related to winning and losing.
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
           
           01 NEW-MESSAGE.
             05 WS-TITLE               PIC X(50).
             05 WS-CONTENT             PIC X(300).
             05 WS-USERNAME            PIC X(16).
           
           LINKAGE SECTION.
           01 LS-COUNTER UNSIGNED-INT.
           01 LS-NUM UNSIGNED-INT.
           01 LS-MESSAGE PIC X(60).  

           SCREEN SECTION.
           01 LOGIN-SCREEN
               BACKGROUND-COLOR IS 0.
               05 BLANK SCREEN.
               05 LINE 2 COL 2 PIC X(2) USING WS-FORMATTED-HOUR.
               05 LINE 2 COL 4 VALUE ":".
               05 LINE 2 COL 5 PIC X(2) USING WS-FORMATTED-MINS.  
               05 LINE 4 COL 12 VALUE "MAKERS BBS" UNDERLINE, BLINK
               HIGHLIGHT, FOREGROUND-COLOR IS 3.
          
               05 LINE 7 COLUMN 10 VALUE "(L) Go to Log-in.".
               05 LINE 8 COLUMN 10 VALUE "(C) Create an account.".
               05 LINE 9 COLUMN 10 VALUE "(Q) Quit.".
               05 LINE 11 COLUMN 10 VALUE "Pick: ".
               05 LOGIN-CHOICE-FIELD LINE 11 COLUMN 16 PIC X
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
             05 USER-NAME-FIELD LINE 7 COLUMN 10 PIC X(10)
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
             05 LINE  2 COL 2 PIC X(2) USING WS-FORMATTED-HOUR.
             05 LINE  2 COL 4 VALUE ":".
             05 LINE  2 COL 5 PIC X(2) USING WS-FORMATTED-MINS.  
             05 LINE  4 COL 10 VALUE "MAKERS BBS" UNDERLINE.
             05 LINE  6 COL 10 VALUE "Hi, ".
             05 LINE  6 COL 14 PIC X(16) USING USER-NAME.
             05 LINE  8 COL 10 VALUE "Welcome to COBOL The Barbarian's s
      -      "tate of the art Bulletin Board.".  
             05 LINE  9 COL 10 VALUE "Feel free to:".
             05 LINE 10 COL 24 VALUE "* " FOREGROUND-COLOR IS 2.
             05 LINE 10 COL 26 VALUE "Read our message board.".
             05 LINE 11 COL 24 VALUE "* " FOREGROUND-COLOR IS 5.
             05 LINE 11 COL 26 VALUE "Play a few games.".
             05 LINE 12 COL 24 VALUE "* " FOREGROUND-COLOR IS 2.
             05 LINE 12 COL 26 VALUE "Leave a message of your own.". 
             05 LINE 13 COL 24 VALUE "* " FOREGROUND-COLOR IS 5.
             05 LINE 13 COL 26 VALUE "Most importantly. HAVE FUN!". 

             05 LINE 19 COL 24 VALUE "(m) Messages    "
                REVERSE-VIDEO HIGHLIGHT FOREGROUND-COLOR IS 2.
             05 LINE 19 COL 42 VALUE "(f) Fun & games "
                REVERSE-VIDEO, HIGHLIGHT FOREGROUND-COLOR IS 5.
             05 LINE 21 COL 24 VALUE "(l) Logout      "
                REVERSE-VIDEO , HIGHLIGHT.            
             05 LINE 21 COL 42 VALUE "(q) Quit        "
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
             05 LINE 27 COL 24 VALUE "(w) Write your own message "
             REVERSE-VIDEO HIGHLIGHT FOREGROUND-COLOR IS 2.               
             05 LINE 29 COL 18 VALUE "(n) Next Page     "
             REVERSE-VIDEO, HIGHLIGHT FOREGROUND-COLOR IS 6.  
             05 LINE 29 COL 41 VALUE "(p) Previous Page "
             REVERSE-VIDEO, HIGHLIGHT FOREGROUND-COLOR IS 6. 
             05 LINE 31 COL 18 VALUE "(g) Go back       "
             REVERSE-VIDEO, HIGHLIGHT.
             05 LINE 31 COL 41 VALUE "(q) Quit          "
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

             05 LINE 27 COL 25 VALUE "(g) Go back"
                REVERSE-VIDEO , HIGHLIGHT.            
             05 LINE 27 COL 39 VALUE "(q) Quit   "
                REVERSE-VIDEO, HIGHLIGHT.  
             05 LINE 28 COL 25 VALUE "Pick: ".
             05 MSG-VIEW-CHOICE-FIELD LINE 28 COL 31 PIC X 
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
           
           01 GAMES-MENU-SCREEN
             BACKGROUND-COLOR IS 0.
             05 BLANK SCREEN.
             05 LINE  2 COL 2 PIC X(2) USING WS-FORMATTED-HOUR.
             05 LINE  2 COL 4 VALUE ":".
             05 LINE  2 COL 5 PIC X(2) USING WS-FORMATTED-MINS.

             05 LINE 30 COL 21 VALUE "(n) Guess The Number" 
             REVERSE-VIDEO, HIGHLIGHT FOREGROUND-COLOR IS 5.
             05 LINE 32 COL 21 VALUE "(o) O and X         "  
             REVERSE-VIDEO, HIGHLIGHT FOREGROUND-COLOR IS 5.
             05 LINE 34 COL 21 VALUE "(m) Monkey?         " 
             REVERSE-VIDEO, HIGHLIGHT FOREGROUND-COLOR IS 6.
             05 LINE 36 COL 18 VALUE "(g) Go back "
             REVERSE-VIDEO, HIGHLIGHT.
             05 LINE 36 COL 32 VALUE "(q) Quit    "
             REVERSE-VIDEO, HIGHLIGHT.
             05 LINE 38 COL 18 VALUE "Pick: ".
             05 GAMES-MENU-CHOICE-FIELD LINE 38 COL 24 PIC X
                USING GAMES-MENU-CHOICE.     

       PROCEDURE DIVISION.

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

       0130-MSG-MENU.
           PERFORM 0200-TIME-AND-DATE.
           CALL 'number-of-file-lines' USING NUM-FILE-LINES.
           CALL 'get-list-page-alt' USING NUM-FILE-LINES WS-LIST-TABLE.
          *>  CALL 'id-sort' USING WS-LIST-TABLE.
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
           MOVE LIST-CONTENT(MSG-SELECT) TO WS-CONTENT-DISPLAY.
           INITIALIZE MSG-VIEW-CHOICE.
           DISPLAY MESSAGE-VIEW-SCREEN.
           ACCEPT MSG-VIEW-CHOICE-FIELD.
           IF MSG-VIEW-CHOICE =        "g" OR "G" THEN
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
           DISPLAY WRITE-MSG-SCREEN.
           
           ACCEPT WS-TITLE-FIELD.
           ACCEPT LINE-1-FIELD.
           ACCEPT LINE-2-FIELD.
           ACCEPT LINE-3-FIELD.
           ACCEPT LINE-4-FIELD.
           ACCEPT LINE-5-FIELD.
           
           MOVE WS-CONTENT-DISPLAY TO WS-CONTENT.
           MOVE USER-NAME TO WS-USERNAME.

           IF WS-TITLE-FIELD NOT = SPACE AND LOW-VALUE THEN
             CALL 'post-message' USING NEW-MESSAGE
             PERFORM 0130-MSG-MENU
           END-IF.

           PERFORM 0120-DISPLAY-MENU.
       
       0160-GAMES-MENU.
           PERFORM 0200-TIME-AND-DATE.
           INITIALIZE GAMES-MENU-CHOICE.
           DISPLAY GAMES-MENU-SCREEN.
           ACCEPT GAMES-MENU-CHOICE-FIELD
           IF GAMES-MENU-CHOICE =      "q" or "Q" THEN
               STOP RUN
           ELSE IF GAMES-MENU-CHOICE = "g" or "G" THEN
               PERFORM 0120-DISPLAY-MENU   
           END-IF.

           PERFORM 0160-GAMES-MENU.

       0200-TIME-AND-DATE.
           MOVE FUNCTION CURRENT-DATE TO WS-DATETIME. 
           MOVE WS-DATETIME(1:4)  TO WS-FORMATTED-YEAR.
           MOVE WS-DATETIME(5:2)  TO WS-FORMATTED-MONTH.
           MOVE WS-DATETIME(7:2)  TO WS-FORMATTED-DY.
           MOVE WS-DATETIME(9:2)  TO WS-FORMATTED-HOUR.
           MOVE WS-DATETIME(11:2) TO WS-FORMATTED-MINS.
           MOVE WS-DATETIME(13:2) TO WS-FORMATTED-SEC.
           MOVE WS-DATETIME(15:2) TO WS-FORMATTED-MS.

       