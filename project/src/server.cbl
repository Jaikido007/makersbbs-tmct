       IDENTIFICATION DIVISION.
       PROGRAM-ID. server.
       ENVIRONMENT DIVISION.
           CONFIGURATION SECTION.
           REPOSITORY.
               FUNCTION MESSAGE-CHOICE-TO-NUM
               FUNCTION DISPLAY-MESSAGE-TITLE
               FUNCTION DISPLAY-MESSAGE-BODY
               FUNCTION REPLACE-LETTER.
           INPUT-OUTPUT SECTION.
           FILE-CONTROL.
           SELECT F-MESSAGE-FILE ASSIGN TO "messages.dat"
             ORGANIZATION IS LINE SEQUENTIAL.
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
           FD F-MESSAGE-FILE.
           01 MESSAGES.
              05 MESSAGE-TITLE PIC X(60).
              05 MESSAGE-BODY PIC X(500).
              05 MESSAGE-DATE PIC X(10).
              05 MESSAGE-AUTHOR PIC X(10).
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
           01 USER-NAME PIC X(10).
           01 WS-PASSWORD PIC X(20).
           01 NEW-USER-NAME PIC X(10).
           01 NEW-PASSWORD PIC X(20).
           01 LOGIN-CHOICE PIC X.
           01 MENU-CHOICE PIC X.
           01 ERROR-CHOICE PIC X.
           01 CREATE-CHOICE PIC X.
           01 WS-USERS.
               05 WS-USER OCCURS 100 TIMES
               ASCENDING KEY IS WS-USERNAME
               INDEXED BY USER-IDX.
                   10 WS-USERNAME PIC X(10).
                   10 WS-PWORD PIC X(20).
           01 WS-FOUND PIC 9.
           01 WS-IDX UNSIGNED-INT. 

      *    Variables related to creating table and reading file
           01 WS-FILE-IS-ENDED PIC 9.
           01 WS-MSGS.
               05 WS-MSG OCCURS 100 TIMES
               ASCENDING KEY IS WS-TITLE
               INDEXED BY MSG-IDX.
                   10 WS-TITLE PIC X(60).
                   10 WS-BODY PIC X(500).
                   10 WS-DATE PIC X(10).
                   10 WS-AUTHOR PIC X(10).

      *    Variables related to display message board screen
           01 PAGE-NUM PIC 99.
           01 TITLE-NUM PIC 99.
           01 DISPLAY-MESSAGE PIC X(40).
           01 COUNTER UNSIGNED-INT.
           01 OFFSET UNSIGNED-INT.
           01 MESSAGE-CHOICE PIC XX.

      *    Variables related to read message screen
           01 READ-CHOICE PIC X.
           01 BODY PIC X(500).
           01 TITLE PIC X(60).
           01 POST-AUTHOR PIC X(10).
           01 POST-DATE PIC X(10).
           01 MESSAGE-NUM UNSIGNED-INT.
           01 RESULT UNSIGNED-INT.

      *    Variables related to post message screen
           01 POST-TITLE PIC X(60).
           01 POST-BODY PIC X(500).
           01 POST-CHOICE PIC X.
           01 WS-FORMATTED-DATE PIC X(10).
      *    Variables related to guessing game
           01 WS-ANSWERWORD PIC X(20).
           01 RANDOMNUMBER PIC 99.
           01 WS-WORD PIC X(20).
           01 WS-GUESSING-CHOICE-WORDS.
               05 WS-GUESSING-CHOICE-WORD OCCURS 213 TIMES
               DESCENDING KEY IS WS-GUESSING-WORDS-WORD
               INDEXED BY WORD-IDX.
                   10 WS-GUESSING-WORDS-WORD PIC X(20).
           01 WS-GUESS-CHOICE PIC X(20).

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
    
           SCREEN SECTION.
           01 LOGIN-SCREEN.
             05 BLANK SCREEN.
             05 LINE 2 COLUMN 10 VALUE "Makers BBS".
             05 LINE 4 COLUMN 10 VALUE "(l) Go to Log-in.".
             05 LINE 5 COLUMN 10 VALUE "(c) Create an account.".
             05 LINE 6 COLUMN 10 VALUE "(q) Quit.".
             05 LINE 8 COLUMN 10 VALUE "Pick: ".
             05 LOGIN-CHOICE-FIELD LINE 8 COLUMN 16 PIC X
                USING LOGIN-CHOICE.

           01 SIGN-IN-SCREEN
             BACKGROUND-COLOR IS 8.
             05 BLANK SCREEN.
             05 LINE 2 COLUMN 10 VALUE "Makers BBS".
             05 LINE 4 COLUMN 10 VALUE "Enter your username:".
             05 USER-NAME-FIELD LINE 6 COLUMN 10 PIC X(10)
                USING USER-NAME.
             05 LINE 8 COLUMN 10 VALUE "Enter your password:".
             05 PASSWORD-FIELD LINE 10 COLUMN 10 PIC X(20)
                USING WS-PASSWORD.
           
           01 ERROR-SCREEN
             BACKGROUND-COLOR IS 8.
             05 BLANK SCREEN.
             05 LINE 2 COLUMN 10 VALUE "Makers BBS".
             05 LINE 4 COLUMN 10 VALUE "Incorrect Username or Password".
             05 LINE 6 COLUMN 10 VALUE "(l) Back to Log-in.".
             05 LINE 7 COLUMN 10 VALUE "(c) Create an account.".
             05 LINE 9 COLUMN 10 VALUE "Pick: ".
             05 ERROR-CHOICE-FIELD LINE 9 COLUMN 16 PIC X
                USING ERROR-CHOICE.

           01 CREATE-AN-ACCOUNT-SCREEN
             BACKGROUND-COLOR IS 8.
             05 BLANK SCREEN.
             05 LINE 2 COLUMN 10 VALUE "Makers BBS".
             05 LINE 4 COLUMN 10 VALUE "Create your account".
             05 LINE 6 COLUMN 10 VALUE "Enter a username:".
             05 NEW-USER-NAME-FIELD LINE 8 COLUMN 10 PIC X(10)
                USING NEW-USER-NAME.
             05 LINE 10 COLUMN 10 VALUE "Enter a password:".
             05 LINE 10 COLUMN 28 VALUE "(password must be lowercase,".
             05 LINE 10 COLUMN 56 VALUE "max 20 characters)".
             05 NEW-PASSWORD-FIELD LINE 12 COLUMN 10 PIC X(20)
                USING NEW-PASSWORD.
             05 LINE 14 COLUMN 10 VALUE "(s) Submit".
             05 LINE 15 COLUMN 10 VALUE "(q) Go Back".
             05 LINE 17 COLUMN 10 VALUE "Pick: ".
             05 CREATE-CHOICE-FIELD LINE 17 COLUMN 16 PIC X
                USING CREATE-CHOICE.
 

           01 MENU-SCREEN
             BACKGROUND-COLOR IS 8.
             05 BLANK SCREEN.
             05 LINE 2 COLUMN 10 VALUE "Makers BBS".
             05 LINE 4 COLUMN 10 VALUE "Welcome, ".
             05 LINE 4 COLUMN 19 PIC X(10) USING USER-NAME.
             05 LINE 8 COLUMN 10 VALUE "(n) Nothing".
             05 LINE 8 COLUMN 80 VALUE "(m) Message board".
             05 LINE 8 COLUMN 100 VALUE "(g) Guessing Game".
             05 LINE 8 COLUMN 30 VALUE "(l) Logout".
             05 LINE 8 COLUMN 60 VALUE "(q) Quit".
             05 LINE 20 COLUMN 10 VALUE "Pick: ".
             05 MENU-CHOICE-FIELD LINE 20 COLUMN 16 PIC X
                USING MENU-CHOICE.

           01 MESSAGEBOARD-SCREEN
             BACKGROUND-COLOR IS 8.
            05 BLANK SCREEN.
            05 LINE 2 COLUMN 10 VALUE "Makers BBS".
            05 LINE 2 COLUMN 30 VALUE "Page: ".
            05 LINE 2 COLUMN 37 PIC 99 USING PAGE-NUM.
            05 LINE 4 COLUMN 10 PIC X(40) USING DISPLAY-MESSAGE.
            05 LINE 6 COLUMN 10 VALUE "1.".
            05 LINE 6 COLUMN 14 PIC X(60) USING WS-TITLE(OFFSET).
            05 LINE 6 COLUMN 75 VALUE "Posted by:".
            05 LINE 6 COLUMN 87 PIC X(10) USING WS-AUTHOR(OFFSET).
            05 LINE 7 COLUMN 10 VALUE "2.".
            05 LINE 7 COLUMN 14 PIC X(60) USING WS-TITLE(OFFSET - 1).
            05 LINE 7 COLUMN 75 VALUE "Posted by:".
            05 LINE 7 COLUMN 87 PIC X(10) USING WS-AUTHOR(OFFSET - 1).
            05 LINE 8 COLUMN 10 VALUE "3.".
            05 LINE 8 COLUMN 14 PIC X(60) USING WS-TITLE(OFFSET - 2).
            05 LINE 8 COLUMN 75 VALUE "Posted by:".
            05 LINE 8 COLUMN 87 PIC X(10) USING WS-AUTHOR(OFFSET - 2).
            05 LINE 9 COLUMN 10 VALUE "4.".
            05 LINE 9 COLUMN 14 PIC X(60) USING WS-TITLE(OFFSET - 3).
            05 LINE 9 COLUMN 75 VALUE "Posted by:".
            05 LINE 9 COLUMN 87 PIC X(10) USING WS-AUTHOR(OFFSET - 3).
            05 LINE 10 COLUMN 10 VALUE "5.".
            05 LINE 10 COLUMN 14 PIC X(60) USING WS-TITLE(OFFSET - 4).
            05 LINE 10 COLUMN 75 VALUE "Posted by:".
            05 LINE 10 COLUMN 87 PIC X(10) USING WS-AUTHOR(OFFSET - 4).
            05 LINE 11 COLUMN 10 VALUE "6.".
            05 LINE 11 COLUMN 14 PIC X(60) USING WS-TITLE(OFFSET - 5).
            05 LINE 11 COLUMN 75 VALUE "Posted by:".
            05 LINE 11 COLUMN 87 PIC X(10) USING WS-AUTHOR(OFFSET - 5).
            05 LINE 12 COLUMN 10 VALUE "7.".
            05 LINE 12 COLUMN 14 PIC X(60) USING WS-TITLE(OFFSET - 6).
            05 LINE 12 COLUMN 75 VALUE "Posted by:".
            05 LINE 12 COLUMN 87 PIC X(10) USING WS-AUTHOR(OFFSET - 6).
            05 LINE 13 COLUMN 10 VALUE "8.".
            05 LINE 13 COLUMN 14 PIC X(60) USING WS-TITLE(OFFSET - 7).
            05 LINE 13 COLUMN 75 VALUE "Posted by:".
            05 LINE 13 COLUMN 87 PIC X(10) USING WS-AUTHOR(OFFSET - 7).
            05 LINE 14 COLUMN 10 VALUE "9.".
            05 LINE 14 COLUMN 14 PIC X(60) USING WS-TITLE(OFFSET - 8).    
            05 LINE 14 COLUMN 75 VALUE "Posted by:".
            05 LINE 14 COLUMN 87 PIC X(10) USING WS-AUTHOR(OFFSET - 8).
            05 LINE 15 COLUMN 10 VALUE "10.".
            05 LINE 15 COLUMN 14 PIC X(60) USING WS-TITLE(OFFSET - 9).
            05 LINE 15 COLUMN 75 VALUE "Posted by:".
            05 LINE 15 COLUMN 87 PIC X(10) USING WS-AUTHOR(OFFSET - 9).
            05 LINE 17 COLUMN 10 VALUE "( ) Read the full message by".
            05 LINE 17 COLUMN 39 VALUE "number".
            05 LINE 18 COLUMN 10 VALUE "(m) Post a message of your own".
            05 LINE 19 COLUMN 10 VALUE "(n) Next page".
            05 LINE 19 COLUMN 30 VALUE "(p) Previous page".
            05 LINE 19 COLUMN 60 VALUE "(q) Go back".
            05 LINE 21 COLUMN 10 VALUE "Pick: ".
            05 MESSAGE-CHOICE-FIELD LINE 21 COLUMN 16 PIC X
                USING MESSAGE-CHOICE.

           01 READ-MESSAGE-SCREEN
           BACKGROUND-COLOR IS 8.
            05 BLANK SCREEN.
            05 LINE 2 COLUMN 10 VALUE "Makers BBS".
            05 LINE 4 COLUMN 10 VALUE "Title:".
            05 LINE 4 COLUMN 18 PIC X(60) USING TITLE.
            05 LINE 6 COLUMN 10 PIC X(500) USING BODY.
            05 LINE 15 COLUMN 10 VALUE "Post Author:".
            05 LINE 15 COLUMN 23 PIC X(10) USING POST-AUTHOR.
            05 LINE 15 COLUMN 34 VALUE "Posted On:".
            05 LINE 15 COLUMN 45 PIC X(10) USINg POST-DATE. 
            05 LINE 18 COLUMN 10 VALUE "(n) Next message".
            05 LINE 18 COLUMN 30 VALUE "(p) Previous message".
            05 LINE 18 COLUMN 60 VALUE "(q) Go back".   
            05 LINE 20 COLUMN 10 VALUE "Pick: ".
            05 READ-CHOICE-FIELD LINE 20 COLUMN 16 PIC X
                USING READ-CHOICE.

           01 POST-MESSAGE-SCREEN
           BACKGROUND-COLOR IS 8.
           05 BLANK SCREEN. 
           05 LINE 2 COLUMN 10 VALUE "Makers BBS".
           05 LINE 4 COLUMN 10 VALUE "Post a message".
           05 LINE 6 COLUMN 10 VALUE "Title".
           05 POST-TITLE-FIELD LINE 7 COLUMN 10 PIC X(60)
           USING POST-TITLE.
           05 LINE 9 COLUMN 10 VALUE "Body".
           05 POST-BODY-FIELD LINE 10 COLUMN 10 PIC X(500)
           USING POST-BODY.
           05 LINE 18 COLUMN 10 VALUE "(p) Post".
           05 LINE 18 COLUMN 30 VALUE "(d) Discard".
           05 LINE 20 COLUMN 10 VALUE "Pick: ".
           05 POST-CHOICE-FIELD LINE 20 COLUMN 16 PIC X
                USING POST-CHOICE.
        
           01 WORD-GUESSING-SCREEN
               BACKGROUND-COLOR IS 8.
             05 BLANK SCREEN.
             05 LINE 2 COLUMN 10 VALUE "Makers BBS".
             05 LINE 4 COLUMN 10 VALUE "Guess this word: ".
             05 LINE 6 COLUMN 10 PIC X(20) USING WS-WORD.
             05 LINE 8 COLUMN 10 VALUE "Guesses left: ".
             05 LINE 8 COLUMN 40 PIC 99 USING WS-GUESSES-LEFT.
             05 LINE 10 COLUMN 10 VALUE "( ) Enter a letter to guess".
             05 LINE 11 COLUMN 10 VALUE "(!) Quit game".
             05 LINE 13 COLUMN 10 VALUE "Pick: ".
   
           01 IN-GAME-SCREEN
           BACKGROUND-COLOR IS 8.
             05 BLANK SCREEN.
             05 LINE 2 COLUMN 10 VALUE "Makers BBS".
             05 LINE 4 COLUMN 10 VALUE "Guess this word: ".
             05 LINE 6 COLUMN 10 PIC X(20) USING WS-WORD.
             05 LINE 8 COLUMN 10 VALUE "Guesses left: ".
             05 LINE 8 COLUMN 40 PIC 99 USING WS-GUESSES-LEFT.
             05 LINE 10 COLUMN 10 VALUE "( ) Enter a letter to guess".
             05 LINE 11 COLUMN 10 VALUE "(!) Quit game".
             05 LINE 13 COLUMN 10 VALUE "Pick: ".
             05 WS-GUESS-CHOICE-FIELD LINE 13 COLUMN 16 PIC X
               USING WS-GUESS-CHOICE.

           01 WORD-GUESSING-LOSE-SCREEN
             BACKGROUND-COLOR IS 8.
             05 BLANK SCREEN.
             05 LINE 2 COLUMN 10 VALUE "Makers BBS".
             05 LINE 4 COLUMN 10 VALUE "You lost!".
             05 LINE 6 COLUMN 10 PIC X(20) USING WS-WORD.
             05 LINE 8 COLUMN 10 VALUE "Guesses left: ".
             05 LINE 8 COLUMN 40 PIC 99 USING WS-GUESSES-LEFT.
             05 LINE 10 COLUMN 10 VALUE "(p) Play again".
             05 LINE 11 COLUMN 10 VALUE "(h) See high scores".
             05 LINE 12 COLUMN 10 VALUE "(!) Quit game".
             05 LINE 13 COLUMN 10 VALUE "Pick: ".
             05 WS-GUESSING-CHOICE-LOSE-FIELD LINE 13 COLUMN 16 PIC X
               USING WS-GUESSING-LOSING-CHOICE.

           01 WORD-GUESSING-WINNING-SCREEN
             BACKGROUND-COLOR IS 8.
             05 BLANK SCREEN.
             05 LINE 2 COLUMN 10 VALUE "Makers BBS".
             05 LINE 4 COLUMN 10 VALUE "You guessed the word!".
             05 LINE 6 COLUMN 10 PIC X(20) USING WS-ANSWERWORD.
             05 LINE 8 COLUMN 10 PIC 99 USING WS-GUESSES-LEFT.
             05 LINE 10 COLUMN 10 VALUE "You scored: ".
             05 LINE 10 COLUMN 22 PIC 99 USING WS-HIGH-SCORE.
             05 LINE 12 COLUMN 10 VALUE "(p) Play Again".
             05 LINE 13 COLUMN 10 VALUE "(h) See High Scores".
             05 LINE 14 COLUMN 10 VALUE "(!) Quit game".
             05 LINE 15 COLUMN 10 VALUE "Pick: ".
             05 WS-GUESSING-CHOICE-WINNING-FIELD LINE 13 COLUMN 16 PIC X
               USING WS-GUESSING-WINNING-CHOICE.

           01 HIGH-SCORE-SCREEN
           BACKGROUND-COLOR IS 8.
             05 BLANK SCREEN.
             05 LINE 2 COLUMN 10 VALUE "Makers BBS".
             05 LINE 4 COLUMN 10 VALUE "High Scores:".
             05 LINE 6 COLUMN 10 PIC XX USING WS-SCORE(1).
             05 LINE 6 COLUMN 14 PIC X(10) USING WS-NAME(1).
             05 LINE 8 COLUMN 10 PIC XX USING WS-SCORE(2).
             05 LINE 8 COLUMN 14 PIC X(10) USING WS-NAME(2).
             05 LINE 10 COLUMN 10 PIC XX USING WS-SCORE(3).
             05 LINE 10 COLUMN 14 PIC X(10) USING WS-NAME(3).
             05 LINE 12 COLUMN 10 VALUE "(b) Go back".
             05 LINE 14 COLUMN 10 VALUE "Pick: ".
             05 WS-HIGH-SCORE-FIELD LINE 13 COLUMN 16 PIC X
               USING WS-HIGH-SCORE-CHOICE.
       PROCEDURE DIVISION.
      
       0100-DISPLAY-LOGIN.
           INITIALIZE LOGIN-CHOICE.      
           DISPLAY LOGIN-SCREEN.
           ACCEPT LOGIN-CHOICE-FIELD.
           IF LOGIN-CHOICE = "l" THEN 
               PERFORM 0101-SIGN-IN 
           ELSE IF LOGIN-CHOICE = "c" THEN 
               PERFORM 0102-SIGN-UP
           ELSE IF LOGIN-CHOICE = "q" THEN 
               STOP RUN
           ELSE 
               PERFORM 0100-DISPLAY-LOGIN
           END-IF.

       0101-SIGN-IN.
           SET COUNTER TO 0.
           OPEN INPUT F-USERS-FILE.
           MOVE 0 TO WS-FILE-IS-ENDED.
           PERFORM UNTIL WS-FILE-IS-ENDED = 1
               READ F-USERS-FILE
                   NOT AT END
                       ADD 1 TO COUNTER
                       MOVE USERNAME TO WS-USERNAME(COUNTER)
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
               IF USER-NAME = WS-USERNAME(WS-IDX) AND 
               WS-PASSWORD = WS-PWORD(WS-IDX) THEN
                   MOVE 1 TO WS-FOUND 
               END-IF
               ADD 1 TO WS-IDX 
           END-PERFORM.

           IF WS-FOUND = 1 THEN
               PERFORM 0110-DISPLAY-MENU 
           ELSE 
               PERFORM 0103-ERROR-PAGE 
           END-IF. 

       0102-SIGN-UP.
           INITIALIZE NEW-USER-NAME.
           INITIALIZE NEW-PASSWORD.
           INITIALIZE CREATE-CHOICE
           DISPLAY CREATE-AN-ACCOUNT-SCREEN.
           ACCEPT NEW-USER-NAME-FIELD.
           ACCEPT NEW-PASSWORD-FIELD.
           ACCEPT CREATE-CHOICE-FIELD.
           IF CREATE-CHOICE = "q" THEN 
               PERFORM 0100-DISPLAY-LOGIN
           ELSE IF CREATE-CHOICE = "s" THEN 
               OPEN EXTEND F-USERS-FILE
               MOVE NEW-USER-NAME TO USERNAME
               MOVE NEW-PASSWORD TO USER-PASSWORD
               WRITE USERS
               END-WRITE               
           END-IF.
           CLOSE F-USERS-FILE.
           PERFORM 0101-SIGN-IN.

       0103-ERROR-PAGE.
           INITIALIZE ERROR-CHOICE.
           DISPLAY ERROR-SCREEN.
           ACCEPT ERROR-CHOICE-FIELD.
           IF ERROR-CHOICE = "l" THEN 
               PERFORM 0101-SIGN-IN
           ELSE IF ERROR-CHOICE = "c" THEN 
               PERFORM 0102-SIGN-UP 
           ELSE 
               PERFORM 0103-ERROR-PAGE 
           END-IF.

       0110-DISPLAY-MENU.
           INITIALIZE MENU-CHOICE.
           DISPLAY MENU-SCREEN.
           ACCEPT MENU-CHOICE-FIELD.
           IF MENU-CHOICE = "q" THEN
           STOP RUN
           ELSE IF MENU-CHOICE = "l" THEN
           PERFORM 0100-DISPLAY-LOGIN
           ELSE IF MENU-CHOICE = "n" THEN
           PERFORM 0110-DISPLAY-MENU
           ELSE IF MENU-CHOICE = 'm' THEN
             PERFORM 0120-GENERATE-TABLE
           ELSE IF MENU-CHOICE = 'g' THEN
             PERFORM 0160-DISPLAY-GUESSING-GAME
           ELSE 
               PERFORM 0110-DISPLAY-MENU
           END-IF. 
       
       0120-GENERATE-TABLE.
           SET COUNTER TO 0.
           OPEN INPUT F-MESSAGE-FILE.
           MOVE 0 TO WS-FILE-IS-ENDED.
           PERFORM UNTIL WS-FILE-IS-ENDED = 1
               READ F-MESSAGE-FILE
                   NOT AT END
                       ADD 1 TO COUNTER
                       MOVE MESSAGE-TITLE TO WS-TITLE(COUNTER)
                       MOVE MESSAGE-BODY TO WS-BODY(COUNTER)
                       MOVE MESSAGE-DATE TO WS-DATE(COUNTER)
                       MOVE MESSAGE-AUTHOR TO WS-AUTHOR(COUNTER)
                   AT END 
                       MOVE 1 TO WS-FILE-IS-ENDED
                       MOVE COUNTER TO OFFSET
                       MOVE 1 TO PAGE-NUM
                       MOVE 1 TO TITLE-NUM
                       MOVE "Here are the last 10 messages:" TO 
                       DISPLAY-MESSAGE
               END-READ 
           END-PERFORM.
           CLOSE F-MESSAGE-FILE.
           PERFORM 0130-DISPLAY-MESSAGEBOARD.
      
       0130-DISPLAY-MESSAGEBOARD.
           INITIALIZE MESSAGE-CHOICE.
           DISPLAY MESSAGEBOARD-SCREEN.
           ACCEPT MESSAGE-CHOICE-FIELD.
           IF MESSAGE-CHOICE = "q" THEN 
               PERFORM 0110-DISPLAY-MENU
           ELSE IF MESSAGE-CHOICE = "m" THEN 
               PERFORM 0150-POST-MESSAGE
           ELSE IF MESSAGE-CHOICE = "n" THEN
               IF OFFSET > 20
                   COMPUTE OFFSET = OFFSET - 10
                   COMPUTE PAGE-NUM = PAGE-NUM + 1
                   MOVE "Here are the next 10 messages:" TO 
                       DISPLAY-MESSAGE
               END-IF
               PERFORM 0130-DISPLAY-MESSAGEBOARD
           ELSE IF MESSAGE-CHOICE = "p" THEN
               IF PAGE-NUM = "01"
                   PERFORM 0130-DISPLAY-MESSAGEBOARD
               ELSE IF PAGE-NUM = "02"
                   COMPUTE OFFSET = OFFSET + 10
                   COMPUTE PAGE-NUM = PAGE-NUM - 1
                   MOVE "Here are the last 10 messages:" TO 
                       DISPLAY-MESSAGE
                   PERFORM 0130-DISPLAY-MESSAGEBOARD
               ELSE 
                   COMPUTE OFFSET = OFFSET + 10
                   COMPUTE PAGE-NUM = PAGE-NUM - 1
                   PERFORM 0130-DISPLAY-MESSAGEBOARD
               END-IF
           ELSE IF MESSAGE-CHOICE = "1" OR "2" OR "3" OR "4" OR "5" 
             OR "6" OR "7" OR "8" OR "9" OR "10"
               SET MESSAGE-NUM TO MESSAGE-CHOICE-TO-NUM(MESSAGE-CHOICE)
               PERFORM 0140-READ-MESSAGE
           ELSE 
               PERFORM 0130-DISPLAY-MESSAGEBOARD
           END-IF.

       0140-READ-MESSAGE.
           INITIALIZE READ-CHOICE.
           IF MESSAGE-NUM = 1 OR 2 OR 3 OR 4 OR 5 OR 6 OR 7 OR 8 OR 9 
           OR 10
               MOVE DISPLAY-MESSAGE-TITLE(OFFSET MESSAGE-NUM WS-MSGS) 
               TO TITLE 
               MOVE DISPLAY-MESSAGE-BODY(OFFSET MESSAGE-NUM WS-MSGS) 
               TO BODY                      
           END-IF.
           DISPLAY READ-MESSAGE-SCREEN.
           ACCEPT READ-CHOICE.
           IF READ-CHOICE = "q" THEN
               PERFORM 0130-DISPLAY-MESSAGEBOARD
           ELSE IF READ-CHOICE = 'n' THEN 
               IF MESSAGE-NUM < 10
                   COMPUTE MESSAGE-NUM = MESSAGE-NUM + 1
                ELSE 
                   MOVE 1 TO MESSAGE-NUM
               END-IF
               PERFORM 0140-READ-MESSAGE
           ELSE IF READ-CHOICE = 'p' THEN 
               IF MESSAGE-NUM > 1
                   COMPUTE MESSAGE-NUM = MESSAGE-NUM - 1
               ELSE
                   MOVE 10 TO MESSAGE-NUM
               END-IF
               PERFORM 0140-READ-MESSAGE
           END-IF.

       0150-POST-MESSAGE.
           STRING FUNCTION CURRENT-DATE(1:4) "-" 
               FUNCTION CURRENT-DATE(5:2) "-" FUNCTION CURRENT-DATE(7:2)
               INTO WS-FORMATTED-DATE
           END-STRING.
           INITIALIZE POST-CHOICE.
           INITIALIZE POST-TITLE.
           INITIALIZE POST-BODY.
           DISPLAY POST-MESSAGE-SCREEN.
           ACCEPT POST-TITLE-FIELD.
           ACCEPT POST-BODY-FIELD.
           ACCEPT POST-CHOICE-FIELD.
           IF POST-CHOICE = "d" THEN 
               PERFORM 0130-DISPLAY-MESSAGEBOARD
           ELSE IF POST-CHOICE = "p" THEN 
               OPEN EXTEND F-MESSAGE-FILE
               MOVE POST-TITLE TO MESSAGE-TITLE
               MOVE POST-BODY TO MESSAGE-BODY
               MOVE WS-FORMATTED-DATE TO MESSAGE-DATE
               MOVE USER-NAME TO MESSAGE-AUTHOR
               WRITE MESSAGES
               END-WRITE               
           END-IF.
           CLOSE F-MESSAGE-FILE.
           PERFORM 0120-GENERATE-TABLE.

       0160-DISPLAY-GUESSING-GAME.
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
           PERFORM 0170-IN-GAME-SCREEN.
          
       0170-IN-GAME-SCREEN.
           INITIALIZE WS-GUESS-CHOICE.
           DISPLAY IN-GAME-SCREEN.
           ACCEPT WS-GUESS-CHOICE-FIELD.
           IF WS-GUESS-CHOICE = '!' THEN 
               PERFORM 0110-DISPLAY-MENU
           ELSE
               PERFORM 0180-CHECK-GUESS
           END-IF.
           
       0180-CHECK-GUESS.
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
              PERFORM 0190-WINNING-SCREEN
             ELSE IF WS-GUESSES-LEFT = 0
              THEN 
              PERFORM 0200-LOSING-SCREEN
             ELSE
              PERFORM 0170-IN-GAME-SCREEN
             END-IF.
      
           
       0190-WINNING-SCREEN.
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
               THEN PERFORM 0160-DISPLAY-GUESSING-GAME
           ELSE IF WS-GUESSING-WINNING-CHOICE = 'h'
             THEN PERFORM 0210-HIGH-SCORE-TABLE
           ELSE IF WS-GUESSING-WINNING-CHOICE = '!'
             THEN PERFORM 0110-DISPLAY-MENU
           ELSE
             PERFORM 0190-WINNING-SCREEN
           END-IF.


       0200-LOSING-SCREEN.
           INITIALIZE WS-GUESSING-LOSING-CHOICE.
           DISPLAY WORD-GUESSING-LOSE-SCREEN.
           ACCEPT WS-GUESSING-LOSING-CHOICE.
           IF WS-GUESSING-LOSING-CHOICE = 'p'
               THEN PERFORM 0160-DISPLAY-GUESSING-GAME
           ELSE IF WS-GUESSING-LOSING-CHOICE = 'h'
             THEN PERFORM 0210-HIGH-SCORE-TABLE
           ELSE IF WS-GUESSING-LOSING-CHOICE = '!'
             THEN PERFORM 0110-DISPLAY-MENU
           ELSE
             PERFORM 0200-LOSING-SCREEN
           END-IF.

       0210-HIGH-SCORE-TABLE.
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
           PERFORM 0220-HIGH-SCORE-SCREEN.
           

       0220-HIGH-SCORE-SCREEN.
           INITIALIZE WS-HIGH-SCORE-CHOICE.
           SORT WS-TABLE-HIGH-SCORE ON DESCENDING WS-SCORE.
           DISPLAY HIGH-SCORE-SCREEN.
           ACCEPT WS-HIGH-SCORE-CHOICE.
