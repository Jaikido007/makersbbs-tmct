       IDENTIFICATION DIVISION.
       PROGRAM-ID. server.
       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
           FILE-CONTROL.
           SELECT F-MESSAGE-FILE ASSIGN TO "messages.dat"
             ORGANIZATION IS LINE SEQUENTIAL.
           SELECT F-WORD-FILE ASSIGN TO 'guessing-words.dat'
             ORGANIZATION IS LINE SEQUENTIAL.
           SELECT F-HIGH-SCORES-FILE ASSIGN TO 'high-scores.dat'
             ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
           FILE SECTION.
           FD F-WORD-FILE.
           01 WORD PIC X(20).
           FD F-MESSAGE-FILE.
           01 MESSAGES.
              05 MESSAGE-TITLE PIC X(60).
              05 MESSAGE-BODY PIC X(500).
           FD F-HIGH-SCORES-FILE.
           01 PLAYER-SCORES.
              05 HIGH-SCORE PIC 99.
              05 PLAYER-NAME PIC X(10).

           WORKING-STORAGE SECTION.
      *     Variables related to login and menu screen
           01 USER-NAME PIC X(10).
           01 MENU-CHOICE PIC X.

      *    Variables related to creating table and reading file
           01 WS-FILE-IS-ENDED PIC 9.
           01 WS-MSGS.
               05 WS-MSG OCCURS 100 TIMES
               ASCENDING KEY IS WS-TITLE
               INDEXED BY MSG-IDX.
                   10 WS-TITLE PIC X(60).
                   10 WS-BODY PIC X(500).

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
           01 MESSAGE-NUM UNSIGNED-INT.

      *    Variables related to post message screen
           01 POST-TITLE PIC X(60).
           01 POST-BODY PIC X(500).
           01 POST-CHOICE PIC X.
           01 MESSAGE-DATE UNSIGNED-INT.

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
      *       BACKGROUND-COLOR IS 10.
             05 BLANK SCREEN.
             05 LINE 2 COLUMN 10 VALUE "Makers BBS".
             05 LINE 4 COLUMN 10 VALUE "What's your name?".
             05 USER-NAME-FIELD LINE 6 COLUMN 10 PIC X(10)
                USING USER-NAME.
      *    TMNCT ART HERE      
             05 LINE 38 COLUMN 30 VALUE 
           "                                          <  !'`.::!!!!!!".
             05 LINE 37 COLUMN 30 VALUE
           "                                           !  !!!!''``..,".
             05 LINE 36 COLUMN 30 VALUE
           "                                           <!  !!!!!!!!!!".
             05 LINE 35 COLUMN 30 VALUE 
           "`'''                                        <!! ;!!!!!!!!".
             05 LINE 34 COLUMN 30 VALUE
           ":::::' `'        `?P??F'                    :!!! ;!!!!!!!".
             05 LINE 33 COLUMN 30 VALUE
           ":::::::.$$$P'    $$becd$$$'                  !!!!  !!!!!!".
             05 LINE 32 COLUMN 30 value
           "::::::.4$$$$$$F  J$ `:' z$$$'            `'?  !!!!  !!!!!".
             05 LINE 31 COLUMN 30 VALUE
           "  ..''$',$$$$$$$'' . ::;; .d$$' .$$F',c$?$$$$F <!!!!  !!!".
             05 LINE 30 COLUMN 30 VALUE
           "  $$$$$$P',$$$$$$P'   ;;;' c$$$' zd$F.- ze$$$' !!! <' <!!".
             05 LINE 29 COLUMN 30 VALUE 
           "  J$$$$$$$F.d$$$$$$P V  ,;; z$$$$$$$$$P''`.d$$ ;!!! :' <!".
             05 LINE 28 COLUMN 30 value
           "  .$$$$$$$$$P'`,e$$$$P'    , .$$$$$$$$$$$$$$$$P <!!' ;' ;".
             05 LINE 27 COLUMN 30 VALUE 
           "    $$$$$$$$$$$$P'`,e$$P''V    c$$$$$$$$$$$$$$$' <!!' ;! ".
             05 LINE 26 COLUMN 30 VALUE
           "    .$$$$$$$$$$$$$$$P',d$$$P''  ,d$$$$$$$$$$$$$$% <!!  ,!".
             05 LINE 25 COLUMN 30 VALUE
           "      z$$$$$$$$$$$$$$$P'.c$$$$$$P' ,c$$$$$$$$Lz$$' <!!  ;".
             05 LINE 24 COLUMN 30 VALUE
           "        z$$$$$$$$$$$$$$$$P'.cd$ed$$P .d$$$$$$$'.$$% <!!  ".
             05 LINE 23 COLUMN 30 VALUE
           "        4'.,ccd$$$$$$$$$$$$$$P???'L.z' d$$$$$$$ .d$% <!! ".
             05 LINE 22 COLUMN 30 VALUE
           "        4$P'cedd$$$$$$$$$$$$$$$$$$$$$P' d$$$$$$ % .$' ;!!".
             05 LINE 21 COLUMN 30 VALUE
           "       ?$$$$'dF''(($$$$$$$cec$Le$$$$$$$P',$$$$$$F %J$'  !".
             05 LINE 20 COLUMN 30 VALUE
           "       ,ec,,. zd$$$$$uCC$$$$$??$'z$$$$$$$?$$$$$$'3 z$$P' ".
             05 LINE 19 COLUMN 30 VALUE
           "        -$L   $FF''??%,,,,,ced$$$P$$$$$$$$$$$$$$$.' $$$$P".
             05 LINE 18 COLUMN 30 VALUE
           "       $P== '' $c`'',cd',gd$b  .e$$$$$$$$$$$$$$$'?L $c   ".
             05 LINE 17 COLUMN 30 VALUE
           "       .J'?$$$F$ $$$$$',eF' ,    '3$$$$$$$$$$$$$$c $$??4F".
             05 LINE 16 COLUMN 30 VALUE
           "        z$$$$$$$P'$$$$$$P?$$$$$$$$$$$$$$$$$$$$$$.'$$$$$$$".
             05 LINE 15 COLUMN 30 VALUE
           "          e$$$$$$$$$$$$$$$P'.,cecc,C$$$$$$$$$$$P$$$$$$$$$".
             05 LINE 14 COLUMN 30 VALUE
           "           J$$$$$$$$$$b$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$".
             05 LINE 13 COLUMN 30 VALUE
           "            $$$$$$$$$$$'. z$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$".
             05 LINE 12 COLUMN 30 VALUE 
           "          ''4$$$$$$$$$$$$P' . .,ce$be,3$$$$$$$$$$$$$$bc,.".
             05 LINE 11 COLUMN 30 VALUE
           "      `'$$$$ z$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$c,.     ".
             05 LINE 10 COLUMN 30 VALUE
           "     ?$$$$$$P z$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$c.         ".
             05 LINE 9 COLUMN 30 VALUE
           "    ?$$$$$$$$$'.r ,$$$$$$$$$$$$$$$$$$$$$$$$$c'           ".
             05 LINE 8 COLUMN 30 VALUE 
           "   `$$duJu,ec.      ,c$ce$$$$$$$$$$$$$$$$$$r'$P          ".
             05 LINE 7 COLUMN 30 VALUE
           "                      ,r= ze$e$$$$$$$$$P'z$'?            ".
             05 LINE 6 COLUMN 30 VALUE
           "                        c$$$$$$$$$, $F                   ".
             05 LINE 5 COLUMN 30 VALUE 
           "                          ,d$$$$$$F 4''                  ".
             05 LINE 4 COLUMN 30 VALUE
           "                            .zd$$$b $$c                  ".
             05 LINE 3 COLUMN 30 VALUE 
           "                               ,$$F $$                   ".
             05 LINE 2 COLUMN 30 VALUE
           "                                .d$ d$                   ".
             05 LINE 1 COLUMN 30 VALUE 
           "                                  ,d'e                   ".
           
             
           01 MENU-SCREEN
             BACKGROUND-COLOR IS 8.
             05 BLANK SCREEN.
             05 LINE 2 COLUMN 10 VALUE "Teenage Mutant Ninja Cobol".
             05 LINE 2 COLUMN 37 VALUE "Turtles Bulletin Board".
             05 LINE 4 COLUMN 10 VALUE "Welcome, ".
             05 LINE 4 COLUMN 19 PIC X(10) USING USER-NAME.
             05 LINE 28 COLUMN 10 VALUE "(n) Nothing".
             05 LINE 28 COLUMN 60 VALUE "(m) Message board".
             05 LINE 28 COLUMN 90 VALUE "(g) Guessing Game".
             05 LINE 28 COLUMN 30 VALUE "(l) Logout".
             05 LINE 28 COLUMN 40 VALUE "(q) Quit".
             05 LINE 30 COLUMN 10 VALUE "Pick: ".
             05 MENU-CHOICE-FIELD LINE 30 COLUMN 16 PIC X
                USING MENU-CHOICE.
      *TMNCT letters bttom is line 23 to 7 up
             05 LINE 11 COLUMN 60 VALUE 
           "   \__|   \__|     \__|\__|  \__| \______/   \__|   ".
             05 LINE 10 COLUMN 60 VALUE
           "   $$ |   $$ | \_/ $$ |$$ | \$$ |\$$$$$$  |  $$ |   ".
             05 LINE 9 COLUMN 60 VALUE
           "   $$ |   $$ |\$  /$$ |$$ |\$$$ |$$ |  $$\   $$ |   ".
             05 LINE 8 COLUMN 60 VALUE
           "   $$ |   $$ \$$$  $$ |$$ \$$$$ |$$ |        $$ |   ".
             05 LINE 7 COLUMN 60 VALUE
           "   $$ |   $$\$$\$$ $$ |$$ $$\$$ |$$ |        $$ |   ".
             05 LINE 6 COLUMN 60 VALUE
           "   $$ |   $$$$\  $$$$ |$$$$\ $$ |$$ /  \__|  $$ |   ".
             05 LINE 5 COLUMN 60 VALUE
           "\__$$  __|$$$\    $$$ |$$$\  $$ |$$  __$$\\__$$  __|".
             05 LINE 4 COLUMN 60 VALUE
           "$$$$$$$$\ $$\      $$\ $$\   $$\  $$$$$$\ $$$$$$$$\ ".
           01 MESSAGEBOARD-SCREEN
             BACKGROUND-COLOR IS 8.
            05 BLANK SCREEN.
            05 LINE 2 COLUMN 10 VALUE "Makers BBS".
            05 LINE 2 COLUMN 30 VALUE "Page: ".
            05 LINE 2 COLUMN 37 PIC 99 USING PAGE-NUM.
            05 LINE 4 COLUMN 10 PIC X(40) USING DISPLAY-MESSAGE.
            05 LINE 6 COLUMN 10 VALUE "1.".
            05 LINE 6 COLUMN 14 PIC X(60) USING WS-MSG(OFFSET).
            05 LINE 7 COLUMN 10 VALUE "2.".
            05 LINE 7 COLUMN 14 PIC X(60) USING WS-MSG(OFFSET - 1).
            05 LINE 8 COLUMN 10 VALUE "3.".
            05 LINE 8 COLUMN 14 PIC X(60) USING WS-MSG(OFFSET - 2).
            05 LINE 9 COLUMN 10 VALUE "4.".
            05 LINE 9 COLUMN 14 PIC X(60) USING WS-MSG(OFFSET - 3).
            05 LINE 10 COLUMN 10 VALUE "5.".
            05 LINE 10 COLUMN 14 PIC X(60) USING WS-MSG(OFFSET - 4).
            05 LINE 11 COLUMN 10 VALUE "6.".
            05 LINE 11 COLUMN 14 PIC X(60) USING WS-MSG(OFFSET - 5).
            05 LINE 12 COLUMN 10 VALUE "7.".
            05 LINE 12 COLUMN 14 PIC X(60) USING WS-MSG(OFFSET - 6).
            05 LINE 13 COLUMN 10 VALUE "8.".
            05 LINE 13 COLUMN 14 PIC X(60) USING WS-MSG(OFFSET - 7).
            05 LINE 14 COLUMN 10 VALUE "9.".
            05 LINE 14 COLUMN 14 PIC X(60) USING WS-MSG(OFFSET - 8).
            05 LINE 15 COLUMN 10 VALUE "10.".
            05 LINE 15 COLUMN 14 PIC X(60) USING WS-MSG(OFFSET - 9).
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
           INITIALIZE USER-NAME.
           DISPLAY LOGIN-SCREEN.
           ACCEPT USER-NAME-FIELD.
           PERFORM 0110-DISPLAY-MENU.

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
           ELSE IF MESSAGE-CHOICE = "1" 
               SET MESSAGE-NUM TO 1 
               PERFORM 0140-READ-MESSAGE
           ELSE IF MESSAGE-CHOICE = "2" 
               SET MESSAGE-NUM TO 2 
               PERFORM 0140-READ-MESSAGE
           ELSE IF MESSAGE-CHOICE = "3" 
               SET MESSAGE-NUM TO 3 
               PERFORM 0140-READ-MESSAGE
           ELSE IF MESSAGE-CHOICE = "4" 
               SET MESSAGE-NUM TO 4 
               PERFORM 0140-READ-MESSAGE
           ELSE IF MESSAGE-CHOICE = "5" 
               SET MESSAGE-NUM TO 5 
               PERFORM 0140-READ-MESSAGE
           ELSE IF MESSAGE-CHOICE = "6" 
               SET MESSAGE-NUM TO 6 
               PERFORM 0140-READ-MESSAGE
           ELSE IF MESSAGE-CHOICE = "7" 
               SET MESSAGE-NUM TO 7 
               PERFORM 0140-READ-MESSAGE
           ELSE IF MESSAGE-CHOICE = "8" 
               SET MESSAGE-NUM TO 8 
               PERFORM 0140-READ-MESSAGE
           ELSE IF MESSAGE-CHOICE = "9" 
               SET MESSAGE-NUM TO 9
               PERFORM 0140-READ-MESSAGE
           ELSE IF MESSAGE-CHOICE = "10" 
               SET MESSAGE-NUM TO 10 
               PERFORM 0140-READ-MESSAGE
           ELSE 
               PERFORM 0130-DISPLAY-MESSAGEBOARD
           END-IF.

       0140-READ-MESSAGE.
           INITIALIZE READ-CHOICE.
           IF MESSAGE-NUM = 1
                       MOVE WS-TITLE(OFFSET) TO TITLE
                       MOVE WS-BODY(OFFSET) TO BODY
           ELSE IF MESSAGE-NUM = 2
                       MOVE WS-TITLE(OFFSET - 1) TO TITLE
                       MOVE WS-BODY(OFFSET - 1) TO BODY
           ELSE IF MESSAGE-NUM = 3
                       MOVE WS-TITLE(OFFSET - 2) TO TITLE
                       MOVE WS-BODY(OFFSET - 2) TO BODY 
           ELSE IF MESSAGE-NUM = 4
                       MOVE WS-TITLE(OFFSET - 3) TO TITLE
                       MOVE WS-BODY(OFFSET - 3) TO BODY 
           ELSE IF MESSAGE-NUM = 5
                       MOVE WS-TITLE(OFFSET - 4) TO TITLE
                       MOVE WS-BODY(OFFSET - 4) TO BODY
           ELSE IF MESSAGE-NUM = 6
                       MOVE WS-TITLE(OFFSET - 5) TO TITLE
                       MOVE WS-BODY(OFFSET - 5) TO BODY 
           ELSE IF MESSAGE-NUM = 7
                       MOVE WS-TITLE(OFFSET - 6) TO TITLE
                       MOVE WS-BODY(OFFSET - 6) TO BODY
           ELSE IF MESSAGE-NUM = 8
                       MOVE WS-TITLE(OFFSET - 7) TO TITLE
                       MOVE WS-BODY(OFFSET - 7) TO BODY
           ELSE IF MESSAGE-NUM = 9
                       MOVE WS-TITLE(OFFSET - 8) TO TITLE
                       MOVE WS-BODY(OFFSET - 8) TO BODY
           ELSE IF MESSAGE-NUM = 10
                       MOVE WS-TITLE(OFFSET - 9) TO TITLE
                       MOVE WS-BODY(OFFSET - 9) TO BODY                       
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
               MOVE FUNCTION CURRENT-DATE(1:8) TO MESSAGE-DATE
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
           INSPECT WS-WORD REPLACING ALL 'a' BY '*'.
           INSPECT WS-WORD REPLACING ALL 'b' BY '*'.
           INSPECT WS-WORD REPLACING ALL 'c' BY '*'.
           INSPECT WS-WORD REPLACING ALL 'd' BY '*'.
           INSPECT WS-WORD REPLACING ALL 'e' BY '*'.
           INSPECT WS-WORD REPLACING ALL 'f' BY '*'.
           INSPECT WS-WORD REPLACING ALL 'g' BY '*'.
           INSPECT WS-WORD REPLACING ALL 'h' BY '*'.
           INSPECT WS-WORD REPLACING ALL 'i' BY '*'.
           INSPECT WS-WORD REPLACING ALL 'j' BY '*'.
           INSPECT WS-WORD REPLACING ALL 'k' BY '*'.
           INSPECT WS-WORD REPLACING ALL 'l' BY '*'.
           INSPECT WS-WORD REPLACING ALL 'm' BY '*'.
           INSPECT WS-WORD REPLACING ALL 'n' BY '*'.
           INSPECT WS-WORD REPLACING ALL 'o' BY '*'.
           INSPECT WS-WORD REPLACING ALL 'p' BY '*'.
           INSPECT WS-WORD REPLACING ALL 'q' BY '*'.
           INSPECT WS-WORD REPLACING ALL 'r' BY '*'.
           INSPECT WS-WORD REPLACING ALL 's' BY '*'.
           INSPECT WS-WORD REPLACING ALL 't' BY '*'.
           INSPECT WS-WORD REPLACING ALL 'u' BY '*'.
           INSPECT WS-WORD REPLACING ALL 'v' BY '*'.
           INSPECT WS-WORD REPLACING ALL 'w' BY '*'.
           INSPECT WS-WORD REPLACING ALL 'x' BY '*'.
           INSPECT WS-WORD REPLACING ALL 'y' BY '*'.
           INSPECT WS-WORD REPLACING ALL 'z' BY '*'.
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
