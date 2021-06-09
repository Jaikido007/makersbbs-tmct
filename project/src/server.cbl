       IDENTIFICATION DIVISION.
       PROGRAM-ID. server.
       ENVIRONMENT DIVISION.
           CONFIGURATION SECTION.
           REPOSITORY.
               FUNCTION MESSAGE-CHOICE-TO-NUM
               FUNCTION DISPLAY-MESSAGE-TITLE
               FUNCTION DISPLAY-MESSAGE-BODY
               FUNCTION DISPLAY-MESSAGE-AUTHOR
               FUNCTION DISPLAY-MESSAGE-DATE
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

      *    Time
           01 WS-TIME.
               05 WS-YEAR PIC X(4).
               05 WS-MONTH PIC X(2).
               05 WS-DAY PIC X(2).
               05 WS-HOURS-MINS.
                   10 WS-HOURS PIC X(2).
                   10 WS-MINS PIC X(2).
                 
           SCREEN SECTION.

           01 TIME-SCREEN.
             05 LINE 2 COLUMN 80 PIC X(2) USING WS-HOURS.
             05 LINE 2 COLUMN 82 VALUE ":".
             05 LINE 2 COLUMN 83 PIC X(2) USING WS-MINS.
            
           01 LOGIN-SCREEN.
      *       BACKGROUND-COLOR IS 10.
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
      *      Turtle goes here
             05 LINE 25 COLUMN 10 VALUE
           "               (_(___/                         \_____)_)  ".
             05 LINE 24 COLUMN 10 VALUE
           "        (_(__/  ./     /                    \_\      \.   ".
             05 LINE 23 COLUMN 10 VALUE
           "       ~-----||====/~     |==================|       |/~~~".
             05 LINE 22 COLUMN 10 VALUE
           "     \       ||------_-~~-_ ------------- \ --/~   ~\    |".
             05 LINE 21 COLUMN 10 VALUE 
           "   \         ||           \______________/      _-_      |".
             05 LINE 20 COLUMN 10 VALUE
           " \         \^\\         \                  /             |".
             05 LINE 19 COLUMN 10 VALUE
           "|         / /         \                      /           |".
             05 LINE 18 COLUMN 10 VALUE
           "|   (_______) /______/                        \_________ \".
             05 LINE 17 COLUMN 10 VALUE
           "|          \    /      /                    \          \  ".
             05 LINE 16 COLUMN 10 VALUE 
           "| |___||__|      /       /                \          \    ".
             05 LINE 15 COLUMN 10 VALUE
           " /|  O|| O|        /      \_______________/        \      ".
             05 LINE 14 COLUMN 10 VALUE
           "   /^\__/^\         /~  \                   /    \        ".
             05 LINE 13 COLUMN 10 VALUE
           "                      _-~                    /~-_         ".
             05 LINE 12 COLUMN 10 VALUE
           "                         _-~~             ~~-_            ".
             05 LINE 11 COLUMN 10 VALUE
           "                             ___-------___                ".
             05 LINE 2 COLUMN 10 VALUE "Teenage Mutant Ninja Cobol".
             05 LINE 2 COLUMN 37 VALUE "Turtles Bulletin Board".
             05 LINE 4 COLUMN 10 VALUE "Welcome, ".
             05 LINE 4 COLUMN 19 PIC X(10) USING USER-NAME.
             05 LINE 28 COLUMN 10 VALUE "(n) Nothing".
             05 LINE 28 COLUMN 50 VALUE "(m) Message board".
             05 LINE 28 COLUMN 70 VALUE "(g) Guessing Game".
             05 LINE 28 COLUMN 25 VALUE "(l) Logout".
             05 LINE 28 COLUMN 39 VALUE "(q) Quit".
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
            05 LINE 2 COLUMN 10 VALUE "Teenage Mutant Ninja Cobol".
             05 LINE 2 COLUMN 37 VALUE "Turtles Message Board".
            05 LINE 18 COLUMN 10 VALUE "Page: ".
            05 LINE 18 COLUMN 17 PIC 99 USING PAGE-NUM.
            05 LINE 17 COLUMN 10 PIC X(40) USING DISPLAY-MESSAGE.
            05 LINE 19 COLUMN 10 VALUE "1.".
            05 LINE 19 COLUMN 14 PIC X(60) USING WS-TITLE(OFFSET).
            05 LINE 19 COLUMN 75 VALUE "Posted by:".
            05 LINE 19 COLUMN 87 PIC X(10) USING WS-AUTHOR(OFFSET).
            05 LINE 20 COLUMN 10 VALUE "2.".
            05 LINE 20 COLUMN 14 PIC X(60) USING WS-TITLE(OFFSET - 1).
            05 LINE 20 COLUMN 75 VALUE "Posted by:".
            05 LINE 20 COLUMN 87 PIC X(10) USING WS-AUTHOR(OFFSET - 1).
            05 LINE 21 COLUMN 10 VALUE "3.".
            05 LINE 21 COLUMN 14 PIC X(60) USING WS-TITLE(OFFSET - 2).
            05 LINE 21 COLUMN 75 VALUE "Posted by:".
            05 LINE 21 COLUMN 87 PIC X(10) USING WS-AUTHOR(OFFSET - 2).
            05 LINE 22 COLUMN 10 VALUE "4.".
            05 LINE 22 COLUMN 14 PIC X(60) USING WS-TITLE(OFFSET - 3).
            05 LINE 22 COLUMN 75 VALUE "Posted by:".
            05 LINE 22 COLUMN 87 PIC X(10) USING WS-AUTHOR(OFFSET - 3).
            05 LINE 23 COLUMN 10 VALUE "5.".
            05 LINE 23 COLUMN 14 PIC X(60) USING WS-TITLE(OFFSET - 4).
            05 LINE 23 COLUMN 75 VALUE "Posted by:".
            05 LINE 23 COLUMN 87 PIC X(10) USING WS-AUTHOR(OFFSET - 4).
            05 LINE 24 COLUMN 10 VALUE "6.".
            05 LINE 24 COLUMN 14 PIC X(60) USING WS-TITLE(OFFSET - 5).
            05 LINE 24 COLUMN 75 VALUE "Posted by:".
            05 LINE 24 COLUMN 87 PIC X(10) USING WS-AUTHOR(OFFSET - 5).
            05 LINE 25 COLUMN 10 VALUE "7.".
            05 LINE 25 COLUMN 14 PIC X(60) USING WS-TITLE(OFFSET - 6).
            05 LINE 25 COLUMN 75 VALUE "Posted by:".
            05 LINE 25 COLUMN 87 PIC X(10) USING WS-AUTHOR(OFFSET - 6).
            05 LINE 26 COLUMN 10 VALUE "8.".
            05 LINE 26 COLUMN 14 PIC X(60) USING WS-TITLE(OFFSET - 7).
            05 LINE 26 COLUMN 75 VALUE "Posted by:".
            05 LINE 26 COLUMN 87 PIC X(10) USING WS-AUTHOR(OFFSET - 7).
            05 LINE 27 COLUMN 10 VALUE "9.".
            05 LINE 27 COLUMN 14 PIC X(60) USING WS-TITLE(OFFSET - 8).
            05 LINE 27 COLUMN 75 VALUE "Posted by:".
            05 LINE 27 COLUMN 87 PIC X(10) USING WS-AUTHOR(OFFSET - 8).
            05 LINE 28 COLUMN 10 VALUE "10.".
            05 LINE 28 COLUMN 14 PIC X(60) USING WS-TITLE(OFFSET - 9).
            05 LINE 28 COLUMN 75 VALUE "Posted by:".
            05 LINE 28 COLUMN 87 PIC X(10) USING WS-AUTHOR(OFFSET - 9).
            05 LINE 30 COLUMN 10 VALUE "( ) Read the full message by".
            05 LINE 30 COLUMN 39 VALUE "number".
            05 LINE 31 COLUMN 10 VALUE "(m) Post a message of your own".
            05 LINE 32 COLUMN 10 VALUE "(n) Next page".
            05 LINE 32 COLUMN 30 VALUE "(p) Previous page".
            05 LINE 32 COLUMN 60 VALUE "(q) Go back".
            05 LINE 34 COLUMN 10 VALUE "Pick: ".
            05 MESSAGE-CHOICE-FIELD LINE 34 COLUMN 16 PIC X
                USING MESSAGE-CHOICE.

           01 READ-MESSAGE-SCREEN
           BACKGROUND-COLOR IS 8.
            05 BLANK SCREEN.
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
            05 LINE 2 COLUMN 10 VALUE "Teenage Mutant Ninja Cobol".
             05 LINE 2 COLUMN 37 VALUE "Turtles Message Board".
            05 LINE 18 COLUMN 10 VALUE "Title:".
            05 LINE 18 COLUMN 18 PIC X(60) USING TITLE.
            05 LINE 22 COLUMN 10 PIC X(500) USING BODY.
            05 LINE 31 COLUMN 10 VALUE "Post Author:".
            05 LINE 31 COLUMN 23 PIC X(10) USING POST-AUTHOR.
            05 LINE 31 COLUMN 34 VALUE "Posted On:".
            05 LINE 31 COLUMN 45 PIC X(10) USING POST-DATE. 
            05 LINE 35 COLUMN 10 VALUE "(n) Next message".
            05 LINE 35 COLUMN 30 VALUE "(p) Previous message".
            05 LINE 35 COLUMN 60 VALUE "(q) Go back".   
            05 LINE 37 COLUMN 10 VALUE "Pick: ".
            05 READ-CHOICE-FIELD LINE 37 COLUMN 16 PIC X
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
           BACKGROUND-COLOR IS 8.
             05 BLANK SCREEN.
           
             05 LINE 32 COLUMN 10 VALUE
           "c$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$, '$$$$$$$$$$$$$$$$$$$$".
             05 LINE 31 COLUMN 10 VALUE
           "'.c$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$b  $$$$$$$$$$$$$$$$$$$$r".
             05 LINE 30 COLUMN 10 VALUE
           "!!' .d$$$$$$$$$$$$$$$$$$$$$$$$$$b ' z$$$$$$$$$$$$$$$$$$c <".
             05 LINE 29 COLUMN 10 VALUE
           "!!!!' .c$$$$$$$$$$$$$$$$$$$$$$$c  :: .c$$$$$$$$$$$$$$$. <!".
             05 LINE 28 COLUMN 10 VALUE
           " ;!!!!!'`.z$$$$$$$$$$$$$ec,. ```'''''''``` .,,ccecec,`'!!!".
             05 LINE 27 COLUMN 10 VALUE
           "$',;!!!!!!'``.,,,,,.```''!!!!!!!!!!!!!!!!!!!!'''''!!!!!>".
             05 LINE 26 COLUMN 10 VALUE
           "$$$P',;!!!!!!!!!!!!!!!!!!!!!!!;;;;;;!!!!!!!!!!!!!!!!!;  '".
             05 LINE 25 COLUMN 10 VALUE
           "$$$$$$$P' ,;;;<!!!!!>;;,. `'??????'  ,;;;;;;;;;, `'?$$".
             05 LINE 24 COLUMN 10 VALUE
           "$$$$$$$$ ?$???%   `'??$$$$$$$$$$$$bcucd$$$P'  ==$$$$$$$".
             05 LINE 23 COLUMN 10 VALUE
           "$$$$$$$b bc,.'??$$$$$$$$$$$$$$FF'?????',J$$$$$P' ,zd$$$".
             05 LINE 22 COLUMN 10 VALUE
           "$$$$$$c  '?$$$$$$$$$$$$$$$$$$$$$bc,,.`` .,,c$$$$$$$P',cb".
             05 LINE 21 COLUMN 10 VALUE
           "ec,.  `?$$$$$$$$$$$$$$$$$$$$$c.```%%%%,%%%,   c$$$$$$$$P'".
             05 LINE 20 COLUMN 10 VALUE
           "     '$$$$$$$$$$$$$$$$$$$$c.   ._              J$$$$$$$$$".
             05 LINE 19 COLUMN 10 VALUE
           "    ?$$$$$$$$$$$$$$$$$$c.      '????????' c$$$$$$$$P".
             05 LINE 18 COLUMN 10 VALUE
           "    $$$$$$$$$$$$$$ dbc `'?$$$$$$$$$$$$$$$$$$$$$$?$$$$$$$c".
             05 LINE 17 COLUMN 10 VALUE
           "    $$$$$$$$$$$$$$P'`?$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$b".
           05 LINE 16 COLUMN 10 VALUE
           "    J$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$c".
             05 LINE 15 COLUMN 10 VALUE
           "     z$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$c .".
             05 LINE 14 COLUMN 10 VALUE
           "      .e$$$$$$$$$$$$$$,$$$$$$$$$$$$$$$$$$$$$$$$$$.".
             05 LINE 13 COLUMN 10 VALUE
           "         .ze$$$$$$$$$er  .,cd$$$$$$$$$$$$$$$$bc.'".
             05 LINE 12 COLUMN 10 VALUE
           "              ```````''<!!!- '=-='     .  `--=',!>".
             05 LINE 11 COLUMN 10 VALUE
           "          `'-;,(<!!!!!!!!!> $F   )...:!.  d'  3 !>".
             05 LINE 10 COLUMN 10 VALUE
           "        `!  `!!!!><;;;!!!!! J$$b,`!>;!!:!!`,d?b`!>".
             05 LINE 9 COLUMN 10 VALUE
           "<!'''`  !!! ;,`'``''!!!;!!!!`..`!;  ,,,  .<!''`).".
             05 LINE 8 COLUMN 10 VALUE
           ".,,,.`` ,!!!' ;,(?';!!''<; `?$$$$$$PF ,;,".
             05 LINE 7 COLUMN 10 VALUE
           "!!!!>; `. ,;!>> .e$$$$$$$$''.  '?$$$$$$$e.".
             05 LINE 6 COLUMN 10 VALUE
           ";;, `\. `\         .,c$$$$$$$$$$$$$ec,.".
             05 LINE 5 COLUMN 10 VALUE
           "!!(``'!!".
             05 LINE 4 COLUMN 10 VALUE
           "!!!!!!;".  

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
             BACKGROUND-COLOR IS 8.
             05 BLANK SCREEN.
             05 LINE 32 COLUMN 10 VALUE
           ":!! <!!!!!!!! ; z$F ` ?$$?bc,ze$$$$$$ `- $$$$$$$c ` ;!!!".
             05 LINE 31 COLUMN 10 VALUE
           "!`:! !!!!!!!!! ; 4e . 3$$$$$$P' .d$$$$$ ccd$$$$.`!!!' ;;".
             05 LINE 30 COLUMN 10 VALUE
           "!!!`> <!!!!!!!! ; ?$$$$$$$$$$cec- .$$$' .'$$$c`'!!!!!>".
             05 LINE 29 COLUMN 10 VALUE
           "!!!!' `:!!!!!!> ; $$$$$$$C???????' .z $$$b.`!!!!!!: :".
             05 LINE 28 COLUMN 10 VALUE
           "!!!!! !!`,;;;,`.`$cececd$$$$$$$$$$$$$$' zc' <!!!>: `<!!".
             05 LINE 27 COLUMN 10 VALUE
           "!!!!>'!!!!: `.'.'??????$$$$$$$$$$beeeeee' <!'``` `!!!!;".
             05 LINE 26 COLUMN 10 VALUE
           ";;.``:,(. `.`??$$$ec.??',,cecece$$$$cucdP=  .,,;; ;;;;,".
             05 LINE 25 COLUMN 10 VALUE
           "::.``<!> : '$$$$$$$$ed$??$$$$PF','??? '?$$$$$????''".
             05 LINE 24 COLUMN 10 VALUE
           "!!!!;,`~.`?$$$$d$$$ 4$$$$$$$$$$$$$$$$$$$$$$$$b,.```` ,;'".
             05 LINE 23 COLUMN 10 VALUE
           "`''--.'?$$ed$F4$$$'? ' ,cc,,,,.`' '.JL <,',' ;'".
             05 LINE 22 COLUMN 10 VALUE
           "  `$$$$$$$$,'3F4$$$$$'xn`$$ MM ?$'dMMM> bFJ$> < b$$$. > ;".
             05 LINE 21 COLUMN 10 VALUE
           "  4$$$$$$$c d$$$$$$$$$??$$$P'?$$P MMM> $$4$P';' 4$$ < `;".
             05 LINE 20 COLUMN 10 VALUE
           " ',$$$$$$$'=?$$$$$$$$$$$$$$$$$$$$$',dM>`$Fd''cPF'$$$??b,".
             05 LINE 19 COLUMN 10 VALUE
           " dP$$$$$$$$-'c$$$$$$$$$$$$$$$$$$$$$P'?$$F'dF'J$$$$$b".
             05 LINE 18 COLUMN 10 VALUE
           "d'z$$$$$$$$'z$$$$$$$$$$$$$$$$$$$$$$$$$bce$$'?>,r,cec,".
             05 LINE 17 COLUMN 10 VALUE
           "`,c$$$$$$$$$P',zce$$$$$b ``''''''''`,zec. `''''''".
           05 LINE 16 COLUMN 10 VALUE
           "!!'`..,,,,,,,,,```````` <CCC>>>>>>>>CCCCC,,,,,,,>".
             05 LINE 15 COLUMN 10 VALUE
           " !!!!!> <<<CCCCCCCCCCCC:CCC>       'CCCCC```````>".
             05 LINE 14 COLUMN 10 VALUE
           "  >,''  $$P'',,,ccCCC:CCCCCCCCCCCCCCCCCCCC>>>>".
             05 LINE 13 COLUMN 10 VALUE
           "    $$$',cc,,,ced$$PF'' ' `?'''':'".
             05 LINE 12 COLUMN 10 VALUE
           "    $$$$P ze`$$$P'd$$$$$$'.d$$$$$$b.'$$P',c, !!!".
             05 LINE 11 COLUMN 10 VALUE
           "    d$u$$$$$$$$ec,?$$$$$$$$'.zec,.'$$$$$$$c,`!!!".
             05 LINE 10 COLUMN 10 VALUE
           "     . ,c$$$c, =$$$$$$$$$$$$$$??$$$$$$$c.`'',;;;".
             05 LINE 9 COLUMN 10 VALUE
           "           `'!!!!'',,cecec$$$$$$$cec,.'! ;,,".
             05 LINE 8 COLUMN 10 VALUE
           "            <!!!!!!!!!!!!!''''''<!!!!!;!!'".
             05 LINE 7 COLUMN 10 VALUE
           "             ,!!!!!!!!!!!!!!!!!(;!!!!!! ,!!'".
             05 LINE 6 COLUMN 10 VALUE
           "               ,<!!!!!!!!!!!!!!''; !!!!!' ;!!".
             05 LINE 5 COLUMN 10 VALUE
           "                  .,!!!!!!!!!!!! /  !!!!!!  ;>".
             05 LINE 4 COLUMN 10 VALUE
           "                       .,;;<!!!!! /  <!!!;;".  
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
             BACKGROUND-COLOR IS 8.
             05 BLANK SCREEN.
             05 LINE 32 COLUMN 10 VALUE
           "$$$$$$P $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$' d$$$$$$$$$$$$".
             05 LINE 31 COLUMN 10 VALUE
           "$$$$$$$F.$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$' .$$$$$$$$$$$".
             05 LINE 30 COLUMN 10 VALUE
           "$$$$$$$$'.$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$P' d$$$$$$$$$".
             05 LINE 29 COLUMN 10 VALUE
           "$$$$$$$$$'.$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$% .$$$$$$$$".
             05 LINE 28 COLUMN 10 VALUE
           "$$$$$$$$$$'.d$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ec   '.c$$$$$".
             05 LINE 27 COLUMN 10 VALUE
           "$$$$$$$$$$$$'.c$$$$$$$$$$$$$$$$$$$$$$$$$c,.  '?$$$$P' .,c".
             05 LINE 26 COLUMN 10 VALUE
           "$$$$$$$$c,. `'?'.,cd$$$$$$$$$$$$ecc,.      .cd$$$$$c. `!''".
             05 LINE 25 COLUMN 10 VALUE
           "ec,,.  '??$$$$$$$%=-     `'???$$$$$PP'  ..``<!!!!!".
             05 LINE 24 COLUMN 10 VALUE
           "'               `'??$$$$$$$$$$$$$$$$$$$$$$$$P'  ,<!!;,".
             05 LINE 23 COLUMN 10 VALUE
           ". '$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$P'".
             05 LINE 22 COLUMN 10 VALUE
           "'$$$$$$$$$$$$$$$$$$$$$$$$$bhbhU$$$$$$$$$$$$$$$$$$$$$$$'".
             05 LINE 21 COLUMN 10 VALUE
           "$$$$$$$$$$b,,,,ce$$$$$$$$$$$$$?????????$$$$$$$$$$$$$$$$P'".
             05 LINE 20 COLUMN 10 VALUE
           "$$$$$$$$$.'?4MMb`' .,,ce$$$$$$$$$$$$$$$$eee$$$$$$$$$$$$$$".
             05 LINE 19 COLUMN 10 VALUE
           "$$$$$$$$, C,um. 'MMMMPP'`````'TTTTT '.z$$$$$$$$".
             05 LINE 18 COLUMN 10 VALUE
           "$$$$$$$$'4>?ML`NMMT4beeuueuueuueedMMMMMCLnn.'MMP 4$$$$$$$".
             05 LINE 17 COLUMN 10 VALUE
           "z$$$$$$$$P',n.nmn,'???$$$$$$$$PPP' .,nMMP ?P' ' $$$$$$.".
           05 LINE 16 COLUMN 10 VALUE
           "  z$$$$$$$$$$$??$$$$$$$$$$$$$$$$$$$$$$$P'.,r<MM  `$$$$b.".
             05 LINE 15 COLUMN 10 VALUE
           "     ,cd$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$P'?$$$c.".
             05 LINE 14 COLUMN 10 VALUE
           " ``  >'` .,ce$'z$$$$$$$$$$$$$$$$$$$$$$$$$$$$d$$$$c.".
             05 LINE 13 COLUMN 10 VALUE
           "!!!!! `,;<!'''`` z$$$$$$$$$$$$$$$$$$$$$$$$c,c,.  `".
             05 LINE 12 COLUMN 10 VALUE
           ";!!;' <!!!'!!!!!.'.zd$$$$$$$$$$$$$$$$$c. ``''!;,".
             05 LINE 11 COLUMN 10 VALUE
           " ;<' ;!';<!!!!! ?$.   '.cc$$$$$$$$bc,.' `!!(`''!-".
             05 LINE 10 COLUMN 10 VALUE
           " !      ;<!!;!!'z$'  ')`'``      ^'   ,F !!!!;,`'".
             05 LINE 9 COLUMN 10 VALUE
           " !>     ,''.> <!',c$b.`!!!!!!!!' z$'3P !!!!!;.".
             05 LINE 8 COLUMN 10 VALUE
           "'!!       ,$P' ;!!'` !!>!!!!(,;!!',d$$b,\. .".
             05 LINE 7 COLUMN 10 VALUE
           "<!!>        ,cP' ,;;;, .;;;;;!  <!! C`'> 'c".
             05 LINE 6 COLUMN 10 VALUE
           "!!!>           ,cP???$$$$$$PPPPP' ;<!;, .".
             05 LINE 5 COLUMN 10 VALUE
           "!!!                ,ce$$$$$$$$$$$$P%=".
             05 LINE 4 COLUMN 10 VALUE
           "!!;                      .,,,,,,.".  
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
           BACKGROUND-COLOR IS 8.
             05 BLANK SCREEN.
             05 LINE 32 COLUMN 10 VALUE
           "      :!  !!!!!!!i `'!!!!!!!!!!!!!!!'''`.;ii!!!!!'`.'` ;!!".
             05 LINE 31 COLUMN 10 VALUE
           "          !!!!!!; `!!!!!i;. ~~~~~~~ .;i!!!!''`.;i!!!!!!!'.".
             05 LINE 30 COLUMN 10 VALUE
           "         `!!!!! `!!!!;.  ~~~~~~~~~~~~~~  .;i!!!!' .i!!!!!!".
             05 LINE 29 COLUMN 10 VALUE
           "         :!!!!> !!!;  ~~~~~~~. '$. ~~~~~~~~ .;!!!!'  ;!!!!".
             05 LINE 28 COLUMN 10 VALUE
           "         !!!!i !i. `~~~~~~~~ `$c ~~~~~~~~~~~~  <!!!!'  i!!".
             05 LINE 27 COLUMN 10 VALUE
           "          ,i!    ~~~~~~~~~~ '$r'~~~~~~~~~~~~ '  ;!!!!!  ;!".
             05 LINE 26 COLUMN 10 VALUE
           "       :::'`   `~~~~~~~~~~ '$.`~~~~~~~~~~~~~~ .~ .!!!!!' ;".
             05 LINE 25 COLUMN 10 VALUE
           "      `:::::  ~~~~~~~~~~~ `$.`~~~~~~~~~~~~~~~~  ~  <!!!!' ".
             05 LINE 24 COLUMN 10 VALUE
           "      :::::  ~~~~~~~~~~~ '$c`~~~~~~~~~~~~~~~~~~~ ~~ ;!!!!'".
             05 LINE 23 COLUMN 10 VALUE
           "      ::::  ~~~~~~~~~~~ `$c'~~~~~~~~~~~~~~~~~~~~~ ~~ ,iiii".
             05 LINE 22 COLUMN 10 VALUE
           "     `:::  ~~~~~~~~~~~ `$L ~~~~~~~~~~~~~~~~~~~~~~~ .  `''`".
             05 LINE 21 COLUMN 10 VALUE
           "      ::  ~~~~~~~~~~~.`$b ~~~~~~~~~~~~~~~~~~~~~~~~. `:::::".
             05 LINE 20 COLUMN 10 VALUE
           "     ::       .~~~~~~ ?$ ~~~~~~~~~~~~~~~~~~~~~~~~.  ::::::".
             05 LINE 19 COLUMN 10 VALUE
           "    :::::  $$$PF' .~~.$.~~~~~~~~~~~~~~~~~~~~~~~~.  :::::::".
             05 LINE 18 COLUMN 10 VALUE
           "    :::::  $$$eeed' .~~~~~~~~~~~~~~~~~~~~~~~~~~~  ::::::::".
             05 LINE 17 COLUMN 10 VALUE
           "    :::: `? =       '   '?????????'  .~~~.  :'.::::::".
           05 LINE 16 COLUMN 10 VALUE
           "   `:::  $$$$$r-.  P9$$$?$bedE' .,d$$$$$$$P'   `::::' .:::".
             05 LINE 15 COLUMN 10 VALUE
           "   :::: ?Fx$b. '?$ $$$b($$'   dF   'ud$$$$$$c `:::::::' .:".
             05 LINE 14 COLUMN 10 VALUE
           "`''''''''''''''''` ..z e$$$F   d$P'`'??<<3c :::::::::::' ".
             05 LINE 13 COLUMN 10 VALUE
           "!!!!!!!!!!!!!!!!!!!!!!!!!''`,   $$$$$$$$$Fc ::::::::::::::".
             05 LINE 12 COLUMN 10 VALUE
           "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'`..euJB$. :::::::::::::::".
             05 LINE 11 COLUMN 10 VALUE
           "!!i;,;i!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!''`  ::::::::::::::::".
             05 LINE 10 COLUMN 10 VALUE
           "; `::' .!!!!!!!!!!!i;,;i!!!!!!!!!!!!!!!!!!' .:::::::::::::".
             05 LINE 9 COLUMN 10 VALUE
           "  ::::  !!!!!!!!!, `''''```  .,;ii!!!!!!!!!!!'' .:::::::: ".
             05 LINE 8 COLUMN 10 VALUE
           " `::::  !!!!!!!!.`::::::::::::::'` .,;i!!!!!!!!!!' ::::. ".
             05 LINE 7 COLUMN 10 VALUE
           "  :::: `!!!!!!!> :::::::::::::::::::''`  ,i!!!!!!!!'..   ".
             05 LINE 6 COLUMN 10 VALUE
           "  :::: <!!!!!!!> ::::::::::::::::::::::::'`  ,i!!!!!!'   ".
             05 LINE 5 COLUMN 10 VALUE
           "  .::: <!!!!!!!> ::::::::::::::::::::::::::::'` i!!!!!'  ".
             05 LINE 4 COLUMN 10 VALUE
           "   ::: <!!!!!!!! ::::::::::::::::::::::::::::::: i!!!!>".  
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

           
       0100-DISPLAY-LOGIN.
           PERFORM 0230-CURRENT-TIME.
           INITIALIZE LOGIN-CHOICE.      
           DISPLAY LOGIN-SCREEN.
           DISPLAY TIME-SCREEN.
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
           DISPLAY TIME-SCREEN.

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
           DISPLAY TIME-SCREEN.
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
           PERFORM 0230-CURRENT-TIME.
           INITIALIZE MENU-CHOICE.
           DISPLAY MENU-SCREEN.
           DISPLAY TIME-SCREEN.
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
           PERFORM 0230-CURRENT-TIME.
           INITIALIZE MESSAGE-CHOICE.
           DISPLAY MESSAGEBOARD-SCREEN.
           DISPLAY TIME-SCREEN.
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
           PERFORM 0230-CURRENT-TIME.
           INITIALIZE READ-CHOICE.
           IF MESSAGE-NUM = 1 OR 2 OR 3 OR 4 OR 5 OR 6 OR 7 OR 8 OR 9 
           OR 10
               MOVE DISPLAY-MESSAGE-TITLE(OFFSET MESSAGE-NUM WS-MSGS) 
               TO TITLE 
               MOVE DISPLAY-MESSAGE-BODY(OFFSET MESSAGE-NUM WS-MSGS) 
               TO BODY  
               MOVE DISPLAY-MESSAGE-AUTHOR(OFFSET MESSAGE-NUM WS-MSGS) 
               TO POST-AUTHOR 
               MOVE DISPLAY-MESSAGE-DATE(OFFSET MESSAGE-NUM WS-MSGS) 
               TO POST-DATE                    
           END-IF.
           DISPLAY READ-MESSAGE-SCREEN.
           DISPLAY TIME-SCREEN.
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
           PERFORM 0230-CURRENT-TIME.
           STRING FUNCTION CURRENT-DATE(1:4) "-" 
               FUNCTION CURRENT-DATE(5:2) "-" FUNCTION CURRENT-DATE(7:2)
               INTO WS-FORMATTED-DATE
           END-STRING.

           INITIALIZE POST-CHOICE.
           INITIALIZE POST-TITLE.
           INITIALIZE POST-BODY.
           DISPLAY POST-MESSAGE-SCREEN.
           DISPLAY TIME-SCREEN.
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
           PERFORM 0230-CURRENT-TIME.
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
           DISPLAY TIME-SCREEN.
           MOVE 1 TO COUNTER.
           PERFORM UNTIL COUNTER = 20
             IF '*' EQUALS WS-WORD(COUNTER:1) 
              THEN ADD 1 TO WS-WORD-LENGTH
             END-IF
             ADD 1 TO COUNTER
           END-PERFORM.
           PERFORM 0170-IN-GAME-SCREEN.
          
       0170-IN-GAME-SCREEN.
           PERFORM 0230-CURRENT-TIME.
           INITIALIZE WS-GUESS-CHOICE.
           DISPLAY IN-GAME-SCREEN.
           DISPLAY TIME-SCREEN.
           ACCEPT WS-GUESS-CHOICE-FIELD.
           IF WS-GUESS-CHOICE = '!' THEN 
               PERFORM 0110-DISPLAY-MENU
           ELSE
               PERFORM 0180-CHECK-GUESS
           END-IF.
           
       0180-CHECK-GUESS.
           PERFORM 0230-CURRENT-TIME.
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
           PERFORM 0230-CURRENT-TIME.
           INITIALIZE WS-GUESSING-WINNING-CHOICE.
           COMPUTE WS-HIGH-SCORE = WS-LETTERS-LEFT * WS-GUESSES-LEFT.
           DISPLAY WORD-GUESSING-WINNING-SCREEN.
           DISPLAY TIME-SCREEN.
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
           PERFORM 0230-CURRENT-TIME.
           INITIALIZE WS-GUESSING-LOSING-CHOICE.
           DISPLAY WORD-GUESSING-LOSE-SCREEN.
           DISPLAY TIME-SCREEN.
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
           PERFORM 0230-CURRENT-TIME.
           INITIALIZE WS-HIGH-SCORE-CHOICE.
           SORT WS-TABLE-HIGH-SCORE ON DESCENDING WS-SCORE.
           DISPLAY HIGH-SCORE-SCREEN.
           DISPLAY TIME-SCREEN.
           ACCEPT WS-HIGH-SCORE-CHOICE.
           IF WS-HIGH-SCORE-CHOICE = 'b'
             PERFORM 0110-DISPLAY-MENU
           ELSE 
               PERFORM 0220-HIGH-SCORE-SCREEN
           END-IF.


       0230-CURRENT-TIME.
           MOVE FUNCTION CURRENT-DATE TO WS-TIME.
             
