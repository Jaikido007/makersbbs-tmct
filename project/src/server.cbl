       IDENTIFICATION DIVISION.
       PROGRAM-ID. server.
       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
           FILE-CONTROL.
           SELECT F-MESSAGE-FILE ASSIGN TO "messages.dat"
             ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
           FILE SECTION.
             FD F-MESSAGE-FILE.
             01 MESSAGES.
               05 MESSAGE-TITLE PIC X(60).
               05 MESSAGE-BODY PIC X(500).

           WORKING-STORAGE SECTION.
           01 USER-NAME PIC X(10).
           01 MENU-CHOICE PIC X.
           01 MESSAGE-1 PIC X(60).
      *     01 MESSAGE-2 PIC X(60).
      *     01 MESSAGE-3 PIC X(60).
      *     01 MESSAGE-4 PIC X(60).
      *     01 MESSAGE-5 PIC X(60).
      *     01 MESSAGE-6 PIC X(60).
      *     01 MESSAGE-7 PIC X(60).
      *     01 MESSAGE-8 PIC X(60).
           01 MESSAGE-9 PIC X(60).
           01 MESSAGE-10 PIC X(60).
           01 MESSAGE-CHOICE PIC X.
           01 WS-FILE-IS-ENDED PIC 9.

           

           SCREEN SECTION.
           01 LOGIN-SCREEN.
             05 BLANK SCREEN.
             05 LINE 2 COLUMN 10 VALUE "Makers BBS".
             05 LINE 4 COLUMN 10 VALUE "What's your name?".
             05 USER-NAME-FIELD LINE 6 COLUMN 10 PIC X(10)
                USING USER-NAME.

           01 MENU-SCREEN
             BACKGROUND-COLOR IS 8.
             05 BLANK SCREEN.
             05 LINE 2 COLUMN 10 VALUE "Makers BBS".
             05 LINE 4 COLUMN 10 VALUE "Welcome, ".
             05 LINE 4 COLUMN 19 PIC X(10) USING USER-NAME.
             05 LINE 8 COLUMN 10 VALUE "(n) Nothing".
             05 LINE 8 COLUMN 80 VALUE "(m) Message board".
             05 LINE 8 COLUMN 30 VALUE "(l) Logout".
             05 LINE 8 COLUMN 60 VALUE "(q) Quit".
             05 LINE 20 COLUMN 10 VALUE "Pick: ".
             05 MENU-CHOICE-FIELD LINE 20 COLUMN 16 PIC X
                USING MENU-CHOICE.

           01 MESSAGEBOARD-SCREEN
             BACKGROUND-COLOR IS 8.
             05 BLANK SCREEN.
             05 LINE 2 COLUMN 10 VALUE "Makers BBS".
             05 LINE 4 COLUMN 10 VALUE "Here are the last 10 messages:".
             05 LINE 6 COLUMN 10 VALUE "1.".
             05 LINE 6 COLUMN 14 PIC X(60) USING MESSAGE-1.
             05 LINE 7 COLUMN 10 VALUE "2.".
             05 LINE 7 COLUMN 14 PIC X(60) USING MESSAGE-1.
             05 LINE 8 COLUMN 10 VALUE "3.".
             05 LINE 8 COLUMN 14 PIC X(60) USING MESSAGE-1.
             05 LINE 9 COLUMN 10 VALUE "4.".
             05 LINE 9 COLUMN 14 PIC X(60) USING MESSAGE-1.
             05 LINE 10 COLUMN 10 VALUE "5.".
             05 LINE 10 COLUMN 14 PIC X(60) USING MESSAGE-1.
             05 LINE 11 COLUMN 10 VALUE "6.".
             05 LINE 11 COLUMN 14 PIC X(60) USING MESSAGE-1.
             05 LINE 12 COLUMN 10 VALUE "7.".
             05 LINE 12 COLUMN 14 PIC X(60) USING MESSAGE-1.
             05 LINE 13 COLUMN 10 VALUE "8.".
             05 LINE 13 COLUMN 14 PIC X(60) USING MESSAGE-1.
             05 LINE 14 COLUMN 10 VALUE "9.".
             05 LINE 14 COLUMN 14 PIC X(60) USING MESSAGE-1.
             05 LINE 15 COLUMN 10 VALUE "10.".
             05 LINE 15 COLUMN 14 PIC X(60) USING MESSAGE-1.
             05 LINE 17 COLUMN 10 VALUE "( ) Read the full message by ".
      *         "number".
            05 LINE 18 COLUMN 10 VALUE "(m) Post a message of your ".
      *        "own".
             05 LINE 19 COLUMN 10 VALUE "(n) Next page".
             05 LINE 19 COLUMN 30 VALUE "(p) Previous page".
             05 LINE 19 COLUMN 60 VALUE "(q) Go back".
             05 LINE 21 COLUMN 10 VALUE "Pick: ".
             05 MESSAGE-CHOICE-FIELD LINE 21 COLUMN 16 PIC X
                USING MESSAGE-CHOICE.

       PROCEDURE DIVISION.
       

       0110-DISPLAY-LOGIN.
           INITIALIZE USER-NAME.
           DISPLAY LOGIN-SCREEN.
           ACCEPT USER-NAME-FIELD.
           PERFORM 0120-DISPLAY-MENU.

       0120-DISPLAY-MENU.
           INITIALIZE MENU-CHOICE.
           DISPLAY MENU-SCREEN.
           ACCEPT MENU-CHOICE-FIELD.
           IF MENU-CHOICE = "q" THEN
           STOP RUN
           ELSE IF MENU-CHOICE = "l" THEN
           PERFORM 0110-DISPLAY-LOGIN
           ELSE IF MENU-CHOICE = "n" THEN
           PERFORM 0120-DISPLAY-MENU
           ELSE IF MENU-CHOICE = 'm' THEN
             PERFORM 0130-DISPLAY-MESSAGEBOARD
           END-IF. 
       

       0130-DISPLAY-MESSAGEBOARD.
           OPEN INPUT F-MESSAGE-FILE.
           MOVE 0 TO WS-FILE-IS-ENDED.
           PERFORM UNTIL WS-FILE-IS-ENDED = 1
               READ F-MESSAGE-FILE
               NOT AT END
               MOVE MESSAGE-TITLE TO MESSAGE-1
      *         MOVE MESSAGE-TITLE TO MESSAGE-2
      *         MOVE MESSAGE-TITLE TO MESSAGE-3
      *         MOVE MESSAGE-TITLE TO MESSAGE-4
      *         MOVE MESSAGE-TITLE TO MESSAGE-5
      *         MOVE MESSAGE-TITLE TO MESSAGE-6
      *         MOVE MESSAGE-TITLE TO MESSAGE-7
      *         MOVE MESSAGE-TITLE TO MESSAGE-8
      *         MOVE MESSAGE-TITLE TO MESSAGE-9
      *         MOVE MESSAGE-TITLE TO MESSAGE-10
               AT END
                   MOVE 1 TO WS-FILE-IS-ENDED
               END-READ
           END-PERFORM.
           CLOSE F-MESSAGE-FILE.
           INITIALIZE MESSAGE-CHOICE.
           DISPLAY MESSAGEBOARD-SCREEN.
           ACCEPT MESSAGE-CHOICE-FIELD.
           IF MESSAGE-CHOICE = "q" THEN 
               PERFORM 0120-DISPLAY-MENU
           END-IF.
