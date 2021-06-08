       IDENTIFICATION DIVISION. 
       FUNCTION-ID. REPLACE_ALL.
       ENVIRONMENT DIVISION.

       DATA DIVISION.

       FILE SECTION.

       WORKING-STORAGE SECTION.

       LINKAGE SECTION.
           01 LS-WORD PIC X(20).
           01 LS-HIDDEN-WORD PIC X(20).

       PROCEDURE DIVISION USING LS-WORD RETURNING LS-HIDDEN-WORD.
           INSPECT LS-WORD REPLACING ALL 'a' BY 'x'.
           INSPECT LS-WORD REPLACING ALL 'b' BY 'x'.
           INSPECT LS-WORD REPLACING ALL 'c' BY 'x'.
           INSPECT LS-WORD REPLACING ALL 'd' BY 'x'.
           INSPECT LS-WORD REPLACING ALL 'e' BY 'x'.
           INSPECT LS-WORD REPLACING ALL 'f' BY 'x'.
           INSPECT LS-WORD REPLACING ALL 'g' BY 'x'.
           INSPECT LS-WORD REPLACING ALL 'h' BY 'x'.
           INSPECT LS-WORD REPLACING ALL 'i' BY 'x'.
           INSPECT LS-WORD REPLACING ALL 'j' BY 'x'.
           INSPECT LS-WORD REPLACING ALL 'k' BY 'x'.
           INSPECT LS-WORD REPLACING ALL 'l' BY 'x'.
           INSPECT LS-WORD REPLACING ALL 'm' BY 'x'.
           INSPECT LS-WORD REPLACING ALL 'n' BY 'x'.
           INSPECT LS-WORD REPLACING ALL 'o' BY 'x'.
           INSPECT LS-WORD REPLACING ALL 'p' BY 'x'.
           INSPECT LS-WORD REPLACING ALL 'q' BY 'x'.
           INSPECT LS-WORD REPLACING ALL 'r' BY 'x'.
           INSPECT LS-WORD REPLACING ALL 's' BY 'x'.
           INSPECT LS-WORD REPLACING ALL 't' BY 'x'.
           INSPECT LS-WORD REPLACING ALL 'u' BY 'x'.
           INSPECT LS-WORD REPLACING ALL 'v' BY 'x'.
           INSPECT LS-WORD REPLACING ALL 'w' BY 'x'.
           INSPECT LS-WORD REPLACING ALL 'x' BY 'x'.
           INSPECT LS-WORD REPLACING ALL 'y' BY 'x'.
           INSPECT LS-WORD REPLACING ALL 'z' BY 'x'.
           END FUNCTION REPLACE_ALL.
