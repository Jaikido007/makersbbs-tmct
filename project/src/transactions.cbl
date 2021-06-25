       IDENTIFICATION DIVISION.
       PROGRAM-ID. transactions.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-TRANSACTION-LOG-FILE ASSIGN TO "transaction-log.dat"
           ORGANISATION IS LINE SEQUENTIAL.
       DATA DIVISION.
           FILE SECTION.
           FD F-TRANSACTION-LOG-FILE.
           01 TRANSACTION.
               05 DY PIC X(2).
               05 MTH PIC X(2).
               05 YR PIC X(4).
               05 USERNAME PIC X(16).
               05 CHARGE PIC 9(2).
           
           LINKAGE SECTION.
           01 LS-FORMATTED-DT.
             05 LS-FORMATTED-DTE-TME.
               15 LS-FORMATTED-YEAR    PIC  X(4). 
               15 FILLER               PIC X VALUE '-'.
               15 LS-FORMATTED-MONTH   PIC  X(2).
               15 FILLER               PIC X VALUE '-'.
               15 LS-FORMATTED-DY      PIC  X(2).
               15 FILLER               PIC X VALUE '-'.
               15 LS-FORMATTED-HOUR    PIC  X(2).
               15 FILLER               PIC X VALUE ':'.
               15 LS-FORMATTED-MINS    PIC  X(2).
               15 FILLER               PIC X VALUE ':'.
               15 LS-FORMATTED-SEC     PIC  X(2).
               15 FILLER               PIC X VALUE ':'.
               15 LS-FORMATTED-MS      PIC  X(2).

           01 LS-USERNAME PIC X(16).
           01 LS-STORE-CHARGE PIC 9(2).

       PROCEDURE DIVISION USING LS-FORMATTED-DT, LS-USERNAME,
           LS-STORE-CHARGE.

           OPEN EXTEND F-TRANSACTION-LOG-FILE
               MOVE LS-FORMATTED-DY TO DY
               MOVE LS-FORMATTED-MONTH TO MTH
               MOVE LS-FORMATTED-YEAR TO YR
               MOVE LS-USERNAME TO USERNAME
               MOVE LS-STORE-CHARGE TO CHARGE
               WRITE TRANSACTION
               END-WRITE
           CLOSE F-TRANSACTION-LOG-FILE.
