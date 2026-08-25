       IDENTIFICATION DIVISION.
       PROGRAM-ID. INIT-WAREHOUSE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT STOCK-FILE
               ASSIGN TO "STOCKS.DAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS STOCK-SKU
               FILE STATUS IS WS-STOCK-STATUS.

           SELECT MOVEMENTS-FILE
               ASSIGN TO "MOVEMENTS.DAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS MOVEMENT-ID
               FILE STATUS IS WS-MOVEMENT-STATUS.

       DATA DIVISION.
       FILE SECTION.

       FD STOCK-FILE.
           COPY "STOCK-RECORD.CPY".

       FD MOVEMENTS-FILE.
       01 MOVEMENT-RECORD.
           05 MOVEMENT-ID              PIC X(12).
           05 MOVEMENT-SKU             PIC X(10).
           05 MOVEMENT-TYPE            PIC X(8).
           05 MOVEMENT-DATE            PIC 9(8).
           05 MOVEMENT-QUANTITY        PIC 9(7).
           05 MOVEMENT-REFERENCE       PIC X(15).
           05 MOVEMENT-QTY-AFTER       PIC 9(7).

       WORKING-STORAGE SECTION.

       01 WS-STOCK-STATUS              PIC XX.
       01 WS-MOVEMENT-STATUS           PIC XX.

       PROCEDURE DIVISION.

       MAIN-PROCEDURE.

           OPEN OUTPUT STOCK-FILE

           IF WS-STOCK-STATUS = "00"
               DISPLAY "STOCKS.DAT CREATED SUCCESSFULLY."
           ELSE
               DISPLAY "ERROR CREATING STOCKS.DAT: "
                       WS-STOCK-STATUS
           END-IF

           CLOSE STOCK-FILE

           OPEN OUTPUT MOVEMENTS-FILE

           IF WS-MOVEMENT-STATUS = "00"
               DISPLAY "MOVEMENTS.DAT CREATED SUCCESSFULLY."
           ELSE
               DISPLAY "ERROR CREATING MOVEMENTS.DAT: "
                       WS-MOVEMENT-STATUS
           END-IF

           CLOSE MOVEMENTS-FILE

           STOP RUN.
