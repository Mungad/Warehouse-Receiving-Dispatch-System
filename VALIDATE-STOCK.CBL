       IDENTIFICATION DIVISION.
       PROGRAM-ID. VALIDATE-STOCK.

       DATA DIVISION.
       LINKAGE SECTION.

       01  LS-QTY-ON-HAND       PIC 9(7).
       01  LS-DISPATCH-QTY      PIC 9(7).
       01  LS-VALID              PIC X.

       PROCEDURE DIVISION USING
           LS-QTY-ON-HAND
           LS-DISPATCH-QTY
           LS-VALID.

           IF LS-DISPATCH-QTY <= LS-QTY-ON-HAND
               MOVE "Y" TO LS-VALID
           ELSE
               MOVE "N" TO LS-VALID
           END-IF

           GOBACK.
