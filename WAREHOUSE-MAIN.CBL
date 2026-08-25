       IDENTIFICATION DIVISION.
       PROGRAM-ID. WAREHOUSE-MAIN.

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

       FD  STOCK-FILE.
           COPY "STOCK-RECORD.CPY".

       FD  MOVEMENTS-FILE.
       01  MOVEMENT-RECORD.
           05 MOVEMENT-ID              PIC X(12).
           05 MOVEMENT-SKU             PIC X(10).
           05 MOVEMENT-TYPE            PIC X(8).
           05 MOVEMENT-DATE            PIC 9(8).
           05 MOVEMENT-QUANTITY        PIC 9(7).
           05 MOVEMENT-REFERENCE       PIC X(15).
           05 MOVEMENT-QTY-AFTER       PIC 9(7).

       WORKING-STORAGE SECTION.

       01  WS-STOCK-STATUS             PIC XX.
       01  WS-MOVEMENT-STATUS          PIC XX.

       01  WS-CHOICE                   PIC 9.
       
       01  WS-MOVEMENT-QTY             PIC 9(7).
       01  WS-MOVEMENT-REFERENCE       PIC X(15).
       01  WS-MOVEMENT-NUMBER          PIC 9(9) VALUE 1.
       01  WS-CURRENT-DATE             PIC 9(8).
       01  WS-STOCK-VALID             PIC X.
       01  WS-DISPATCH-REFERENCE      PIC X(15).
       
       PROCEDURE DIVISION.

       P000-MAIN.

           OPEN I-O STOCK-FILE
                I-O MOVEMENTS-FILE

           IF WS-STOCK-STATUS NOT = "00"
               DISPLAY "STOCK FILE OPEN ERROR: "
                       WS-STOCK-STATUS
               STOP RUN
           END-IF

           IF WS-MOVEMENT-STATUS NOT = "00"
               DISPLAY "MOVEMENTS FILE OPEN ERROR: "
                       WS-MOVEMENT-STATUS
               STOP RUN
           END-IF

           MOVE 0 TO WS-CHOICE

           PERFORM P010-MENU
               UNTIL WS-CHOICE = 8

           CLOSE STOCK-FILE
                 MOVEMENTS-FILE

           DISPLAY " "
           DISPLAY "WAREHOUSE SYSTEM CLOSED."

           STOP RUN.


       P010-MENU.

           DISPLAY " "
           DISPLAY "=========================================="
           DISPLAY "     WAREHOUSE RECEIVING & DISPATCH"
           DISPLAY "=========================================="
           DISPLAY "1. Add New SKU"
           DISPLAY "2. Record Goods Receipt"
           DISPLAY "3. Record Dispatch"
           DISPLAY "4. Look Up Current Stock"
           DISPLAY "5. List SKUs Below Reorder Point"
           DISPLAY "6. Stock Movement Report"
           DISPLAY "7. Discontinue SKU"
           DISPLAY "8. Exit"
           DISPLAY "=========================================="
           DISPLAY "Enter choice: " WITH NO ADVANCING
           ACCEPT WS-CHOICE

           EVALUATE WS-CHOICE

               WHEN 1
                   PERFORM P020-ADD

               WHEN 2
                   PERFORM P030-RECEIPT

               WHEN 3
                   PERFORM P040-DISPATCH

               WHEN 4
                   PERFORM P050-LOOKUP

               WHEN 5
                   PERFORM P060-REORDER

               WHEN 6
                   PERFORM P070-REPORT

               WHEN 7
                   PERFORM P080-DISCONTINUE

               WHEN 8
                   CONTINUE

               WHEN OTHER
                   DISPLAY "Invalid choice."

           END-EVALUATE.


       P020-ADD.

           DISPLAY " "
           DISPLAY "--- ADD NEW SKU ---"

           DISPLAY "Enter SKU: " WITH NO ADVANCING
           ACCEPT STOCK-SKU

           READ STOCK-FILE
               INVALID KEY
                   CONTINUE
               NOT INVALID KEY
                   DISPLAY "SKU already exists."
                   EXIT PARAGRAPH
           END-READ

           DISPLAY "Enter description: " WITH NO ADVANCING
           ACCEPT STOCK-DESCRIPTION

           DISPLAY "Enter unit of measure: " WITH NO ADVANCING
           ACCEPT STOCK-UOM

           DISPLAY "Enter quantity on hand: " WITH NO ADVANCING
           ACCEPT STOCK-QTY-ON-HAND

           DISPLAY "Enter reorder point: " WITH NO ADVANCING
           ACCEPT STOCK-REORDER-POINT

           DISPLAY "Enter warehouse location: " WITH NO ADVANCING
           ACCEPT STOCK-LOCATION

           MOVE "ACTIVE" TO STOCK-STATUS

           WRITE STOCK-RECORD
               INVALID KEY
                   DISPLAY "Error adding SKU."
               NOT INVALID KEY
                   DISPLAY "SKU added successfully."
           END-WRITE.


       P030-RECEIPT.

           DISPLAY " "
           DISPLAY "--- GOODS RECEIPT ---"

           DISPLAY "Enter SKU: " WITH NO ADVANCING
           ACCEPT STOCK-SKU

           READ STOCK-FILE

               INVALID KEY
                   DISPLAY "SKU not found."
                   EXIT PARAGRAPH

           END-READ

           IF STOCK-STATUS = "DISCONTINUED"
               DISPLAY "Cannot receive stock for a discontinued SKU."
               EXIT PARAGRAPH
           END-IF

           DISPLAY "Current quantity: "
                   STOCK-QTY-ON-HAND

           DISPLAY "Enter receipt quantity: "
                   WITH NO ADVANCING
           ACCEPT WS-MOVEMENT-QTY

           IF WS-MOVEMENT-QTY <= 0
               DISPLAY "Receipt quantity must be greater than zero."
               EXIT PARAGRAPH
           END-IF

           DISPLAY "Enter GRN/reference number: "
                   WITH NO ADVANCING
           ACCEPT WS-MOVEMENT-REFERENCE

           ADD WS-MOVEMENT-QTY
               TO STOCK-QTY-ON-HAND

           REWRITE STOCK-RECORD

               INVALID KEY
                   DISPLAY "Error updating stock."
                   EXIT PARAGRAPH

               NOT INVALID KEY
                   CONTINUE

           END-REWRITE

           PERFORM P090-NEXT-ID

           MOVE STOCK-SKU
               TO MOVEMENT-SKU

           MOVE "RECEIPT"
               TO MOVEMENT-TYPE

           MOVE FUNCTION CURRENT-DATE(1:8)
               TO WS-CURRENT-DATE

           MOVE WS-CURRENT-DATE
               TO MOVEMENT-DATE

           MOVE WS-MOVEMENT-QTY
               TO MOVEMENT-QUANTITY

           MOVE WS-MOVEMENT-REFERENCE
               TO MOVEMENT-REFERENCE

           MOVE STOCK-QTY-ON-HAND
               TO MOVEMENT-QTY-AFTER

           WRITE MOVEMENT-RECORD

               INVALID KEY
                   DISPLAY "Error writing receipt movement."
                   DISPLAY "Receipt cancelled."
               NOT INVALID KEY
                   DISPLAY " "
                   DISPLAY "Receipt recorded successfully."
                   DISPLAY "Quantity received: "
                           WS-MOVEMENT-QTY
                   DISPLAY "New quantity: "
                           STOCK-QTY-ON-HAND
                   DISPLAY "Movement ID: "
                           MOVEMENT-ID

           END-WRITE.

       P040-DISPATCH.

           DISPLAY " "
           DISPLAY "--- DISPATCH ---"

           DISPLAY "Enter SKU: " WITH NO ADVANCING
           ACCEPT STOCK-SKU

           READ STOCK-FILE

               INVALID KEY
                   DISPLAY "SKU not found."
                   EXIT PARAGRAPH

           END-READ

           IF STOCK-STATUS = "DISCONTINUED"
               DISPLAY "Cannot dispatch a discontinued SKU."
               EXIT PARAGRAPH
           END-IF

           DISPLAY "Current quantity: "
                   STOCK-QTY-ON-HAND

           DISPLAY "Enter dispatch quantity: "
                   WITH NO ADVANCING
           ACCEPT WS-MOVEMENT-QTY

           IF WS-MOVEMENT-QTY <= 0
               DISPLAY "Dispatch quantity must be greater than zero."
               EXIT PARAGRAPH
           END-IF

           CALL "VALIDATE-STOCK"
               USING
                   STOCK-QTY-ON-HAND
                   WS-MOVEMENT-QTY
                   WS-STOCK-VALID

           IF WS-STOCK-VALID = "N"
               DISPLAY " "
               DISPLAY "INSUFFICIENT STOCK."
               DISPLAY "Available: "
                       STOCK-QTY-ON-HAND
               DISPLAY "Requested: "
                       WS-MOVEMENT-QTY
               EXIT PARAGRAPH
           END-IF

           DISPLAY "Enter delivery note/reference: "
                   WITH NO ADVANCING
           ACCEPT WS-DISPATCH-REFERENCE

           SUBTRACT WS-MOVEMENT-QTY
               FROM STOCK-QTY-ON-HAND

           PERFORM P090-NEXT-ID

           MOVE STOCK-SKU
               TO MOVEMENT-SKU

           MOVE "DISPATCH"
               TO MOVEMENT-TYPE

           MOVE FUNCTION CURRENT-DATE(1:8)
               TO WS-CURRENT-DATE

           MOVE WS-CURRENT-DATE
               TO MOVEMENT-DATE

           MOVE WS-MOVEMENT-QTY
               TO MOVEMENT-QUANTITY

           MOVE WS-DISPATCH-REFERENCE
               TO MOVEMENT-REFERENCE

           MOVE STOCK-QTY-ON-HAND
               TO MOVEMENT-QTY-AFTER

           WRITE MOVEMENT-RECORD

               INVALID KEY
                   DISPLAY "Error writing receipt movement."
                   DISPLAY "Stock quantity was updated."
                   ADD WS-MOVEMENT-QTY
                       TO STOCK-QTY-ON-HAND
                   EXIT PARAGRAPH

               NOT INVALID KEY
                   CONTINUE

           END-WRITE

           REWRITE STOCK-RECORD

               INVALID KEY
                   DISPLAY "Error updating stock record."

               NOT INVALID KEY
                   DISPLAY " "
                   DISPLAY "Dispatch recorded successfully."
                   DISPLAY "Quantity dispatched: "
                           WS-MOVEMENT-QTY
                   DISPLAY "New quantity: "
                           STOCK-QTY-ON-HAND
                   DISPLAY "Movement ID: "
                           MOVEMENT-ID

           END-REWRITE.


       P090-NEXT-ID.

           MOVE 1 TO WS-MOVEMENT-NUMBER

           PERFORM UNTIL WS-MOVEMENT-NUMBER > 999999999

               MOVE "MOV" TO MOVEMENT-ID(1:3)

               MOVE WS-MOVEMENT-NUMBER
                   TO MOVEMENT-ID(4:9)

               READ MOVEMENTS-FILE
                   INVALID KEY
                       EXIT PERFORM
               END-READ

               ADD 1 TO WS-MOVEMENT-NUMBER

           END-PERFORM.

       P050-LOOKUP.

           DISPLAY " "
           DISPLAY "--- STOCK LOOKUP ---".

           DISPLAY "Enter SKU: " WITH NO ADVANCING
           ACCEPT STOCK-SKU

           READ STOCK-FILE

               INVALID KEY
                   DISPLAY "SKU not found."

               NOT INVALID KEY
                   DISPLAY " "
                   DISPLAY "SKU:              " STOCK-SKU
                   DISPLAY "Description:      " STOCK-DESCRIPTION
                   DISPLAY "Unit:              " STOCK-UOM
                   DISPLAY "Quantity:          " STOCK-QTY-ON-HAND
                   DISPLAY "Reorder Point:     " STOCK-REORDER-POINT
                   DISPLAY "Location:          " STOCK-LOCATION
                   DISPLAY "Status:            " STOCK-STATUS

           END-READ.


       P060-REORDER.

           DISPLAY " "
           DISPLAY "--- BELOW REORDER POINT ---"
           DISPLAY "Function will be implemented next.".


       P070-REPORT.

           DISPLAY " "
           DISPLAY "--- STOCK MOVEMENT REPORT ---"
           DISPLAY "Function will be implemented next."
.

       P080-DISCONTINUE.

           DISPLAY " "
           DISPLAY "--- DISCONTINUE SKU ---"
           DISPLAY "Function will be implemented next.".

