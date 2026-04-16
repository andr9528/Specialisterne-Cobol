       IDENTIFICATION DIVISION.
       PROGRAM-ID. FILE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL. 
           SELECT IN-TRANSACTIONS-FILE
               ASSIGN TO "Transactions.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT IN-BANKS-FILE
               ASSIGN TO "Banks.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT OUT-REPORT-FILE
               ASSIGN TO "Report.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD IN-BANKS-FILE.
       01 IN-BANK-RECORD.
           COPY "BankRecord.cpy".

       FD IN-TRANSACTIONS-FILE.
       01 IN-TRANSACTION-RECORD.
           COPY "TransactionRecord.cpy".

       FD OUT-REPORT-FILE.
       01 OUT-REPORT-RECORD PIC X(120).

       WORKING-STORAGE SECTION.
       
       01 BANKS OCCURS 40 TIMES.
           COPY "BankRecord.cpy".
       
       01 BANKS-MAX-COUNT PIC 99 VALUE 40.
       01 BANKS-COUNT PIC 99 VALUE 0.
       01 BANK-INDEX PIC 99 VALUE 1.
       01 BANK-MATCH-FOUND PIC X VALUE "N".
       01 MATCHED-BANK-INDEX PIC 99 VALUE 0.

       01 TRANSACTIONS OCCURS 600 TIMES.
           COPY "TransactionRecord.cpy". 
           02 AMOUNT PIC S9(11)V99.
           02 AMOUNT-DISPLAY PIC -Z,ZZZ,ZZZ,ZZZ,ZZ9.99.
           02 DKK-AMOUNT PIC S9(11)V99.
           02 DKK-AMOUNT-DISPLAY PIC -Z,ZZZ,ZZZ,ZZZ,ZZ9.99.
       
       01 TRANSACTION-INDEX PIC 999 VALUE 1.
       01 TRANSACTIONS-MAX-COUNT PIC 999 VALUE 600.
       01 TRANSACTIONS-COUNT PIC 999 VALUE 0.
       

      * 1 = 2020, 2 = 2021, 3 = 2022, etc...
       01 TRANSACTIONS-BY-YEAR-MONTH OCCURS 6 TIMES.
           02 MONTHS OCCURS 12 TIMES.
               03 MONTH-TRANSACTION-INDICES PIC 99 OCCURS 50 TIMES.
               03 TRANSACTIONS-MONTH-COUNT PIC 99.
               03 TOTAL-MONTH-INCOME PIC S9(11)V99 VALUE 0.
               03 TOTAL-MONTH-PAYMENT PIC S9(11)V99 VALUE 0.
               03 DKK-INCOME PIC S9(11)V99 VALUE 0.
               03 DKK-PAYMENT PIC S9(11)V99 VALUE 0.
               03 EUR-INCOME PIC S9(11)V99 VALUE 0.
               03 EUR-PAYMENT PIC S9(11)V99 VALUE 0.
               03 USD-INCOME PIC S9(11)V99 VALUE 0.
               03 USD-PAYMENT PIC S9(11)V99 VALUE 0.
               03 INCOME-COUNT PIC 99 VALUE 0.
               03 PAYMENT-COUNT PIC 99 VALUE 0.
               03 TRANSFER-COUNT PIC 99 VALUE 0.

       01 YEAR-INDEX        PIC 9.
       01 MONTH-INDEX       PIC 99.
       01 MONTH-SLOT        PIC 99.
       01 TEMP-YEAR         PIC 9(4).
       01 TEMP-MONTH        PIC 99.
       01 MONTH-NAME PIC X(12).

       01 CUSTOMERS OCCURS 60 TIMES.
           COPY "Customer.cpy".       
       01 CUSTOMER-MAX-COUNT PIC 99 VALUE 60.
       01 CUSTOMER-TRANSACTIONS-MAX-COUNT PIC 99 VALUE 20.

       01 TEMP-CUSTOMER.
           COPY "Customer.cpy".

       01 CUSTOMER-INDEX PIC 99 VALUE 1. 
       01 CUSTOMER-TRANSACTION-INDEX PIC 99 VALUE 1.
       01 CUSTOMER-COUNT PIC 99 VALUE 0.
       01 CUSTOMER-FOUND PIC X VALUE "N".
       01 MATCHED-CUSTOMER-INDEX PIC 99 VALUE 0.

       01 SHOPS OCCURS 60 TIMES.
           02 SHOP-NAME PIC X(20).
           02 TRANSACTIONS-SHOP-COUNT PIC 999 VALUE 0.
           02 SHOP-TRANSACTION-INDICES PIC 99 OCCURS 50 TIMES.               
           02 SHOP-REVENUE PIC S9(11)V99 VALUE 0.

       01 TEMP-SHOP.
           02 SHOP-NAME PIC X(20).
           02 TRANSACTIONS-SHOP-COUNT PIC 999 VALUE 0.
           02 SHOP-TRANSACTION-INDICES PIC 99 OCCURS 50 TIMES.               
           02 SHOP-REVENUE PIC S9(11)V99 VALUE 0.

       01 SHOPS-MAX-COUNT PIC 99 VALUE 60.
       01 SHOP-INDEX PIC 99 VALUE 1.
       01 SHOPS-COUNT PIC 99 VALUE 0.
       01 SHOP-MATCH-FOUND PIC X VALUE "N".
       01 MATCHED-SHOP-INDEX PIC 99 VALUE 0.
     
       01 SHOP-REVENUE-DISPLAY PIC -ZZ,ZZZ,ZZZ,ZZ9.99.

       01 OUTPUT-LINE-INDEX PIC 999 VALUE 0.
       01 OUTPUT-LINE-COUNT PIC 999 VALUE 0.
       01 OUTPUT-TEXT-LINE PIC X(120).
       01 OUTPUT-TEXT PIC X(120) OCCURS 700 TIMES.
       01 OUTPUT-LINE-MAX-COUNT PIC 999 VALUE 950.
       
       01 END-OF-FILE PIC X VALUE "N".
       01 LINE-INDEX PIC 9999 VALUE 1. 
      
       01 SEARCH-CPR PIC X(15).
       01 CURRENT-TRANSACTION-INDEX PIC 99 VALUE 0.
       01 NEXT-TRANSACTION-SLOT PIC 99 VALUE 0.
       
       01 SEPARATOR-LINE PIC X(90) VALUE ALL "-".

       01 SORT-INDEX PIC 99.
       01 COMPARE-INDEX PIC 99.
       01 COMPARE-INDEX-START PIC 99.
       01 COUNT-DISPLAY PIC ZZ9.

       01 SOURCE-AMOUNT PIC S9(11)V99.
       01 ABS-AMOUNT PIC 9(11)V99.
       01 FORMAT-AMOUNT-DISPLAY PIC Z,ZZZ,ZZZ,ZZ9.99.
       01 SIGNED-FORMAT-AMOUNT-DISPLAY PIC X(20).
       01 FORMATTED-DKK-AMOUNT-DISPLAY PIC X(20).
       01 FORMATTED-AMOUNT-DISPLAY PIC X(20).

       PROCEDURE DIVISION.          
           PERFORM LOAD-BANKS-DATA-TO-ARRAY
           PERFORM RESET-LOOP-VARIABLES
           PERFORM LOAD-TRANSACTIONS-DATA-TO-ARRAY
           PERFORM FILL-CUSTOMER-ARRAY
           PERFORM SORT-TRANSACTIONS-TO-MONTHS
           PERFORM COUNT-TRANSACTIONS-PER-SHOP

           PERFORM BUILD-REPORT
           PERFORM PRINT-REPORT
       STOP RUN.

       PRINT-REPORT.
           OPEN OUTPUT OUT-REPORT-FILE
           PERFORM VARYING OUTPUT-LINE-INDEX FROM 1 BY 1
               UNTIL OUTPUT-LINE-INDEX > OUTPUT-LINE-COUNT

               DISPLAY OUTPUT-TEXT(OUTPUT-LINE-INDEX)

               MOVE OUTPUT-TEXT(OUTPUT-LINE-INDEX)
                   TO OUT-REPORT-RECORD

               WRITE OUT-REPORT-RECORD

           END-PERFORM
           CLOSE OUT-REPORT-FILE

           EXIT.
       
       COUNT-TRANSACTIONS-PER-SHOP.
           DISPLAY "START COUNT-TRANSACTIONS-PER-SHOP"
           PERFORM CLEAR-SHOPS-STRUCTURE
           
           PERFORM VARYING TRANSACTION-INDEX FROM 1 BY 1
               UNTIL TRANSACTION-INDEX > TRANSACTIONS-COUNT
           
               PERFORM FIND-SHOP-BY-NAME
       
               IF SHOP-MATCH-FOUND = "Y"
                   ADD 1 TO TRANSACTIONS-SHOP-COUNT
                       OF SHOPS(MATCHED-SHOP-INDEX)
                   PERFORM ADD-TRANSACTION-TO-SHOP-REVENUE
                   MOVE TRANSACTIONS-SHOP-COUNT 
                           OF SHOPS(MATCHED-SHOP-INDEX)
                           TO NEXT-TRANSACTION-SLOT
                   MOVE TRANSACTION-INDEX
                       TO SHOP-TRANSACTION-INDICES
                       OF SHOPS(MATCHED-SHOP-INDEX, 
                           NEXT-TRANSACTION-SLOT)
               ELSE IF SHOPS-COUNT < SHOPS-MAX-COUNT
                   ADD 1 TO SHOPS-COUNT
              
                   MOVE TRANSACTION-SHOP 
                       OF TRANSACTIONS(TRANSACTION-INDEX)
                       TO SHOP-NAME OF SHOPS(SHOPS-COUNT)
                   
                   MOVE 1
                       TO TRANSACTIONS-SHOP-COUNT 
                           OF SHOPS(SHOPS-COUNT)
                   MOVE TRANSACTION-INDEX
                       TO SHOP-TRANSACTION-INDICES
                           OF SHOPS(SHOPS-COUNT, 1)
                   
                   MOVE SHOPS-COUNT TO MATCHED-SHOP-INDEX
                   
                   PERFORM ADD-TRANSACTION-TO-SHOP-REVENUE
               ELSE
                   DISPLAY "WARNING: SHOPS array is full. " 
                   "Could not add shop '" TRANSACTION-SHOP 
                       OF TRANSACTIONS(TRANSACTION-INDEX)
                   "'."
                   EXIT PARAGRAPH
               END-IF
       
           END-PERFORM

           PERFORM SORT-SHOPS-BY-REVENUE
           DISPLAY "END COUNT-TRANSACTIONS-PER-SHOP. SHOPS-COUNT = "
               SHOPS-COUNT
           EXIT.

       ADD-TRANSACTION-TO-SHOP-REVENUE.
           IF TRANSACTION-TYPE OF TRANSACTIONS(TRANSACTION-INDEX)
               = "Udbetaling"
       
               SUBTRACT DKK-AMOUNT OF TRANSACTIONS(TRANSACTION-INDEX)
                   FROM SHOP-REVENUE OF SHOPS(MATCHED-SHOP-INDEX)
       
           ELSE IF TRANSACTION-TYPE OF TRANSACTIONS(TRANSACTION-INDEX)
               = "Indbetaling"
               OR TRANSACTION-TYPE OF TRANSACTIONS(TRANSACTION-INDEX)
               = "Overfoersel"
               
               ADD DKK-AMOUNT OF TRANSACTIONS(TRANSACTION-INDEX)
                   TO SHOP-REVENUE OF SHOPS(MATCHED-SHOP-INDEX)
           END-IF
       
           EXIT.

       SORT-SHOPS-BY-REVENUE.
           DISPLAY "START SORT-SHOPS-BY-REVENUE"
           PERFORM VARYING SORT-INDEX FROM 1 BY 1
               UNTIL SORT-INDEX >= SHOPS-COUNT
       
               ADD 1 TO SORT-INDEX GIVING COMPARE-INDEX-START
               PERFORM VARYING COMPARE-INDEX
                   FROM COMPARE-INDEX-START BY 1
                   UNTIL COMPARE-INDEX > SHOPS-COUNT
       
                   IF SHOP-REVENUE OF SHOPS(COMPARE-INDEX)
                       > SHOP-REVENUE OF SHOPS(SORT-INDEX)
       
                       MOVE SHOPS(SORT-INDEX) TO TEMP-SHOP
                       MOVE SHOPS(COMPARE-INDEX)
                           TO SHOPS(SORT-INDEX)
                       MOVE TEMP-SHOP TO SHOPS(COMPARE-INDEX)
       
                   END-IF
       
               END-PERFORM
       
           END-PERFORM
           DISPLAY "END SORT-SHOPS-BY-REVENUE"
           EXIT.

       CLEAR-SHOPS-STRUCTURE.
           MOVE 0 TO SHOPS-COUNT
       
           PERFORM VARYING SHOP-INDEX FROM 1 BY 1
               UNTIL SHOP-INDEX > SHOPS-MAX-COUNT
       
               MOVE SPACES TO SHOP-NAME OF SHOPS(SHOP-INDEX)
               MOVE 0 TO TRANSACTIONS-SHOP-COUNT OF SHOPS(SHOP-INDEX)
       
           END-PERFORM
       
           EXIT.

       FIND-SHOP-BY-NAME.
           MOVE "N" TO SHOP-MATCH-FOUND
           MOVE 0 TO MATCHED-SHOP-INDEX
       
           PERFORM VARYING SHOP-INDEX FROM 1 BY 1
               UNTIL SHOP-INDEX > SHOPS-COUNT
                   OR SHOP-MATCH-FOUND = "Y"
       
               IF SHOP-NAME OF SHOPS(SHOP-INDEX) =
                   TRANSACTION-SHOP OF TRANSACTIONS(TRANSACTION-INDEX)
                   MOVE "Y" TO SHOP-MATCH-FOUND
                   MOVE SHOP-INDEX TO MATCHED-SHOP-INDEX
               END-IF
       
           END-PERFORM
       
           EXIT.

       CLEAR-TRANSACTIONS-TO-MONTHS-STRUCTURE.
           PERFORM VARYING YEAR-INDEX FROM 1 BY 1
               UNTIL YEAR-INDEX > 6
       
               PERFORM VARYING MONTH-INDEX FROM 1 BY 1
                   UNTIL MONTH-INDEX > 12
       
                   MOVE 0 TO TRANSACTIONS-MONTH-COUNT
                       OF TRANSACTIONS-BY-YEAR-MONTH(YEAR-INDEX, 
                           MONTH-INDEX)

                   MOVE 0 TO INCOME-COUNT
                       OF TRANSACTIONS-BY-YEAR-MONTH(YEAR-INDEX, 
                           MONTH-INDEX)

                   MOVE 0 TO PAYMENT-COUNT
                       OF TRANSACTIONS-BY-YEAR-MONTH(YEAR-INDEX, 
                           MONTH-INDEX)
                   
                   MOVE 0 TO TRANSFER-COUNT
                       OF TRANSACTIONS-BY-YEAR-MONTH(YEAR-INDEX, 
                           MONTH-INDEX)

                   MOVE 0 TO DKK-INCOME
                       OF TRANSACTIONS-BY-YEAR-MONTH(YEAR-INDEX, 
                           MONTH-INDEX)
                   MOVE 0 TO DKK-PAYMENT
                       OF TRANSACTIONS-BY-YEAR-MONTH(YEAR-INDEX, 
                           MONTH-INDEX)
                   
                   MOVE 0 TO EUR-INCOME
                       OF TRANSACTIONS-BY-YEAR-MONTH(YEAR-INDEX, 
                           MONTH-INDEX)
                   MOVE 0 TO EUR-PAYMENT
                       OF TRANSACTIONS-BY-YEAR-MONTH(YEAR-INDEX, 
                           MONTH-INDEX)
                   
                   MOVE 0 TO USD-INCOME
                       OF TRANSACTIONS-BY-YEAR-MONTH(YEAR-INDEX, 
                           MONTH-INDEX)
                   MOVE 0 TO USD-PAYMENT
                       OF TRANSACTIONS-BY-YEAR-MONTH(YEAR-INDEX, 
                           MONTH-INDEX)
       
               END-PERFORM
           END-PERFORM

           EXIT.

       POSITION-TRANSACTION-IN-MONTH.
           MOVE FUNCTION NUMVAL(
               TIME-OF-TRANSACTION
                   OF TRANSACTIONS(TRANSACTION-INDEX)(1:4)
           ) TO TEMP-YEAR
       
           MOVE FUNCTION NUMVAL(
               TIME-OF-TRANSACTION
                   OF TRANSACTIONS(TRANSACTION-INDEX)(6:2)
           ) TO TEMP-MONTH
       
           COMPUTE YEAR-INDEX = TEMP-YEAR - 2019
       
           MOVE TEMP-MONTH TO MONTH-INDEX
       
           IF YEAR-INDEX >= 1 AND YEAR-INDEX <= 6
              AND MONTH-INDEX >= 1 AND MONTH-INDEX <= 12
       
               ADD 1 TO TRANSACTIONS-MONTH-COUNT
                   OF TRANSACTIONS-BY-YEAR-MONTH(YEAR-INDEX, 
                   MONTH-INDEX)
       
               MOVE TRANSACTIONS-MONTH-COUNT
                   OF TRANSACTIONS-BY-YEAR-MONTH(YEAR-INDEX, 
                       MONTH-INDEX)
                   TO MONTH-SLOT
       
               MOVE TRANSACTION-INDEX
                   TO MONTH-TRANSACTION-INDICES
                      OF TRANSACTIONS-BY-YEAR-MONTH(YEAR-INDEX, 
                       MONTH-INDEX, MONTH-SLOT)
               DISPLAY "Assigned Transaction '" TRANSACTION-INDEX 
                   "' to Year '" YEAR-INDEX 
                   "'-'" MONTH-INDEX 
                   "' on Slot '" MONTH-SLOT "'."
       
           END-IF
           
           EXIT.

       ADD-TRANSACTION-TO-MONTH-TOTALS.
           IF TRANSACTION-TYPE
               OF TRANSACTIONS(TRANSACTION-INDEX)
                   = "Indbetaling"

               ADD DKK-AMOUNT
                   OF TRANSACTIONS(TRANSACTION-INDEX)
                   TO TOTAL-MONTH-INCOME
                       OF TRANSACTIONS-BY-YEAR-MONTH(YEAR-INDEX, 
                           MONTH-INDEX)
               ADD 1 TO INCOME-COUNT
                   OF TRANSACTIONS-BY-YEAR-MONTH(YEAR-INDEX, 
                       MONTH-INDEX)

           ELSE IF TRANSACTION-TYPE
                       OF TRANSACTIONS(TRANSACTION-INDEX)
                       = "Udbetaling"

               SUBTRACT DKK-AMOUNT
                   OF TRANSACTIONS(TRANSACTION-INDEX)
                   FROM TOTAL-MONTH-PAYMENT
                       OF TRANSACTIONS-BY-YEAR-MONTH(YEAR-INDEX, 
                           MONTH-INDEX)
               ADD 1 TO PAYMENT-COUNT
                   OF TRANSACTIONS-BY-YEAR-MONTH(YEAR-INDEX, 
                       MONTH-INDEX)

           ELSE IF TRANSACTION-TYPE
                       OF TRANSACTIONS(TRANSACTION-INDEX)
                       = "Overfoersel"
               
               ADD 1 TO TRANSFER-COUNT
                   OF TRANSACTIONS-BY-YEAR-MONTH(YEAR-INDEX, 
                       MONTH-INDEX)

               IF DKK-AMOUNT
                   OF TRANSACTIONS(TRANSACTION-INDEX)
                       >= 0

                   ADD DKK-AMOUNT
                       OF TRANSACTIONS(TRANSACTION-INDEX)
                       TO TOTAL-MONTH-INCOME
                           OF TRANSACTIONS-BY-YEAR-MONTH(YEAR-INDEX, 
                               MONTH-INDEX)

               ELSE

                   ADD DKK-AMOUNT
                       OF TRANSACTIONS(TRANSACTION-INDEX)
                       TO TOTAL-MONTH-PAYMENT
                           OF TRANSACTIONS-BY-YEAR-MONTH(YEAR-INDEX, 
                               MONTH-INDEX)

               END-IF
           END-IF

           EXIT.

       SORT-TRANSACTIONS-TO-MONTHS.
           PERFORM CLEAR-TRANSACTIONS-TO-MONTHS-STRUCTURE
       
           PERFORM VARYING TRANSACTION-INDEX FROM 1 BY 1
               UNTIL TRANSACTION-INDEX > TRANSACTIONS-COUNT
       
           PERFORM POSITION-TRANSACTION-IN-MONTH
           PERFORM ADD-TRANSACTION-TO-MONTH-TOTALS
           PERFORM ADD-TRANSACTION-TO-MONTH-CURRENCY-TOTALS
       
           END-PERFORM
       
           EXIT.
       ADD-TRANSACTION-TO-MONTH-CURRENCY-TOTALS.
           IF CURRENCY-CODE OF TRANSACTIONS(TRANSACTION-INDEX) = "DKK"
               PERFORM ADD-TRANSACTION-TO-MONTH-DKK-TOTALS
           ELSE IF CURRENCY-CODE OF TRANSACTIONS(TRANSACTION-INDEX) 
               = "EUR"
               PERFORM ADD-TRANSACTION-TO-MONTH-EUR-TOTALS
           ELSE IF CURRENCY-CODE OF TRANSACTIONS(TRANSACTION-INDEX) 
               = "USD"
               PERFORM ADD-TRANSACTION-TO-MONTH-USD-TOTALS
           END-IF
       
           EXIT.

       ADD-TRANSACTION-TO-MONTH-DKK-TOTALS.
           IF TRANSACTION-TYPE OF TRANSACTIONS(TRANSACTION-INDEX)
               = "Indbetaling"
       
               ADD AMOUNT OF TRANSACTIONS(TRANSACTION-INDEX)
                   TO DKK-INCOME
                       OF TRANSACTIONS-BY-YEAR-MONTH(YEAR-INDEX, 
                           MONTH-INDEX)
       
           ELSE IF TRANSACTION-TYPE OF TRANSACTIONS(TRANSACTION-INDEX)
               = "Udbetaling"
       
               SUBTRACT AMOUNT OF TRANSACTIONS(TRANSACTION-INDEX)
                   FROM DKK-PAYMENT
                       OF TRANSACTIONS-BY-YEAR-MONTH(YEAR-INDEX, 
                           MONTH-INDEX)
       
           ELSE IF TRANSACTION-TYPE OF TRANSACTIONS(TRANSACTION-INDEX)
               = "Overfoersel"
       
               IF AMOUNT OF TRANSACTIONS(TRANSACTION-INDEX) >= 0
                   ADD AMOUNT OF TRANSACTIONS(TRANSACTION-INDEX)
                       TO DKK-INCOME
                           OF TRANSACTIONS-BY-YEAR-MONTH(YEAR-INDEX, 
                               MONTH-INDEX)
               ELSE
                   ADD AMOUNT OF TRANSACTIONS(TRANSACTION-INDEX)
                       TO DKK-PAYMENT
                           OF TRANSACTIONS-BY-YEAR-MONTH(YEAR-INDEX, 
                               MONTH-INDEX)
               END-IF
           END-IF
       
           EXIT.

       ADD-TRANSACTION-TO-MONTH-EUR-TOTALS.
           IF TRANSACTION-TYPE OF TRANSACTIONS(TRANSACTION-INDEX)
               = "Indbetaling"
       
               ADD AMOUNT OF TRANSACTIONS(TRANSACTION-INDEX)
                   TO EUR-INCOME
                       OF TRANSACTIONS-BY-YEAR-MONTH(YEAR-INDEX, 
                           MONTH-INDEX)
       
           ELSE IF TRANSACTION-TYPE OF TRANSACTIONS(TRANSACTION-INDEX)
               = "Udbetaling"
       
               SUBTRACT AMOUNT OF TRANSACTIONS(TRANSACTION-INDEX)
                   FROM EUR-PAYMENT
                       OF TRANSACTIONS-BY-YEAR-MONTH(YEAR-INDEX, 
                           MONTH-INDEX)
       
           ELSE IF TRANSACTION-TYPE OF TRANSACTIONS(TRANSACTION-INDEX)
               = "Overfoersel"
       
               IF AMOUNT OF TRANSACTIONS(TRANSACTION-INDEX) >= 0
                   ADD AMOUNT OF TRANSACTIONS(TRANSACTION-INDEX)
                       TO EUR-INCOME
                           OF TRANSACTIONS-BY-YEAR-MONTH(YEAR-INDEX, 
                               MONTH-INDEX)
               ELSE
                   ADD AMOUNT OF TRANSACTIONS(TRANSACTION-INDEX)
                       TO EUR-PAYMENT
                           OF TRANSACTIONS-BY-YEAR-MONTH(YEAR-INDEX, 
                               MONTH-INDEX)
               END-IF
           END-IF
       
           EXIT.

       ADD-TRANSACTION-TO-MONTH-USD-TOTALS.
           IF TRANSACTION-TYPE OF TRANSACTIONS(TRANSACTION-INDEX)
               = "Indbetaling"
       
               ADD AMOUNT OF TRANSACTIONS(TRANSACTION-INDEX)
                   TO USD-INCOME
                       OF TRANSACTIONS-BY-YEAR-MONTH(YEAR-INDEX, 
                           MONTH-INDEX)
       
           ELSE IF TRANSACTION-TYPE OF TRANSACTIONS(TRANSACTION-INDEX)
               = "Udbetaling"
       
               SUBTRACT AMOUNT OF TRANSACTIONS(TRANSACTION-INDEX)
                   FROM USD-PAYMENT
                       OF TRANSACTIONS-BY-YEAR-MONTH(YEAR-INDEX, 
                           MONTH-INDEX)
       
           ELSE IF TRANSACTION-TYPE OF TRANSACTIONS(TRANSACTION-INDEX)
               = "Overfoersel"
       
               IF AMOUNT OF TRANSACTIONS(TRANSACTION-INDEX) >= 0
                   ADD AMOUNT OF TRANSACTIONS(TRANSACTION-INDEX)
                       TO USD-INCOME
                           OF TRANSACTIONS-BY-YEAR-MONTH(YEAR-INDEX, 
                               MONTH-INDEX)
               ELSE
                   ADD AMOUNT OF TRANSACTIONS(TRANSACTION-INDEX)
                       TO USD-PAYMENT
                           OF TRANSACTIONS-BY-YEAR-MONTH(YEAR-INDEX, 
                               MONTH-INDEX)
               END-IF
           END-IF
       
           EXIT.

       FILL-CUSTOMER-ARRAY.
           PERFORM VARYING TRANSACTION-INDEX FROM 1 BY 1
               UNTIL TRANSACTION-INDEX > TRANSACTIONS-COUNT
           
           MOVE CPR OF TRANSACTIONS(TRANSACTION-INDEX)
               TO SEARCH-CPR
           PERFORM CHECK-CUSTOMER-EXISTS

           IF CUSTOMER-FOUND = "Y"
               PERFORM ADD-INDEX-TO-EXISTING-CUSTOMER
           ELSE
               PERFORM ADD-NEW-CUSTOMER-TO-ARRAY
           END-IF

           END-PERFORM
           EXIT.
       
       ADD-INDEX-TO-EXISTING-CUSTOMER.
           DISPLAY "Adding Transaction Index '" TRANSACTION-INDEX 
               "' to existing Customer with CPR '" 
               FUNCTION TRIM(CPR 
                   OF TRANSACTIONS(TRANSACTION-INDEX) TRAILING)
               "'."
           IF CUSTOMER-TRANSACTIONS-COUNT 
               OF CUSTOMERS(MATCHED-CUSTOMER-INDEX)
                   >= CUSTOMER-TRANSACTIONS-MAX-COUNT
           
               DISPLAY "ERROR: Customer transaction limit reached."
               DISPLAY "CPR: "
                   CPR OF CUSTOMERS(MATCHED-CUSTOMER-INDEX)
               DISPLAY "TRANSACTION-INDEX: " TRANSACTION-INDEX
               DISPLAY "CURRENT COUNT: "
                   CUSTOMER-TRANSACTIONS-COUNT
                       OF CUSTOMERS(MATCHED-CUSTOMER-INDEX)
               DISPLAY "MAX COUNT: "
                   CUSTOMER-TRANSACTIONS-MAX-COUNT
           ELSE
               ADD 1 TO CUSTOMER-TRANSACTIONS-COUNT
                   OF CUSTOMERS(MATCHED-CUSTOMER-INDEX)               
    
               MOVE CUSTOMER-TRANSACTIONS-COUNT
                   OF CUSTOMERS(MATCHED-CUSTOMER-INDEX)
                   TO NEXT-TRANSACTION-SLOT
    
               MOVE TRANSACTION-INDEX
                   TO TRANSACTION-INDICES OF CUSTOMERS
                      (MATCHED-CUSTOMER-INDEX, NEXT-TRANSACTION-SLOT)

               DISPLAY "Added '1' to Customer Transactions Count. " 
                   "It is now: " CUSTOMER-TRANSACTIONS-COUNT
                       OF CUSTOMERS(MATCHED-CUSTOMER-INDEX)
           END-IF
           EXIT.

       ADD-NEW-CUSTOMER-TO-ARRAY.
           DISPLAY "Creating new Customer with CPR '"
               FUNCTION TRIM(CPR 
                   OF TRANSACTIONS(TRANSACTION-INDEX) TRAILING) "'."
           DISPLAY "Adding '" TRANSACTION-INDEX "' to new Customer."
           ADD 1 TO CUSTOMER-COUNT

           MOVE CPR OF TRANSACTIONS(TRANSACTION-INDEX)
               TO CPR OF CUSTOMERS(CUSTOMER-COUNT)
           MOVE CUSTOMER-NAME OF TRANSACTIONS(TRANSACTION-INDEX)
               TO CUSTOMER-NAME OF CUSTOMERS(CUSTOMER-COUNT)
           MOVE CUSTOMER-ADDRESS OF TRANSACTIONS(TRANSACTION-INDEX)
               TO CUSTOMER-ADDRESS OF CUSTOMERS(CUSTOMER-COUNT)
           
           DISPLAY "Looking for Bank '" REGISTRATION-NUMBER 
               OF TRANSACTIONS(TRANSACTION-INDEX) "'."
           PERFORM FIND-MATCHING-BANK
           MOVE MATCHED-BANK-INDEX
               TO CUSTOMER-BANK-INDEX OF CUSTOMERS(CUSTOMER-COUNT)
           DISPLAY "Saved Bank '" MATCHED-BANK-INDEX "'."

           ADD 1 TO CUSTOMER-TRANSACTIONS-COUNT
               OF CUSTOMERS(CUSTOMER-COUNT)
           DISPLAY "Added '1' to Customer Transactions Count. " 
               "It is now - " CUSTOMER-TRANSACTIONS-COUNT
                   OF CUSTOMERS(CUSTOMER-COUNT)

           MOVE CUSTOMER-TRANSACTIONS-COUNT
               OF CUSTOMERS(CUSTOMER-COUNT)
               TO NEXT-TRANSACTION-SLOT

           MOVE TRANSACTION-INDEX
               TO TRANSACTION-INDICES OF CUSTOMERS
                  (CUSTOMER-COUNT, NEXT-TRANSACTION-SLOT)

           EXIT.

       CHECK-CUSTOMER-EXISTS.
           MOVE "N" TO CUSTOMER-FOUND
           MOVE 0 TO MATCHED-CUSTOMER-INDEX

           PERFORM VARYING CUSTOMER-INDEX FROM 1 BY 1
               UNTIL CUSTOMER-INDEX > CUSTOMER-COUNT
                  OR CUSTOMER-FOUND = "Y"

               IF CPR OF CUSTOMERS(CUSTOMER-INDEX)
                  = SEARCH-CPR
                   MOVE "Y" TO CUSTOMER-FOUND
                   MOVE CUSTOMER-INDEX TO MATCHED-CUSTOMER-INDEX
               END-IF
           END-PERFORM

           MOVE SPACES TO SEARCH-CPR

           EXIT.
       
       BUILD-REPORT.
           DISPLAY "START BUILD-REPORT"
           PERFORM VARYING CUSTOMER-INDEX FROM 1 BY 1
               UNTIL CUSTOMER-INDEX > CUSTOMER-COUNT
           DISPLAY "Computing Totals for Customer '" 
               FUNCTION TRIM(CUSTOMER-NAME 
                       OF CUSTOMERS(CUSTOMER-INDEX) TRAILING) "'."
           PERFORM COMPUTE-CUSTOMER-TOTALS

           END-PERFORM

           PERFORM SORT-CUSTOMERS-BY-SALDO
           PERFORM ADD-TOP-3-CUSTOMERS-TO-OUTPUT
           PERFORM ADD-MONTHLY-STATISTICS-TO-OUTPUT
           PERFORM ADD-SHOP-STATISTICS-TO-OUTPUT
           DISPLAY "END BUILD-REPORT"
           EXIT.
       
       ADD-SHOP-STATISTICS-TO-OUTPUT.
           DISPLAY "START ADD-SHOP-STATISTICS-TO-OUTPUT"
           MOVE SPACES TO OUTPUT-TEXT-LINE
           PERFORM ADD-OUTPUT-LINE-SAFE
       
           MOVE "Statistik for butikker:"
               TO OUTPUT-TEXT-LINE
           PERFORM ADD-OUTPUT-LINE-SAFE
       
           MOVE SPACES TO OUTPUT-TEXT-LINE
           MOVE "Butik" TO OUTPUT-TEXT-LINE(1:20)
           MOVE "Antal transaktioner" TO OUTPUT-TEXT-LINE(26:19)
           MOVE "Omsaetning (DKK)" TO OUTPUT-TEXT-LINE(50:16)
           PERFORM ADD-OUTPUT-LINE-SAFE
       
           PERFORM VARYING SHOP-INDEX FROM 1 BY 1
               UNTIL SHOP-INDEX > SHOPS-COUNT
               DISPLAY "Processing SHOP-INDEX: " SHOP-INDEX
               " Name: '" SHOP-NAME OF SHOPS(SHOP-INDEX) "'"
               MOVE SPACES TO OUTPUT-TEXT-LINE
       
               MOVE SHOP-NAME OF SHOPS(SHOP-INDEX)
                   TO OUTPUT-TEXT-LINE(1:20)
       
               MOVE TRANSACTIONS-SHOP-COUNT OF SHOPS(SHOP-INDEX)
                   TO COUNT-DISPLAY
               
               MOVE FUNCTION TRIM(COUNT-DISPLAY LEADING)
                   TO OUTPUT-TEXT-LINE(26:3)

               MOVE SHOP-REVENUE OF SHOPS(SHOP-INDEX)
                   TO SOURCE-AMOUNT
               PERFORM FORMAT-SIGNED-AMOUNT
               
               MOVE FUNCTION TRIM(SIGNED-FORMAT-AMOUNT-DISPLAY LEADING)
                   TO OUTPUT-TEXT-LINE(50:16)
       
               PERFORM ADD-OUTPUT-LINE-SAFE
       
           END-PERFORM
       
           MOVE SPACES TO OUTPUT-TEXT-LINE
           PERFORM ADD-OUTPUT-LINE-SAFE
           DISPLAY "END ADD-SHOP-STATISTICS-TO-OUTPUT"
           EXIT.

       ADD-MONTHLY-STATISTICS-TO-OUTPUT.
           DISPLAY "START ADD-MONTHLY-STATISTICS-TO-OUTPUT"
           MOVE SPACES TO OUTPUT-TEXT-LINE
           PERFORM ADD-OUTPUT-LINE-SAFE

           MOVE "Monthly statistics:"
               TO OUTPUT-TEXT-LINE
           PERFORM ADD-OUTPUT-LINE-SAFE

           PERFORM VARYING YEAR-INDEX FROM 1 BY 1
               UNTIL YEAR-INDEX > 6
               DISPLAY "Processing YEAR-INDEX: " YEAR-INDEX

               COMPUTE TEMP-YEAR = 2019 + YEAR-INDEX

               MOVE SEPARATOR-LINE TO OUTPUT-TEXT-LINE
               PERFORM ADD-OUTPUT-LINE-SAFE

               MOVE SPACES TO OUTPUT-TEXT-LINE
               STRING
                   "Year: "
                   DELIMITED BY SIZE
                   TEMP-YEAR
                   DELIMITED BY SIZE
                   INTO OUTPUT-TEXT-LINE
               END-STRING
               PERFORM ADD-OUTPUT-LINE-SAFE

               MOVE SPACES TO OUTPUT-TEXT-LINE
               MOVE "Month"
                   TO OUTPUT-TEXT-LINE(1:12)
               MOVE "Income"
                   TO OUTPUT-TEXT-LINE(18:6)
               MOVE "Payments"
                   TO OUTPUT-TEXT-LINE(42:8)
               MOVE "Income Count"
                   TO OUTPUT-TEXT-LINE(64:12)
               MOVE "Payment Count"
                   TO OUTPUT-TEXT-LINE(78:13)
               MOVE "Transfer Count"
                   TO OUTPUT-TEXT-LINE(93:14)
               PERFORM ADD-OUTPUT-LINE-SAFE

               PERFORM VARYING MONTH-INDEX FROM 1 BY 1
                   UNTIL MONTH-INDEX > 12
               DISPLAY "  Processing MONTH-INDEX: " MONTH-INDEX
               PERFORM ADD-MONTH-STATISTIC-TO-OUTPUT

               END-PERFORM

           END-PERFORM

           MOVE SEPARATOR-LINE TO OUTPUT-TEXT-LINE
           PERFORM ADD-OUTPUT-LINE-SAFE

           MOVE SPACES TO OUTPUT-TEXT-LINE
           PERFORM ADD-OUTPUT-LINE-SAFE
           DISPLAY "END ADD-MONTHLY-STATISTICS-TO-OUTPUT"
           EXIT.
       
       ADD-OUTPUT-LINE-SAFE.
           ADD 1 TO OUTPUT-LINE-COUNT

           IF OUTPUT-LINE-COUNT > OUTPUT-LINE-MAX-COUNT
               DISPLAY "ERROR: OUTPUT-TEXT overflow"
               DISPLAY "Line: " OUTPUT-LINE-COUNT
               DISPLAY "Max : " OUTPUT-LINE-MAX-COUNT
               EXIT PARAGRAPH
           END-IF

           MOVE OUTPUT-TEXT-LINE
               TO OUTPUT-TEXT(OUTPUT-LINE-COUNT)

           MOVE SPACES TO OUTPUT-TEXT-LINE

           EXIT.

       ADD-MONTH-STATISTIC-TO-OUTPUT.
           PERFORM GET-MONTH-NAME

           MOVE SPACES TO OUTPUT-TEXT-LINE

           MOVE MONTH-NAME
               TO OUTPUT-TEXT-LINE(1:12)

           IF TRANSACTIONS-MONTH-COUNT
               OF TRANSACTIONS-BY-YEAR-MONTH(YEAR-INDEX, 
                   MONTH-INDEX) = 0

               MOVE "No transactions this month"
                   TO OUTPUT-TEXT-LINE(16:26)

           ELSE               
               PERFORM ADD-MAIN-MONTH-LINE
               PERFORM ADD-DKK-MONTH-LINE
               PERFORM ADD-EUR-MONTH-LINE
               PERFORM ADD-USD-MONTH-LINE
           END-IF
           
           EXIT.
       
       ADD-DKK-MONTH-LINE.
           MOVE SPACES TO OUTPUT-TEXT-LINE
           MOVE "DKK" TO OUTPUT-TEXT-LINE(14:3)

           MOVE DKK-INCOME
               OF TRANSACTIONS-BY-YEAR-MONTH(YEAR-INDEX,
                   MONTH-INDEX)
               TO SOURCE-AMOUNT

           DISPLAY "Formatting 'DKK-INCOME'..."
           PERFORM FORMAT-SIGNED-AMOUNT

           MOVE SIGNED-FORMAT-AMOUNT-DISPLAY
               TO OUTPUT-TEXT-LINE(18:18)

           MOVE "DKK" TO OUTPUT-TEXT-LINE(38:3)

           MOVE DKK-PAYMENT
               OF TRANSACTIONS-BY-YEAR-MONTH(YEAR-INDEX,
                   MONTH-INDEX)
               TO SOURCE-AMOUNT

           DISPLAY "Formatting 'DKK-PAYMENT'..."
           PERFORM FORMAT-SIGNED-AMOUNT

           MOVE SIGNED-FORMAT-AMOUNT-DISPLAY
               TO OUTPUT-TEXT-LINE(42:18)

           PERFORM ADD-OUTPUT-LINE-SAFE

           EXIT.

       ADD-EUR-MONTH-LINE.
           MOVE SPACES TO OUTPUT-TEXT-LINE
           MOVE "EUR" TO OUTPUT-TEXT-LINE(14:3)

           MOVE EUR-INCOME
               OF TRANSACTIONS-BY-YEAR-MONTH(YEAR-INDEX,
                   MONTH-INDEX)
               TO SOURCE-AMOUNT

           DISPLAY "Formatting 'EUR-INCOME'..."
           PERFORM FORMAT-SIGNED-AMOUNT

           MOVE SIGNED-FORMAT-AMOUNT-DISPLAY
               TO OUTPUT-TEXT-LINE(18:18)

           MOVE "EUR" TO OUTPUT-TEXT-LINE(38:3)

           MOVE EUR-PAYMENT
               OF TRANSACTIONS-BY-YEAR-MONTH(YEAR-INDEX,
                   MONTH-INDEX)
               TO SOURCE-AMOUNT

           DISPLAY "Formatting 'EUR-PAYMENT'..."
           PERFORM FORMAT-SIGNED-AMOUNT

           MOVE SIGNED-FORMAT-AMOUNT-DISPLAY
               TO OUTPUT-TEXT-LINE(42:18)

           PERFORM ADD-OUTPUT-LINE-SAFE

           EXIT.

       ADD-USD-MONTH-LINE.
           MOVE SPACES TO OUTPUT-TEXT-LINE
           MOVE "USD" TO OUTPUT-TEXT-LINE(14:3)

           MOVE USD-INCOME
               OF TRANSACTIONS-BY-YEAR-MONTH(YEAR-INDEX,
                   MONTH-INDEX)
               TO SOURCE-AMOUNT

           DISPLAY "Formatting 'USD-INCOME'..."
           PERFORM FORMAT-SIGNED-AMOUNT

           MOVE SIGNED-FORMAT-AMOUNT-DISPLAY
               TO OUTPUT-TEXT-LINE(18:18)

           MOVE "USD" TO OUTPUT-TEXT-LINE(38:3)

           MOVE USD-PAYMENT
               OF TRANSACTIONS-BY-YEAR-MONTH(YEAR-INDEX,
                   MONTH-INDEX)
               TO SOURCE-AMOUNT

           DISPLAY "Formatting 'USD-PAYMENT'..."
           PERFORM FORMAT-SIGNED-AMOUNT

           MOVE SIGNED-FORMAT-AMOUNT-DISPLAY
               TO OUTPUT-TEXT-LINE(42:18)

           PERFORM ADD-OUTPUT-LINE-SAFE

           EXIT.

       ADD-MAIN-MONTH-LINE.
           MOVE "DKK"
               TO OUTPUT-TEXT-LINE(14:3)

           MOVE TOTAL-MONTH-INCOME
               OF TRANSACTIONS-BY-YEAR-MONTH(YEAR-INDEX, 
                   MONTH-INDEX)
               TO SOURCE-AMOUNT

           DISPLAY "Formatting 'TOTAL-MONTH-INCOME'..."
           PERFORM FORMAT-SIGNED-AMOUNT

           MOVE FUNCTION TRIM(SIGNED-FORMAT-AMOUNT-DISPLAY LEADING)
                   TO OUTPUT-TEXT-LINE(18:18)

           MOVE "DKK"
               TO OUTPUT-TEXT-LINE(38:3)

    
           MOVE TOTAL-MONTH-PAYMENT
               OF TRANSACTIONS-BY-YEAR-MONTH(YEAR-INDEX, 
                   MONTH-INDEX)
               TO SOURCE-AMOUNT  

           DISPLAY "Formatting 'TOTAL-MONTH-PAYMENT'..."
           PERFORM FORMAT-SIGNED-AMOUNT

           MOVE FUNCTION TRIM(SIGNED-FORMAT-AMOUNT-DISPLAY LEADING)
                   TO OUTPUT-TEXT-LINE(42:18)

           PERFORM ADD-MONTH-COUNT-PARTS
           PERFORM ADD-OUTPUT-LINE-SAFE

       EXIT.

       ADD-MONTH-COUNT-PARTS.
           MOVE INCOME-COUNT
               OF TRANSACTIONS-BY-YEAR-MONTH(YEAR-INDEX,
                   MONTH-INDEX)
               TO COUNT-DISPLAY
           MOVE FUNCTION TRIM(COUNT-DISPLAY, LEADING)
               TO OUTPUT-TEXT-LINE(74:3)

           MOVE PAYMENT-COUNT
               OF TRANSACTIONS-BY-YEAR-MONTH(YEAR-INDEX,
                   MONTH-INDEX)
               TO COUNT-DISPLAY
           MOVE FUNCTION TRIM(COUNT-DISPLAY, LEADING)
               TO OUTPUT-TEXT-LINE(88:3)

           MOVE TRANSFER-COUNT
               OF TRANSACTIONS-BY-YEAR-MONTH(YEAR-INDEX,
                   MONTH-INDEX)
               TO COUNT-DISPLAY
           MOVE FUNCTION TRIM(COUNT-DISPLAY, LEADING)
               TO OUTPUT-TEXT-LINE(104:3)

           EXIT.

       GET-MONTH-NAME.
           EVALUATE MONTH-INDEX
               WHEN 1
                   MOVE "January" TO MONTH-NAME
               WHEN 2
                   MOVE "February" TO MONTH-NAME
               WHEN 3
                   MOVE "March" TO MONTH-NAME
               WHEN 4
                   MOVE "April" TO MONTH-NAME
               WHEN 5
                   MOVE "May" TO MONTH-NAME
               WHEN 6
                   MOVE "June" TO MONTH-NAME
               WHEN 7
                   MOVE "July" TO MONTH-NAME
               WHEN 8
                   MOVE "August" TO MONTH-NAME
               WHEN 9
                   MOVE "September" TO MONTH-NAME
               WHEN 10
                   MOVE "October" TO MONTH-NAME
               WHEN 11
                   MOVE "November" TO MONTH-NAME
               WHEN 12
                   MOVE "December" TO MONTH-NAME
               WHEN OTHER
                   MOVE "Unknown" TO MONTH-NAME
           END-EVALUATE

           EXIT.

       SORT-CUSTOMERS-BY-SALDO.
           DISPLAY "START SORT-CUSTOMERS-BY-SALDO"
           PERFORM VARYING SORT-INDEX FROM 1 BY 1
               UNTIL SORT-INDEX >= CUSTOMER-COUNT
       
               ADD 1 TO SORT-INDEX GIVING COMPARE-INDEX-START
               PERFORM VARYING COMPARE-INDEX 
                   FROM COMPARE-INDEX-START BY 1
                   UNTIL COMPARE-INDEX > CUSTOMER-COUNT
       
                   IF DKK-SALDO OF CUSTOMERS(SORT-INDEX)
                      < DKK-SALDO OF CUSTOMERS(COMPARE-INDEX)
       
                       MOVE CUSTOMERS(SORT-INDEX) TO TEMP-CUSTOMER
                       MOVE CUSTOMERS(COMPARE-INDEX) 
                           TO CUSTOMERS(SORT-INDEX)
                       MOVE TEMP-CUSTOMER TO CUSTOMERS(COMPARE-INDEX)
       
                   END-IF
       
               END-PERFORM
           END-PERFORM
           DISPLAY "END SORT-CUSTOMERS-BY-SALDO"
           EXIT.
       
       ADD-TOP-3-CUSTOMERS-TO-OUTPUT.
           DISPLAY "START ADD-TOP-3-CUSTOMERS-TO-OUTPUT"           
           MOVE SPACES TO OUTPUT-TEXT-LINE
           PERFORM ADD-OUTPUT-LINE-SAFE
           MOVE "Top 3 kunder baseret paa saldo:"
               TO OUTPUT-TEXT-LINE
           PERFORM ADD-OUTPUT-LINE-SAFE

           PERFORM VARYING CUSTOMER-INDEX FROM 1 BY 1
               UNTIL CUSTOMER-INDEX > 3
                   OR CUSTOMER-INDEX > CUSTOMER-COUNT

               MOVE SPACES TO OUTPUT-TEXT-LINE
               STRING
                   "CPR: "
                   DELIMITED BY SIZE
                   CPR OF CUSTOMERS(CUSTOMER-INDEX)
                   DELIMITED BY SPACE
                   ", Name: "
                   DELIMITED BY SIZE                   
                       FUNCTION TRIM(CUSTOMER-NAME 
                           OF CUSTOMERS(CUSTOMER-INDEX) TRAILING)
                   DELIMITED BY SIZE
                   ", Saldo: "
                   DELIMITED BY SIZE
                   SIGN-FORMATTED-DKK-SALDO-DISPLAY 
                       OF CUSTOMERS(CUSTOMER-INDEX)
                   DELIMITED BY SPACE
                   " DKK"
                   DELIMITED BY SIZE
                   INTO OUTPUT-TEXT-LINE
               END-STRING
               PERFORM ADD-OUTPUT-LINE-SAFE

           END-PERFORM

           MOVE SEPARATOR-LINE TO OUTPUT-TEXT-LINE
           PERFORM ADD-OUTPUT-LINE-SAFE
           DISPLAY "END ADD-TOP-3-CUSTOMERS-TO-OUTPUT"
           EXIT.

       FORMAT-SIGNED-AMOUNT.
           MOVE SPACES TO SIGNED-FORMAT-AMOUNT-DISPLAY
           MOVE FUNCTION ABS(SOURCE-AMOUNT) TO ABS-AMOUNT
           MOVE ABS-AMOUNT TO FORMAT-AMOUNT-DISPLAY

           DISPLAY "Formatting... '" SOURCE-AMOUNT "'."
       
           IF SOURCE-AMOUNT < 0
               STRING
                   "-"
                   DELIMITED BY SIZE
                   FUNCTION TRIM(FORMAT-AMOUNT-DISPLAY LEADING)
                   DELIMITED BY SIZE
                   INTO SIGNED-FORMAT-AMOUNT-DISPLAY
           ELSE IF SOURCE-AMOUNT = 0
               MOVE "0.00" TO SIGNED-FORMAT-AMOUNT-DISPLAY           
           ELSE
               MOVE FUNCTION TRIM(FORMAT-AMOUNT-DISPLAY LEADING)
                   TO SIGNED-FORMAT-AMOUNT-DISPLAY
           END-IF
       
           EXIT.

       COMPUTE-CUSTOMER-TOTALS.
           MOVE 0 TO DKK-TOTAL-INCOME OF CUSTOMERS(CUSTOMER-INDEX)
           MOVE 0 TO DKK-TOTAL-PAYMENTS OF CUSTOMERS(CUSTOMER-INDEX)
       
           PERFORM VARYING CUSTOMER-TRANSACTION-INDEX FROM 1 BY 1
               UNTIL CUSTOMER-TRANSACTION-INDEX >
                   CUSTOMER-TRANSACTIONS-COUNT 
                       OF CUSTOMERS(CUSTOMER-INDEX)
       
               MOVE TRANSACTION-INDICES OF CUSTOMERS
                  (CUSTOMER-INDEX, CUSTOMER-TRANSACTION-INDEX)
                   TO CURRENT-TRANSACTION-INDEX
       
               IF TRANSACTION-TYPE 
                   OF TRANSACTIONS(CURRENT-TRANSACTION-INDEX)
                       = "Indbetaling"
                   OR
                  (TRANSACTION-TYPE 
                   OF TRANSACTIONS(CURRENT-TRANSACTION-INDEX)
                       = "Overfoersel"
                   AND DKK-AMOUNT 
                       OF TRANSACTIONS(CURRENT-TRANSACTION-INDEX)
                       >= 0)
       
                   ADD DKK-AMOUNT 
                       OF TRANSACTIONS(CURRENT-TRANSACTION-INDEX)
                       TO DKK-TOTAL-INCOME OF CUSTOMERS(CUSTOMER-INDEX)
               END-IF
       
               IF TRANSACTION-TYPE 
                      OF TRANSACTIONS(CURRENT-TRANSACTION-INDEX)
                      = "Udbetaling"
               
                   SUBTRACT DKK-AMOUNT 
                       OF TRANSACTIONS(CURRENT-TRANSACTION-INDEX)
                   FROM DKK-TOTAL-PAYMENTS 
                       OF CUSTOMERS(CUSTOMER-INDEX)
               
               ELSE IF TRANSACTION-TYPE 
                           OF TRANSACTIONS(CURRENT-TRANSACTION-INDEX)
                           = "Overfoersel"
                       AND DKK-AMOUNT 
                           OF TRANSACTIONS(CURRENT-TRANSACTION-INDEX)
                           < 0
               
                   ADD DKK-AMOUNT 
                       OF TRANSACTIONS(CURRENT-TRANSACTION-INDEX)
                       TO DKK-TOTAL-PAYMENTS 
                           OF CUSTOMERS(CUSTOMER-INDEX)
               
               END-IF
           END-PERFORM
    
           COMPUTE DKK-SALDO OF CUSTOMERS(CUSTOMER-INDEX) =
               50000.00
               + DKK-TOTAL-INCOME OF CUSTOMERS(CUSTOMER-INDEX)
               + DKK-TOTAL-PAYMENTS OF CUSTOMERS(CUSTOMER-INDEX)
           
           MOVE DKK-TOTAL-INCOME OF CUSTOMERS(CUSTOMER-INDEX)
               TO DKK-TOTAL-INCOME-DISPLAY OF CUSTOMERS(CUSTOMER-INDEX)

           MOVE DKK-TOTAL-PAYMENTS OF CUSTOMERS(CUSTOMER-INDEX)
               TO DKK-TOTAL-PAYMENTS-DISPLAY 
                   OF CUSTOMERS(CUSTOMER-INDEX)

           MOVE DKK-SALDO OF CUSTOMERS(CUSTOMER-INDEX)
               TO DKK-SALDO-DISPLAY OF CUSTOMERS(CUSTOMER-INDEX)
           
           MOVE DKK-SALDO OF CUSTOMERS(CUSTOMER-INDEX)
               TO SOURCE-AMOUNT
           PERFORM FORMAT-SIGNED-AMOUNT
           Move SIGNED-FORMAT-AMOUNT-DISPLAY 
               TO SIGN-FORMATTED-DKK-SALDO-DISPLAY 
                   OF CUSTOMERS(CUSTOMER-INDEX)

           DISPLAY "Customer '" FUNCTION TRIM(CUSTOMER-NAME 
                       OF CUSTOMERS(CUSTOMER-INDEX) TRAILING)
                   "' has '" 
                   FUNCTION TRIM(SIGNED-FORMAT-AMOUNT-DISPLAY TRAILING) 
                   "' DKK."

           EXIT.
           

       FIND-MATCHING-BANK.
           MOVE "N" TO BANK-MATCH-FOUND
           MOVE 0 TO MATCHED-BANK-INDEX

           PERFORM VARYING BANK-INDEX FROM 1 BY 1
               UNTIL BANK-INDEX > BANKS-COUNT
                  OR BANK-MATCH-FOUND = "Y"

               IF REGISTRATION-NUMBER OF TRANSACTIONS(TRANSACTION-INDEX)
                  = REGISTRATION-NUMBER OF BANKS(BANK-INDEX)
                   MOVE "Y" TO BANK-MATCH-FOUND
                   MOVE BANK-INDEX TO MATCHED-BANK-INDEX
                   DISPLAY "Found Bank information for '" 
                       REGISTRATION-NUMBER 
                           OF TRANSACTIONS(TRANSACTION-INDEX)
                       "' on Index: " BANK-INDEX
               END-IF
           END-PERFORM
           EXIT.

       LOAD-TRANSACTIONS-DATA-TO-ARRAY.
           DISPLAY "START LOAD-TRANSACTIONS, LINE-INDEX = " LINE-INDEX
           OPEN INPUT IN-TRANSACTIONS-FILE
           PERFORM UNTIL END-OF-FILE = "Y"
               READ IN-TRANSACTIONS-FILE INTO IN-TRANSACTION-RECORD
               AT END
                   DISPLAY "Found End of Transactions "
                   "Information file on line: " LINE-INDEX
                   MOVE "Y" TO END-OF-FILE
               NOT AT END
                   IF TRANSACTIONS-COUNT > TRANSACTIONS-MAX-COUNT
                       DISPLAY "ERROR: LOAD-TRANSACTIONS-DATA-TO-ARRAY "
                       "attempted to write outside TRANSACTIONS array."
                       DISPLAY "File line: " LINE-INDEX
                       DISPLAY "TRANSACTIONS-MAX-COUNT: "
                           TRANSACTIONS-MAX-COUNT
                       MOVE "Y" TO END-OF-FILE
                       EXIT PARAGRAPH
                   END-IF                       
                   PERFORM MOVE-TRANSACTION-RECORD-TO-ARRAY
                   DISPLAY "Not At End Index: " LINE-INDEX
                   ADD 1 TO LINE-INDEX
                   ADD 1 TO TRANSACTIONS-COUNT
               END-READ
           END-PERFORM
           DISPLAY "END LOAD-TRANSACTIONS, LINE-INDEX = " LINE-INDEX
           CLOSE IN-TRANSACTIONS-FILE
           EXIT.
       
       MOVE-TRANSACTION-RECORD-TO-ARRAY.
           MOVE CPR OF IN-TRANSACTION-RECORD
               TO CPR OF TRANSACTIONS(LINE-INDEX)

           MOVE CUSTOMER-NAME OF IN-TRANSACTION-RECORD
               TO CUSTOMER-NAME OF TRANSACTIONS(LINE-INDEX)

           MOVE CUSTOMER-ADDRESS OF IN-TRANSACTION-RECORD
               TO CUSTOMER-ADDRESS OF TRANSACTIONS(LINE-INDEX)

           MOVE BIRTHDAY OF IN-TRANSACTION-RECORD
               TO BIRTHDAY OF TRANSACTIONS(LINE-INDEX)

           MOVE ACCOUNT-IDENTIFICATION OF IN-TRANSACTION-RECORD
               TO ACCOUNT-IDENTIFICATION OF TRANSACTIONS(LINE-INDEX)

           MOVE REGISTRATION-NUMBER OF IN-TRANSACTION-RECORD
               TO REGISTRATION-NUMBER OF TRANSACTIONS(LINE-INDEX)

           MOVE RAW-AMOUNT OF IN-TRANSACTION-RECORD
               TO RAW-AMOUNT OF TRANSACTIONS(LINE-INDEX)

           MOVE CURRENCY-CODE OF IN-TRANSACTION-RECORD
               TO CURRENCY-CODE OF TRANSACTIONS(LINE-INDEX)

           MOVE TRANSACTION-TYPE OF IN-TRANSACTION-RECORD
               TO TRANSACTION-TYPE OF TRANSACTIONS(LINE-INDEX)

           MOVE TRANSACTION-SHOP OF IN-TRANSACTION-RECORD
               TO TRANSACTION-SHOP OF TRANSACTIONS(LINE-INDEX)

           MOVE TIME-OF-TRANSACTION OF IN-TRANSACTION-RECORD
               TO TIME-OF-TRANSACTION OF TRANSACTIONS(LINE-INDEX)
           
           PERFORM COMPUTE-AMOUNT

           EXIT.

       COMPUTE-AMOUNT.
           COMPUTE AMOUNT OF TRANSACTIONS(LINE-INDEX) = 
               FUNCTION NUMVAL(RAW-AMOUNT OF IN-TRANSACTION-RECORD)
           
           IF TRANSACTION-TYPE OF IN-TRANSACTION-RECORD = "Indbetaling"
              OR TRANSACTION-TYPE 
               OF IN-TRANSACTION-RECORD = "Udbetaling"
              
              COMPUTE AMOUNT OF TRANSACTIONS(LINE-INDEX) =
                      FUNCTION ABS(AMOUNT OF TRANSACTIONS(LINE-INDEX))
           END-IF           
           
           MOVE AMOUNT OF TRANSACTIONS(LINE-INDEX)
               TO AMOUNT-DISPLAY OF TRANSACTIONS(LINE-INDEX)           

           IF CURRENCY-CODE OF IN-TRANSACTION-RECORD = "USD"
               COMPUTE DKK-AMOUNT OF TRANSACTIONS(LINE-INDEX) = 
                   AMOUNT OF TRANSACTIONS(LINE-INDEX) * 6.8
               MOVE DKK-AMOUNT OF TRANSACTIONS(LINE-INDEX)
                   TO DKK-AMOUNT-DISPLAY OF TRANSACTIONS(LINE-INDEX)
           ELSE IF CURRENCY-CODE OF IN-TRANSACTION-RECORD = "EUR"
               COMPUTE DKK-AMOUNT OF TRANSACTIONS(LINE-INDEX) = 
                   AMOUNT OF TRANSACTIONS(LINE-INDEX) * 7.5
               MOVE DKK-AMOUNT OF TRANSACTIONS(LINE-INDEX)
                   TO DKK-AMOUNT-DISPLAY OF TRANSACTIONS(LINE-INDEX)
           ELSE IF CURRENCY-CODE OF IN-TRANSACTION-RECORD = "DKK"
               MOVE AMOUNT OF TRANSACTIONS(LINE-INDEX)
                   TO DKK-AMOUNT OF TRANSACTIONS(LINE-INDEX)
               MOVE DKK-AMOUNT OF TRANSACTIONS(LINE-INDEX)
                   TO DKK-AMOUNT-DISPLAY OF TRANSACTIONS(LINE-INDEX)
           END-IF

           EXIT.


       LOAD-BANKS-DATA-TO-ARRAY.
           DISPLAY "START LOAD-BANKS, LINE-INDEX = " LINE-INDEX
           OPEN INPUT IN-BANKS-FILE
           PERFORM UNTIL END-OF-FILE = "Y"
               READ IN-BANKS-FILE INTO IN-BANK-RECORD
               AT END
                   DISPLAY "Found End of Banks "
                   "Information file on line: " LINE-INDEX
                   MOVE "Y" TO END-OF-FILE
               NOT AT END
                   IF BANKS-COUNT > BANKS-MAX-COUNT
                       DISPLAY "ERROR: LOAD-BANKS-DATA-TO-ARRAY "
                       "attempted to write outside BANKS array."
                       DISPLAY "File line: " LINE-INDEX
                       DISPLAY "BANKS-MAX-COUNT: " BANKS-MAX-COUNT
                       MOVE "Y" TO END-OF-FILE
                       EXIT PARAGRAPH
                   END-IF
                   PERFORM MOVE-BANK-RECORD-TO-ARRAY
                   DISPLAY "Not At End Index: " LINE-INDEX
                   ADD 1 TO LINE-INDEX
                   ADD 1 TO BANKS-COUNT
               END-READ
           END-PERFORM
           DISPLAY "END LOAD-BANKS, LINE-INDEX = " LINE-INDEX
           CLOSE IN-BANKS-FILE

           EXIT.

       MOVE-BANK-RECORD-TO-ARRAY.
           MOVE REGISTRATION-NUMBER OF IN-BANK-RECORD
               TO REGISTRATION-NUMBER OF BANKS(LINE-INDEX)

           MOVE BANK-NAME OF IN-BANK-RECORD
               TO BANK-NAME OF BANKS(LINE-INDEX)

           MOVE BANK-ADDRESS OF IN-BANK-RECORD
               TO BANK-ADDRESS OF BANKS(LINE-INDEX)

           MOVE TELEFON OF IN-BANK-RECORD
               TO TELEFON OF BANKS(LINE-INDEX)

           MOVE EMAIL OF IN-BANK-RECORD
               TO EMAIL OF BANKS(LINE-INDEX)

           EXIT.


       RESET-LOOP-VARIABLES.
           DISPLAY "RESETTING LOOP VARIABLES. LINE-INDEX BEFORE = "
               LINE-INDEX
           MOVE 1 TO LINE-INDEX
           MOVE "N" TO END-OF-FILE
           DISPLAY "RESET DONE. LINE-INDEX AFTER = " LINE-INDEX
           EXIT.
       