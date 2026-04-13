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
       01 OUT-REPORT-RECORD PIC X(100).

       WORKING-STORAGE SECTION.
       
       01 BANKS OCCURS 25 TIMES.
           COPY "BankRecord.cpy".

       01 TRANSACTIONS OCCURS 100 TIMES.
           COPY "TransactionRecord.cpy". 
           02 AMOUNT PIC S9(11)V99.
           02 AMOUNT-DISPLAY PIC -Z,ZZZ,ZZZ,ZZZ,ZZ9.99.
           02 DKK-AMOUNT PIC S9(11)V99.
           02 DKK-AMOUNT-DISPLAY PIC -Z,ZZZ,ZZZ,ZZZ,ZZ9.99.

      * 1 = 2020, 2 = 2021, 3 = 2022, etc...
       01 TRANSACTIONS-BY-YEAR-MONTH OCCURS 6 TIMES.
           02 MONTHS OCCURS 12 TIMES.
               03 MONTH-TRANSACTION-INDICES PIC 99 OCCURS 20 TIMES.
               03 TRANSACTIONS-MONTH-COUNT PIC 99.
               03 TOTAL-MONTH-INCOME PIC S9(11)V99.
               03 TOTAL-MONTH-PAYMENT PIC S9(11)V99.
       
       01 YEAR-INDEX        PIC 9.
       01 MONTH-INDEX       PIC 99.
       01 MONTH-SLOT        PIC 99.
       01 TEMP-YEAR         PIC 9(4).
       01 TEMP-MONTH        PIC 99.
       01 MONTH-NAME PIC X(12).

       01 CUSTOMERS OCCURS 25 TIMES.
           COPY "Customer.cpy".

       01 TEMP-CUSTOMER.
           COPY "Customer.cpy".
           
       01 I PIC 99.
       01 J PIC 99.
       01 J-START PIC 99.

       01 END-OF-FILE PIC X VALUE "N".
       01 LINE-INDEX PIC 99 VALUE 1.
       01 TRANSACTIONS-COUNT PIC 99 VALUE 0.
       01 BANKS-COUNT PIC 99 VALUE 0.
       01 CUSTOMER-COUNT PIC 99 VALUE 0.

       01 BANK-INDEX PIC 99 VALUE 1.
       01 CUSTOMER-INDEX PIC 99 VALUE 1. 
       01 CUSTOMER-TRANSACTION-INDEX PIC 9 VALUE 1.
       01 CURRENT-TRANSACTION-INDEX PIC 99 VALUE 0.

       01 TRANSACTION-INDEX PIC 99 VALUE 1.

       01 BANK-MATCH-FOUND PIC X VALUE "N".
       01 MATCHED-BANK-INDEX PIC 99 VALUE 0.

       01 CUSTOMER-FOUND PIC X VALUE "N".
       01 MATCHED-CUSTOMER-INDEX PIC 99 VALUE 0.
       01 SEARCH-CPR PIC X(15).

       01 NEXT-TRANSACTION-SLOT PIC 99 VALUE 0.

       01 OUTPUT-LINE-INDEX PIC 99 VALUE 0.
       01 OUTPUT-LINE-COUNT PIC 99 VALUE 0.
       01 OUTPUT-TEXT PIC X(100) OCCURS 120 TIMES.
       01 SEPARATOR-LINE PIC X(90) VALUE ALL "-".

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
       
       CLEAR-TRANSACTIONS-TO-MONTHS-STRUCTURE.
           PERFORM VARYING YEAR-INDEX FROM 1 BY 1
               UNTIL YEAR-INDEX > 6
       
               PERFORM VARYING MONTH-INDEX FROM 1 BY 1
                   UNTIL MONTH-INDEX > 12
       
                   MOVE 0 TO TRANSACTIONS-MONTH-COUNT
                       OF TRANSACTIONS-BY-YEAR-MONTH(YEAR-INDEX)
                       (MONTH-INDEX)
       
               END-PERFORM
           END-PERFORM

           EXIT.

       POSITION-TRANSACTION-IN-MONTH.
      *> Extract year and month
               MOVE FUNCTION NUMVAL(
                   TRANSACTION-DATETIME
                       OF TRANSACTIONS(TRANSACTION-INDEX)(1:4)
               ) TO TEMP-YEAR
       
               MOVE FUNCTION NUMVAL(
                   TRANSACTION-DATETIME
                       OF TRANSACTIONS(TRANSACTION-INDEX)(6:2)
               ) TO TEMP-MONTH
       
      *> Convert year to index (2020 → 1)
               COMPUTE YEAR-INDEX = TEMP-YEAR - 2019
       
      *> Month is already 1–12
               MOVE TEMP-MONTH TO MONTH-INDEX
       
      *> Safety check (optional but good)
               IF YEAR-INDEX >= 1 AND YEAR-INDEX <= 6
                  AND MONTH-INDEX >= 1 AND MONTH-INDEX <= 12
       
      *> Get next slot
                   ADD 1 TO TRANSACTIONS-MONTH-COUNT
                       OF TRANSACTIONS-BY-YEAR-MONTH(YEAR-INDEX)
                       (MONTH-INDEX)
       
                   MOVE TRANSACTIONS-MONTH-COUNT
                       OF TRANSACTIONS-BY-YEAR-MONTH(YEAR-INDEX)
                       (MONTH-INDEX)
                       TO MONTH-SLOT
       
      *> Store transaction index
                   MOVE TRANSACTION-INDEX
                       TO MONTH-TRANSACTION-INDICES
                          OF TRANSACTIONS-BY-YEAR-MONTH(YEAR-INDEX)
                          (MONTH-INDEX, MONTH-SLOT)
       
               END-IF
           
           EXIT.

       ADD-TRANSACTION-TO-MONTH-TOTALS.
           IF TRANSACTION-TYPE
               OF TRANSACTIONS(TRANSACTION-INDEX)
                   = "Indbetaling"

               ADD DKK-AMOUNT
                   OF TRANSACTIONS(TRANSACTION-INDEX)
                   TO TOTAL-MONTH-INCOME
                       OF TRANSACTIONS-BY-YEAR-MONTH(YEAR-INDEX)
                       (MONTH-INDEX)

           ELSE IF TRANSACTION-TYPE
                       OF TRANSACTIONS(TRANSACTION-INDEX)
                       = "Udbetaling"

               SUBTRACT DKK-AMOUNT
                   OF TRANSACTIONS(TRANSACTION-INDEX)
                   FROM TOTAL-MONTH-PAYMENT
                       OF TRANSACTIONS-BY-YEAR-MONTH(YEAR-INDEX)
                       (MONTH-INDEX)

           ELSE IF TRANSACTION-TYPE
                       OF TRANSACTIONS(TRANSACTION-INDEX)
                       = "Overfoersel"

               IF DKK-AMOUNT
                   OF TRANSACTIONS(TRANSACTION-INDEX)
                       >= 0

                   ADD DKK-AMOUNT
                       OF TRANSACTIONS(TRANSACTION-INDEX)
                       TO TOTAL-MONTH-INCOME
                           OF TRANSACTIONS-BY-YEAR-MONTH(YEAR-INDEX)
                           (MONTH-INDEX)

               ELSE

                   ADD DKK-AMOUNT
                       OF TRANSACTIONS(TRANSACTION-INDEX)
                       TO TOTAL-MONTH-PAYMENT
                           OF TRANSACTIONS-BY-YEAR-MONTH(YEAR-INDEX)
                           (MONTH-INDEX)

               END-IF
           END-IF

           EXIT.

       SORT-TRANSACTIONS-TO-MONTHS.
      *> Step 1: Clear structure
           PERFORM CLEAR-TRANSACTIONS-TO-MONTHS-STRUCTURE
       
      *> Step 2: Loop transactions
           PERFORM VARYING TRANSACTION-INDEX FROM 1 BY 1
               UNTIL TRANSACTION-INDEX > TRANSACTION-COUNT
       
           PERFORM POSITION-TRANSACTION-IN-MONTH
           PERFORM ADD-TRANSACTION-TO-MONTH-TOTALS
       
           END-PERFORM
       
           EXIT.

       FILL-CUSTOMER-ARRAY.
           PERFORM VARYING TRANSACTION-INDEX FROM 1 BY 1
               UNTIL TRANSACTION-INDEX > TRANSACTIONS-COUNT
           
      *    DISPLAY TRANSACTIONS(TRANSACTION-INDEX)
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

           ADD 1 TO CUSTOMER-TRANSACTIONS-COUNT
               OF CUSTOMERS(MATCHED-CUSTOMER-INDEX)
           DISPLAY "Added '1' to Customer Transactions Count. " 
               "It is now - " CUSTOMER-TRANSACTIONS-COUNT
                   OF CUSTOMERS(MATCHED-CUSTOMER-INDEX)

           MOVE CUSTOMER-TRANSACTIONS-COUNT
               OF CUSTOMERS(MATCHED-CUSTOMER-INDEX)
               TO NEXT-TRANSACTION-SLOT

           MOVE TRANSACTION-INDEX
               TO TRANSACTION-INDICES OF CUSTOMERS
                  (MATCHED-CUSTOMER-INDEX, NEXT-TRANSACTION-SLOT)

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
           PERFORM VARYING CUSTOMER-INDEX FROM 1 BY 1
               UNTIL CUSTOMER-INDEX > CUSTOMER-COUNT
                      
           PERFORM COMPUTE-CUSTOMER-TOTALS

           END-PERFORM

           PERFORM SORT-CUSTOMERS-BY-SALDO
           PERFORM ADD-TOP-3-CUSTOMERS-TO-OUTPUT
           PERFORM ADD-MONTHLY-STATISTICS-TO-OUTPUT

           EXIT.
       
       ADD-MONTHLY-STATISTICS-TO-OUTPUT.
           ADD 1 TO OUTPUT-LINE-COUNT
           MOVE SPACES TO OUTPUT-TEXT(OUTPUT-LINE-COUNT)

           ADD 1 TO OUTPUT-LINE-COUNT
           MOVE "Monthly statistics:"
               TO OUTPUT-TEXT(OUTPUT-LINE-COUNT)

           PERFORM VARYING YEAR-INDEX FROM 1 BY 1
               UNTIL YEAR-INDEX > 6

               COMPUTE TEMP-YEAR = 2019 + YEAR-INDEX

               ADD 1 TO OUTPUT-LINE-COUNT
               MOVE SEPARATOR-LINE TO OUTPUT-TEXT(OUTPUT-LINE-COUNT)

               ADD 1 TO OUTPUT-LINE-COUNT
               MOVE SPACES TO OUTPUT-TEXT(OUTPUT-LINE-COUNT)
               STRING
                   "Year: "
                   DELIMITED BY SIZE
                   TEMP-YEAR
                   DELIMITED BY SIZE
                   INTO OUTPUT-TEXT(OUTPUT-LINE-COUNT)
               END-STRING

               ADD 1 TO OUTPUT-LINE-COUNT
               MOVE SPACES TO OUTPUT-TEXT(OUTPUT-LINE-COUNT)
               MOVE "Month"
                   TO OUTPUT-TEXT(OUTPUT-LINE-COUNT)(1:12)
               MOVE "Income (DKK)"
                   TO OUTPUT-TEXT(OUTPUT-LINE-COUNT)(16:12)
               MOVE "Payments (DKK)"
                   TO OUTPUT-TEXT(OUTPUT-LINE-COUNT)(36:14)

               PERFORM VARYING MONTH-INDEX FROM 1 BY 1
                   UNTIL MONTH-INDEX > 12

                   PERFORM GET-MONTH-NAME

                   ADD 1 TO OUTPUT-LINE-COUNT
                   MOVE SPACES TO OUTPUT-TEXT(OUTPUT-LINE-COUNT)

                   MOVE MONTH-NAME
                       TO OUTPUT-TEXT(OUTPUT-LINE-COUNT)(1:12)

                   IF TRANSACTIONS-MONTH-COUNT
                       OF TRANSACTIONS-BY-YEAR-MONTH(YEAR-INDEX)
                       (MONTH-INDEX) = 0

                       MOVE "No transactions this month"
                           TO OUTPUT-TEXT(OUTPUT-LINE-COUNT)(16:26)

                   ELSE

                       MOVE TOTAL-MONTH-INCOME
                           OF TRANSACTIONS-BY-YEAR-MONTH(YEAR-INDEX)
                           (MONTH-INDEX)
                           TO FORMAT-AMOUNT-DISPLAY

                       MOVE FUNCTION TRIM(FORMAT-AMOUNT-DISPLAY LEADING)
                           TO OUTPUT-TEXT(OUTPUT-LINE-COUNT)(16:18)

                       MOVE TOTAL-MONTH-PAYMENT
                           OF TRANSACTIONS-BY-YEAR-MONTH(YEAR-INDEX)
                           (MONTH-INDEX)
                           TO SOURCE-AMOUNT

                       PERFORM FORMAT-SIGNED-AMOUNT

                       MOVE FUNCTION TRIM(
                           SIGNED-FORMAT-AMOUNT-DISPLAY LEADING
                           )
                           TO OUTPUT-TEXT(OUTPUT-LINE-COUNT)(36:18)

                   END-IF

               END-PERFORM

           END-PERFORM

           ADD 1 TO OUTPUT-LINE-COUNT
           MOVE SEPARATOR-LINE TO OUTPUT-TEXT(OUTPUT-LINE-COUNT)

           ADD 1 TO OUTPUT-LINE-COUNT
           MOVE SPACES TO OUTPUT-TEXT(OUTPUT-LINE-COUNT)

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
           PERFORM VARYING I FROM 1 BY 1
               UNTIL I >= CUSTOMER-COUNT
       
               ADD 1 TO I GIVING J-START
               PERFORM VARYING J FROM J-START BY 1
                   UNTIL J > CUSTOMER-COUNT
       
                   IF DKK-SALDO OF CUSTOMERS(I)
                      < DKK-SALDO OF CUSTOMERS(J)
       
                       MOVE CUSTOMERS(I) TO TEMP-CUSTOMER
                       MOVE CUSTOMERS(J) TO CUSTOMERS(I)
                       MOVE TEMP-CUSTOMER TO CUSTOMERS(J)
       
                   END-IF
       
               END-PERFORM
           END-PERFORM
       
           EXIT.
       
       ADD-TOP-3-CUSTOMERS-TO-OUTPUT.
           ADD 1 TO OUTPUT-LINE-COUNT           
           MOVE SPACES TO OUTPUT-TEXT(OUTPUT-LINE-COUNT)
           ADD 1 TO OUTPUT-LINE-COUNT           
           MOVE "Top 3 kunder baseret paa saldo:" 
               TO OUTPUT-TEXT(OUTPUT-LINE-COUNT)

           PERFORM VARYING CUSTOMER-INDEX FROM 1 BY 1
               UNTIL CUSTOMER-INDEX > 3
                   OR CUSTOMER-INDEX > CUSTOMER-COUNT

               ADD 1 TO OUTPUT-LINE-COUNT
               MOVE SPACES TO OUTPUT-TEXT(OUTPUT-LINE-COUNT)               
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
                   INTO OUTPUT-TEXT(OUTPUT-LINE-COUNT)
               END-STRING

           END-PERFORM

           ADD 1 TO OUTPUT-LINE-COUNT           
           MOVE SPACES TO OUTPUT-TEXT(OUTPUT-LINE-COUNT)
           ADD 1 TO OUTPUT-LINE-COUNT           
           MOVE SEPARATOR-LINE TO OUTPUT-TEXT(OUTPUT-LINE-COUNT)
           ADD 1 TO OUTPUT-LINE-COUNT
           MOVE SPACES TO OUTPUT-TEXT(OUTPUT-LINE-COUNT)

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
                       PERFORM MOVE-TRANSACTION-RECORD-TO-ARRAY
      *                DISPLAY IN-TRANSACTION-RECORD
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

           MOVE SHOP OF IN-TRANSACTION-RECORD
               TO SHOP OF TRANSACTIONS(LINE-INDEX)

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
                       PERFORM MOVE-BANK-RECORD-TO-ARRAY
      *                DISPLAY IN-BANK-RECORD
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
       