       IDENTIFICATION DIVISION.
       PROGRAM-ID. FILE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OUT-CUSTOMER-FILE
               ASSIGN TO "CustomerinformationOut.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT IN-BANK-FILE
               ASSIGN TO "BankInfo.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT IN-ADDRESS-FILE
               ASSIGN TO "AddressInfo.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT IN-CUSTOMER-FILE
               ASSIGN TO "CustomerinformationIn.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD IN-BANK-FILE.
       01 IN-BANK-RECORD OCCURS 5 TIMES.
           COPY "Identification.cpy".
           COPY "BankInfo.cpy".

       FD IN-ADDRESS-FILE.
       01 IN-ADDRESS-RECORD OCCURS 5 TIMES.
           COPY "Identification.cpy".
           COPY "AddressInfo.cpy".

       FD IN-CUSTOMER-FILE.
       01 IN-CUSTOMER-RECORD OCCURS 5 TIMES.
           COPY "Identification.cpy".
           02 FIRST-NAME PIC X(20).
           02 LAST-NAME PIC X(20).
           02 CONTACT-INFO.
           03 PHONE-NUMBER PIC X(15).
           03 EMAIL PIC X(80).

       FD OUT-CUSTOMER-FILE.
       01 OUT-CUSTOMER-RECORD.
           COPY "Identification.cpy".
           COPY "Customer.cpy".

       WORKING-STORAGE SECTION.
       01 END-OF-FILE PIC X VALUE "N".
       01 LINE-INDEX PIC 9 VALUE 1.
       01 BALANCE-DISPLAY PIC Z(6)9.99 VALUE ZEROES OCCURS 5 TIMES.
       01 FULLNAME PIC X(41) VALUE SPACES OCCURS 5 TIMES.

       01 CUSTOMER-INDEX PIC 9 VALUE 1.
       01 BANK-INDEX PIC 9 VALUE 1.
       01 ADDRESS-INDEX PIC 9 VALUE 1.

       01 BANK-MATCH-FOUND PIC X VALUE "N".
       01 ADDRESS-MATCH-FOUND PIC X VALUE "N".


       PROCEDURE DIVISION.          
           PERFORM LOAD-CUSTOMER-DATA-TO-ARRAY
           PERFORM RESET-LOOP-VARIABLES           
           PERFORM LOAD-BANK-DATA-TO-ARRAY
           PERFORM RESET-LOOP-VARIABLES
           PERFORM LOAD-ADDRESS-DATA-TO-ARRAY

           OPEN OUTPUT OUT-CUSTOMER-FILE           
           PERFORM BUILD-OUTPUT-FILE
           CLOSE OUT-CUSTOMER-FILE

       STOP RUN.
       
       BUILD-OUTPUT-FILE.
           PERFORM VARYING CUSTOMER-INDEX FROM 1 BY 1
               UNTIL CUSTOMER-INDEX > 5
               
               PERFORM FIND-MATCHING-BANK
               PERFORM FIND-MATCHING-ADDRESS

               IF BANK-MATCH-FOUND = "Y"
                  AND ADDRESS-MATCH-FOUND = "Y"
                   PERFORM MOVE-INPUT-TO-OUTPUT
                   PERFORM DISPLAY-OUTPUT-CUSTOMER
                   WRITE OUT-CUSTOMER-RECORD
               ELSE
                   DISPLAY "NO MATCH FOUND FOR REFERENCE-ID: "
                       REFERENCE-ID 
                       OF IN-CUSTOMER-RECORD(CUSTOMER-INDEX)
               END-IF
           END-PERFORM
           EXIT.

       DISPLAY-OUTPUT-CUSTOMER.
           DISPLAY "------------------------------"

           DISPLAY "REFERENCE-ID   :"
               REFERENCE-ID OF OUT-CUSTOMER-RECORD

           DISPLAY "FULLNAME       :"
               FULLNAME(CUSTOMER-INDEX)

           DISPLAY "ACCOUNT-NUMBER :"
               ACCOUNT-NUMBER OF OUT-CUSTOMER-RECORD

           DISPLAY "CURRENCY-CODE  :"
               CURRENCY-CODE OF OUT-CUSTOMER-RECORD

           DISPLAY "BALANCE        :"
               FUNCTION TRIM(
                   BALANCE-DISPLAY(BANK-INDEX)
                   LEADING)

           DISPLAY "STREET-NAME    :"
               STREET-NAME OF OUT-CUSTOMER-RECORD

           DISPLAY "HOUSE-NUMBER   :"
               HOUSE-NUMBER OF OUT-CUSTOMER-RECORD

           DISPLAY "FLOOR          :"
               FLOOR OF OUT-CUSTOMER-RECORD

           DISPLAY "SIDE           :"
               SIDE OF OUT-CUSTOMER-RECORD

           DISPLAY "CITY           :"
               CITY OF OUT-CUSTOMER-RECORD

           DISPLAY "ZIPCODE        :"
               ZIPCODE OF OUT-CUSTOMER-RECORD

           DISPLAY "COUNTRYCODE    :"
               COUNTRYCODE OF OUT-CUSTOMER-RECORD

           DISPLAY "PHONE-NUMBER   :"
               PHONE-NUMBER OF OUT-CUSTOMER-RECORD

           DISPLAY "EMAIL          :"
               EMAIL OF OUT-CUSTOMER-RECORD

           EXIT.

       FIND-MATCHING-BANK.
           MOVE "N" TO BANK-MATCH-FOUND

           PERFORM VARYING BANK-INDEX FROM 1 BY 1
               UNTIL BANK-INDEX > 5
                  OR BANK-MATCH-FOUND = "Y"

               IF REFERENCE-ID OF IN-CUSTOMER-RECORD(CUSTOMER-INDEX)
                  = REFERENCE-ID OF IN-BANK-RECORD(BANK-INDEX)
                   MOVE "Y" TO BANK-MATCH-FOUND
               END-IF
           END-PERFORM
           EXIT.

       FIND-MATCHING-ADDRESS.
           MOVE "N" TO ADDRESS-MATCH-FOUND

           PERFORM VARYING ADDRESS-INDEX FROM 1 BY 1
               UNTIL ADDRESS-INDEX > 5
                  OR ADDRESS-MATCH-FOUND = "Y"

               IF REFERENCE-ID OF IN-CUSTOMER-RECORD(CUSTOMER-INDEX)
                  = REFERENCE-ID OF IN-ADDRESS-RECORD(ADDRESS-INDEX)
                   MOVE "Y" TO ADDRESS-MATCH-FOUND
               END-IF
           END-PERFORM
           EXIT.

       MOVE-INPUT-TO-OUTPUT.
           MOVE SPACES TO OUT-CUSTOMER-RECORD

           MOVE REFERENCE-ID OF IN-CUSTOMER-RECORD(CUSTOMER-INDEX)
               TO REFERENCE-ID OF OUT-CUSTOMER-RECORD

           MOVE FIRST-NAME OF IN-CUSTOMER-RECORD(CUSTOMER-INDEX)
               TO FIRST-NAME OF OUT-CUSTOMER-RECORD

           MOVE LAST-NAME OF IN-CUSTOMER-RECORD(CUSTOMER-INDEX)
               TO LAST-NAME OF OUT-CUSTOMER-RECORD

           MOVE ACCOUNT-NUMBER OF IN-BANK-RECORD(BANK-INDEX)
               TO ACCOUNT-NUMBER OF OUT-CUSTOMER-RECORD

           MOVE CURRENCY-CODE OF IN-BANK-RECORD(BANK-INDEX)
               TO CURRENCY-CODE OF OUT-CUSTOMER-RECORD

           MOVE BALANCE OF IN-BANK-RECORD(BANK-INDEX)
               TO BALANCE OF OUT-CUSTOMER-RECORD

           MOVE STREET-NAME OF IN-ADDRESS-RECORD(ADDRESS-INDEX)
               TO STREET-NAME OF OUT-CUSTOMER-RECORD

           MOVE HOUSE-NUMBER OF IN-ADDRESS-RECORD(ADDRESS-INDEX)
               TO HOUSE-NUMBER OF OUT-CUSTOMER-RECORD

           MOVE FLOOR OF IN-ADDRESS-RECORD(ADDRESS-INDEX)
               TO FLOOR OF OUT-CUSTOMER-RECORD

           MOVE SIDE OF IN-ADDRESS-RECORD(ADDRESS-INDEX)
               TO SIDE OF OUT-CUSTOMER-RECORD

           MOVE CITY OF IN-ADDRESS-RECORD(ADDRESS-INDEX)
               TO CITY OF OUT-CUSTOMER-RECORD

           MOVE ZIPCODE OF IN-ADDRESS-RECORD(ADDRESS-INDEX)
               TO ZIPCODE OF OUT-CUSTOMER-RECORD

           MOVE COUNTRYCODE OF IN-ADDRESS-RECORD(ADDRESS-INDEX)
               TO COUNTRYCODE OF OUT-CUSTOMER-RECORD

           MOVE PHONE-NUMBER OF IN-CUSTOMER-RECORD(CUSTOMER-INDEX)
               TO PHONE-NUMBER OF OUT-CUSTOMER-RECORD

           MOVE EMAIL OF IN-CUSTOMER-RECORD(CUSTOMER-INDEX)
               TO EMAIL OF OUT-CUSTOMER-RECORD

           EXIT.

       LOAD-ADDRESS-DATA-TO-ARRAY.
           DISPLAY "START ADDRESS, LINE-INDEX = " LINE-INDEX           
           DISPLAY "Opening Address Information File"
           OPEN INPUT IN-ADDRESS-FILE
           PERFORM UNTIL END-OF-FILE = "Y"
               READ IN-ADDRESS-FILE INTO IN-ADDRESS-RECORD(LINE-INDEX)
                   AT END
                       DISPLAY "Found End of Address "
                       "Information file on line: " LINE-INDEX
                       MOVE "Y" TO END-OF-FILE
                   NOT AT END
                       ADD 1 TO LINE-INDEX
               END-READ
           END-PERFORM
           DISPLAY "END ADDRESS, LINE-INDEX = " LINE-INDEX           
           DISPLAY "Closing Address Information File"
           CLOSE IN-ADDRESS-FILE
           EXIT.

       LOAD-BANK-DATA-TO-ARRAY.
           DISPLAY "START LOAD-BANK, LINE-INDEX = " LINE-INDEX           
           DISPLAY "Opening Bank Information File"
           OPEN INPUT IN-BANK-FILE
           PERFORM UNTIL END-OF-FILE = "Y"
               READ IN-BANK-FILE INTO IN-BANK-RECORD(LINE-INDEX)
                   AT END
                       DISPLAY "Found End of Bank "
                       "Information file on line: " LINE-INDEX
                       MOVE "Y" TO END-OF-FILE
                   NOT AT END
                       PERFORM BUILD-DERIVED-BALANCE
                       ADD 1 TO LINE-INDEX
               END-READ
           END-PERFORM
           DISPLAY "END LOAD-BANK, LINE-INDEX = " LINE-INDEX
           DISPLAY "Closing Bank Information File"
           CLOSE IN-BANK-FILE
           EXIT.

       LOAD-CUSTOMER-DATA-TO-ARRAY.
           DISPLAY "START LOAD-CUSTOMER, LINE-INDEX = " LINE-INDEX
           DISPLAY "Opening Customer Information File"
           OPEN INPUT IN-CUSTOMER-FILE
           PERFORM UNTIL END-OF-FILE = "Y"
               READ IN-CUSTOMER-FILE INTO IN-CUSTOMER-RECORD(LINE-INDEX)
                   AT END
                       DISPLAY "Found End of Customer "
                       "Information file on line: " LINE-INDEX
                       MOVE "Y" TO END-OF-FILE
                   NOT AT END
                       PERFORM BUILD-DERIVED-FULLNAME
                       ADD 1 TO LINE-INDEX
               END-READ
           END-PERFORM
           DISPLAY "END LOAD-CUSTOMER, LINE-INDEX = " LINE-INDEX
           DISPLAY "Closing Customer Information File"
           CLOSE IN-CUSTOMER-FILE
           EXIT.

       RESET-LOOP-VARIABLES.
           DISPLAY "RESETTING LOOP VARIABLES. LINE-INDEX BEFORE = "
               LINE-INDEX
           MOVE 1 TO LINE-INDEX
           MOVE "N" TO END-OF-FILE
           DISPLAY "RESET DONE. LINE-INDEX AFTER = " LINE-INDEX
           EXIT.
       
       BUILD-DERIVED-BALANCE.
           MOVE BALANCE OF IN-BANK-RECORD(LINE-INDEX)
               TO BALANCE-DISPLAY(LINE-INDEX)
           EXIT.

       BUILD-DERIVED-FULLNAME.
           STRING
               FIRST-NAME OF IN-CUSTOMER-RECORD(LINE-INDEX) 
                   DELIMITED BY SPACE
               " " DELIMITED BY SIZE
               LAST-NAME OF IN-CUSTOMER-RECORD(LINE-INDEX)
                   DELIMITED BY SPACE
               INTO FULLNAME(LINE-INDEX)
           END-STRING

           EXIT.
           