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
       01 IN-BANK-RECORD.
           Copy "Identification.cpy".
           COPY "BankInfo.cpy".

       FD IN-ADDRESS-FILE.
       01 IN-ADDRESS-RECORD.
           Copy "Identification.cpy".
           COPY "AddressInfo.cpy".

       FD IN-CUSTOMER-FILE.
       01 IN-CUSTOMER-RECORD.
           Copy "Identification.cpy".
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
       01 BALANCE-DISPLAY PIC Z(6)9.99 VALUE ZEROES.
       01 FULLNAME PIC X(41) VALUE SPACES.


       PROCEDURE DIVISION.
           OPEN INPUT IN-BANK-FILE
           OPEN INPUT IN-ADDRESS-FILE
           OPEN INPUT IN-CUSTOMER-FILE
           OPEN OUTPUT OUT-CUSTOMER-FILE

           PERFORM UNTIL END-OF-FILE = "Y"
               READ IN-CUSTOMER-FILE INTO IN-CUSTOMER-RECORD
                   AT END
                       MOVE "Y" TO END-OF-FILE
                   NOT AT END
                       READ IN-BANK-FILE INTO IN-BANK-RECORD
                          AT END
                              MOVE "Y" TO END-OF-FILE
                       END-READ
                  
                       READ IN-ADDRESS-FILE INTO IN-ADDRESS-RECORD
                          AT END
                              MOVE "Y" TO END-OF-FILE
                       END-READ

                   IF REFERENCE-ID OF IN-CUSTOMER-RECORD =
                       REFERENCE-ID OF IN-BANK-RECORD
                       AND REFERENCE-ID OF IN-CUSTOMER-RECORD =
                       REFERENCE-ID OF IN-ADDRESS-RECORD
                       PERFORM MOVE-INPUT-TO-OUTPUT
                       PERFORM BUILD-DERIVED-FIELDS
                       WRITE OUT-CUSTOMER-RECORD
                       PERFORM DISPLAY-CUSTOMER
                   ELSE
                      DISPLAY "REFERENCE-ID mismatch found"
                   END-IF
              END-READ
           END-PERFORM

           CLOSE IN-BANK-FILE
           CLOSE IN-ADDRESS-FILE
           CLOSE IN-CUSTOMER-FILE
           CLOSE OUT-CUSTOMER-FILE

       STOP RUN.
       
       DISPLAY-CUSTOMER.
           DISPLAY "------------------------------"
           DISPLAY "REFERENCE-ID   :" REFERENCE-ID 
               OF OUT-CUSTOMER-RECORD
           DISPLAY "FULLNAME       :" FULLNAME
           DISPLAY "ACCOUNT-NUMBER :" ACCOUNT-NUMBER 
               OF OUT-CUSTOMER-RECORD
           DISPLAY "CURRENCY-CODE  :" CURRENCY-CODE 
               OF OUT-CUSTOMER-RECORD
           DISPLAY "BALANCE        :" FUNCTION
               TRIM(BALANCE-DISPLAY LEADING)
           DISPLAY "STREET-NAME    :" STREET-NAME OF OUT-CUSTOMER-RECORD
           DISPLAY "HOUSE-NUMBER   :" HOUSE-NUMBER 
               OF OUT-CUSTOMER-RECORD
           DISPLAY "FLOOR          :" FLOOR OF OUT-CUSTOMER-RECORD
           DISPLAY "SIDE           :" SIDE OF OUT-CUSTOMER-RECORD
           DISPLAY "CITY           :" CITY OF OUT-CUSTOMER-RECORD
           DISPLAY "ZIPCODE        :" ZIPCODE OF OUT-CUSTOMER-RECORD
           DISPLAY "COUNTRYCODE    :" COUNTRYCODE OF OUT-CUSTOMER-RECORD
           DISPLAY "PHONE-NUMBER   :" PHONE-NUMBER 
               OF OUT-CUSTOMER-RECORD
           DISPLAY "EMAIL          :" EMAIL OF OUT-CUSTOMER-RECORD

           EXIT.

       BUILD-DERIVED-FIELDS.
           MOVE SPACES TO FULLNAME

           STRING
               FIRST-NAME OF OUT-CUSTOMER-RECORD DELIMITED BY SPACE
               " " DELIMITED BY SIZE
               LAST-NAME OF OUT-CUSTOMER-RECORD DELIMITED BY SPACE
               INTO FULLNAME
           END-STRING

           MOVE BALANCE OF OUT-CUSTOMER-RECORD
               TO BALANCE-DISPLAY

           EXIT.

       MOVE-INPUT-TO-OUTPUT.
           MOVE SPACES TO OUT-CUSTOMER-RECORD

           MOVE REFERENCE-ID OF IN-CUSTOMER-RECORD
               TO REFERENCE-ID OF OUT-CUSTOMER-RECORD

           MOVE FIRST-NAME OF IN-CUSTOMER-RECORD
               TO FIRST-NAME OF OUT-CUSTOMER-RECORD

           MOVE LAST-NAME OF IN-CUSTOMER-RECORD
               TO LAST-NAME OF OUT-CUSTOMER-RECORD

           MOVE PHONE-NUMBER OF IN-CUSTOMER-RECORD
               TO PHONE-NUMBER OF OUT-CUSTOMER-RECORD

           MOVE EMAIL OF IN-CUSTOMER-RECORD
               TO EMAIL OF OUT-CUSTOMER-RECORD

           MOVE ACCOUNT-NUMBER OF IN-BANK-RECORD
               TO ACCOUNT-NUMBER OF OUT-CUSTOMER-RECORD

           MOVE CURRENCY-CODE OF IN-BANK-RECORD
               TO CURRENCY-CODE OF OUT-CUSTOMER-RECORD

           MOVE BALANCE OF IN-BANK-RECORD
               TO BALANCE OF OUT-CUSTOMER-RECORD

           MOVE STREET-NAME OF IN-ADDRESS-RECORD
               TO STREET-NAME OF OUT-CUSTOMER-RECORD

           MOVE HOUSE-NUMBER OF IN-ADDRESS-RECORD
               TO HOUSE-NUMBER OF OUT-CUSTOMER-RECORD

           MOVE FLOOR OF IN-ADDRESS-RECORD
               TO FLOOR OF OUT-CUSTOMER-RECORD

           MOVE SIDE OF IN-ADDRESS-RECORD
               TO SIDE OF OUT-CUSTOMER-RECORD

           MOVE CITY OF IN-ADDRESS-RECORD
               TO CITY OF OUT-CUSTOMER-RECORD

           MOVE ZIPCODE OF IN-ADDRESS-RECORD
               TO ZIPCODE OF OUT-CUSTOMER-RECORD

           MOVE COUNTRYCODE OF IN-ADDRESS-RECORD
               TO COUNTRYCODE OF OUT-CUSTOMER-RECORD

           EXIT.
