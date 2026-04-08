       IDENTIFICATION DIVISION.
       PROGRAM-ID. FILE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTOMER-FILE
               ASSIGN TO "Customerinformation.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD CUSTOMER-FILE.
       01 CUSTOMER-RECORD.
           COPY "Customer.cpy".

       WORKING-STORAGE SECTION.
       01 END-OF-FILE PIC X VALUE "N".
       01 BALANCE-DISPLAY PIC Z(6)9.99 VALUE ZEROES.
       01 FULLNAME PIC X(41) VALUE SPACES.


       PROCEDURE DIVISION.
           OPEN INPUT CUSTOMER-FILE

           PERFORM UNTIL END-OF-FILE = "Y"
               READ CUSTOMER-FILE
                   AT END
                       MOVE "Y" TO END-OF-FILE
                   NOT AT END
                       PERFORM BUILD-DERIVED-FIELDS
                       PERFORM DISPLAY-CUSTOMER
               END-READ
           END-PERFORM

           CLOSE CUSTOMER-FILE

           STOP RUN.

       DISPLAY-CUSTOMER.
           DISPLAY "------------------------------"
           DISPLAY "REFERENCE-ID   :" REFERENCE-ID
           DISPLAY "FULLNAME       :" FULLNAME
           DISPLAY "ACCOUNT-NUMBER :" ACCOUNT-NUMBER
           DISPLAY "CURRENCY-CODE  :" CURRENCY-CODE
           DISPLAY "BALANCE        :" FUNCTION 
               TRIM(BALANCE-DISPLAY LEADING)
           DISPLAY "STREET-NAME    :" STREET-NAME
           DISPLAY "HOUSE-NUMBER   :" HOUSE-NUMBER
           DISPLAY "FLOOR          :" FLOOR
           DISPLAY "SIDE           :" SIDE
           DISPLAY "CITY           :" CITY
           DISPLAY "ZIPCODE        :" ZIPCODE
           DISPLAY "COUNTRYCODE    :" COUNTRYCODE
           DISPLAY "PHONE-NUMBER   :" PHONE-NUMBER
           DISPLAY "EMAIL          :" EMAIL.

       BUILD-DERIVED-FIELDS.
           STRING
               FIRST-NAME DELIMITED BY SPACE
               " " DELIMITED BY SIZE
               LAST-NAME DELIMITED BY SPACE
               INTO FULLNAME
           END-STRING

           MOVE BALANCE TO BALANCE-DISPLAY.
