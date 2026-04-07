       IDENTIFICATION DIVISION. 
       PROGRAM-ID. Bank.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 CUSTOMER-INFO.
       COPY "Customer.cpy".

       01 INDEX-ONE PIC 9(2) VALUE ZEROES.
       01 INDEX-TWO PIC 9(2) VALUE ZEROES.
       01 CURRENT-CHAR PIC X VALUE SPACES.
       01 PREVIOUS-CHAR PIC X VALUE SPACES.

       PROCEDURE DIVISION.       

      * Base Information
       MOVE "9876543210" TO REFERENCE-ID.
       MOVE "Hans" TO FIRST-NAME.
       MOVE "Hansen" TO LAST-NAME.
       MOVE "DK98765432112345" TO ACCOUNT-NUMBER.
       MOVE "DKK" TO CURRENCY-CODE.
       MOVE 420.69 TO BALANCE.
       MOVE BALANCE TO BALANCE-DISPLAY.

      * Address info
       MOVE "Main Street" TO STREET-NAME
       MOVE "42" TO HOUSE-NUMBER
       MOVE "2" TO FLOOR
       MOVE "TV" TO SIDE
       MOVE "Odense" TO CITY
       MOVE "5000" TO ZIPCODE
       MOVE "DK" TO COUNTRYCODE

      * Contact info
       MOVE "+4512345678" TO PHONE-NUMBER
       MOVE "hans.hansen@example.com" TO EMAIL

       STRING FIRST-NAME DELIMITED BY SIZE " " 
           DELIMITED BY SIZE LAST-NAME 
           DELIMITED BY SIZE 
           INTO FULLNAME.

       PERFORM VARYING INDEX-ONE FROM 1 BY 1 
           UNTIL INDEX-ONE > LENGTH OF FULLNAME
       
       MOVE CURRENT-CHAR TO PREVIOUS-CHAR
       MOVE FULLNAME(INDEX-ONE:1) TO CURRENT-CHAR
       
       IF CURRENT-CHAR NOT = SPACE OR PREVIOUS-CHAR NOT = SPACE

           ADD 1 TO INDEX-TWO
           MOVE CURRENT-CHAR TO CLEANED-FULLNAME(INDEX-TWO:1)

       END-IF
       END-PERFORM

       DISPLAY "--------------------------------------".
       DISPLAY "Customer ID    : " REFERENCE-ID.
       DISPLAY "Full Name      : " CLEANED-FULLNAME.
       
       DISPLAY "Account Number : " ACCOUNT-NUMBER.
       DISPLAY "Balance        : " FUNCTION 
           TRIM(BALANCE-DISPLAY LEADING) " " CURRENCY-CODE.
       
       DISPLAY " ".
       
       DISPLAY "Address        : "
        FUNCTION TRIM(STREET-NAME TRAILING)
        " "
        FUNCTION TRIM(HOUSE-NUMBER TRAILING).

       DISPLAY "                 Floor "
        FUNCTION TRIM(FLOOR TRAILING)
        " "
        FUNCTION TRIM(SIDE TRAILING).

       DISPLAY "                 "
        FUNCTION TRIM(ZIPCODE TRAILING)
        " "
        FUNCTION TRIM(CITY TRAILING).
       DISPLAY "                 " COUNTRYCODE.
       
       DISPLAY " ".
       
       DISPLAY "Phone Number   : " PHONE-NUMBER.
       DISPLAY "Email          : " EMAIL.
       
       DISPLAY "--------------------------------------".
       STOP RUN.
