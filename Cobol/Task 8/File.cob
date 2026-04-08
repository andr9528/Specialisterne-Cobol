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
           Copy "Identification.cpy"
           COPY "BankInfo.cpy".

       FD IN-ADDRESS-FILE.
       01 IN-ADDRESS-RECORD.
           Copy "Identification.cpy"
           COPY "AddressInfo.cpy".

       FD IN-CUSTOMER-FILE.
       01 IN-CUSTOMER-RECORD.
           Copy "Identification.cpy"
           02 FIRST-NAME PIC X(20) VALUE SPACES.
           02 LAST-NAME PIC X(20) VALUE SPACES.
           02 CONTACT-INFO.
           03 PHONE-NUMBER PIC X(15) VALUE SPACES.
           03 EMAIL PIC X(80) VALUE SPACES.

       FD OUT-CUSTOMER-FILE.
       01 OUT-CUSTOMER-RECORD.
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
    
               END-READ
           END-PERFORM

           CLOSE IN-BANK-FILE
           CLOSE IN-ADDRESS-FILE
           CLOSE IN-CUSTOMER-FILE
           CLOSE OUT-CUSTOMER-FILE

       STOP RUN.
