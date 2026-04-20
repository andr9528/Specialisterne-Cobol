       IDENTIFICATION DIVISION.
       PROGRAM-ID. FILE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL. 

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       01 CUSTOMERS-WRAPPER.
           COPY "CustomersWrapper.cpy".

       01 SANCTIONS-WRAPPER.
           COPY "SanctionsWrapper.cpy".

       01 OUTPUT-WRAPPER.
           COPY "OutputWrapper.cpy".

       01 OUTPUT-HELPER.
           02 OUTPUT-TEXT-LINE PIC X(120).

       01 REPORT-WORK.
           02 MATCH-INDEX PIC 999 VALUE 0.
           02 CURRENT-SANCTION-INDEX PIC 999 VALUE 0.
           02 DISPLAY-TOTAL-MATCH-PERCENT PIC ZZ9.9.
           02 DISPLAY-NAME-MATCH-PERCENT PIC ZZ9.9.
           02 DISPLAY-BIRTHDAY-MATCH-PERCENT PIC ZZ9.9.
           02 DISPLAY-COUNTRY-MATCH-PERCENT PIC ZZ9.9.
           02 DISPLAY-ALIAS-MATCH-PERCENT PIC ZZ9.9.
           02 REPORT-SEPARATOR PIC X(90) VALUE ALL "-".       

       01 SORT-WRAPPER.
           02 SORT-INDEX PIC 99.
           02 COMPARE-INDEX PIC 99.
           02 COMPARE-INDEX-START PIC 99.
       
       01 MISQ-WRAPPER.
           02 COUNT-DISPLAY PIC ZZ9.       
           02 CUSTOMER-INDEX PIC 999 VALUE 1.
           02 SANCTION-INDEX PIC 999 VALUE 1.
       
       01 FORMATTER-WRAPPER.
           COPY "FormatterWrapper.cpy".

       PROCEDURE DIVISION.          
           CALL "INPUTLOADER"
               USING CUSTOMERS-WRAPPER
                   SANCTIONS-WRAPPER.
           
           CALL "MATCHER"
               USING CUSTOMERS-WRAPPER
                   SANCTIONS-WRAPPER.

           PERFORM BUILD-REPORT

           CALL "OUTPUTWRITTER" USING OUTPUT-WRAPPER.
       STOP RUN. 
       
       

       BUILD-REPORT.
           DISPLAY "START BUILD-REPORT"
           
           PERFORM BUILD-SANCTIONS-REPORT

           DISPLAY "END BUILD-REPORT"
           EXIT.

       BUILD-SANCTIONS-REPORT.
           PERFORM VARYING CUSTOMER-INDEX FROM 1 BY 1
               UNTIL CUSTOMER-INDEX > CUSTOMERS-COUNT
               IF MATCHED-SANCTIONS-COUNT 
                   OF CUSTOMERS(CUSTOMER-INDEX) > 0
                   PERFORM ADD-CUSTOMER-SANCTIONS-TO-OUTPUT
               END-IF
           END-PERFORM

           EXIT.

       ADD-CUSTOMER-SANCTIONS-TO-OUTPUT.
           STRING
               "Customer ID: "
               DELIMITED BY SIZE
               CUSTOMER-ID OF CUSTOMERS(CUSTOMER-INDEX)
               DELIMITED BY SPACE
               INTO OUTPUT-TEXT-LINE
           END-STRING
           PERFORM ADD-OUTPUT-LINE-SAFE

           STRING
               "Customer Name: "
               DELIMITED BY SIZE
               CUSTOMER-NAME OF CUSTOMERS(CUSTOMER-INDEX)
               DELIMITED BY SIZE
               INTO OUTPUT-TEXT-LINE
           END-STRING
           PERFORM ADD-OUTPUT-LINE-SAFE

           STRING
               "Birthday: "
               DELIMITED BY SIZE
               SANCTION-FORMATTED-BIRTHDAY OF CUSTOMERS(CUSTOMER-INDEX)
               DELIMITED BY SPACE
               INTO OUTPUT-TEXT-LINE
           END-STRING
           PERFORM ADD-OUTPUT-LINE-SAFE

           STRING
               "Address: "
               DELIMITED BY SIZE
               FUNCTION TRIM(CUSTOMER-ADDRESS 
                   OF CUSTOMERS(CUSTOMER-INDEX) TRAILING)
               DELIMITED BY SIZE
               ", "
               DELIMITED BY SIZE
               CUSTOMER-COUNTRY OF CUSTOMERS(CUSTOMER-INDEX)
               DELIMITED BY SPACE
               INTO OUTPUT-TEXT-LINE
           END-STRING
           PERFORM ADD-OUTPUT-LINE-SAFE

           MOVE SPACES TO OUTPUT-TEXT-LINE
           PERFORM ADD-OUTPUT-LINE-SAFE

           PERFORM VARYING MATCH-INDEX FROM 1 BY 1
               UNTIL MATCH-INDEX >
                   MATCHED-SANCTIONS-COUNT OF CUSTOMERS(CUSTOMER-INDEX)
               PERFORM ADD-ONE-SANCTION-MATCH-TO-OUTPUT
           END-PERFORM

           MOVE REPORT-SEPARATOR TO OUTPUT-TEXT-LINE
           PERFORM ADD-OUTPUT-LINE-SAFE

           MOVE SPACES TO OUTPUT-TEXT-LINE
           PERFORM ADD-OUTPUT-LINE-SAFE

           EXIT.

       ADD-ONE-SANCTION-MATCH-TO-OUTPUT.
           MOVE MATCHED-SANCTION-INDEX(CUSTOMER-INDEX, MATCH-INDEX)
               TO CURRENT-SANCTION-INDEX

           MOVE TOTAL-MATCH-PERCENT(CUSTOMER-INDEX, MATCH-INDEX)
               TO DISPLAY-TOTAL-MATCH-PERCENT

           MOVE NAME-MATCH-PERCENT(CUSTOMER-INDEX, MATCH-INDEX)
               TO DISPLAY-NAME-MATCH-PERCENT

           MOVE ALIAS-MATCH-PERCENT(CUSTOMER-INDEX, MATCH-INDEX)
               TO DISPLAY-ALIAS-MATCH-PERCENT

            MOVE BIRTHDAY-MATCH-PERCENT(CUSTOMER-INDEX, MATCH-INDEX)
               TO DISPLAY-BIRTHDAY-MATCH-PERCENT

           MOVE COUNTRY-MATCH-PERCENT(CUSTOMER-INDEX, MATCH-INDEX)
               TO DISPLAY-COUNTRY-MATCH-PERCENT

           MOVE "Match found with:" TO OUTPUT-TEXT-LINE
           PERFORM ADD-OUTPUT-LINE-SAFE

           STRING
               "Sanction-ID: "
               DELIMITED BY SIZE
               SANCTION-ID OF SANCTIONS(CURRENT-SANCTION-INDEX)
               DELIMITED BY SPACE
               INTO OUTPUT-TEXT-LINE
           END-STRING
           PERFORM ADD-OUTPUT-LINE-SAFE

           STRING
               "Name: "
               DELIMITED BY SIZE
               SANCTION-NAME OF SANCTIONS(CURRENT-SANCTION-INDEX)
               DELIMITED BY SIZE
               INTO OUTPUT-TEXT-LINE
           END-STRING
           PERFORM ADD-OUTPUT-LINE-SAFE

           STRING
               "Birthday: "
               DELIMITED BY SIZE
               SANCTION-BIRTHDATE OF SANCTIONS(CURRENT-SANCTION-INDEX)
               DELIMITED BY SPACE
               INTO OUTPUT-TEXT-LINE
           END-STRING
           PERFORM ADD-OUTPUT-LINE-SAFE

           STRING
               "Country: "
               DELIMITED BY SIZE
               SANCTION-COUNTRY OF SANCTIONS(CURRENT-SANCTION-INDEX)
               DELIMITED BY SPACE
               INTO OUTPUT-TEXT-LINE
           END-STRING
           PERFORM ADD-OUTPUT-LINE-SAFE

           MOVE SPACES TO OUTPUT-TEXT-LINE
           PERFORM ADD-OUTPUT-LINE-SAFE

           MOVE "Match-beskrivelse:" TO OUTPUT-TEXT-LINE
           PERFORM ADD-OUTPUT-LINE-SAFE

           IF NAME-MATCH-PERCENT(CUSTOMER-INDEX, MATCH-INDEX) > 0
               STRING
                   "- Match by Name: "
                   DELIMITED BY SIZE
                   DISPLAY-NAME-MATCH-PERCENT
                   DELIMITED BY SIZE
                   "%."
                   DELIMITED BY SIZE
                   INTO OUTPUT-TEXT-LINE
               END-STRING
               PERFORM ADD-OUTPUT-LINE-SAFE
           END-IF

           IF ALIAS-MATCH-PERCENT(CUSTOMER-INDEX, MATCH-INDEX) > 0
               STRING
                   "- Match by alias: "
                   DELIMITED BY SIZE
                   DISPLAY-ALIAS-MATCH-PERCENT
                   DELIMITED BY SIZE
                   "%."
                   DELIMITED BY SIZE
                   INTO OUTPUT-TEXT-LINE
               END-STRING
               PERFORM ADD-OUTPUT-LINE-SAFE
           END-IF

           IF BIRTHDAY-MATCH-PERCENT(CUSTOMER-INDEX, MATCH-INDEX) > 0
               STRING
                   "- Match by Birthday: "
                   DELIMITED BY SIZE
                   DISPLAY-BIRTHDAY-MATCH-PERCENT
                   DELIMITED BY SIZE
                   "%."
                   DELIMITED BY SIZE
                   INTO OUTPUT-TEXT-LINE
               END-STRING
               PERFORM ADD-OUTPUT-LINE-SAFE
           END-IF

           IF COUNTRY-MATCH-PERCENT(CUSTOMER-INDEX, MATCH-INDEX) > 0
               STRING
                   "- Match by Country: "
                   DELIMITED BY SIZE
                   DISPLAY-COUNTRY-MATCH-PERCENT
                   DELIMITED BY SIZE
                   "%."
                   DELIMITED BY SIZE
                   INTO OUTPUT-TEXT-LINE
               END-STRING
               PERFORM ADD-OUTPUT-LINE-SAFE
           END-IF

           MOVE SPACES TO OUTPUT-TEXT-LINE
           PERFORM ADD-OUTPUT-LINE-SAFE

           STRING
               "Sum of match %: "
               DELIMITED BY SIZE
               DISPLAY-TOTAL-MATCH-PERCENT
               DELIMITED BY SIZE
               "%"
               DELIMITED BY SIZE
               INTO OUTPUT-TEXT-LINE
           END-STRING
           PERFORM ADD-OUTPUT-LINE-SAFE

           MOVE SPACES TO OUTPUT-TEXT-LINE
           PERFORM ADD-OUTPUT-LINE-SAFE

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
