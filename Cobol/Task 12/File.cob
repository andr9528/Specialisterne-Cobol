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
       02 SEPARATOR-LINE PIC X(90) VALUE ALL "-".

       01 SORT-WRAPPER.
       02 SORT-INDEX PIC 99.
       02 COMPARE-INDEX PIC 99.
       02 COMPARE-INDEX-START PIC 99.
       
       01 MISQ-WRAPPER.
       02 COUNT-DISPLAY PIC ZZ9.
       
       01 FORMATTER-WRAPPER.
           COPY "FormatterWrapper.cpy".

       PROCEDURE DIVISION.          
           CALL "INPUTLOADER"
               USING CUSTOMERS-WRAPPER
                   SANCTIONS-WRAPPER.
           
           PERFORM BUILD-REPORT

           CALL "OUTPUTWRITTER" USING OUTPUT-WRAPPER.
       STOP RUN. 

       BUILD-REPORT.
           DISPLAY "START BUILD-REPORT"
           

           DISPLAY "END BUILD-REPORT"
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
