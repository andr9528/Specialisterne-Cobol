       COPY "Identification.cpy"
       02 FIRST-NAME PIC X(20) VALUE SPACES.
       02 LAST-NAME PIC X(20) VALUE SPACES.
       
       02 BANK-INFO.
       COPY "BankInfo.cpy"

       02 ADDRESS-INFO.
       COPY "AddressInfo.cpy"    

       02 CONTACT-INFO.
       03 PHONE-NUMBER PIC X(15) VALUE SPACES.
       03 EMAIL PIC X(80) VALUE SPACES.
