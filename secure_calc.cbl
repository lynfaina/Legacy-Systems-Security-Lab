       IDENTIFICATION DIVISION.
       PROGRAM-ID. LEGACY-SYSTEMS-SECURITY-LAB.
       AUTHOR. ANGELINE-NICOLE-FAINA.
      *****************************************************************
      * SECURE MORTGAGE CALCULATOR WITH INPUT VALIDATION              *
      * DEMONSTRATES DEFENSIVE PROGRAMMING FOR LEGACY BANKING SYSTEMS *
      * PROTECTS AGAINST: SQL INJECTION, BUFFER OVERFLOW,             *
      * INVALID DATA, NUMERIC OVERFLOW, PRECISION ERROS               *
      *****************************************************************
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
      * FINANCIAL CALCULATION VARIABLES (FIXED PRECISION)
       01 WS-LOAN-AMOUNT           PIC 9(10)V99 VALUE ZERO.
       01 WS-ANNUAL-RATE           PIC 9(2)V9(4) VALUE ZERO.
       01 WS-LOAN-YEARS            PIC 9(2) VALUE ZERO.
       01 WS-MONTHLY-PAYMENT       PIC 9(8)V99 VALUE ZERO.
       01 WS-MONTHLY-RATE          PIC 9V9(8) VALUE ZERO.
       01 WS-NUM-PAYMENTS          PIC 9(4) VALUE ZERO.
       01 WS-TEMP-CALC             PIC 9(10)V9(8) VALUE ZERO.
       01 WS-POWER-RESULT          PIC 9(10)V9(8) VALUE ZERO.
       01 WS-TOTAL-PAID            PIC 9(12)V99 VALUE ZERO.
       01 WS-TOTAL-INTEREST        PIC 9(12)V99 VALUE ZERO.
       
      * INPUT VARIABLES WITH BOUNDARY CHECKS
       01 WS-INPUT-AMOUNT          PIC X(15).
       01 WS-INPUT-RATE            PIC X(10).
       01 WS-INPUT-YEARS           PIC X(5).
      
      * VALIDATION FLAGS
       01 WS-VALID-INPUT           PIC X VALUE 'N'.
       01 WS-CONTINUE              PIC X VALUE 'Y'.
       01 WS-RETRY                 PIC X VALUE ' '.

      * SECURITY AUDIT TRAIL
       01 WS-ATTEMPT-COUNTER       PIC 9(3) VALUE ZERO.
       01 WS-MAX-ATTEMPTS          PIC 9(3) VALUE 5.
       01 WS-ERROR-CODE            PIC X(4).
       
      * DISPLAY FORMATIING
       01 WS-DISPLAY-AMOUNT        PIC $,$$$,$$$,$$9.99.
       01 WS-DISPLAY-PAYMENT       PIC $,$$$,$$$,$$9.99.
       01 WS-DISPLAY-TOTAL         PIC $,$$$,$$$,$$9.99.
       01 WS-DISPLAY-INTEREST      PIC $,$$$,$$$,$$9.99.
       01 WS-DISPLAY-RATE          PIC ZZ9.9999.

      * LOOP COUNTERS FOR POWER CALCULATION
       01 WS-LOOP-CTR              PIC 9(4) VALUE ZERO.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM DISPLAY-BANNER
           PERFORM MAIN-LOOP UNTIL WS-CONTINUE = 'N'
           PERFORM SHUTDOWN-SEQUENCE
           STOP RUN.

       DISPLAY-BANNER.
           DISPLAY "======================================"
           DISPLAY "   SECURE MORTGAGE CALCULATOR"
           DISPLAY "   DEFENSING PROGRAMING ENABLED"
           DISPLAY "======================================"
           DISPLAY " ".
       
       MAIN-LOOP.
           MOVE ZERO TO WS-ATTEMPT-COUNTER
           MOVE 'N' TO WS-VALID-INPUT

           PERFORM GET-LOAN-AMOUNT
           PERFORM GET-INTEREST-RATE
           PERFORM GET-LOAN-YEARS

           IF WS-VALID-INPUT = 'Y' THEN 
               PERFORM CALCULATE-MORTGAGE
               PERFORM DISPLAY-RESULTS
           END-IF 

           PERFORM ASK-CONTINUE.
       
       GET-LOAN-AMOUNT.
           PERFORM UNTIL WS-VALID-INPUT = 'Y'
                   OR WS-ATTEMPT-COUNTER >= WS-MAX-ATTEMPTS
               DISPLAY " "
               DISPLAY "Enter Loan Amount (Max: 9999999999.99): "
               ACCEPT WS-INPUT-AMOUNT

               ADD 1 TO WS-ATTEMPT-COUNTER

               PERFORM VALIDATE-LOAN-AMOUNT

               IF WS-VALID-INPUT = 'N' THEN 
                   DISPLAY "ERROR: Invalid loan amount. Try again."
                   IF WS-ATTEMPT-COUNTER >= WS-MAX-ATTEMPTS THEN 
                       DISPLAY "SECURITY: Max attempts reached."
                       MOVE 'E001' TO WS-ERROR-CODE
                       PERFORM LOG-SECURITY-EVENT
                       MOVE 'N' TO WS-CONTINUE
                   END-IF
               END-IF
           END-PERFORM.    

       VALIDATE-LOAN-AMOUNT.
           MOVE 'Y' TO WS-VALID-INPUT
      
      * CHECK FOR EMPTY INPUT
           IF WS-INPUT-AMOUNT = SPACES THEN 
               MOVE 'N' TO WS-VALID-INPUT
           END-IF

      * CHECK FOR NON-NUMERIC CHARACTERS (PREVENT INJECTION)
           INSPECT WS-INPUT-AMOUNT REPLACING ALL ";" BY " "       
           INSPECT WS-INPUT-AMOUNT REPLACING ALL "'" BY " "       
           INSPECT WS-INPUT-AMOUNT REPLACING ALL '"' BY " "       
           INSPECT WS-INPUT-AMOUNT REPLACING ALL "-" BY " "      

           IF FUNCTION TEST-NUMVAL(WS-INPUT-AMOUNT) = 0 THEN 
               COMPUTE WS-LOAN-AMOUNT =
                   FUNCTION NUMVAL(WS-INPUT-AMOUNT) 

      * BOUNDARY VALIDATION (PREVENT OVERFLOW)
               IF WS-LOAN-AMOUNT <= 0 OR
                  WS-LOAN-AMOUNT > 9999999999.99 THEN 
                   MOVE 'N' TO WS-VALID-INPUT
               END-IF 
           ELSE 
               MOVE 'N' TO WS-VALID-INPUT
           END-IF.

       GET-INTEREST-RATE.
           MOVE 'N' TO WS-VALID-INPUT
           MOVE ZERO TO WS-ATTEMPT-COUNTER

           PERFORM UNTIL WS-VALID-INPUT = 'Y'
                    OR WS-ATTEMPT-COUNTER >= WS-MAX-ATTEMPTS
               DISPLAY "Enter Annual Interest Rate (e.g., 3.5-): "
               ACCEPT WS-INPUT-RATE 

               ADD 1 TO WS-ATTEMPT-COUNTER
               
               PERFORM VALIDATE-INTEREST-RATE

               IF WS-VALID-INPUT = 'N' THEN 
                   DISPLAY "ERROR: Invalid interest rate. Try again."
                   IF WS-ATTEMPT-COUNTER >= WS-MAX-ATTEMPTS THEN 
                       DISPLAY "SECURITY: Max attemps reached."
                       MOVE 'E002' TO WS-ERROR-CODE
                       PERFORM LOG-SECURITY-EVENT
                       MOVE 'N' TO WS-CONTINUE 
                   END-IF
               END-IF
           END-PERFORM.