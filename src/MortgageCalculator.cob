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
       01 WS-ORIGINAL-RATE         PIC 9(2)V9(4) VALUE ZERO.
       01 WS-TEMP-COUNT            PIC 9(4) VALUE ZERO.
       01 WS-CHAR-POS              PIC 9(4) VALUE ZERO.
       01 WS-CURRENT-CHAR          PIC X VALUE SPACE.
       
      * INPUT VARIABLES WITH BOUNDARY CHECKS
       01 WS-INPUT-AMOUNT          PIC X(15).
       01 WS-INPUT-RATE            PIC X(10).
       01 WS-INPUT-YEARS           PIC X(5).
       01 WS-INPUT-LENGTH          PIC 9(4) VALUE ZERO. 
      
      * VALIDATION FLAGS
       01 WS-VALID-INPUT           PIC X VALUE 'N'.
       01 WS-CONTINUE              PIC X VALUE 'Y'.
       01 WS-RETRY                 PIC X(10) VALUE SPACES.

      * SECURITY AUDIT TRAIL
       01 WS-ATTEMPT-COUNTER       PIC 9(3) VALUE ZERO.
       01 WS-MAX-ATTEMPTS          PIC 9(3) VALUE 5.
       01 WS-ERROR-CODE            PIC X(4).
       
      * DISPLAY FORMATIING
       01 WS-DISPLAY-AMOUNT        PIC $,$$$,$$$,$$$,$$9.99.
       01 WS-DISPLAY-PAYMENT       PIC $,$$$,$$$,$$$,$$9.99.
       01 WS-DISPLAY-TOTAL         PIC $,$$$,$$$,$$$,$$9.99.
       01 WS-DISPLAY-INTEREST      PIC $,$$$,$$$,$$$,$$9.99.
       01 WS-DISPLAY-RATE          PIC Z9.9999.

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
           
           IF WS-CONTINUE = 'Y'
               PERFORM GET-INTEREST-RATE
           END-IF 

           IF WS-CONTINUE = 'Y'
               PERFORM GET-LOAN-YEARS
           END-IF 

           IF WS-CONTINUE = 'Y' AND WS-VALID-INPUT = 'Y'
               PERFORM CALCULATE-MORTGAGE
               PERFORM DISPLAY-RESULTS
           END-IF 

           IF WS-CONTINUE = 'Y'
               PERFORM ASK-CONTINUE
           END-IF.
       
       GET-LOAN-AMOUNT.
           PERFORM UNTIL WS-VALID-INPUT = 'Y'
                   OR WS-ATTEMPT-COUNTER >= WS-MAX-ATTEMPTS
               DISPLAY " "
               DISPLAY "Enter Loan Amount (Max: 9999999999.99): "
                    WITH NO ADVANCING
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
           MOVE ZERO TO WS-TEMP-COUNT
      
      * CHECK FOR EMPTY INPUT
           IF WS-INPUT-AMOUNT = SPACES THEN 
               MOVE 'N' TO WS-VALID-INPUT
           END-IF

      * REJECT IF NEGATIVE SIGN PRESENT
           IF WS-INPUT-AMOUNT (1:1) = '-' THEN 
               MOVE 'N' TO WS-VALID-INPUT
           END-IF

      * CHECK LENGTH - PREVENT OVERFLOW BEFORE NUMVAL
           IF FUNCTION LENGTH(
               FUNCTION TRIM(
               FUNCTION TRIM(WS-INPUT-AMOUNT LEADING)
               TRAILING)) > 13 THEN 
               MOVE 'N' TO WS-VALID-INPUT
           END-IF

      * STRICT CHARACTER-BY-CHARACTER VALIIDATION
           MOVE ZERO TO WS-CHAR-POS
           MOVE ZERO TO WS-TEMP-COUNT 
           MOVE FUNCTION LENGTH(FUNCTION TRIM(
               WS-INPUT-AMOUNT TRAILING))
               TO WS-INPUT-LENGTH

           PERFORM VARYING WS-CHAR-POS FROM 1 BY 1 
               UNTIL WS-CHAR-POS > WS-INPUT-LENGTH
               MOVE WS-INPUT-AMOUNT(WS-CHAR-POS:1)
                   TO WS-CURRENT-CHAR 
               IF WS-CURRENT-CHAR NOT = '0'
               AND WS-CURRENT-CHAR NOT = '1'
               AND WS-CURRENT-CHAR NOT = '2'
               AND WS-CURRENT-CHAR NOT = '3'
               AND WS-CURRENT-CHAR NOT = '4'
               AND WS-CURRENT-CHAR NOT = '5'
               AND WS-CURRENT-CHAR NOT = '6'
               AND WS-CURRENT-CHAR NOT = '7'
               AND WS-CURRENT-CHAR NOT = '8'
               AND WS-CURRENT-CHAR NOT = '9'
               AND WS-CURRENT-CHAR NOT = '.'
               AND WS-CURRENT-CHAR NOT = ' '
                   ADD 1 TO WS-TEMP-COUNT
               END-IF 
           END-PERFORM 

           IF WS-TEMP-COUNT > 0 THEN 
               MOVE 'N' TO WS-VALID-INPUT 
           END-IF 
           
      * CONVERT AND BOUNDARY CHECK
           IF WS-VALID-INPUT = 'Y' THEN 
               COMPUTE WS-LOAN-AMOUNT = 
                   FUNCTION NUMVAL(WS-INPUT-AMOUNT)
               IF WS-LOAN-AMOUNT <= 0 OR 
                  WS-LOAN-AMOUNT > 9999999999.99 THEN
                   MOVE 'N' TO WS-VALID-INPUT 
               END-IF 
           END-IF.

       GET-INTEREST-RATE.
           MOVE 'N' TO WS-VALID-INPUT
           MOVE ZERO TO WS-ATTEMPT-COUNTER

           PERFORM UNTIL WS-VALID-INPUT = 'Y'
                    OR WS-ATTEMPT-COUNTER >= WS-MAX-ATTEMPTS
               DISPLAY "Enter Annual Interest Rate (e.g., 3.5): "
                    WITH NO ADVANCING
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

       VALIDATE-INTEREST-RATE.
           MOVE 'Y' TO WS-VALID-INPUT
           MOVE ZERO TO WS-TEMP-COUNT
       
      * CHECK FOR EMPTY INPUT 
           IF WS-INPUT-RATE = SPACES THEN 
               MOVE 'N' TO WS-VALID-INPUT
           END-IF 
      
      * REJECT IF NEGATIVE SIGN PRESENT
           IF WS-INPUT-RATE (1:1) = '-' THEN 
               MOVE 'N' TO WS-VALID-INPUT
           END-IF 

      * STRICT CHARACTER-BY-CHARACTER VALIIDATION
           MOVE ZERO TO WS-CHAR-POS
           MOVE ZERO TO WS-TEMP-COUNT 
           MOVE FUNCTION LENGTH(FUNCTION TRIM(
               WS-INPUT-RATE TRAILING))
               TO WS-INPUT-LENGTH

           PERFORM VARYING WS-CHAR-POS FROM 1 BY 1 
               UNTIL WS-CHAR-POS > WS-INPUT-LENGTH
               MOVE WS-INPUT-RATE(WS-CHAR-POS:1)
                   TO WS-CURRENT-CHAR 
               IF WS-CURRENT-CHAR NOT = '0'
               AND WS-CURRENT-CHAR NOT = '1'
               AND WS-CURRENT-CHAR NOT = '2'
               AND WS-CURRENT-CHAR NOT = '3'
               AND WS-CURRENT-CHAR NOT = '4'
               AND WS-CURRENT-CHAR NOT = '5'
               AND WS-CURRENT-CHAR NOT = '6'
               AND WS-CURRENT-CHAR NOT = '7'
               AND WS-CURRENT-CHAR NOT = '8'
               AND WS-CURRENT-CHAR NOT = '9'
               AND WS-CURRENT-CHAR NOT = '.'
               AND WS-CURRENT-CHAR NOT = ' '
                   ADD 1 TO WS-TEMP-COUNT
               END-IF 
           END-PERFORM 

           IF WS-TEMP-COUNT > 0 THEN 
               MOVE 'N' TO WS-VALID-INPUT 
           END-IF
                  
      * CONVERT AND BOUNDARY CHECK
           IF WS-VALID-INPUT = 'Y' THEN 
               COMPUTE WS-ANNUAL-RATE = 
                   FUNCTION NUMVAL(WS-INPUT-RATE)
               IF WS-ANNUAL-RATE <= 0 OR 
                  WS-ANNUAL-RATE > 30 THEN 
                   MOVE 'N' TO WS-VALID-INPUT
               END-IF 
           END-IF.

       GET-LOAN-YEARS.
           MOVE 'N' TO WS-VALID-INPUT
           MOVE ZERO TO WS-ATTEMPT-COUNTER

           PERFORM UNTIL WS-VALID-INPUT = 'Y'
                    OR WS-ATTEMPT-COUNTER >= WS-MAX-ATTEMPTS
               DISPLAY "Enter Loan Term in Years (1-30): "
                    WITH NO ADVANCING
               ACCEPT WS-INPUT-YEARS 

               ADD 1 TO WS-ATTEMPT-COUNTER 

               PERFORM VALIDATE-LOAN-YEARS

               IF WS-VALID-INPUT = 'N' THEN 
                   DISPLAY "ERROR: Invalid loan term. Try again."
                   IF WS-ATTEMPT-COUNTER >= WS-MAX-ATTEMPTS THEN 
                       DISPLAY "SECURITY: Max attempts reached."
                       MOVE 'E003' TO WS-ERROR-CODE
                       PERFORM LOG-SECURITY-EVENT
                       MOVE 'N' TO WS-CONTINUE 
                   END-IF 
               END-IF 
           END-PERFORM.

       VALIDATE-LOAN-YEARS.
           MOVE 'Y' TO WS-VALID-INPUT
           MOVE ZERO TO WS-TEMP-COUNT

           IF WS-INPUT-YEARS = SPACES THEN 
               MOVE 'N' TO WS-VALID-INPUT
           END-IF 

      * CHECK FOR EMPTY INPUT
           IF WS-INPUT-YEARS = SPACES THEN 
               MOVE 'N' TO WS-VALID-INPUT 
           END-IF 

      * REJECT IF NEGATIVE SIGN PRESENT
           IF WS-INPUT-YEARS (1:1) = '-' THEN 
               MOVE 'N' TO WS-VALID-INPUT
           END-IF 

      * STRICT CHARACTER-BY-CHARACTER VALIIDATION
           MOVE ZERO TO WS-CHAR-POS
           MOVE ZERO TO WS-TEMP-COUNT 
           MOVE FUNCTION LENGTH(FUNCTION TRIM(
               WS-INPUT-YEARS TRAILING))
               TO WS-INPUT-LENGTH

           PERFORM VARYING WS-CHAR-POS FROM 1 BY 1 
               UNTIL WS-CHAR-POS > WS-INPUT-LENGTH
               MOVE WS-INPUT-YEARS(WS-CHAR-POS:1)
                   TO WS-CURRENT-CHAR 
               IF WS-CURRENT-CHAR NOT = '0'
               AND WS-CURRENT-CHAR NOT = '1'
               AND WS-CURRENT-CHAR NOT = '2'
               AND WS-CURRENT-CHAR NOT = '3'
               AND WS-CURRENT-CHAR NOT = '4'
               AND WS-CURRENT-CHAR NOT = '5'
               AND WS-CURRENT-CHAR NOT = '6'
               AND WS-CURRENT-CHAR NOT = '7'
               AND WS-CURRENT-CHAR NOT = '8'
               AND WS-CURRENT-CHAR NOT = '9'
               AND WS-CURRENT-CHAR NOT = ' '
                   ADD 1 TO WS-TEMP-COUNT
               END-IF 
           END-PERFORM 

           IF WS-TEMP-COUNT > 0 THEN 
               MOVE 'N' TO WS-VALID-INPUT
           END-IF 

      * CONVERT AND BOUNDARY CHECK
           IF WS-VALID-INPUT = 'Y' THEN 
               COMPUTE WS-LOAN-YEARS = 
                   FUNCTION NUMVAL(WS-INPUT-YEARS)
               IF WS-LOAN-YEARS < 1 OR  
                  WS-LOAN-YEARS > 30 THEN 
                   MOVE 'N' TO WS-VALID-INPUT 
               END-IF 
           END-IF.

      *****************************************************************
      * M = P x [r(1+r)^n] / [(1+r)^n - 1]                            *
      * Where:                                                        *
      *    P = principal (loan amount)                                *
      *    r = monthly interest rate (annual / 12 / 100)              *
      *    n = number of payments (years x 12)                        *
      *****************************************************************

       CALCULATE-MORTGAGE.
           MOVE WS-ANNUAL-RATE TO WS-ORIGINAL-RATE
      * CONVERT ANNUAL RATE TO MONTHLY DECIMAL 
      * e.g. 6% annual -> 0.005 monthly
           COMPUTE WS-MONTHLY-RATE =
               WS-ANNUAL-RATE / 1200
              
      * TOTAL NUMBER OF MONTHLY PAYMENTS
           COMPUTE WS-NUM-PAYMENTS =
               WS-LOAN-YEARS * 12

      * COMPUTE (1 + r)^n VIA LOOP (NO BUILT-IN POWER IN COBOL)
           MOVE 1 TO WS-POWER-RESULT
           PERFORM VARYING WS-LOOP-CTR FROM 1 BY 1 
               UNTIL WS-LOOP-CTR > WS-NUM-PAYMENTS
               COMPUTE WS-POWER-RESULT = 
                   WS-POWER-RESULT * (1 + WS-MONTHLY-RATE)
           END-PERFORM 

      * MONTHLY PAYMENT FORMULA: P*r*(1+r)^n / ((1+r)^n - 1)
           COMPUTE WS-MONTHLY-PAYMENT ROUNDED =
               WS-LOAN-AMOUNT * WS-MONTHLY-RATE * WS-POWER-RESULT 
               / (WS-POWER-RESULT - 1)

      * TOTAL AMOUNT PAID OVER LIFE OF LOAN
           COMPUTE WS-TOTAL-PAID ROUNDED =
               WS-MONTHLY-PAYMENT * WS-NUM-PAYMENTS

      * TOTAL INTEREST = TOTAL PAID MINUES ORIGINAL PRINCIPAL
           COMPUTE WS-TOTAL-INTEREST ROUNDED = 
               WS-TOTAL-PAID - WS-LOAN-AMOUNT.
       
       DISPLAY-RESULTS.
           MOVE WS-LOAN-AMOUNT TO WS-DISPLAY-AMOUNT
           MOVE WS-MONTHLY-PAYMENT TO WS-DISPLAY-PAYMENT
           MOVE WS-TOTAL-PAID TO WS-DISPLAY-TOTAL
           MOVE WS-TOTAL-INTEREST TO WS-DISPLAY-INTEREST
           MOVE WS-ORIGINAL-RATE TO WS-DISPLAY-RATE 

           DISPLAY " "
           DISPLAY "====================================="
           DISPLAY "    MORTGAGE CALCULATION RESULTS     "
           DISPLAY "====================================="
           DISPLAY "Loan Amount     : " WS-DISPLAY-AMOUNT
           DISPLAY "Annual Rate     : " WS-DISPLAY-RATE "%"
           DISPLAY "Term            : " WS-NUM-PAYMENTS " months"
           DISPLAY "------------------------------------"
           DISPLAY "Monthly Payment : " WS-DISPLAY-PAYMENT
           DISPLAY "Total Paid      : " WS-DISPLAY-TOTAL
           DISPLAY "Total Interest  : " WS-DISPLAY-INTEREST
           DISPLAY "====================================="
           DISPLAY " ".

       LOG-SECURITY-EVENT.
           DISPLAY " "
           DISPLAY "*** SECURITY AUDIT EVENT ***"
           DISPLAY "Error Code      : " WS-ERROR-CODE
           DISPLAY "Attempts Made   : " WS-ATTEMPT-COUNTER 
           DISPLAY "***************************"
           DISPLAY " ".

        ASK-CONTINUE.
           MOVE SPACE TO WS-CONTINUE
           PERFORM UNTIL WS-CONTINUE = 'Y' OR WS-CONTINUE = 'N'
               DISPLAY "Calculate another mortgage? (Y/N): "
                   WITH NO ADVANCING
               ACCEPT WS-RETRY
               IF FUNCTION UPPER-CASE(FUNCTION TRIM(WS-RETRY)) = 'Y'
                   MOVE 'Y' TO WS-CONTINUE
               ELSE
                   IF FUNCTION UPPER-CASE(FUNCTION TRIM(WS-RETRY)) = 'N'
                       MOVE 'N' TO WS-CONTINUE
                   ELSE
                       DISPLAY "Invalid choice. Please enter Y or N."
                   END-IF
               END-IF
           END-PERFORM.

       SHUTDOWN-SEQUENCE.
           DISPLAY " "
           DISPLAY "======================================"
           DISPLAY "   SESSION ENDED SECURELY"
           DISPLAY "   All inputs sanitized and logged."
           DISPLAY "======================================"
           DISPLAY " ".
       
