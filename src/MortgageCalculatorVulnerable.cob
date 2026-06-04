       IDENTIFICATION DIVISION.
       PROGRAM-ID. LEGACY-SYSTEMS-SECURITY-LAB.
       AUTHOR. ANGELINE-NICOLE-FAINA.
      ***************************************************************
      * VULNERABLE BASELINE                                         *
      * DEMONSTRATES LEGACY INPUT-VALIDATION FLAWS IN BANKING CODE  * 
      * THIS IS THE "BEFORE" VERSION. DO NOT USE IN PRODUCTION      *     
      * EACH FLOW BELOW IS FIXED IN THE HARDENED VERSION            *
      ***************************************************************

       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * FLAW: fields undersized. Principal caps at 999,999.99 -
      *       larger input is silently truncated (overflow).
       01 WS-PRINCIPAL              PIC 9(6)V99.
       01 WS-ANNUAL-RATE            PIC 9(2)V9(4).
       01 WS-TERM-YEARS             PIC 9(2).
       01 WS-MONTHS                 PIC 9(4).
       01 WS-MONTHLY-RATE           PIC 9V9(8).
       01 WS-FACTOR                 PIC 9(9)V9(8).
       01 WS-PAYMENT                PIC 9(6)V99.
       01 WS-TOTAL-PAID             PIC 9(9)V99.
       01 WS-TOTAL-INTEREST         PIC 9(9)V99.
       01 WS-RETRY                  PIC X VALUE SPACE.

      * Display-formatted copies for output
       01 WS-DISPLAY-AMOUNT         PIC $,$$$,$$$,$$$,$$9.99.
       01 WS-DISPLAY-PAYMENT        PIC $,$$$,$$$,$$$,$$9.99.
       01 WS-DISPLAY-TOTAL          PIC $,$$$,$$$,$$$,$$9.99.
       01 WS-DISPLAY-INTEREST       PIC $,$$$,$$$,$$$,$$9.99.
       01 WS-DISPLAY-RATE           PIC Z9.9999.

       PROCEDURE DIVISION.
       MAIN-PARA.
            MOVE 'Y' TO WS-RETRY 
            PERFORM CALC-LOOP UNTIL WS-RETRY = 'N'
            PERFORM SHUTDOWN-SEQUENCE
            STOP RUN.

       CALC-LOOP.
           DISPLAY "======================================"
           DISPLAY "   MORTGAGE CALCULATOR"
           DISPLAY "   VULNERABLE BASELINE"
           DISPLAY "======================================"
           DISPLAY " ".

      * FLAW: ACCEPT with no NUMERIC class test. Letters or symbols
      *       land in numeric fields as garbage and compute anyway.
           DISPLAY "Enter Loan Amount: " WITH NO ADVANCING
           ACCEPT WS-PRINCIPAL
           DISPLAY "Enter Annual Interest Rate (e.g. 5.25): "
               WITH NO ADVANCING
           ACCEPT WS-ANNUAL-RATE
           DISPLAY "Enter Loan Term in Years: " WITH NO ADVANCING
           ACCEPT WS-TERM-YEARS

           COMPUTE WS-MONTHS = WS-TERM-YEARS * 12
           COMPUTE WS-MONTHLY-RATE = WS-ANNUAL-RATE / 12 / 100

      * FLAW: no ON SIZE ERROR. Overflow in the result is silent.
      * FLAW: no guard for 0% rate -> divide by zero aborts the run.
           COMPUTE WS-FACTOR = (1 + WS-MONTHLY-RATE) ** WS-MONTHS
           COMPUTE WS-PAYMENT =
               WS-PRINCIPAL * WS-MONTHLY-RATE * WS-FACTOR
               / (WS-FACTOR - 1)

           COMPUTE WS-TOTAL-PAID = WS-PAYMENT * WS-MONTHS 
           COMPUTE WS-TOTAL-INTEREST = WS-TOTAL-PAID - WS-PRINCIPAL
           
           MOVE WS-PRINCIPAL        TO WS-DISPLAY-AMOUNT
           MOVE WS-PAYMENT          TO WS-DISPLAY-PAYMENT
           MOVE WS-TOTAL-PAID       TO WS-DISPLAY-TOTAL
           MOVE WS-TOTAL-INTEREST   TO WS-DISPLAY-INTEREST
           MOVE WS-ANNUAL-RATE      TO WS-DISPLAY-RATE

           DISPLAY " "
           DISPLAY "===================================="
           DISPLAY "    MORTGAGE CALCULATION RESULTS    "
           DISPLAY "===================================="
           DISPLAY "Loan Amount    : " WS-DISPLAY-AMOUNT
           DISPLAY "Annual Rate    : " WS-DISPLAY-RATE "%"
           DISPLAY "Term           : " WS-MONTHS " months"
           DISPLAY "------------------------------------"
           DISPLAY "Monthly Payment: " WS-DISPLAY-PAYMENT
           DISPLAY "Total Paid     : " WS-DISPLAY-TOTAL
           DISPLAY "Total Interest : " WS-DISPLAY-INTEREST
           DISPLAY "===================================="
           DISPLAY " "
      * FLAW: retry input not validated either - any non-N loops.
          DISPLAY "Calculate another mortgage? (Y/N): "
              WITH NO ADVANCING 
          ACCEPT WS-RETRY 
          IF WS-RETRY = 'y' THEN MOVE 'Y' TO WS-RETRY END-IF 
          IF WS-RETRY = 'n' THEN MOVE 'N' TO WS-RETRY END-IF.

        SHUTDOWN-SEQUENCE.
           DISPLAY " "
           DISPLAY "======================================"
           DISPLAY "   SESSION ENDED SECURELY"
           DISPLAY "   All inputs sanitized and logged."
           DISPLAY "======================================"
           DISPLAY " ".
