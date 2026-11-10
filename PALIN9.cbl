      *
      *        COBOL version of leet code problem #9: palindrome number.
      *
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. PALIN9.
       AUTHOR. "Pierre".
      *
      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.
      *-----------------------------------------------------------------
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  NUMBER-X     PIC S9(12) VALUE -123321 COMP-3.
       01  MIRROR       PIC S9(12) COMP-3.
       01  TEMP         PIC S9(12) COMP-3.
       01  CURRENT      PIC S9(12) COMP-3.
       01  QUOT         PIC S9(12) COMP-3.
       01  DISP-X       PIC -(12)9.
       01  DISP-X-TXT   PIC X(13).
      *-----------------------------------------------------------------
      *
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------    
      *
      *                           Testing the logic on different values.
      *     
           PERFORM CHECK-NUMBER.
           MOVE 111222111 TO NUMBER-X.
           PERFORM CHECK-NUMBER.
           MOVE 1234 TO NUMBER-X.
           PERFORM CHECK-NUMBER.
           MOVE 0 TO NUMBER-X.
           PERFORM CHECK-NUMBER.
           MOVE 1001 TO NUMBER-X.
           PERFORM CHECK-NUMBER.
           MOVE 8 TO NUMBER-X.
           PERFORM CHECK-NUMBER.
           MOVE -1234 TO NUMBER-X.
           PERFORM CHECK-NUMBER.
           MOVE 123454321 TO NUMBER-X.
           PERFORM CHECK-NUMBER.
           MOVE 99999 TO NUMBER-X.
           PERFORM CHECK-NUMBER.
      *
           STOP RUN.
      *
      *                                  This paragraph contain the step
      *                                   used to turn mirror the number
      *       
       MIRROR-NUMBER.
           DIVIDE TEMP BY 10 GIVING QUOT REMAINDER CURRENT
           COMPUTE MIRROR = MIRROR * 10 + CURRENT
           COMPUTE TEMP = TEMP - CURRENT
           DIVIDE TEMP BY 10 GIVING TEMP REMAINDER QUOT.
       MIRROR-NUMBER-END.
      *
      *                                        The logic to check wheter
      *                           a number is a palindrom number or not.
      * 
       CHECK-NUMBER.
           MOVE NUMBER-X TO DISP-X
           MOVE FUNCTION TRIM(DISP-X LEADING) TO DISP-X-TXT.
           IF NUMBER-X LESS THAN 0
               THEN DISPLAY DISP-X-TXT " is not a palindrome number."
           ELSE 
               MOVE NUMBER-X TO TEMP
               MOVE ZERO TO MIRROR
               PERFORM MIRROR-NUMBER UNTIL TEMP = 0
      *
      *                                           perform the comparison
      *                                          on the mirrored number.
      *
               IF MIRROR EQUAL NUMBER-X
                   THEN DISPLAY DISP-X-TXT " is a palindrome number."
               ELSE
                   DISPLAY DISP-X-TXT " is not a palindrome number."
               END-IF
           END-IF.
