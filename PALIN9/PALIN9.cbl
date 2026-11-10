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
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT NUMS-LIST ASSIGN TO NUMS.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
      *-----------------------------------------------------------------
       FILE SECTION.
       FD NUMS-LIST RECORDING MODE F.
       01  NUMS-RECORD  PIC X(80).
      *
       WORKING-STORAGE SECTION.
       01  LAST-RECORD  PIC X(1) VALUE SPACE.
       01  NUMBER-X     PIC S9(12) VALUE -803321 COMP-3.
       01  MIRROR       PIC S9(12) COMP-3.
       01  TEMP         PIC S9(12) COMP-3.
       01  CURRENT      PIC S9(12) COMP-3.
       01  QUOT         PIC S9(12) COMP-3.
       01  DISP-X       PIC -(12)9.
       01  DISP-X-TXT   PIC X(13).
      *
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------
      *
      *                          A paragraph that simply opens the file.
      *
       OPEN-FILE.
           OPEN INPUT NUMS-LIST.
       OPEN-FILE-END.
      *
      *                                 This paragraph starts by calling
      *                                        the READ-RECORD paragraph
      *                                        to check for empty files.
      *
       PROCESS-NEXT-RECORD.
           PERFORM READ-RECORD
           PERFORM UNTIL LAST-RECORD = 'Y'
               COMPUTE NUMBER-X = FUNCTION NUMVAL(NUMS-RECORD)
               PERFORM CHECK-NUMBER
               PERFORM READ-RECORD
           END-PERFORM.
       PROCESS-NEXT-RECORD-END.
      *
       CLOSE-STOP.
           CLOSE NUMS-LIST.
           STOP RUN.
      *
      *                              The paragraph to read the different
      *                                  lines of the record and set the
      *                                             flag once it's done.
      *
       READ-RECORD.
           READ NUMS-LIST
           AT END MOVE 'Y' TO LAST-RECORD
           END-READ.
       READ-RECORD-END.
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
      *     
      *                                         DISP-X-TXT is a variable
      *                                    to simply disply the results.
      *
           MOVE FUNCTION TRIM(DISP-X LEADING) TO DISP-X-TXT.
      *
      *                               If we deal with a negative number.
      *
           IF NUMBER-X LESS THAN 0
               THEN DISPLAY DISP-X-TXT " is not a palindrome number."
      *
      *                                    Otherwise, mirror the number.
      *
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
