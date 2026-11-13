      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. CLIMB70.
       AUTHOR. "Pierre"

      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.
      *-----------------------------------------------------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT NUMS-LIST ASSIGN TO NUMS.
      *-----------------------------------------------------------------
       DATA DIVISION.
      *-----------------------------------------------------------------
       FILE SECTION.
       FD NUMS-LIST RECORDING MODE F.
       01  NUMS-RECORD  PIC X(80).

       WORKING-STORAGE SECTION.
      *
      *                              Variable to hold the current input.
      * 
       01  INPUT-N                       PIC S9(12).
      *
      *                         Variables for the climbing stairs logic.
      * 
       01  PREV1                         PIC 9(12) VALUE 2.
       01  PREV2                         PIC 9(12) VALUE 1.
       01  CURRENT                       PIC 9(12).
       01  STEPS-COUNT                   PIC 9(12) VALUE 3.
      *
      *                                     Variable to hold the result.
      *
       01  RESULT                        PIC 9(12).
      *
      *
      *                                                            Flag.
       01  LAST-RECORD                   PIC X(1) VALUE SPACE.
      *
      *
      *                                  Variables to format the output. 
       01  NUM-TO-DISP                   PIC -(12)9.
       01  NUM-TO-DISP-TXT               PIC X(13).
       01  NUM-TO-DISP-RES-TXT           PIC X(13).
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------
      *
      *                                      Paragraph to open the file.
      * 
       OPEN-FILE.
           OPEN INPUT NUMS-LIST.
       OPEN-FILE-END.
      *
           DISPLAY "Ways to climb a staircase.".
           DISPLAY "Possible moves: 1 step, 2 steps.".
      *     
       PROCESS-NEXT-RECORD.
           PERFORM READ-RECORD.
           PERFORM UNTIL LAST-RECORD EQUAL 'Y'
      *
      *                          Storing the current value into INPUT-N.
      *
               COMPUTE INPUT-N = FUNCTION NUMVAL(NUMS-RECORD)
      *
      *                   Calling the paragraph that outputs the result.
      *
               PERFORM CHECK-RECORD
               PERFORM READ-RECORD
           END-PERFORM.
       PROCESS-NEXT-RECORD-END.
      *
      *                                              Ending the program.
      *
       CLOSE-STOP.
           DISPLAY "Goodbye."
           CLOSE NUMS-LIST.
           STOP RUN.
      *
      *                                      Paragraph to read a record.
      * 
       READ-RECORD.
           READ NUMS-LIST
           AT END MOVE 'Y' TO LAST-RECORD
           END-READ.
       READ-RECORD-END.
      *
      *                                       Logic to check the record.
      *
       CHECK-RECORD.
           EVALUATE TRUE
      *
      *                                          Discard invalid inputs.
      *
               WHEN INPUT-N LESS THAN 1
                   MOVE INPUT-N TO NUM-TO-DISP
                   PERFORM FORMAT-NUMBER
                   DISPLAY
                       "          "
                       NUM-TO-DISP-TXT
                       "is an invalid input."
      *
      *                   Only one way to climb a staircase of one step.
      *
               WHEN INPUT-N EQUAL 1
                   MOVE INPUT-N TO RESULT
                   MOVE RESULT TO NUM-TO-DISP
                   PERFORM FORMAT-NUMBER
                   DISPLAY
                       "There is  "
                       NUM-TO-DISP-TXT
                       "way to climb a staircase of "
                       NUM-TO-DISP-TXT
                       "  step."
      *
      *                      Two ways to climb a staircase of two steps.
      *
               WHEN INPUT-N EQUAL 2
                   MOVE INPUT-N TO RESULT
                   MOVE RESULT TO NUM-TO-DISP
                   PERFORM FORMAT-NUMBER
                   DISPLAY
                       "There are "
                       NUM-TO-DISP-TXT
                       "ways to climb a staircase of "
                       NUM-TO-DISP-TXT
                       " steps."
      *
      *                            From 3 Steps, resetting the variables
      *                          then calling the CLIMB STAIRS paragraph
      *                                to model climbing stairs from the
      *                                        2nd step to the nth step.
      *
               WHEN INPUT-N GREATER THAN 2
                   MOVE 2 TO STEPS-COUNT
                   MOVE 1 TO PREV2
                   MOVE 2 TO PREV1
                   PERFORM CLIMB-STAIRS UNTIL STEPS-COUNT EQUAL INPUT-N
      *
      *                                             Formating the result
      *
                   MOVE PREV1 TO RESULT
                   MOVE RESULT TO NUM-TO-DISP
                   PERFORM FORMAT-NUMBER
      *
      *                                   Formating the amount of steps.
      *
                   MOVE NUM-TO-DISP-TXT TO NUM-TO-DISP-RES-TXT
                   MOVE INPUT-N  TO NUM-TO-DISP
                   PERFORM FORMAT-NUMBER
      *
      *                                            Outputing the result.
      *
                   DISPLAY
                       "There are "
                       NUM-TO-DISP-RES-TXT
                       "ways to climb a staircase of "
                       NUM-TO-DISP-TXT
                       " steps."
           END-EVALUATE.
       CHEC-RECORD-END.
      *
      *                                   The logic to climb the stairs.
      *                                    To reach the next step, there
      *                                   are always two different ways.
      *                                   From the previous step or from
      *                              the one before that. I.E. the total
      *                          amount of ways to climb a stairs equals
      *                           the sum of the amount of ways to climb
      *                             the staircase to the step before the
      *                        nth step and to the step before that one.
      *
       CLIMB-STAIRS.
           COMPUTE CURRENT = PREV1 + PREV2.
           MOVE PREV1 TO PREV2
           MOVE CURRENT TO PREV1
           ADD 1 TO STEPS-COUNT GIVING STEPS-COUNT.
       CLIMB-STAIRS-END.
      *
      *                                     Formating number for output.
      *
       FORMAT-NUMBER.
           MOVE FUNCTION TRIM(NUM-TO-DISP LEADING) TO NUM-TO-DISP-TXT.
       FORMAT-NUMBER-END.
     