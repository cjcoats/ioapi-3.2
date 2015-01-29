
        LOGICAL FUNCTION GETYN ( PROMPT , DEFAULT )

C******************************************************************
C Version "$Id: getyn.f 100 2015-01-16 16:52:16Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
C (C) 2003-2011 by Baron Advanced Meteorological Systems.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  function body begins at line 80
C
C  FUNCTION:
C
C       Safely get a Yes/No response from the keyboard (with
C       default value ([RETURN] response) supplied from caller.
C       Insensitive to case of input, Checks for validity of the
C       response, and allows  MAX = 5 unsuccessful attempts, then
C       exits.  Treats CONTROL-D (end-of-file) as exit signal.
C
C   RETURNS:  .TRUE. for a "Yes" response,  .FALSE.  for a "No"
C
C   SAMPLE USAGES (where FLAG is a LOGICAL   variable):
C
C       FLAG = GETYN ( 'To test this thing, give me a Y or N',
C    &                  .TRUE. )        !  default response means TRUE
C
C       IF ( GETYN ( 'Another test:' , FLAG ) )
C    &          THEN    !  default response: same as FLAG's value
C         WRITE ( *,* ) 'Result of second test was TRUE'
C       ELSE
C         WRITE ( *,* ) 'Result of second test was FALSE'
C       END IF
C                               --Carlie J. Coats, Jr., 10/87
C
C  REVISION HISTORY:
C
C       Modified 5/1990 for ROM 2.2:  now uses EXWST for error abort.
C       Modified 8/1990 for ROM 2.2:  TEMP.LET; treats CONTROL-Z as exit signal
C       Modified 2/1993 by CJC for CRAY.
C       Modified 8/1996 by CJC:  "!" treated as terminator for input 
C       Modified 1/1997 by CJC:  logs result
C       Modified 4/2002 by CJC:  now accepts T, t, F, f, .TRUE., .FALSE., etc.
C       Revised  6/2003 by CJC:  factor through M3PROMPT to ensure flush()
C       of PROMPT for IRIX F90v7.4  
C       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
C
C**********************************************************************

        IMPLICIT NONE

C.......   Arguments:

        CHARACTER*(*), INTENT(IN   ) :: PROMPT      !!  prompt for user
        LOGICAL      , INTENT(IN   ) :: DEFAULT     !!  default return value


C.......   External functions:

        LOGICAL, EXTERNAL :: ENVYN


C.......   Parameter:  maximum number of attempts allowed to the user

        INTEGER, PARAMETER :: MAX = 5


C.......   Local Variables:

        INTEGER         LENGTH , COUNT , IOS
        CHARACTER*80    ANSWER
        CHARACTER*256   MESG

        LOGICAL, SAVE :: PROMPTON
        LOGICAL, SAVE :: FIRSTIME = .TRUE.


C*********************   begin  GETYN   *******************************

        IF( FIRSTIME ) THEN

            PROMPTON = ENVYN( 'PROMPTFLAG', 'Prompt for input flag',
     &                      .TRUE., IOS )
            FIRSTIME = .FALSE.
 
        END IF

        LENGTH  =  LEN_TRIM( PROMPT )

        IF( .NOT. PROMPTON ) THEN
            GETYN = DEFAULT
            IF ( DEFAULT ) THEN
               CALL M3MSG2( 'Returning default value TRUE for query:')
            ELSE
               CALL M3MSG2('Returning default value FALSE for query:')
            END IF
            MESG = '"' // PROMPT ( 1:LENGTH ) // '"'
            CALL M3MSG2( MESG )
            RETURN
        END IF

C.....  Continue only if PROMPTON is true

        COUNT = 0

11      CONTINUE
        ANSWER  =  '    '

        MESG = PROMPT ( 1:LENGTH ) // ' (Y/N) [' // 'Y' // '] >> '
        IF ( DEFAULT ) THEN
          MESG = PROMPT ( 1:LENGTH ) // ' (Y/N) [Y] >> '
        ELSE
          MESG = PROMPT ( 1:LENGTH ) // ' (Y/N) [N] >> '
        END IF
        CALL M3PROMPT( MESG, ANSWER, IOS )


        IF ( IOS .NE. 0 ) THEN

            GO TO  900

        ELSE IF (     ( ANSWER ( 1:1 ) .EQ. 'Y' )
     &          .OR.  ( ANSWER ( 1:1 ) .EQ. 'y' )
     &          .OR.  ( ANSWER ( 1:1 ) .EQ. 'T' )
     &          .OR.  ( ANSWER ( 1:1 ) .EQ. 't' )
     &          .OR.  ( ANSWER ( 1:2 ) .EQ.'.T' )
     &          .OR.  ( ANSWER ( 1:2 ) .EQ.'.t' ) )  THEN

            GETYN  =  .TRUE.
            CALL M3MSG2( 'Returning value TRUE for query:')

        ELSE IF (     ( ANSWER ( 1:1 ) .EQ. 'N' )
     &          .OR.  ( ANSWER ( 1:1 ) .EQ. 'n' )
     &          .OR.  ( ANSWER ( 1:1 ) .EQ. 'F' )
     &          .OR.  ( ANSWER ( 1:1 ) .EQ. 'f' )
     &          .OR.  ( ANSWER ( 1:2 ) .EQ.'.F' )
     &          .OR.  ( ANSWER ( 1:2 ) .EQ.'.f' ) )  THEN

            GETYN  =  .FALSE.
            CALL M3MSG2( 'Returning value FALSE for query:')

        ELSE IF  (    ( ANSWER ( 1:1 ) .EQ. ' ' )
     &           .OR. ( ANSWER ( 1:1 ) .EQ. '!' ) )  THEN

            GETYN = DEFAULT
            IF ( DEFAULT ) THEN
               CALL M3MSG2( 'Returning default value TRUE for query:')
            ELSE
               CALL M3MSG2('Returning default value FALSE for query:')
            END IF

        ELSE

            COUNT  =  COUNT + 1
            IF ( COUNT .GE. MAX )  THEN
                CALL M3EXIT( 'GETYN', 0, 0, 
     &                   'Maximum number of attempts exceeded', 2 )
            END IF
            MESG='Did not understand your response; Please try again.'
            CALL M3MSG2( MESG )
            WRITE ( MESG, '( A, I3, 2X, A )' ) 
     &          '(You are allowed', MAX - COUNT, 'more attempts.)'
            CALL M3MSG2( MESG )
            GO TO  11

        END IF

        MESG = '"' // PROMPT ( 1:LENGTH ) // '"'
        CALL M3MSG2( MESG )
        RETURN

900	CONTINUE	!  error reading response

            COUNT  =  COUNT + 1
            IF ( COUNT .GE. MAX )  THEN
                CALL M3EXIT ( 'GETYN', 0, 0,
     &                        'Maximum error-count exceeded', IOS )
            END IF
            WRITE ( MESG, '( A, I9, 2X, A )' )
     &          'I/O ERROR:  I/O status = ' , IOS , 
     &          'Please try again.'
            CALL M3MSG2( MESG )
            WRITE ( MESG, '( A, I3, 2X, A )' )
     &          '(You are allowed', MAX-COUNT , 'more attempts.)'
            CALL M3MSG2( MESG )
            GO TO  11

        END FUNCTION GETYN 

