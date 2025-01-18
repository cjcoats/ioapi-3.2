
LOGICAL FUNCTION GETYN( PROMPT , DEFAULT )

    !******************************************************************
    ! Version "$Id: getyn.f90 203 2021-10-14 18:02:11Z coats $"
    ! EDSS/Models-3 I/O API.
    ! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
    ! (C) 2003-2013 Baron Advanced Meteorological Systems,
    ! (C) 2007-2013,2021 Carlie J. Coats, Jr., and
    ! (C) 2015 UNC Institute for the Environment.
    ! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    ! See file "LGPL.txt" for conditions of use.
    !.........................................................................
    !  function body begins at line 83
    !
    !  FUNCTION:
    !
    !       Safely get a Yes/No response from the keyboard (with
    !       default value ([RETURN] response) supplied from caller.
    !       Insensitive to case of input, Checks for validity of the
    !       response, and allows  MAX = 5 unsuccessful attempts, then
    !       exits.  Treats CONTROL-D (end-of-file) as exit signal.
    !
    !   RETURNS:  .TRUE. for a "Yes" response,  .FALSE.  for a "No"
    !
    !   SAMPLE USAGES (where FLAG is a LOGICAL   variable):
    !
    !       FLAG = GETYN ( 'To test this thing, give me a Y or N',
    !    &                  .TRUE. )        !  default response means TRUE
    !
    !       IF ( GETYN ( 'Another test:' , FLAG ) )
    !    &          THEN    !  default response: same as FLAG's value
    !         WRITE ( *,* ) 'Result of second test was TRUE'
    !       ELSE
    !         WRITE ( *,* ) 'Result of second test was FALSE'
    !       END IF
    !                               --Carlie J. Coats, Jr., 10/87
    !
    !  REVISION HISTORY:
    !
    !       Modified 5/1990 for ROM 2.2:  now uses EXWST for error abort.
    !       Modified 8/1990 for ROM 2.2:  TEMP.LET; treats CONTROL-Z as exit signal
    !       Modified 2/1993 by CJC for CRAY.
    !       Modified 8/1996 by CJC:  "!" treated as terminator for input
    !       Modified 1/1997 by CJC:  logs result
    !       Modified 8/1997 by MH:   environment variable PROMPTFLAG
    !       Modified 4/2002 by CJC:  now accepts T, t, F, f, .TRUE., .FALSE., etc.
    !       Revised  6/2003 by CJC:  factor through M3PROMPT to ensure flush()
    !       of PROMPT for IRIX F90v7.4
    !       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
    !       Modified 02/2015 by CJC for I/O API 3.2: USE M3UTILIO.
    !       Fix MH violation of coding-standards:  check status IOS from  ENVYN!!
    !       Version 10/2021 by CJC:  free ".f90" source format for IOAPI-4.0
    !***********************************************************************

    IMPLICIT NONE

    !.......   Arguments:

    CHARACTER*(*), INTENT(IN   ) :: PROMPT      !!  prompt for user
    LOGICAL      , INTENT(IN   ) :: DEFAULT     !!  default return value

    !.......   External functions:

    LOGICAL, EXTERNAL :: ENVYN

    !.......   Parameter:  maximum number of attempts allowed to the user

    INTEGER     , PARAMETER :: MAX   = 5
    CHARACTER*16, PARAMETER :: PNAME = 'GETYN'

    !.......   Local Variables:

    INTEGER         COUNT , IOS
    CHARACTER*80    ANSWER
    CHARACTER*256   MESG

    LOGICAL, SAVE :: PROMPTON
    LOGICAL, SAVE :: FIRSTIME = .TRUE.


    !*********************   begin  GETYN   *******************************

    IF( FIRSTIME ) THEN

        PROMPTON = ENVYN( 'PROMPTFLAG', 'Prompt for input flag', .TRUE., IOS )
        IF ( IOS .GT. 0 ) THEN
            CALL M3EXIT( PNAME,0,0,'Bad env vble "PROMPTFLAG"', 2 )
        END IF
        FIRSTIME = .FALSE.

    END IF

    IF( .NOT. PROMPTON ) THEN
        GETYN = DEFAULT
        IF ( DEFAULT ) THEN
            CALL M3MSG2( 'Returning default value TRUE for query:')
        ELSE
            CALL M3MSG2('Returning default value FALSE for query:')
        END IF
        MESG = '"' // TRIM( PROMPT ) // '"'
        CALL M3MSG2( MESG )
        RETURN
    END IF

    !.....  Continue only if PROMPTON is true

    COUNT = 0

11  CONTINUE
    ANSWER  =  '    '

    IF ( DEFAULT ) THEN
        MESG = TRIM( PROMPT ) // ' (Y/N) [Y] >> '
    ELSE
        MESG = TRIM( PROMPT ) // ' (Y/N) [N] >> '
    END IF
    CALL M3PROMPT( MESG, ANSWER, IOS )


    IF ( IOS .NE. 0 ) THEN

        GO TO  900

    ELSE IF (     ( ANSWER ( 1:1 ) .EQ. 'Y' )       &
            .OR.  ( ANSWER ( 1:1 ) .EQ. 'y' )       &
            .OR.  ( ANSWER ( 1:1 ) .EQ. 'T' )       &
            .OR.  ( ANSWER ( 1:1 ) .EQ. 't' )       &
            .OR.  ( ANSWER ( 1:2 ) .EQ.'.T' )       &
            .OR.  ( ANSWER ( 1:2 ) .EQ.'.t' ) )  THEN

        GETYN  =  .TRUE.
        CALL M3MSG2( 'Returning value TRUE for query:')

    ELSE IF (     ( ANSWER ( 1:1 ) .EQ. 'N' )       &
            .OR.  ( ANSWER ( 1:1 ) .EQ. 'n' )       &
            .OR.  ( ANSWER ( 1:1 ) .EQ. 'F' )       &
            .OR.  ( ANSWER ( 1:1 ) .EQ. 'f' )       &
            .OR.  ( ANSWER ( 1:2 ) .EQ.'.F' )       &
            .OR.  ( ANSWER ( 1:2 ) .EQ.'.f' ) )  THEN

        GETYN  =  .FALSE.
        CALL M3MSG2( 'Returning value FALSE for query:')

    ELSE IF  (    ( ANSWER ( 1:1 ) .EQ. ' ' )       &
             .OR. ( ANSWER ( 1:1 ) .EQ. '!' ) )  THEN

        GETYN = DEFAULT
        IF ( DEFAULT ) THEN
            CALL M3MSG2( 'Returning default value TRUE for query:')
        ELSE
            CALL M3MSG2('Returning default value FALSE for query:')
        END IF

    ELSE

        COUNT  =  COUNT + 1
        IF ( COUNT .GE. MAX )  THEN
            CALL M3EXIT( 'GETYN', 0, 0, 'Maximum number of attempts exceeded', 2 )
        END IF
        MESG='Did not understand your response; Please try again.'
        CALL M3MSG2( MESG )
        WRITE ( MESG, '( A, I3, 2X, A )' ) '(You are allowed', MAX - COUNT, 'more attempts.)'
        CALL M3MSG2( MESG )
        GO TO  11

    END IF

    MESG = '"' // TRIM( PROMPT ) // '"'
    CALL M3MSG2( MESG )
    RETURN

900 CONTINUE	!  error reading response

    COUNT  =  COUNT + 1
    IF ( COUNT .GE. MAX )  THEN
        CALL M3EXIT ( 'GETYN', 0, 0, 'Maximum error-count exceeded', IOS )
    END IF
    WRITE ( MESG, '( A, I9, 2X, A )' ) 'I/O ERROR:  I/O status = ' , IOS , 'Please try again.'
    CALL M3MSG2( MESG )
    WRITE ( MESG, '( A, I3, 2X, A )' ) '(You are allowed', MAX-COUNT , 'more attempts.)'
    CALL M3MSG2( MESG )
    GO TO  11

END FUNCTION GETYN

