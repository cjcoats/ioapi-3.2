
SUBROUTINE GETSTR( PROMPT, DEFAULT, RESPONSE )

    !******************************************************************
    ! Version "$Id: getstr.f90 203 2021-10-14 18:02:11Z coats $"
    ! EDSS/Models-3 I/O API.
    ! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
    ! (C) 2003-2013 Baron Advanced Meteorological Systems,
    ! (C) 2007-2013,2021 Carlie J. Coats, Jr., and
    ! (C) 2015 UNC Institute for the Environment.
    ! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    ! See file "LGPL.txt" for conditions of use.
    !.........................................................................
    !  function body begins at line 80
    !
    !  FUNCTION:
    !
    !       Safely get a character-string response from the keyboard (with
    !       default value (<RETURN> response) supplied from caller.
    !       Checks for validity of the response, and allows  MAX = 5
    !       unsuccessful attempts, then exits.
    !       Treats CONTROL-D (end-of-file) as exit signal.
    !       Treats ! as initiation of a comment
    !
    !  ARGUMENT LIST DESCRIPTION:
    !
    !    Input arguments:
    !
    !         PROMPT    prompt for user
    !         DEFAULT   default value
    !
    !    Output arguments: none
    !
    !         RESPONSE  return value
    !
    !  REVISION HISTORY:
    !
    !       Prototype 9/1994 by CJC
    !       Modified  1/1997 by CJC:  logs result
    !       Revised   5/2003 by CJC:  factor through M3MSG2 to ensure flush()
    !       of log-messages
    !       Revised 6/2003 by CJC:  factor through M3PROMPT to ensure flush()
    !       of PROMPT for IRIX F90v7.4
    !       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
    !       Modified 02/2015 by CJC for I/O API 3.2: USE M3UTILIO.
    !       Fix MH violation of coding-standards:  check status IOS from  ENVYN!!
    !       Version 10/2021 by CJC:  free ".f90" source format for IOAPI-4.0
    !**********************************************************************

    IMPLICIT NONE

    !.......   Arguments:

    CHARACTER*(*), INTENT(IN   ) :: PROMPT, DEFAULT
    CHARACTER*(*), INTENT(  OUT) :: RESPONSE

    !.......   Parameter:  maximum number of attempts allowed to the user

    INTEGER,      PARAMETER :: MAX = 5
    CHARACTER*16, PARAMETER :: PNAME   = 'GETSTR'

    !.......   External functions:

    LOGICAL, EXTERNAL :: ENVYN
    INTEGER, EXTERNAL :: LBLANK

    !.......   Local Variables:

    INTEGER         A1, P1, D1, R1, EE
    INTEGER         A0, P0, D0
    INTEGER         COUNT , IOS
    CHARACTER*256   ANSWER, MESG

    LOGICAL, SAVE :: PROMPTON
    LOGICAL, SAVE :: FIRSTIME = .TRUE.


    !*********************   begin  GETSTR   *******************************

    IF( FIRSTIME ) THEN

        PROMPTON = ENVYN( 'PROMPTFLAG', 'Prompt for input flag', .TRUE., IOS )
        IF ( IOS .GT. 0 ) THEN
            CALL M3EXIT( PNAME,0,0,'Bad env vble "PROMPTFLAG"', 2 )
        END IF
        FIRSTIME = .FALSE.

    END IF

    P0    = LBLANK ( PROMPT  ) + 1	!  initial position (first nonblank)
    D0    = LBLANK ( DEFAULT ) + 1	!  initial position (first nonblank)
    P1    = LEN_TRIM( PROMPT  )	!  last nonblank
    D1    = LEN_TRIM( DEFAULT )	!  last nonblank
    R1    = LEN( RESPONSE )

    IF( .NOT. PROMPTON ) THEN
        IF ( D1 - D0 + 1 .GT. R1 ) THEN
            RESPONSE = DEFAULT( D0:R1 + D0 - 1 )
            MESG = 'Default "' // DEFAULT( D0:D1 ) // ' truncated to "' // DEFAULT( D0:R1 + D0 - 1 ) // '" for query'
            CALL M3WARN( 'GETSTR', 0, 0, MESG )
            MESG = '"' // PROMPT( P0:P1 ) // '"'
            CALL M3MESG( MESG )
        ELSE
            RESPONSE = DEFAULT( D0:D1 )
            MESG = 'Using default "' // DEFAULT( D0:D1 ) // '" for query:'
            CALL M3MSG2( MESG )
            MESG = '"' // PROMPT( P0:P1 ) // '"'
            CALL M3MSG2( MESG )
        END IF
        RETURN
    END IF

    COUNT = 0

11  CONTINUE
    ANSWER  =  '    '
    MESG = PROMPT( P0:P1 )//' ["' //DEFAULT( D0:D1 )// '"] >> '
    CALL M3PROMPT( MESG, ANSWER, IOS )
    IF ( IOS .NE. 0 ) GO TO  900

    EE = INDEX( ANSWER, '!' )
    IF ( EE .NE. 0 ) ANSWER( EE : LEN( ANSWER ) ) = ' '

    A0 = LBLANK( ANSWER ) + 1		!  first nonblank
    IF ( A0 .GT. LEN( ANSWER ) ) THEN	!  all blanks
        IF ( D1 - D0 + 1 .GT. R1 ) THEN
            RESPONSE = DEFAULT( D0:R1 + D0 - 1 )
            MESG = 'Default "' // DEFAULT( D0:D1 )  // ' truncated to "' // DEFAULT( D0:R1 + D0 - 1 ) // '"'
            CALL M3WARN( 'GETSTR', 0, 0, MESG )
        ELSE
            RESPONSE = DEFAULT( D0: D1 )
            MESG = 'Using default "'// DEFAULT( D0:D1 ) // '"'
        END IF
    ELSE
        A1 = LEN_TRIM( ANSWER )
        IF ( A1 - A0 + 1 .GT. R1 ) THEN
            RESPONSE = ANSWER( A0:R1 + A0 - 1 )
            MESG = 'Response "' // ANSWER( A0:A1 ) // ' truncated to "' // ANSWER( A0:R1 + A0 - 1 ) // '"'
            CALL M3WARN( 'GETSTR', 0, 0, MESG )
        ELSE
            RESPONSE = ANSWER( A0:A1 )
            MESG = 'Using response "' // ANSWER( A0:A1 ) // '"'
        END IF
    END IF
    CALL M3MSG2( MESG )

    RETURN

900 CONTINUE	!  error reading response

    COUNT  =  COUNT + 1
    IF ( COUNT .GE. MAX )  THEN
        CALL  M3EXIT( 'GETSTR', 0, 0, 'Maximum number of attempts exceeded.',IOS )
    END IF
    WRITE ( 6,92020 ) 'Did not understand your response; ' ,&
            'I/O ERROR:  I/O status = ' , IOS ,             &
            'Please try again (You are allowed ' ,          &
            MAX - COUNT , ' more attempts)'
    CALL M3FLUSH( 6 )
    GO TO  11


    !******************  FORMAT  STATEMENTS   ******************************

    !...........   Informational (LOG) message formats... 92xxx

92020 FORMAT ( /5X , A , /5X , A , I3 , /5X , A , I1 , A )

END SUBROUTINE GETSTR

