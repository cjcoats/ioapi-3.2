
PROGRAM JUL2GREG

    !!******************************************************************
    !! Version "$Id: jul2greg.f90 1 2017-06-10 18:05:20Z coats $"
    !! EDSS/Models-3 M3TOOLS.
    !! Copyright (C) 2014 UNC Institute for the Environment
    !! Distributed under the GNU GENERAL PUBLIC LICENSE version 2
    !! See file "GPL.txt" for conditions of use.
    !!..................................................................
    !!  program body starts at line  65
    !!
    !!  DESCRIPTION:
    !!      Convert Julian-style dates to Gregorian-style and echo
    !!       the result to standard output (e.g., for use in scripting).
    !!       If "--help" is an argument, writes the "USAGE" screen
    !!       and exits.
    !!
    !!  PRECONDITIONS REQUIRED:
    !!       none
    !!
    !!  SUBROUTINES AND FUNCTIONS CALLED:
    !!       M3IO
    !!
    !!  REVISION  HISTORY:
    !!      Prototype  2/2014 by CJC
    !!******************************************************************

    USE M3UTILIO

    IMPLICIT NONE

    !!...........   EXTERNAL FUNCTION:

    INTEGER :: IARGC

    !!...........   PARAMETERs:

    CHARACTER*16, PARAMETER :: PNAME = 'JUL2GREG'

    CHARACTER*64, PARAMETER :: SPLASH( 12 ) = (/            &
    '%  jul2greg <Julian date>                      ',      &
    'or                                             ',      &
    '%  set gdate = `jul2greg <calendar date>`      ',      &
    '                                               ',      &
    'Options for Julian date:                       ',      &
    '     <YYYYDDD>,  e.g., 2010123                 ',      &
    '     TODAY                                     ',      &
    '     YESTERDAY                                 ',      &
    '     TOMORROW                                  ',      &
    '     --HELP                                    ',      &
    '                                               ',      &
    'Output format is 8-digit integer YYYYMMDD      '  /)

    !!...........   LOCAL VARIABLES and their descriptions:

    INTEGER         I, DATE, DAY, MON, YEAR, JTIME
    INTEGER         ARGCNT  !  number of command-line args, from IARGC()
    CHARACTER*80    SCRBUF, LINE


    !!***********************************************************************
    !!   begin body of program JUL2GREG

    ARGCNT = IARGC()
    CALL GETDTTIME( DATE, JTIME )

    IF ( ARGCNT .NE. 1 ) THEN
        WRITE( *, '( 5X, A )' ) 'USAGE ERROR:', ( SPLASH( I ), I = 1, 12 )
        CALL EXIT( 2 )
    END IF
    CALL GETARG( 1, SCRBUF )
    CALL LUSTR( SCRBUF )
    IF ( SCRBUF(1:6) .EQ. '--HELP' ) THEN
        WRITE( *, '( 5X, A )' )                                     &
              'DESCRIPTION:  convert Julian dates to Gregorian',    &
              'USAGE:', ( SPLASH( I ), I = 1, 11 )
    ELSE IF ( SCRBUF .EQ. 'TODAY' ) THEN
        CONTINUE
    ELSE IF ( SCRBUF .EQ. 'YESTERDAY' ) THEN
        CALL NEXTIME( DATE, JTIME, -240000 )
    ELSE IF ( SCRBUF .EQ. 'TOMORROW' ) THEN
        CALL NEXTIME( DATE, JTIME, 240000 )
    ELSE
        DATE = STR2INT( SCRBUF )
    END IF

    IF ( DATE  .LT. -1000  .OR. DATE  .GT. 9999999 ) THEN
        LINE =  'Date for "' //  TRIM( SCRBUF ) // '" out-of-range'
        WRITE( *, '( 5X, A )' ) LINE
        CALL EXIT( 2 )
    END IF

    YEAR = DATE / 1000
    CALL DAYMON( DATE, MON, DAY )
    WRITE( *,'( I8.8)' ) DAY + 100 * ( MON + 100 * YEAR )

    CALL EXIT( 0 )

END PROGRAM JUL2GREG
