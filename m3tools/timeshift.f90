
PROGRAM TIMESHIFT

    !!******************************************************************
    !! Version "$Id: timeshift.f90 1 2017-06-10 18:05:20Z coats $"
    !! EDSS/Models-3 M3TOOLS.
    !! Copyright (C) 2014 UNC Institute for the Environment
    !! Distributed under the GNU GENERAL PUBLIC LICENSE version 2
    !! See file "GPL.txt" for conditions of use.
    !!..................................................................
    !!  program body starts at line  65
    !!
    !!  DESCRIPTION:
    !!      Increment date&time by time interval and echo
    !!      the result to standard output (e.g., for use in scripting).
    !!      If "--help" is an argument, writes the "USAGE" screen
    !!      and exits.
    !!
    !!  PRECONDITIONS REQUIRED:
    !!      none
    !!
    !!  SUBROUTINES AND FUNCTIONS CALLED:
    !!      M3IO
    !!
    !!  REVISION  HISTORY:
    !!      Prototype  2/2014 by CJC
    !!******************************************************************

    USE M3UTILIO

    IMPLICIT NONE

    !!...........   EXTERNAL FUNCTION:

    INTEGER :: IARGC

    !!...........   PARAMETERs:

    CHARACTER*16, PARAMETER :: PNAME = 'TIMESHIFT'

    CHARACTER*64, PARAMETER :: SPLASH( 15 ) = (/                      &
    'DESCRIPTION:  increment date&time by time interval.    ',        &
    '                                                       ',        &
    'USAGE: timeshift <YYYYDDD.HHMMSS> <HHMMSS>             ',        &
    '       timeshift --help                                ',        &
    'or (using back-quotes)                                 ',        &
    '%  set <var> = `timeshift <juldate> <ndays>`           ',        &
    '                                                       ',        &
    'EXAMPLE:                                               ',        &
    '   % set foo  = `timeshift 2011123.120000 240300       ',        &
    '     [${foo}  will be      2011124.123000]             ',        &
    '   % set date = $foo:r                                 ',        &
    '   % set time = $foo:e                                 ',        &
    '                                                       ',        &
    'yields ${date}==2011124 and ${time}==123000            ',        &
    '                                                       '  /)

    !!...........   LOCAL VARIABLES and their descriptions:

    INTEGER         DATE, TIME, STEP, C, L, I
    INTEGER         ARGCNT  !  number of command-line args, from IARGC()
    CHARACTER*80    SCRBUF

    !!******************************************************************
    !!   begin body of program TIMESHIFT

    ARGCNT = IARGC()

    IF ( ARGCNT .GE. 1 ) THEN

        CALL GETARG( 1, SCRBUF )
        CALL LUSTR( SCRBUF )

        IF ( SCRBUF .EQ. '--HELP' )  THEN
            WRITE( *,'( 5X, A )' ) ( SPLASH( I ), I = 1, 15 )
            CALL EXIT( 0 )
        END IF

    END IF

    IF ( ARGCNT .NE. 2 ) THEN
        WRITE( *,'( 5X, A )' ) 'USAGE:  timeshift <YYYYDDD.HHMMSS> <HHMMSS>'
        CALL EXIT( 2 )
    END IF

     C = MAX( INDEX( SCRBUF, '.' ), INDEX( SCRBUF, ':' ) )
     IF ( C .LE. 0 ) THEN
        WRITE( *,'( 5X, A )' )                                  &
            'USAGE:  timeshift <YYYYDDD.HHMMSS> <HHMMSS>',      &
            'Invalid argument<YYYYDDD.HHMMSS>'
        CALL EXIT( 2 )
    END IF

    L    = LEN( SCRBUF )
    DATE = STR2INT( SCRBUF( 1:C-1 ) )
    TIME = STR2INT( SCRBUF( C+1:L ) )
    IF ( DATE  .LT. -1000  .OR. DATE .GT. 9999999  .OR.        &
         TIME  .LT. -1000  .OR. TIME .GT. 9999999 ) THEN
        WRITE( *,'( 5X, A )' ) 'TIMESHIFT USAGE:  DATE and/or TIME out of range'
        CALL EXIT( 2 )
    END IF

    CALL GETARG( 2, SCRBUF )
    STEP = STR2INT( SCRBUF )
    IF ( STEP  .EQ. IMISS3 )  THEN
        WRITE( *,'( 5X, A )' ) 'TIMESHIFT USAGE:  time interval out of range'
        CALL EXIT( 2 )
    END IF

    CALL NEXTIME( DATE, TIME, STEP )
    WRITE( *,'( I7.7, A, I6.6 )' ) DATE, '.', TIME

    CALL EXIT( 0 )

END PROGRAM TIMESHIFT
