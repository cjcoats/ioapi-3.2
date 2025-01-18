
PROGRAM JULSHIFT

    !!******************************************************************
    !! Version "$Id: julshift.f90 1 2017-06-10 18:05:20Z coats $"
    !! EDSS/Models-3 M3TOOLS.
    !! Copyright (C) 2014 UNC Institute for the Environment
    !! Distributed under the GNU GENERAL PUBLIC LICENSE version 2
    !! See file "GPL.txt" for conditions of use.
    !!..................................................................
    !!  program body starts at line  62
    !!
    !!  DESCRIPTION:
    !!      To command-line argument <YYYYDDD> add command line argument
    !!       <DAYS> and echo the result (e.g., for use in scripting).
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
    !!      Version    2/2015 by CJC to handle negative dates
    !!******************************************************************

    USE M3UTILIO

    IMPLICIT NONE

    !!...........   EXTERNAL FUNCTION:

    INTEGER :: IARGC

    !!...........   PARAMETERs:

    CHARACTER*16, PARAMETER :: PNAME = 'JULSHIFT'

    CHARACTER*64, PARAMETER :: SPLASH( 12 ) = (/            &
    '%  julshift <Julian date>  <number of days>    ',      &
    'or                                             ',      &
    '%  set jdate = `julshift <juldate>  <ndays>`   ',      &
    '                                               ',      &
    'Options for Julian date:                       ',      &
    '     <YYYYDDD>,  e.g., 2010123                 ',      &
    '     TODAY                                     ',      &
    '     YESTERDAY                                 ',      &
    '     TOMORROW                                  ',      &
    '     --HELP                                    ',      &
    '                                               ',      &
    'Output format is 7-digit integer YYYYDDD       '  /)

    !!...........   LOCAL VARIABLES and their descriptions:

    INTEGER         I, DATE, STEP, DAYS, JDATE, JTIME
    INTEGER         ARGCNT  !  number of command-line args, from IARGC()
    CHARACTER*80    SCRBUF

    !!******************************************************************
    !!   begin body of program JULSHIFT

    ARGCNT = IARGC()
    CALL GETDTTIME( JDATE, JTIME )

    IF ( ARGCNT .GE. 1 ) THEN

        CALL GETARG( 1, SCRBUF )
        CALL LUSTR( SCRBUF )

        IF ( SCRBUF .EQ. '--HELP' )  THEN

            WRITE( *,'( 5X, A )' )                              &
                'DESCRIPTION:  increment <JDATE> by <NDAYS>',   &
                'USAGE:', ( SPLASH( I ), I = 1, 12 )
            CALL EXIT( 0 )

        ELSE IF ( SCRBUF .EQ. 'TODAY' ) THEN

            DATE = JDATE

        ELSE IF ( SCRBUF .EQ. 'YESTERDAY' ) THEN

            DATE = JDATE
            CALL NEXTIME( DATE, JTIME, -240000 )

        ELSE IF ( SCRBUF .EQ. 'TOMORROW' ) THEN

            DATE = JDATE
            CALL NEXTIME( DATE, JTIME, 240000 )

        ELSE

            DATE = STR2INT( SCRBUF )
            IF ( DATE  .LT. -1000 .OR. DATE .GT. 9999999 ) THEN
                WRITE( *,'( 5X, A )' ) 'JULSHIFT USAGE:  date out of range'
                CALL EXIT( 2 )
            END IF

        END IF

    ELSE

        WRITE( *,'( 5X, A )' ) 'USAGE ERROR:', ( SPLASH( I ), I = 1, 11 )
        CALL EXIT( 2 )

    END IF

    IF ( ARGCNT .EQ. 2 ) THEN

            CALL GETARG( 2, SCRBUF )
            STEP = STR2INT( SCRBUF )
            IF ( STEP  .EQ. IMISS3 )  THEN
                WRITE( *,'( 5X, A )' ) 'JULSHIFT USAGE:  <ndays> invalid'
                CALL EXIT( 2 )
            END IF

    END IF

11  CONTINUE        !!  loop in overflow-safe chunks

        IF ( STEP .GT. 365 ) THEN
            CALL NEXTIME( DATE, JTIME, 240000 * 365 )
            STEP = STEP - 365
            GO TO  11
        ELSE IF ( STEP .LT. -365 ) THEN
            CALL NEXTIME( DATE, JTIME, -240000 * 365 )
            STEP = STEP + 365
            GO TO  11
        ELSE
            CALL NEXTIME( DATE, JTIME, 240000 * STEP )
        END IF

    WRITE( SCRBUF,'( I9.7 )' ) DATE
    WRITE( *,'( A )' ) ADJUSTL( SCRBUF )

    CALL EXIT( 0 )

END PROGRAM JULSHIFT
