
PROGRAM JULDIFF

    !!******************************************************************
    !! Version "$Id: juldiff.f90 125 2019-09-03 14:51:51Z coats $"
    !! EDSS/Models-3 M3TOOLS.
    !! Copyright (C) 2014 UNC Institute for the Environment
    !! Distributed under the GNU GENERAL PUBLIC LICENSE version 2
    !! See file "GPL.txt" for conditions of use.
    !!..................................................................
    !!  program body starts at line  51
    !!
    !!  DESCRIPTION:
    !!       Takes two command line arguments JDATE, KDATE, computes
    !!       the number of days from JDATE to KDATE, and echo
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

    CHARACTER*16, PARAMETER :: PNAME = 'JULDIFF'

    !!...........   LOCAL VARIABLES and their descriptions:

    INTEGER         I, J, IDATE, JDATE, KDATE, ITIME, IDAYS
    INTEGER         ARGCNT  !  number of command-line args, from IARGC()
    CHARACTER*80    SCRBUF

    !!******************************************************************
    !!   begin body of program JULDIFF

    CALL GETDTTIME( IDATE, ITIME )      !!  "now"

    !!...........   process the command line arguments

    ARGCNT = IARGC()
    IF ( ARGCNT .GE. 1 ) THEN
        CALL GETARG( 1, SCRBUF )
        JDATE = BUF2DATE( SCRBUF, IDATE )
    END IF

    IF ( ARGCNT .EQ. 2 ) THEN
        CALL GETARG( 2, SCRBUF )
        KDATE = BUF2DATE( SCRBUF, IDATE )
    ELSE
        WRITE( *,'( 5X, A )' ) 'USAGE:  "juldiff <JDATE> <KDATE>"'
        CALL EXIT( 2 )
    END IF

    !!...........   normalize the dates (so DDD component is within range)

    ITIME = 0
    CALL NEXTIME( JDATE, ITIME, 0 )
    CALL NEXTIME( KDATE, ITIME, 0 )

    !!...........   Number of days, in terms of 24-hour timestep sequence:

    !!  WRITE( *,'( 2( A, I10, 2X ) )' ) 'JDATE=',JDATE, 'KDATE=',KDATE
    IF ( JDATE .LT. KDATE ) THEN
        IDAYS = -1 + CURREC( KDATE, 0, JDATE, 0, 240000, I, J )
    ELSE
        IDAYS =  1 - CURREC( JDATE, 0, KDATE, 0, 240000, I, J )
    END IF

    WRITE( *,'( I10 )' ) IDAYS

    CALL EXIT( 0 )


  CONTAINS  !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    INTEGER FUNCTION  BUF2DATE( ABUF, IDATE )

        CHARACTER*(*), INTENT(INOUT) :: ABUF
        INTEGER      , INTENT(IN   ) :: IDATE

        CHARACTER*64, PARAMETER :: SPLASH( 12 ) = (/                    &
          '%  juldiff  <Julian date 1>  <Julian date 2>       ',        &
          'or                                                 ',        &
          '%  set ndays = `JULDIFF <juldate1>  <juldate2>`    ',        &
          '                                                   ',        &
          'Options for Julian dates:                          ',        &
          '     <YYYYDDD>,  e.g., 2010123                     ',        &
          '     TODAY                                         ',        &
          '     YESTERDAY                                     ',        &
          '     TOMORROW                                      ',        &
          '     --HELP                                        ',        &
          '                                                   ',        &
          'Output format is integer number of days.           '  /)

        INTEGER     I, ADATE, ITIME

        CALL LUSTR( ABUF )
        ITIME = 0

        IF ( ABUF .EQ. '--HELP' ) THEN

            WRITE( *,'( 5X, A )' )                              &
                'DESCRIPTION:  # days from <JDATE> to <KDATE>', &
                'USAGE:', ( SPLASH( I ), I = 1, 12 )
            CALL EXIT( 0 )

        ELSE IF ( ABUF .EQ. 'TODAY' ) THEN

            ADATE = IDATE

        ELSE IF ( ABUF .EQ. 'YESTERDAY' ) THEN

            ADATE = IDATE
            CALL NEXTIME( ADATE, ITIME, -240000 )

        ELSE IF ( ABUF .EQ. 'TOMORROW' ) THEN

            ADATE = IDATE
            CALL NEXTIME( ADATE, ITIME, 240000 )

        ELSE

            ADATE = STR2INT( ABUF )
            IF ( ADATE .LT.-1000 .OR. ADATE .GT. 9999999 ) THEN
                WRITE( *,'( 5X, A )' ) 'JULDIFF date out of range: ', ABUF
                CALL EXIT( 2 )
            END IF

        END IF

        BUF2DATE = ADATE

    END FUNCTION BUF2DATE

END PROGRAM JULDIFF

