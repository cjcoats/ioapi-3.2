
PROGRAM TIMEDIFF

    !!******************************************************************
    !! Version "$Id: timediff.f90 146 2020-03-25 18:03:32Z coats $"
    !! EDSS/Models-3 M3TOOLS.
    !! Copyright (C) 2014 UNC Institute for the Environment
    !! Distributed under the GNU GENERAL PUBLIC LICENSE version 2
    !! See file "GPL.txt" for conditions of use.
    !!..................................................................
    !!  program body starts at line  65
    !!
    !!  DESCRIPTION:
    !!      Difference two date&time values and echo the result
    !!      in seconds to standard output (e.g., for use in scripting).
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

    CHARACTER*16, PARAMETER :: PNAME = 'TIMEDIFF'

    CHARACTER*80, PARAMETER :: SPLASH( 11 ) = (/                        &
    'DESCRIPTION:  difference two date&time values, in seconds.   ',    &
    '                                                             ',    &
    'USAGE: TIMEDIFF <YYYYDDD.HHMMSS> <YYYYDDD.HHMMSS>            ',    &
    '       TIMEDIFF --help                                       ',    &
    'or (using back-quotes)                                       ',    &
    '%  set <var> = `timediff <juldate1.hhmmss> <juldate2.hhmmss>`',    &
    '                                                             ',    &
    'EXAMPLE:                                                     ',    &
    '   % set foo  = `TIMEDIFF 2019123.120000 2019124.130000      ',    &
    '     [${foo}  will be    9000 (= 25 hours*3600 sec/hour)]    ',    &
    '                                                             '  /)

    !!...........   LOCAL VARIABLES and their descriptions:

    INTEGER         JDATE, JTIME, KDATE, KTIME, STEP, C, L, I
    INTEGER         ARGCNT  !  number of command-line args, from IARGC()
    CHARACTER*256   SCRBUF

    !!******************************************************************
    !!   begin body of program TIMEDIFF

    ARGCNT = IARGC()

    IF ( ARGCNT .GE. 1 ) THEN

        CALL GETARG( 1, SCRBUF )
        CALL LUSTR( SCRBUF )

        IF ( SCRBUF .EQ. '--HELP' )  THEN
            WRITE( *,'( 5X, A )' ) ( SPLASH( I ), I = 1, 11 )
            CALL EXIT( 0 )
        END IF

    END IF

    IF ( ARGCNT .NE. 2 ) THEN
        WRITE( *,'( 5X, A )' ) 'USAGE:  TIMEDIFF <YYYYDDD.HHMMSS> <YYYYDDD.HHMMSS>'
        CALL EXIT( 2 )
    END IF

    C = MAX( INDEX( SCRBUF, '.' ), INDEX( SCRBUF, ':' ) )
    IF ( C .LE. 0 ) THEN
        WRITE( *,'( 5X, A )' )                                      &
            'USAGE:  TIMEDIFF <YYYYDDD.HHMMSS> <YYYYDDD.HHMMSS>',   &
            'Invalid first argument ' // SCRBUF
        CALL EXIT( 2 )
    END IF

    L     = LEN( SCRBUF )
    JDATE = STR2INT( SCRBUF( 1:C-1 ) )
    JTIME = STR2INT( SCRBUF( C+1:L ) )
    IF ( JDATE  .LT. -1000  .OR. JDATE .GT. 9999999  .OR.        &
         JTIME  .LT. -1000  .OR. JTIME .GT. 9999999 ) THEN
        WRITE( *,'( 5X, A )' ) 'TIMEDIFF USAGE: first DATE and/or TIME out of range'
        CALL EXIT( 2 )
    END IF

    CALL GETARG( 2, SCRBUF )
    C = MAX( INDEX( SCRBUF, '.' ), INDEX( SCRBUF, ':' ) )
    IF ( C .LE. 0 ) THEN
        WRITE( *,'( 5X, A )' )                                      &
            'USAGE:  TIMEDIFF <YYYYDDD.HHMMSS> <YYYYDDD.HHMMSS>',   &
            'Invalid second argument ' // SCRBUF
        CALL EXIT( 2 )
    END IF

    L     = LEN( SCRBUF )
    KDATE = STR2INT( SCRBUF( 1:C-1 ) )
    KTIME = STR2INT( SCRBUF( C+1:L ) )
    IF ( KDATE  .LT. -1000  .OR. KDATE .GT. 9999999  .OR.        &
         KTIME  .LT. -1000  .OR. KTIME .GT. 9999999 ) THEN
        WRITE( *,'( 5X, A )' ) 'TIMEDIFF USAGE: second DATE and/or TIME out of range'
        CALL EXIT( 2 )
    END IF

    STEP = SECSDIFF( JDATE, JTIME, KDATE, KTIME )
    WRITE( *,'( I10 )' ) STEP

    CALL EXIT( 0 )

END PROGRAM TIMEDIFF
