
PROGRAM JULDATE

    !!***********************************************************************
    !! Version "$Id: juldate.f90 1 2017-06-10 18:05:20Z coats $"
    !! EDSS/Models-3 M3TOOLS.
    !! Copyright (C) 1992-2002 MCNC, (C) 1995-2002,2005-2013 Carlie J. Coats, Jr.,
    !! and (C) 2002-2010 Baron Advanced Meteorological Systems. LLC.
    !! Distributed under the GNU GENERAL PUBLIC LICENSE version 2
    !! See file "GPL.txt" for conditions of use.
    !!.........................................................................
    !!  program body starts at line  72
    !!
    !!  DESCRIPTION:
    !!       interactively month, day, year;
    !!       get julian date YYYYDD back.
    !!
    !!  PRECONDITIONS REQUIRED:
    !!       none
    !!
    !!  SUBROUTINES AND FUNCTIONS CALLED:
    !!       JULIAN, GETNUM
    !!
    !!  REVISION  HISTORY:
    !!       Prototype  8/95 by CJC
    !!       Enhanced   6/98 to support YESTERDAY, TODAY, TOMORROW.
    !!       Version 11/2001 by CJc for I/O API Version 2.1
    !!
    !!       Version 02/2010 by CJC for I/O API v3.1:  Fortran-90 only;
    !!       USE M3UTILIO, and related changes.
    !!
    !!       Version 06/2011 by CJC: Fortran-90 for I/O API 3.1
    !!
    !!       Version 11/2013 by CJC:  Add "juldate --help" options
    !!***********************************************************************

    USE M3UTILIO

    IMPLICIT NONE

    !!...........   EXTERNAL FUNCTIONS and their descriptions:

    INTEGER :: IARGC

    !!.......   PARAMETERs:  Lookup table for months, days:

    INTEGER, PARAMETER :: MLENS ( 12 ) =         &
          (/  31 , 29 , 31, 30 , 31 , 30,        &
              31 , 31 , 30, 31 , 30 , 31  /)

    CHARACTER*3, PARAMETER :: MONTHS ( 12 ) =                   &
          (/ 'JAN' , 'FEB' , 'MAR', 'APR' , 'MAY' , 'JUN',      &
             'JUL' , 'AUG' , 'SEP', 'OCT' , 'NOV' , 'DEC'  /)

    CHARACTER*10, PARAMETER :: DAYS( 7 ) =                      &
       (/ 'Monday   ', 'Tuesday  ', 'Wednesday',                &
          'Thursday ', 'Friday   ', 'Saturday ', 'Sunday   '  /)

    !!...........   LOCAL VARIABLES and their descriptions:

    INTEGER         I, ISTAT, MON, DAY, YR
    INTEGER         JDATE, JTIME
    INTEGER         ARGCNT  !  number of command-line args, from IARGC()
    LOGICAL         PFLAG
    CHARACTER*80    MONBUF, DAYBUF, YRBUF
    CHARACTER*80 :: SCRBUF = ' '


    !!***********************************************************************
    !!   begin body of program JULDATE

    CALL GETDTTIME( JDATE, JTIME )
    ARGCNT = IARGC()
    IF ( ARGCNT .EQ. 1 ) THEN
        CALL GETARG( ARGCNT, SCRBUF )
        CALL LUSTR( SCRBUF )
    END IF

    IF ( SCRBUF .EQ. '--HELP' .OR.  &
         SCRBUF .EQ. '?'      .OR.  &
         SCRBUF .EQ. '-?' ) THEN
        WRITE( *,'( 5X, A )' ) ' ', ' ',                              &
 'Program JULDATE takes calendar date (in form Month DD YYYY)',       &
 'and returns the date in Julian-date form "YYYYDDD".',               &
 ' ',                                                                 &
 'USAGE:',                                                            &
'    juldate [<MONTH DAY YEAR> |',                                    &
'             <YYYYMMDD>       |',                                    &
'             YESTERDAY        |',                                    &
'             TODAY            |',                                    &
'             TOMORROW]',                                             &
' ',                                                                  &
 'If the command-line arguments are missing, prompts the ',           &
 'user for them.',                                                    &
 ' ',                                                                 &
'See URL  http://www.baronams.com/products/ioapi/AA.html#tools',      &
' ',                                                                  &
'Program copyright (C) 1992-2002 MCNC,',                              &
'(C) 1995-2013 Carlie J. Coats, Jr., and (C) 2002-2010 Baron',        &
'Advanced Meteorological Systems, LLC.  Released under Version 2',    &
'of the GNU General Public License. See enclosed GPL.txt, or URL',    &
''  ,                                                                 &
'    https://www.gnu.org/licenses/old-licenses/gpl-2.0.html',         &
''  ,                                                                 &
'Comments and questions are welcome and can be sent to',              &
'',                                                                   &
'    Carlie J. Coats, Jr.    carlie@jyarborough.com',                 &
'or',                                                                 &
'    UNC Institute for the Environment',                              &
'    100 Europa Dr., Suite 490 Rm 405',                               &
'    Campus Box 1105',                                                &
'    Chapel Hill, NC 27599-1105',                                     &
'',                                                                   &
'See URL',                                                            &
'https://www.cmascenter.org/ioapi/documentation/3.1/html#tools',      &
' ',                                                                  &
'Program version: ',                                                  &
'$Id:: juldate.f90 1 2017-06-10 18:05:20Z coats   $',&
' '
        CALL EXIT( 0 )
    END IF      !!  if


    IF ( ARGCNT .EQ. 1 ) THEN

        CALL GETARG( 1, MONBUF )
        CALL LUSTR( MONBUF )
        IF ( MONBUF .EQ. 'TODAY' ) THEN
            GO TO  99
        ELSE IF ( MONBUF .EQ. 'YESTERDAY' ) THEN
            CALL NEXTIME( JDATE, JTIME, -240000 )
            GO TO  99
        ELSE IF ( MONBUF .EQ. 'TOMORROW' ) THEN
            CALL NEXTIME( JDATE, JTIME, 240000 )
            GO TO  99
        ELSE
            YR    = STR2INT( MONBUF )
            DAY   = MOD( YR , 100 )
            YR    =      YR / 100
            MON   = MOD( YR , 100 )
            YR    =      YR / 100
            JDATE = 1000 * YR  +  JULIAN( YR, MON, DAY )
            IF ( MON    .GT.   12  .OR.        &
                 MON    .LT.    1  .OR.        &
                 DAY    .GT.   31  .OR.        &
                 DAY    .LT.    1  .OR.        &
                 YR     .LT. 1000  .OR.        &
                 YR     .GT. 9999 )  ISTAT = 1  !  malformed input:  prompt user
            GO TO  99
        END IF

        PFLAG = .TRUE.      !  malformed input:  prompt user

    ELSE IF ( ARGCNT .EQ. 3 ) THEN

        CALL GETARG( 1, MONBUF )
        CALL LUSTR( MONBUF )
        DO  11  MON = 1, 12
            IF ( INDEX( MONBUF, MONTHS( MON ) ) .GT. 0 ) THEN
                GO TO 12
            END IF
11      CONTINUE
        MON = STR2INT( MONBUF )
12      CONTINUE        !  month found by name

        CALL GETARG( 2, DAYBUF )
        DAY = STR2INT( DAYBUF )

        CALL GETARG( 3, YRBUF )
        YR = STR2INT( YRBUF )

        PFLAG = ( MON    .GT.   12  .OR.        &
                  MON    .LT.    1  .OR.        &
                  DAY    .GT.   31  .OR.        &
                  DAY    .LT.    1  .OR.        &
                  YR     .LT. 1000  .OR.        &
                  YR     .GT. 9999 ) !  malformed input:  prompt user

    ELSE

        PFLAG = .TRUE.      !  prompt user

    END IF

    IF ( PFLAG ) THEN

        CALL DAYMON( JDATE, MON, DAY )

        MON = GETNUM( 1, 12, MON, 'Enter month (1-12)' )

        DAY = GETNUM( 1, MLENS(MON), DAY, 'Enter day (1-31)' )

        YR = GETNUM( 1000, 9999, JDATE / 1000, 'Enter year' )

    END IF      !  if pflag

    JDATE = 1000 * YR  +  JULIAN( YR, MON, DAY )

99  CONTINUE        !  generate output

    DAY   = WKDAY( JDATE )
    IF ( ISDSTIME( JDATE ) ) THEN
        WRITE( *,'( /, 5X, 2 A, I7.7, /5X, A, / )' )    &
            TRIM( DAYS( DAY ) ), ', ', JDATE,           &
            'Daylight Savings Time in effect.'
    ELSE
        WRITE( *,'( /, 5X, 2 A, I7.7, /5X, A, / )' )    &
            TRIM( DAYS( DAY ) ), ', ', JDATE,           &
            'Standard Time in effect.'
    END IF

  CALL EXIT( JDATE )

END PROGRAM JULDATE

