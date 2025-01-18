
PROGRAM GREGDATE

    !!***********************************************************************
    !! Version "$Id: gregdate.f90 1 2017-06-10 18:05:20Z coats $"
    !! EDSS/Models-3 M3TOOLS.
    !! Copyright (C) 1992-2002 MCNC, (C) 1995-2002,2005-2013 Carlie J. Coats, Jr.,
    !! and (C) 2002-2010 Baron Advanced Meteorological Systems. LLC.
    !! Distributed under the GNU GENERAL PUBLIC LICENSE version 2
    !! See file "GPL.txt" for conditions of use.
    !!.........................................................................
    !!  program body starts at line 58
    !!
    !!  DESCRIPTION:
    !!       interactively enter julian date YYYYDD;
    !!       get month, day, year back.
    !!
    !!  PRECONDITIONS REQUIRED:
    !!       none
    !!
    !!  SUBROUTINES AND FUNCTIONS CALLED:
    !!       MMDDYY, GETNUM, GETMENU, STR2INT
    !!
    !!  REVISION  HISTORY:
    !!       Prototype  8/95 by CJC
    !!       Enhanced   6/98 to support YESTERDAY, TODAY, TOMORROW.
    !!       Version  11/2001 by CJC for I/O API Version 2.1
    !!       Version  06/2011 by CJC: Fortran-90 for I/O API 3.1
    !!***********************************************************************

    USE M3UTILIO

    IMPLICIT NONE

    !!...........   EXTERNAL FUNCTIONS and their descriptions:

    INTEGER :: IARGC

    !!...........   Local variables, PARAMETERs and their descriptions:

    INTEGER         DATE, DAY, JTIME
    INTEGER         ARGCNT  !  number of command-line args, from IARGC()
    CHARACTER*80    SCRBUF

    CHARACTER*12, PARAMETER :: DAYS( 7 ) =  &
                   (/ 'Monday     ',        &
                      'Tuesday    ',        &
                      'Wednesday  ',        &
                      'Thursday   ',        &
                      'Friday     ',        &
                      'Saturday   ',        &
                      'Sunday     '   /)


    !!***********************************************************************
    !!   begin body of program GREGDATE

    WRITE( *,'( 5X, A )' ) ' ', ' ',                                    &
 'Program GREGDATE takes julian date (in form YYYYDDD) and',            &
 'returns the date in form "Wkday, Month DD, YYYY".',                   &
 ' ',                                                                   &
 '    Usage:  "gregdate [<JDATE>]" ',                                   &
 '    (alt    "gregdate [ YESTERDAY | TODAY | TOMORROW]") ',            &
 ' ',                                                                   &
 '(if the JDATE command-line argument is missing, prompts the ',        &
 'user for JDATE)',                                                     &
' ',                                                                    &
'See URL',                                                              &
'https://www.cmascenter.org/ioapi/documentation/3.1/html#tools',        &
' ',                                                                    &
'Program copyright (C) 1992-2002 MCNC, (C) 1995-2013',                  &
'Carlie J. Coats, Jr., and (C) 2002-2010 Baron Advanced',               &
'Meteorological Systems, LLC.  Released under Version 2',               &
'of the GNU General Public License. See enclosed GPL.txt, or',          &
'URL',                                                                  &
''  ,                                                                   &
'    https://www.gnu.org/licenses/old-licenses/gpl-2.0.html',           &
''  ,                                                                   &
'Comments and questions are welcome and can be sent to',                &
'',                                                                     &
'    Carlie J. Coats, Jr.    carlie@jyarborough.com',                   &
'or',                                                                   &
'    UNC Institute for the Environment',                                &
'    100 Europa Dr., Suite 490 Rm 405',                                 &
'    Campus Box 1105',                                                  &
'    Chapel Hill, NC 27599-1105',                                       &
'',                                                                     &
'Program version: ',                                                    &
'$Id:: gregdate.f90 1 2017-06-10 18:05:20Z coats   $',    &
' '


    ARGCNT = IARGC()
    IF ( ARGCNT .EQ. 1 ) THEN

        CALL GETARG( ARGCNT, SCRBUF )
        CALL LUSTR( SCRBUF )
        IF ( SCRBUF .EQ. 'TODAY' ) THEN
            CALL GETDTTIME( DATE, JTIME )
        ELSE IF ( SCRBUF .EQ. 'YESTERDAY' ) THEN
            CALL GETDTTIME( DATE, JTIME )
            CALL NEXTIME( DATE, JTIME, -240000 )
        ELSE IF ( SCRBUF .EQ. 'TOMORROW' ) THEN
            CALL GETDTTIME( DATE, JTIME )
            CALL NEXTIME( DATE, JTIME, 240000 )
        ELSE
            DATE = STR2INT( SCRBUF )
        END IF

    END IF

    IF ( ARGCNT .NE.       1 .OR.        &
         DATE  .LT. 1000000  .OR.        &
         DATE  .GT. 9999999 ) THEN
        CALL GETDTTIME( DAY, JTIME )
        DATE = GETNUM( 1000000, 9999999, DAY, 'Enter Julian date YYYYDDD' )

    END IF      !  if argcnt=1, or not

    DAY = WKDAY( DATE )
    IF ( ISDSTIME( DATE ) ) THEN
        WRITE( *,'( /5X, 3 A, /5X, A, / )' )            &
            TRIM( DAYS( DAY ) ), ', ', MMDDYY( DATE ),  &
            'Daylight Savings Time in effect.'
    ELSE
        WRITE( *,'( /5X, 3 A, /5X, A, / )' )            &
            TRIM( DAYS( DAY ) ), ', ', MMDDYY( DATE ),  &
            'Standard Time in effect.'
    END IF

    CALL EXIT( 0 )

END PROGRAM GREGDATE

