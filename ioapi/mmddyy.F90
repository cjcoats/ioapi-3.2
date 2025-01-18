
CHARACTER*14 FUNCTION  MMDDYY ( JDATE )

    !***********************************************************************
    ! Version "$Id: mmddyy.F90 203 2021-10-14 18:02:11Z coats $"
    ! EDSS/Models-3 I/O API.
    ! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
    ! (C) 2003-2010 Baron Advanced Meteorological Systems,
    ! (C) 2007-2013,2021 Carlie J. Coats, Jr., and
    ! (C) 2015-2020 UNC Institute for the Environment.
    ! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    ! See file "LGPL.txt" for conditions of use.
    !.........................................................................
    !  function body starts at line  98
    !
    !  FUNCTION:  format and return the date as a character string
    !             "M+ D+, YYYY"
    !
    !  PRECONDITIONS REQUIRED:  valid Julian date YYYYDDD
    !
    !
    !  RETURN VALUE:  date, as character string "MMM DD, YYYY"
    !
    !
    !  SUBROUTINES AND FUNCTIONS CALLED:  none
    !
    !
    !  REVISION  HISTORY:
    !       prototype 10/1990 for ROM by Carlie J. Coats, Jr.
    !
    !       Version    2/1993 by CJC for CRAY, etc.
    !
    !       Unification 2/2002 by CJC with global-climate MMDDYY, which
    !       uses a 360-day "year"
    !
    !       Version 1/2007 by CJC:  handle negative JDATEs correctly
    !
    !       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
    !
    !       Version 5/2013 by CJC:  PARAMETERs; handle standard-year
    !
    !       Version 11/2013 by CJC:  bug-fix for "standard-year" case
    !
    !       Modified 11/2015 by CJC: IO_365 changes
    !
    !       Version 10/2021 by CJC:  free ".f90" source format for IOAPI-4.0
    !***********************************************************************

    IMPLICIT NONE

    !...........   ARGUMENTS and their descriptions:

    INTEGER, INTENT(IN   ) :: JDATE	!  Julian date, coded YYYYDDD


    !...........   PARAMETERS and their descriptions:
    !.......   Lookup table, string lengths for months:

    CHARACTER*5, PARAMETER :: MONTHS ( 12 ) = (/                    &
        'Jan. ' , 'Feb. ' , 'March', 'April', 'May  ' , 'June ',    &
        'July ' , 'Aug. ' , 'Sept.', 'Oct. ', 'Nov  ' , 'Dec. ' /)
    INTEGER, PARAMETER :: MLENS ( 12 ) = (/ 4, 4, 5, 5, 3, 4, 4, 4, 5, 4, 4, 4 /)

    !.......   Lookup table of cumulative days accumulated ( in non-leap year)
    !.......   before the given month.  CUMDAY(13) is total days per year.

#ifdef IO_365
#define CLIMO3
#endif

#ifdef IO_360
#define CLIMO3
    INTEGER, PARAMETER :: CUMDAY ( 13 ) = (/ 0, 30, 60, 90, 120, 150, 180, 210, 240, 270, 300, 330, 360 /)
#endif

#ifndef IO_360
    INTEGER, PARAMETER :: CUMDAY ( 13 ) = (/ 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365 /)
#endif

    INTEGER, PARAMETER :: LEAPDAY( 13 ) = (/ 0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366 /)

    CHARACTER*1, PARAMETER :: DIGITS( 0:9 ) = (/'0','1','2','3','4','5','6','7','8','9' /)


    !...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER  	YEAR
    INTEGER  	IDAY
    INTEGER  	BIAS
    INTEGER  	MNTH
    INTEGER  	J , K
    CHARACTER*14	CHRBUF


    !***********************************************************************
    !   begin body of function  YYMMDD

    IF ( JDATE .GT. 9999999  .OR.  JDATE .LT. -999999 ) THEN
        MMDDYY = '<DATE ERROR>'
        RETURN
    ELSE IF ( JDATE .LT. 0 ) THEN   !  add 2800-year leapyear/weekday cycle
        J    = JDATE + 2800000
        YEAR =       J / 1000 - 2800
        IDAY = MOD ( J , 1000 )
    ELSE
        YEAR = JDATE / 1000
        IDAY = MOD ( JDATE , 1000 )
    END IF

    CHRBUF = '              '

#ifdef CLIMO3
    DO  MNTH = 1 , 12
        IF ( IDAY .LE. CUMDAY ( MNTH + 1 ) )  THEN
            IDAY = IDAY - CUMDAY( MNTH )
            GO TO 201
        END IF
    END  DO
#endif

#ifndef CLIMO3
    IF ( YEAR .LE. 2 ) THEN      !! standard-year case...

        DO  MNTH = 1 , 12
            IF ( IDAY .LE. CUMDAY ( MNTH + 1 ) )  THEN
                IDAY = IDAY - CUMDAY( MNTH )
                GO TO 201
            END IF
        END  DO

    ELSE IF (           ( MOD (YEAR,4)   .EQ. 0 )       & !  leap year adjustment
             .AND. (    ( MOD (YEAR,100) .NE. 0 )       & !  month >= FEB
                   .OR. ( MOD (YEAR,400) .EQ. 0 ) )     &
             .AND. ( IDAY .GE. CUMDAY ( 3 ) + 1 ) ) THEN

        DO  MNTH = 1 , 12
            IF ( IDAY .LE. LEAPDAY( MNTH + 1 ) )  THEN
                IDAY = IDAY - LEAPDAY( MNTH )
                GO TO 201
            END IF
        END  DO

    ELSE

        DO  MNTH = 1 , 12
            IF ( IDAY .LE. CUMDAY ( MNTH + 1 ) )  THEN
                IDAY = IDAY - CUMDAY( MNTH )
                GO TO 201
            END IF
        END  DO

    END IF
#endif



    !.......   If you get to here:  error in formatting of date.

    MMDDYY = '<DATE ERROR>'
    RETURN


201 CONTINUE	!  Month found:  subscript for month is MNTH

    J = MLENS( MNTH )
    CHRBUF( 1 : J ) = MONTHS( MNTH )( 1:J )

    J = J + 1
    CHRBUF( J:J ) = ' '
    J = J + 1

    IF ( IDAY .GE. 10 ) THEN
        CHRBUF( J:J ) = DIGITS( IDAY / 10 )
        J = J + 1
    END IF

    CHRBUF( J:J ) = DIGITS( MOD( IDAY, 10 ) )
    J = J + 1
    CHRBUF( J:J+1 ) = ', '
    J = J + 2

    IF ( YEAR .GT. 0 ) THEN

        K = YEAR / 1000
        CHRBUF( J:J ) = DIGITS( K )
        J = J + 1

        K = MOD( YEAR/100, 10 )
        CHRBUF( J:J ) = DIGITS( K )
        J = J + 1

        K = MOD( YEAR / 10, 10 )
        CHRBUF( J:J ) = DIGITS( K )
        J = J + 1

        K = MOD( YEAR, 10 )
        CHRBUF( J:J ) = DIGITS( K )

    ELSE

        YEAR = -YEAR
        CHRBUF( J:J ) = '-'
        J = J + 1

        K = MOD( YEAR/100, 10 )
        CHRBUF( J:J ) = DIGITS( K )
        J = J + 1

        K = MOD( YEAR / 10, 10 )
        CHRBUF( J:J ) = DIGITS( K )
        J = J + 1

        K = MOD( YEAR, 10 )
        CHRBUF( J:J ) = DIGITS( K )

    END IF

    MMDDYY = CHRBUF

    RETURN

END FUNCTION  MMDDYY

