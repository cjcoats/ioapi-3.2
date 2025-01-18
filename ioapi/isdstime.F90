
LOGICAL FUNCTION ISDSTIME( JDATE )

    !***********************************************************************
    ! Version "$Id: isdstime.F90 262 2024-09-19 16:13:34Z coats $"
    ! EDSS/Models-3 I/O API.
    ! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
    ! (C) 2003-2010 Baron Advanced Meteorological Systems,
    ! (C) 2021 Carlie J. Coats, Jr.
    ! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    ! See file "LGPL.txt" for conditions of use.
    !.........................................................................
    !  function body starts at line  70
    !
    !  RETURNS:
    !       TRUE iff Daylight Savings Time is in effect for JDATE,
    !       calculated using Zeller's Congruence to find the
    !       starting and ending dates for Daylight Savings Time:
    !       pre-2007:  first Sunday in April and the last Sunday in October
    !       post-2007: secnd Sunday in March and the first Sunday in November
    !
    !  PRECONDITIONS REQUIRED:
    !       JDATE represents a date YYYYDDD according to Models-3 conventions
    !
    !  SUBROUTINES AND FUNCTIONS CALLED:
    !       None
    !
    !  REVISION  HISTORY:
    !       Prototype  8/1995 by Carlie J. Coats, Jr., MCNC Environmental Programs
    !
    !       Unification 2/2002 by CJC with global-climate ISDST, which
    !       uses a 360-day "year"
    !
    !       Bugfix     7/2002 by CJC
    !
    !       Version 1/2007 by CJC:  update for 2007 Daylight Saving Time changes;
    !       handle negative JDATE arguments correctly
    !
    !       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
    !
    !       Modified 11/2015 by CJC: IO_365 changes
    !
    !       Version  10/2021 by CJC:  free ".f90" source format for IOAPI-4.0
    !
    !       Modified 06/2024 by CJC: leap-year bug-fix for year >= 2007
    !***********************************************************************

    IMPLICIT NONE

    !...........   ARGUMENTS and their descriptions:

    INTEGER, INTENT( IN ) :: JDATE   !  Julian date, coded YYYYDDD


    !...........   PARAMETERS and their descriptions:

#ifdef IO_360
    INTEGER, PARAMETER :: APR1  =  91
    INTEGER, PARAMETER :: OCT31 = 300
#define CLIMO
#endif

#ifdef IO_365
#define CLIMO
#endif

#ifndef IO_360
    INTEGER, PARAMETER :: APR1  =  91
    INTEGER, PARAMETER :: OCT31 = 304
#endif


    !...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER         YEAR, IDAY
    INTEGER         DAY0, DAY1
    INTEGER         K, IBIAS


    !***********************************************************************
    !   begin body of function  ISDSTIME

    IF ( JDATE .GE. 1000 ) THEN
        YEAR = JDATE / 1000
        IDAY = MOD ( JDATE , 1000 )
    ELSE
        YEAR  = -JDATE
        YEAR  = YEAR / 1000 + 2800  !  leap-year/day-of-week cycle 2800
        IBIAS = 2800000 * ( YEAR / 2800 )
        YEAR =     ( JDATE + IBIAS ) / 1000
        IDAY = MOD ( JDATE + IBIAS   , 1000 )
    END IF


    !.......   Uses Zeller's Congruence calculation of day of wk for APR1, OCT31:
    !.......   wkday = 1 + mod( k + day, 7 )

    DAY0  = APR1        !  get first Sunday in April,
    DAY1  = OCT31       !  last Sunday in October

#ifndef CLIMO
    IF (          ( MOD (YEAR,4)   .EQ. 0 )        &    !  leap year adjustment
       .AND. (    ( MOD (YEAR,100) .NE. 0 )        &    !  month >= FEB
             .OR. ( MOD (YEAR,400) .EQ. 0 ) ) ) THEN

        DAY0 = DAY0 + 1
        DAY1 = DAY1 + 1

    END IF
#endif

    K     = YEAR - 1
    K     = K * 365  +  K / 4  -  K / 100  +  K / 400  -  1
    IF ( YEAR .LT. 2007 ) THEN
        DAY0  = DAY0 + 6 - MOD( K + DAY0, 7 ) !  first Sun. in Apr.
        K     = 1 + MOD( K + DAY1, 7 )        !  day-number 1...7 for OCT31
        DAY1  = DAY1     - MOD( K, 7 )        !  last  Sun. in Oct.
    ELSE
        DAY0  = DAY0 - 31                      !  March 1
        DAY0  = DAY0 + 13 - MOD( K + DAY0, 7 ) !  second Sunday in March,
        K     = 1 + MOD( K + DAY1, 7 )         !  day-number 1...7 for OCT31
        DAY1  = DAY1 +  7 - MOD( K, 7 )        !  first Sunday in November
    END IF

    ISDSTIME = ( ( IDAY .GE. DAY0 ) .AND. ( IDAY .LT. DAY1 ) )

    RETURN

END FUNCTION ISDSTIME

