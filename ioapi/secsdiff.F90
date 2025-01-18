
INTEGER  FUNCTION SECSDIFF ( ADATE, ATIME, ZDATE, ZTIME )

    !***********************************************************************
    ! Version "$Id: secsdiff.F90 203 2021-10-14 18:02:11Z coats $"
    ! EDSS/Models-3 I/O API.
    ! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
    ! (C) 2021 Carlie J. Coats, Jr., and
    ! (C) 2003-2010 Baron Advanced Meteorological Systems., and
    ! (C) 2016 UNC Institute for the Environment.
    ! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    ! See file "LGPL.txt" for conditions of use.
    !.........................................................................
    !  function body starts at line  75
    !
    !  FUNCTION:  returns the time interval (seconds) between ADATE:ATIME
    !             and ZDATE:ZTIME
    !
    !  PRECONDITION:  normalized dates and times (0 <= SS <= 59, etc.)
    !                 stored in format YYYYDDD:HHMMSS.
    !
    !  REVISION  HISTORY:
    !       Prototype   5/1992 by Carlie J. Coats, Jr.,
    !       MCNC Environmental Programs
    !
    !       Version     2/1993 by CJC for CRAY, etc.
    !
    !       Unification 2/2002 by CJC with global-climate SECSDIFF, which
    !       uses a 360-day "year"
    !
    !       Version 1/2007 by CJC:  simplification; handle negative
    !       *DATE arguments correctly
    !
    !       Modified 03/2010 by CJC: F90 changes for I/O API v3.1
    !
    !       Modified 4/2016 by CJC: IO_365 changes, from Chris Nolte, US EPA
    !
    !       Version 10/2021 by CJC:  free ".f90" source format for IOAPI-4.0
    !***********************************************************************

    IMPLICIT NONE

    !...........   ARGUMENTS and their descriptions:

    INTEGER, INTENT(IN   ) :: ADATE, ATIME
    INTEGER, INTENT(IN   ) :: ZDATE, ZTIME


    !...........   PARAMETERS and their descriptions:

    !  "normal"-year, leap-year # of days

#ifdef IO_360
#define     YDAYS   (360)
#define     LDAYS   (360)
#endif
#ifdef IO_365
#define     YDAYS   (365)
#define     LDAYS   (365)
#endif
#ifndef YDAYS
#define     YDAYS   (365)
#define     LDAYS   (366)
#endif


    !...........   Local Variables

    INTEGER         AYEAR,  ZYEAR, YEAR, DAYS, HOURS, MINS, SECS
    INTEGER         TOTAL
    INTEGER         BDATE, YDATE


    !***********************************************************************
    !   begin body of function  SECSDIFF

    IF ( ADATE .GT. 1000 .AND. ZDATE .GT. 1000 ) THEN
        BDATE = ADATE
        YDATE = ZDATE
    ELSE    ! adjust both by multiple of 400-year leap-year cycle
        YEAR  = MAX( -ADATE, -ZDATE ) / 1000 + 1
        YEAR  = 400 * ( YEAR / 400 + 1 )
        BDATE = ADATE + YEAR * 1000
        YDATE = ZDATE + YEAR * 1000
    END IF


    !.......   Start with day, hour, min, sec differences:

    DAYS  = MOD( YDATE,     1000 ) -  MOD( BDATE,   1000 )
    HOURS =      ZTIME /   10000   -       ATIME / 10000
    MINS  = MOD( ZTIME/100, 100 )  -  MOD( ATIME/100, 100 )
    SECS  = MOD( ZTIME,     100 )  -  MOD( ATIME,     100 )

    TOTAL = 60 * ( 60 * ( 24 * DAYS + HOURS ) + MINS ) + SECS


    !.......   Now add corrections for differences in years:

    AYEAR = BDATE / 1000
    ZYEAR = YDATE / 1000

11  CONTINUE        !  loop accumulating seconds if AYEAR < ZYEAR

    IF ( AYEAR .GE. ZYEAR )  THEN
        GO TO 22
    ELSE IF ( MOD( AYEAR, 4   ) .NE. 0 ) THEN   !  nonleap
        TOTAL = TOTAL + YDAYS * 86400
    ELSE IF ( MOD( AYEAR, 100 ) .NE. 0 ) THEN   !  leap noncentury
        TOTAL = TOTAL + LDAYS * 86400
    ELSE IF ( MOD( AYEAR, 400 ) .NE. 0 ) THEN   !  nonleap century
        TOTAL = TOTAL + YDAYS * 86400
    ELSE                                        !  leap century
        TOTAL = TOTAL + LDAYS * 86400
    END IF
    AYEAR = AYEAR + 1

    GO TO  11

22  CONTINUE        !  loop accumulating seconds if AYEAR > ZYEAR

    IF ( ZYEAR .GE. AYEAR )  THEN
        GO TO 33
    ELSE IF ( MOD( ZYEAR, 4   ) .NE. 0 ) THEN   !  nonleap
        TOTAL = TOTAL - YDAYS * 86400
    ELSE IF ( MOD( ZYEAR, 100 ) .NE. 0 ) THEN   !  leap noncentury
        TOTAL = TOTAL - LDAYS * 86400
    ELSE IF ( MOD( ZYEAR, 400 ) .NE. 0 ) THEN   !  nonleap century
        TOTAL = TOTAL - YDAYS * 86400
    ELSE                                        !  leap century
        TOTAL = TOTAL - LDAYS * 86400
    END IF
    ZYEAR = ZYEAR + 1

    GO TO  22

33  CONTINUE        !  end loops dealing with year adjustments

    SECSDIFF = TOTAL

    RETURN

END FUNCTION SECSDIFF

