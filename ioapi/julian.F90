
INTEGER FUNCTION JULIAN (YEAR, MNTH, MDAY)

    !***********************************************************************
    ! Version "$Id: julian.F90 203 2021-10-14 18:02:11Z coats $"
    ! EDSS/Models-3 I/O API.
    ! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
    ! (C) 2003-2010 Baron Advanced Meteorological Systems,
    ! (C) 2007-2013,2021 Carlie J. Coats, Jr., and
    ! (C) 2015 UNC Institute for the Environment.
    ! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    ! See file "LGPL.txt" for conditions of use.
    !.........................................................................
    !  function body starts at line  74
    !
    !  FUNCTION:  returns the Julian day (1...365,366) corresponding to
    !      the date MNTH-MDAY-YEAR.
    !      NOTE:  This is NOT the Julian DATE -- only the
    !      day-number.  To get the Julian date:
    !
    !      JDATE = 1000 * YEAR  +  JULIAN ( YEAR , MNTH , MDAY )
    !
    !  ARGUMENT LIST DESCRIPTION:
    !
    !    Input arguments:
    !
    !      YEAR     Calendar year
    !      MNTH     Month of year  1, 12
    !      MDAY     Day of month   1, 31
    !
    !     Output arguments:  none
    !
    !  RETURN VALUE:
    !
    !      JULIAN   The Julian DAY of the input arguments combined
    !
    !  REVISION HISTORY:
    !
    !    5/1988   Modified for ROMNET
    !
    !    8/1990   Modified for ROM 2.2 by Carlie J. Coats, Jr., CSC
    !       improved comments; improved Zeller's Congruence algorithm
    !       and using IF-THEN ... ELSE IF ... construction.
    !
    !    8/1999   Version for global-climate IO_360, which uses 360-day "year"
    !
    !       2/2002 Unification by CJC with global-climate JULIAN
    !
    !       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
    !
    !       Modified 11/2015 by CJC: IO_365 changes
    !
    !       Version  10/2021 by CJC:  free ".f90" source format for IOAPI-4.0
    !***********************************************************************

    IMPLICIT NONE

    !...........   ARGUMENTS and their descriptions:

    INTEGER, INTENT(IN   ) :: YEAR            ! year YYYY
    INTEGER, INTENT(IN   ) :: MNTH            ! month 1...12
    INTEGER, INTENT(IN   ) :: MDAY            ! day-of-month 1...28,29,30,31

    !...........   SCRATCH LOCAL VARIABLES:

    INTEGER   M, N, L


    !***********************************************************************
    !   begin body of function  JULIAN

    !...........   Climatology-year calculations:

#ifdef IO_360

    JULIAN = MDAY + 30 * ( MNTH - 1 )
    RETURN

#endif

    M = MOD ((MNTH + 9), 12)
    N = (M * 153 + 2) / 5 + MDAY + 58

#ifdef IO_365

    JULIAN = 1 + MOD (N, 365)
    RETURN

#endif


    !...........   Otherwise

    M = MOD ((MNTH + 9), 12)
    N = (M * 153 + 2) / 5 + MDAY + 58

    IF      ( MOD (YEAR,   4) .NE. 0 ) THEN
        L = 365
    ELSE IF ( MOD (YEAR, 100) .NE. 0 ) THEN
        L = 366
        N = 1 + N
    ELSE IF ( MOD (YEAR, 400) .NE. 0 )  THEN
        L = 365
    ELSE
        L = 366
        N = 1 + N
    END IF
    JULIAN = 1 + MOD (N, L)

    RETURN
END FUNCTION JULIAN

