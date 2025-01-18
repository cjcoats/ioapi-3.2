
REAL FUNCTION YR2DAY( YEAR )

    !********************************************************************
    ! Version "$Id: yr2day.F90 203 2021-10-14 18:02:11Z coats $"
    ! EDSS/Models-3 I/O API.
    ! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
    ! (C) 2003-2010 Baron Advanced Meteorological Systems,
    ! (C) 2007-2013,2021 Carlie J. Coats, Jr., and
    ! (C) 2015-2020 UNC Institute for the Environment.
    ! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    ! See file "LGPL.txt" for conditions of use.
    !.........................................................................
    !       function body starts at line  53
    !
    !  FUNCTION:
    !
    !      Returns the year to day conversion factor for a given year
    !
    !  ARGUMENT LIST DESCRIPTION:
    !
    !    Input arguments:
    !
    !        YEAR   - 4 digit year
    !
    !    Output arguments:  none
    !
    !  RETURNS   user response after checking its range; or default.
    !
    !  REVISION HISTORY:
    !
    !       Created 4/1997 by M Houyoux, MCNC Environmental Programs
    !
    !       Unification 2/2002 by CJC with global-climate DAYMON, which
    !       uses a 360-day "year"
    !
    !       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
    !
    !       Modified 11/2015 by CJC: IO_365 changes
    !
    !       Version 10/2021 by CJC:  free ".f90" source format for IOAPI-4.0
    !****************************************************************************

    IMPLICIT NONE

    !.......   ARGUMENTS:

    INTEGER, INTENT(IN   ) :: YEAR  ! 4 digit year YYYY

    !......................................................................
    !       begin YR2DAY

#ifdef IO_360
    YR2DAY = 1.0 / 360.0
    RETURN
#endif

#ifdef IO_365
    YR2DAY = 1.0 / 365.0
    RETURN
#endif

    IF ( MOD( YEAR,4 ) .NE. 0 ) THEN           !  nonleap years
        YR2DAY = 1.0 / 365.0
    ELSE IF ( MOD( YEAR,100 ) .NE. 0 ) THEN    !  noncentury leap years
        YR2DAY = 1.0 / 366.0
    ELSE IF ( MOD( YEAR,400 ) .NE. 0 ) THEN    !  century nonleap years
        YR2DAY = 1.0 / 365.0
    ELSE                                       !  leap centuries
        YR2DAY = 1.0 / 366.0
    END IF

    RETURN

END FUNCTION YR2DAY

