
LOGICAL   FUNCTION CURRSTEP ( JDATE, JTIME,         &
                              SDATE, STIME, TSTEP,  &
                              CDATE, CTIME )

    !***********************************************************************
    ! Version "$Id: currstep.f 1 2017-06-10 18:05:20Z coats $"
    ! EDSS/Models-3 I/O API.
    ! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
    ! (C) 2003-2010 Baron Advanced Meteorological Systems,
    ! (C) 2007-2013,2021 Carlie J. Coats, Jr.
    ! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    ! See file "LGPL.txt" for conditions of use.
    !.........................................................................
    !  subroutine body starts at line  61
    !
    !  FUNCTION:  Compute the date&time CDATE:CTIME for the time step in
    !             the time step sequence starting at SDATE:STIME and having
    !             time step TSTEP.  In particular, it is the largest time
    !             step in the sequence having the property:
    !
    !                 CDATE:CTIME <= JDATE:JTIME
    !
    !  PRECONDITIONS REQUIRED:  Dates represented YYYYDDD,
    !                           times represented HHMMSS.
    !
    !  SUBROUTINES AND FUNCTIONS CALLED:  NEXTIME, SEC2TIME, SECSDIFF, TIME2SEC
    !
    !  REVISION  HISTORY:
    !       prototype 5/92 by CJC
    !
    !       Version 1/2007 by CJC:  simplification; handle negative
    !       *DATE arguments correctly
    !       prototype 5/92 by CJC
    !
    !       Gross simplification 1/2008 by CJC:  use result from CURREC(),
    !       new version of which is now relatively safe from integer overflow.
    !
    !       Version 10/2021 by CJC:  free ".f90" source format for IOAPI-4.0
    !***********************************************************************

    IMPLICIT NONE

    !...........   ARGUMENTS and their descriptions:

    INTEGER, INTENT(IN   ) :: SDATE, STIME    !  starting d&t for the sequence
    INTEGER, INTENT(IN   ) :: TSTEP           !  time step for the sequence
    INTEGER, INTENT(IN   ) :: JDATE, JTIME    !  d&t requested
    INTEGER, INTENT(  OUT) :: CDATE, CTIME    !  d&t for timestep of JDATE:JTIME

    !...........   EXTERNAL FUNCTIONS and their descriptions:

    INTEGER, EXTERNAL :: CURREC

    !...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER     IREC

    !***********************************************************************
    !   begin body of subroutine  CURRSTEP

    IREC = CURREC( JDATE, JTIME,            &
                   SDATE, STIME, TSTEP,     &
                   CDATE, CTIME )

    CURRSTEP = ( IREC .GT. 0 )
    RETURN

END FUNCTION CURRSTEP

