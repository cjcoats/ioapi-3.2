
INTEGER FUNCTION JSTEP3( JDATE, JTIME, SDATE, STIME, TSTEP )

    !***********************************************************************
    ! Version "$Id: jstep3.F90 203 2021-10-14 18:02:11Z coats $"
    ! EDSS/Models-3 I/O API.
    ! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
    ! (C) 2003-2013 Baron Advanced Meteorological Systems,
    ! (C) 2010,2021 Carlie J. Coats, Jr.
    ! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    ! See file "LGPL.txt" for conditions of use.
    !.........................................................................
    !  function body starts at line  75
    !
    !  FUNCTION:
    !       returns the timestep-record number corresponding to JDATE:JTIME
    !       for the timestep sequence starting at SDATE:STIME, with time
    !       step increment TSTEP
    !
    !  RETURN VALUE:
    !       time step record number, if successful, or  -1  if the
    !       indicated  JDATE:JTIME  is not a positive multiple of
    !       TSTEP from SDATE:STIME
    !
    !  PRECONDITIONS REQUIRED:
    !       avoid integer overflow
    !
    !  SUBROUTINES AND FUNCTIONS CALLED:
    !       none
    !
    !  REVISION  HISTORY:
    !       prototype 3/1992 by Carlie J. Coats, Jr., MCNC Environmental Programs
    !
    !       Unification 2/2002 by CJC with global-climate DAYMON, which
    !       uses a 360-day "year"
    !
    !       Version 1/2007 by CJC:  simplification after the fashion of
    !       currec(); handles negative *DATE arguments correctly
    !
    !       Version 1/2008 by CJC:  Problem reported by Christian Hogrefe,
    !       NY Division of Environmental Conservation:  be careful to avoid
    !       integer overflow, for climate modeling applications, etc.
    !       Gross simplification:  use result from (new no-overflow version
    !       of) CURREC()
    !
    !       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
    !
    !       Version  10/2021 by CJC:  free ".f90" source format for IOAPI-4.0
    !***********************************************************************

    IMPLICIT NONE

    !...........   ARGUMENTS and their descriptions:

    INTEGER, INTENT( IN ) :: JDATE   !  requested date YYYYDDD
    INTEGER, INTENT( IN ) :: JTIME   !  requested time HHMMSS
    INTEGER, INTENT( IN ) :: SDATE   !  starting date  YYYYDDD
    INTEGER, INTENT( IN ) :: STIME   !  starting time  HHMMSS
    INTEGER, INTENT( IN ) :: TSTEP   !  time step      H*MMSS

    !...........   EXTERNAL FUNCTIONS and their descriptions:

    INTEGER, EXTERNAL :: CURREC

    !...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER         IREC, SECS
    INTEGER         KDATE, KTIME, CDATE, CTIME


    !***********************************************************************
    !   begin body of function  JSTEP3

    IF ( TSTEP .EQ. 0 ) THEN
        JSTEP3 = 1
        RETURN
    END IF

    IREC = CURREC( JDATE, JTIME,            &
                   SDATE, STIME, TSTEP,     &
                   CDATE, CTIME )


    IF ( IREC .LT. 0 ) THEN
        JSTEP3 = -1
        RETURN
    END IF

    !!  Normalize the JDATE:JTIME argument
    !!  (CDATE:CTIME already normalized by CURREC):

    KDATE = JDATE
    KTIME = JTIME
    CALL NEXTIME( KDATE, KTIME, 0 )

    !!  compare with CURREC() result:

    IF ( KDATE .EQ. CDATE .AND. KTIME .EQ. CTIME ) THEN
        JSTEP3 = IREC
    ELSE
        JSTEP3 = -1
    END IF

    RETURN

END FUNCTION JSTEP3

