
INTEGER FUNCTION CURREC( JDATE, JTIME,          &
                         SDATE, STIME, TSTEP,   &
                         CDATE, CTIME )

    !***********************************************************************
    ! Version "$Id: currec.f 1 2017-06-10 18:05:20Z coats $"
    ! EDSS/Models-3 I/O API.
    ! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
    ! (C) 2003-2010 Baron Advanced Meteorological Systems,
    ! (C) 2007-2013,2021 Carlie J. Coats, Jr.
    ! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    ! See file "LGPL.txt" for conditions of use.
    !.........................................................................
    !  subroutine body starts at line  73
    !
    !  FUNCTION:  Return the record number the time step in the time step
    !             sequence starting at SDATE:STIME and having time step TSTEP
    !             and compute its  date&time  CDATE:CTIME
    !             In particular, this is the largest time step in the sequence
    !             having the property:
    !
    !                 CDATE:CTIME <= JDATE:JTIME
    !
    !             If JDATE:JTIME is out-of-range, return -1
    !
    !  PRECONDITIONS REQUIRED:  Dates represented YYYYDDD,
    !                           times represented HHMMSS.
    !
    !  SUBROUTINES AND FUNCTIONS CALLED:  NEXTIME, SEC2TIME, SECSDIFF, TIME2SEC
    !
    !  REVISION  HISTORY:
    !       Adapted 2/99 by CJC from I/O API routine CURREC()
    !
    !       Version 1/2007 by CJC:  simplification; handle negative
    !       *DATE arguments correctly
    !
    !       Version 1/2008 by CJC:  Problem reported by Christian Hogrefe,
    !       NY Division of Environmental Conservation:  be careful to avoid
    !       integer overflow, for climate modeling applications, etc.
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

    INTEGER, EXTERNAL :: SECSDIFF, SEC2TIME, TIME2SEC

    !...........   PARAMETERS and their descriptions:

    INTEGER, PARAMETER :: YSECS4 = ( 3*365+366 ) * 24 * 60 * 60     !  seconds in 4 years
    INTEGER, PARAMETER :: YSTEP4 = 10000*( YSECS4/3600 )            !  hhmmss for 4 years


    !...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER       SECS, STEP, IREC, JREC
    INTEGER       ASTEP, KDATE, KTIME


    !***********************************************************************
    !   begin body of subroutine  CURREC

    IF ( TSTEP .EQ. 0 ) THEN   !  time-independent case:

        CURREC = 1
        CDATE  = SDATE
        CTIME  = STIME
        RETURN

    END  IF

    IF ( JDATE .LT. -10000000  .OR. JDATE .GT.  10000000  ) THEN           !  probable-error cases

        CURREC = -1
        RETURN

    END  IF


    !!  Normalized copies of the arguments:

    CDATE = SDATE
    CTIME = STIME
    CALL NEXTIME( CDATE, CTIME, 0 )

    KDATE = JDATE
    KTIME = JTIME
    CALL NEXTIME( KDATE, KTIME, 0 )

    STEP = ABS( TSTEP )

    IF (   KDATE .LT. CDATE  .OR. ( KDATE .EQ. CDATE .AND. KTIME .LT. CTIME ) ) THEN

        CURREC = -1

    ELSE IF ( KDATE .EQ. CDATE .AND. KTIME .EQ. CTIME ) THEN

        CURREC = 1
        RETURN

    ELSE IF ( KDATE .LE. CDATE+5000 ) THEN  !  no overflow happens

        SECS = SECSDIFF( CDATE, CTIME, KDATE, KTIME )

        STEP = TIME2SEC( STEP )
        IREC = SECS / STEP
        CALL NEXTIME( CDATE, CTIME, SEC2TIME( IREC * STEP ) )
        CURREC = 1 + IREC

    ELSE IF ( STEP .GT. YSTEP4 ) THEN       !  iterate by STEP (> 4 years)

        IREC  = 0

11      CONTINUE    !  loop by ASTEP while CDATE:CTIME <= KDATE:KTIME

        CALL NEXTIME( CDATE, CTIME, STEP )
        IREC = IREC + 1
        IF ( KDATE .GT. CDATE .OR. ( KDATE .EQ. CDATE .AND. KTIME .GT. CTIME ) ) THEN
            GO TO  11
        END IF

        CALL NEXTIME( CDATE, CTIME, -STEP )
        CURREC = IREC

    ELSE    !  step < ysecs4:  iterate by approx 4 years

        SECS  = TIME2SEC( STEP )
        JREC  = YSECS4 / SECS               !  integer steps per 4 years
        ASTEP = SEC2TIME( JREC * SECS )     !  approx 4 years
        IREC  = 0

22      CONTINUE    !  loop by ASTEPs until within 5 years:

        CALL NEXTIME( CDATE, CTIME, ASTEP )
        IREC = IREC + JREC
        IF ( CDATE+5000 .LT. KDATE ) THEN
            GO TO  22
        END IF

        STEP = SECSDIFF( CDATE, CTIME, KDATE, KTIME )
        JREC = STEP / SECS
        CALL NEXTIME( CDATE, CTIME, SEC2TIME( JREC * SECS ) )

        CURREC = 1 + IREC + JREC

    END IF

    RETURN

END FUNCTION CURREC

