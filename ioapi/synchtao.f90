
SUBROUTINE SYNCHTAO( JDATE, JTIME )

    !*************************************************************************
    ! Version "$Id: synchtao.f90 203 2021-10-14 18:02:11Z coats $"
    ! EDSS/Models-3 I/O API.100 2015-01-16 16:52:16Z coats $"
    ! EDSS/Models-3 I/O API.
    ! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
    ! (C) 2003-2013 Baron Advanced Meteorological Systems,
    ! (C) 2007-2013,2021 Carlie J. Coats, Jr., and
    ! (C) 2015 UNC Institute for the Environment.
    ! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    ! See file "LGPL.txt" for conditions of use.
    !.........................................................................
    !  subroutine body   starts at line   98
    !  entry  INITSYNCH  starts at line  147
    !
    !  DESCRIPTION:
    !       Package for virtual-mode synchronization.
    !
    !       Initialize with INITSYNCH before synchronization with SYNCHTAO
    !
    !       INITSYNCH examines the environment to see whether a synch-file
    !       with the caller-supplied name is active, has the indicated
    !       synch-variable and returns TRUE iff both of these are true.
    !       If the values of the arguments are blank, SYNCHFILE defaults
    !       to 'SYNCH_IN' and  SYNCHVBLE defaults to the first variable
    !       in the variables-list for SYNCHFILE.
    !       SYNCHTAO may be turned off  by setting the SYNCHFILE environment
    !       variable to "NONE"
    !
    !       With an active SYNCHFILE, SYNCHTAO attempts to CHECK3 variable
    !       SYNCHVBLE from SYNCHFILE for the first time step containing
    !       the indicated JDATE:JTIME.
    !       For virtual-mode SYNCHFILE, this has the effect of putting
    !       the caller to sleep until SYNCHVBLE becomes available.
    !
    !  PRECONDITIONS REQUIRED:
    !       setenv ${SYNCHFILE} <environment value | NONE>
    !
    !       SYNCHVBLE must be a variable in SYNCHFILE, or blank
    !
    !       Call INITSYNCH (once) before(repeatedly) calling SYNCHTAO
    !
    !       Trimmed string-length for SYNCHVBLE and SYNCHFILE at most 16
    !
    !  SUBROUTINES AND FUNCTIONS CALLED:
    !       CHECK3, ENVINT, ENVSTR, INDEX1, SECSDIFF, SEC2TIME, TIME2SEC
    !       (I/O API)
    !
    !  REVISION  HISTORY:
    !       Prototype 5/2003 by Carlie J. Coats, Jr., BAMS
    !       Modified 03/2010 by CJC: F90 changes for I/O API v3.1
    !
    !       Modified 02/2015 by CJC for I/O API 3.2: USE M3UTILIO
    !
    !       Version 10/2021 by CJC:  free ".f90" source format for IOAPI-4.0
    !***********************************************************************

    USE M3UTILIO

    IMPLICIT NONE

    !...........   ARGUMENTS and their descriptions:

    CHARACTER*(*), INTENT(IN   ) :: SYNCHFILE
    CHARACTER*(*), INTENT(IN   ) :: SYNCHVBLE
    INTEGER      , INTENT(INOUT) :: STATUS
    INTEGER      , INTENT(IN   ) :: JDATE
    INTEGER      , INTENT(IN   ) :: JTIME

    !...........   PARAMETERS and their descriptions:

    CHARACTER*16, PARAMETER :: BLANK = ' '

    !...........   SAVED LOCAL VARIABLES and their descriptions:
    !...........   NOTE:  the ANSI standard requires the use of SAVE statements
    !...........   for variables which must retain their values from call to call.

    CHARACTER*16, SAVE :: FNAME = BLANK
    CHARACTER*16, SAVE :: VNAME = BLANK

    LOGICAL, SAVE :: SYNCHFLAG
    INTEGER, SAVE :: SDATE, STIME, TSTEP, TSECS, ILAST


    !...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER         IDATE, ITIME, IREC, SECS, SMOD, DTIME
    CHARACTER*16    LNAME
    CHARACTER*256   EQNAME
    CHARACTER*256   MESG


    !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
    !   begin body of function  SYNCHTAO

    IF ( FNAME .EQ. BLANK ) THEN
        MESG = 'SYNCHTAO not yet initialized;  call INITSYNCH() before SYNCHTAO()'
        CALL M3EXIT( 'SYNCHTAO/READSYNCH', JDATE, JTIME, MESG, 2 )
    ELSE IF ( .NOT.SYNCHFLAG ) THEN
        RETURN
    END IF

    WRITE ( MESG, '( A, I7, A, I6.6, A )' ) 'SYNCHTAO(', JDATE, ',', JTIME, ')'
    CALL M3MESG( MESG )

    SECS = SECSDIFF( SDATE, STIME, JDATE, JTIME )
    IF ( SECS .LE. -TSECS ) THEN    !!  error
        WRITE( MESG, '( A, I9.7, A, I6.6 )' )       &
            'Requested time before start of synch file:', SDATE, ':', STIME
        CALL M3EXIT( 'SYNCHTAO/READSYNCH', JDATE, JTIME, MESG, 2 )
    ELSE IF ( SECS .LE. 0 ) THEN
        IDATE = SDATE
        ITIME = STIME
        IREC  = 1
    ELSE
        SMOD = MOD( SECS, TSECS )   !  fractions of a time-step
        IREC = 1 + SECS / TSECS     !  record-# at left edge
        IF ( SMOD .EQ. 0 ) THEN     !  use left-edge (=JDATE:JTIME)
            IDATE = JDATE
            ITIME = JTIME
        ELSE                        !  use time for right-edge record-#
            IDATE = SDATE
            ITIME = STIME
            IREC  = IREC + 1
            DTIME = SEC2TIME( ( IREC-1 ) * TSECS )
            CALL NEXTIME( IDATE, ITIME, DTIME )
        END IF
    END IF

    IF ( IREC .NE. ILAST ) THEN

        IF ( .NOT.CHECK3( FNAME, VNAME, IDATE, ITIME ) ) THEN
            MESG = 'CHECK3 failure:  '// FNAME // ':' // VNAME
            CALL M3EXIT( 'SYNCHTAO/READSYNCH',JDATE,JTIME,MESG,2 )
        END IF

        ILAST = IREC

    END IF

    RETURN  !!  fron synchtao()

    !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  ENTRY INITSYNCH( SYNCHFILE, SYNCHVBLE, STATUS )

  !!  If synchronization active, sets SYNCHFLAG = .TRUE.
  !!  and values for FNAME, VNAME, SDATE, STIME, TSTEP, TSECS, ILAST
  !!  Else sets SYNCHFLAG = .FALSE.

    IF ( SYNCHFILE .EQ. BLANK ) THEN
        LNAME = 'SYNCH_IN'
    ELSE
        LNAME = SYNCHFILE
    END IF

    CALL ENVSTR( LNAME, 'synch-file name, or "NONE"',&
                 'NONE', EQNAME, STATUS )

    IF ( STATUS .GT. 0 ) THEN
        MESG = 'Bad environment variable  ' // LNAME
        CALL M3EXIT( 'SYNCHTAO/INITSYNCH', 0, 0, MESG, 2 )
    ELSE IF ( EQNAME .EQ. 'NONE' ) THEN
        MESG = 'SYNCHTAO/INITSYNCH: synchronization not active'
        FNAME = 'NONE'
        SYNCHFLAG = .FALSE.
        CALL M3MESG( MESG )
        RETURN
    END IF

    FNAME = LNAME

    MESG  = 'SYNCHTAO/INITSYNCH:  using file "' // FNAME
    CALL M3MESG( MESG )

    IF ( .NOT.OPEN3( FNAME,FSREAD3,'SYNCHTAO/INITSYNCH' ) ) THEN
        MESG = 'Could not open ' // FNAME
        CALL M3EXIT( 'SYNCHTAO/INITSYNCH', 0, 0, MESG, 2 )
    ELSE IF ( .NOT.DESC3( FNAME ) ) THEN
        MESG = 'Could not get file-description for ' // FNAME
        CALL M3EXIT( 'SYNCHTAO/INITSYNCH', 0, 0, MESG, 2 )
    ELSE IF ( SYNCHVBLE .NE. BLANK ) THEN
        IF ( 0 .LT. INDEX1( SYNCHVBLE, NVARS3D, VNAME3D ) ) THEN
            MESG = 'Variable ' // SYNCHVBLE // 'not in ' //FNAME
            CALL M3EXIT( 'SYNCHTAO/INITSYNCH', 0, 0, MESG, 2 )
        END IF
        VNAME = SYNCHVBLE
    ELSE
        VNAME = VNAME3D( 1 )
    END IF

    SDATE = SDATE3D
    STIME = STIME3D
    TSTEP = TSTEP3D
    TSECS = TIME2SEC( TSTEP )
    ILAST = -1

    SYNCHFLAG = .TRUE.

    MESG  = 'SYNCHTAO/INITSYNCH:  using variable "' // VNAME //&
            '" from file "' // FNAME // '"'
    CALL M3MESG( MESG )

    RETURN          !!  from initsynch()

END SUBROUTINE SYNCHTAO
