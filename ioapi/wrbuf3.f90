
LOGICAL FUNCTION WRBUF3( FID, VID, JDATE, JTIME, STEP, BUFFER )

    !***********************************************************************
    ! Version "$Id: wrbuf3.f90 203 2021-10-14 18:02:11Z coats $"
    ! EDSS/Models-3 I/O API.
    ! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
    ! (C) 2003-2010 Baron Advanced Meteorological Systems,
    ! (C) 2007-2013,2021 Carlie J. Coats, Jr., and
    ! (C) 2015-2020 UNC Institute for the Environment.
    ! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    ! See file "LGPL.txt" for conditions of use.
    !.........................................................................
    !  function body starts at line  90
    !
    !  FUNCTION:  writes all the data from BUFFER() for timestep JDATE:JTIME
    !             (formatted YYYYDDD and HHMMSS) to the Models-3 BUFFERED
    !             data "file" with file index FID.
    !             If FNAME is time-independent, JDATE and JTIME are ignored.
    !
    !  RETURN VALUE:  TRUE iff the operation succeeds
    !
    !  PRECONDITIONS REQUIRED:
    !       FNAME is a BUFFERED Models-3 data file already opened for
    !       write access by OPEN3()
    !       For ALLLVARS3 reads, all variables must be of type M3REAL
    !
    !  SUBROUTINES AND FUNCTIONS CALLED:
    !                 JSTEP3, BUFPUT3 (from bufint3.c)
    !
    !  REVISION  HISTORY:
    !       prototype 7/1994 by CJC
    !
    !       modified 10/1994 by CJC to permit WRITE3-granularity at the level
    !       of individual variables.
    !
    !       Modified 5/2002 to support types other than REAL
    !
    !       Modified 9/2004 by CJC for I/O API v3 TSTEP/buffer management
    !       unification
    !
    !       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
    !
    !       Bug-fix 04/2011 by CJC:  argument-list fix for BUFPUT3()
    !
    !       Bug-fixes 04/2020 from Fahim Sidi, US EPA.
    !       Mods to allow non-REAL ALLVAR3 writes.  M3INT8 support.
    !
    !       Version 10/2021 by CJC:  free ".f90" source format for IOAPI-4.0
    !***********************************************************************

    IMPLICIT NONE

    !...........   INCLUDES:

    INCLUDE 'PARMS3.EXT'
    INCLUDE 'STATE3.EXT'


    !...........   ARGUMENTS and their descriptions:

    INTEGER, INTENT(IN   ) :: FID             !  file-subscript for STATE3 arrays
    INTEGER, INTENT(IN   ) :: VID             !  vble-subscript for STATE3 arrays
    INTEGER, INTENT(IN   ) :: JDATE           !  date, formatted YYYYDDD
    INTEGER, INTENT(IN   ) :: JTIME           !  time, formatted HHMMSS
    INTEGER, INTENT(IN   ) :: STEP            !  time step record number
    REAL   , INTENT(IN   ) :: BUFFER(*)       !  output buffer array


    !...........   EXTERNAL FUNCTIONS and their descriptions:

    INTEGER, EXTERNAL :: BUFPUT3, BUFPUT3D, BUFPUT3I


    !...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER         V       !  loop counter (over variables)
    INTEGER         IDUM    !  scratch variable
    INTEGER         SIZE, I, TSTEP
    INTEGER         JSTEP
    INTEGER         ADATE, ATIME
    INTEGER         ZDATE, ZTIME
    LOGICAL         WFLAG, OKFLAG
    CHARACTER*256   MESG            !  for m3msg2, m3warn


    !***********************************************************************
    !   begin body of function  WRBUF3

    SIZE  = BSIZE3( FID ) * NLAYS3( FID )
    TSTEP = TSTEP3( FID )

    JSTEP = ABS( TSTEP3( FID ) )
    IF ( JSTEP .GT. 0 ) THEN
        ADATE = JDATE
        ATIME = JTIME
        CALL NEXTIME( ADATE, ATIME, -JSTEP )    !  one time step before J
        ZDATE = JDATE
        ZTIME = JTIME
        CALL NEXTIME( ZDATE, ZTIME, JSTEP )     !  one time step after J
        IDUM = MOD( 1 + STEP, 2 )
    ELSE
        ADATE = 0
        ATIME = 0
        ZDATE = 0
        ZTIME = 0
        IDUM  = 0
    END IF

    IF ( VID .GT. 0 ) THEN          !  single-variable write request

        IF ( LDATE3( VID,FID ) .EQ. IMISS3 ) THEN !  first call

            IF ( JSTEP .NE. 0 ) THEN

                ILAST3( VID,FID ) = IDUM
                LDATE3( VID,FID ) = JDATE
                LTIME3( VID,FID ) = JTIME

            ELSE

                ILAST3( VID,FID ) = 0
                LDATE3( VID,FID ) = 0
                LTIME3( VID,FID ) = 0
                NDATE3( VID,FID ) = 0
                NTIME3( VID,FID ) = 0

            END IF

        ELSE IF ( NDATE3( VID,FID ) .EQ. ADATE  .AND.           &
                  NTIME3( VID,FID ) .EQ. ATIME ) THEN ! advance 1 time step

            IF ( JSTEP .NE. 0 ) THEN

                ILAST3( VID,FID ) = 1 - IDUM
                LDATE3( VID,FID ) = NDATE3( VID,FID )
                LTIME3( VID,FID ) = NTIME3( VID,FID )
                NDATE3( VID,FID ) = JDATE
                NTIME3( VID,FID ) = JTIME

            ELSE

                ILAST3( VID,FID ) = 0
                LDATE3( VID,FID ) = 0
                LTIME3( VID,FID ) = 0
                NDATE3( VID,FID ) = 0
                NTIME3( VID,FID ) = 0

            END IF

        ELSE IF ( LDATE3( VID,FID ) .EQ. ZDATE  .AND.           &
                  LTIME3( VID,FID ) .EQ. ZTIME ) THEN  ! retreat 1 time step

            ILAST3( VID,FID ) = 1 - IDUM            !  case _must_ be
            NDATE3( VID,FID ) = LDATE3( VID,FID )
            NTIME3( VID,FID ) = LTIME3( VID,FID )
            LDATE3( VID,FID ) = JDATE               !  time-dependent
            LTIME3( VID,FID ) = JTIME

        ELSE IF ( NDATE3( VID,FID ) .EQ. IMISS3 ) THEN  ! second call,
        ! time dependent case

            IF  ( LDATE3( VID,FID ) .EQ. ADATE  .AND.           &
                  LTIME3( VID,FID ) .EQ. ATIME ) THEN  ! step forward

                NDATE3( VID,FID ) = JDATE
                NTIME3( VID,FID ) = JTIME

            ELSE IF  ( LDATE3( VID,FID ) .EQ. ZDATE  .AND.      &
                       LTIME3( VID,FID ) .EQ. ZTIME ) THEN  ! step forward

                ILAST3( VID,FID ) = 1 - IDUM
                LDATE3( VID,FID ) = NDATE3( VID,FID )
                LTIME3( VID,FID ) = NTIME3( VID,FID )
                NDATE3( VID,FID ) = JDATE
                NTIME3( VID,FID ) = JTIME

            END IF

        ELSE IF ( ( JDATE .NE. LDATE3( VID,FID ) .OR.       &
                    JTIME .NE. LTIME3( VID,FID ) )          &
                  .AND.                                     &
                  ( JDATE .NE. NDATE3( VID,FID ) .OR.       &
                    JTIME .NE. NTIME3( VID,FID ) ) ) THEN

            WRITE( MESG, 93020 )        &
                'Date/time being written to BUFFERED file ' // FLIST3( FID ) // ':', JDATE, JTIME
            CALL M3MSG2( MESG )
            WRITE( MESG, 93020 )        &
                'Last date/time  written:', LDATE3( VID,FID ), LTIME3( VID,FID )
            CALL M3MSG2( MESG )
            MESG = 'Out-of-order write to BUFFERED file '
            CALL M3WARN( 'WRITE3/WRBUF3', JDATE, JTIME, MESG )

            WRBUF3 = .FALSE.
            RETURN

        END IF

        IF ( VTYPE3( VID,FID ) .EQ. M3REAL ) THEN
            WRBUF3 = ( 0 .NE. BUFPUT3 ( FID, VID, SIZE, IDUM, TSTEP, BUFFER ) )
        ELSE IF ( VTYPE3( VID,FID ) .EQ. M3INT ) THEN
            WRBUF3 = ( 0 .NE. BUFPUT3I( FID, VID, SIZE, IDUM, TSTEP, BUFFER ) )
        ELSE IF ( VTYPE3( VID,FID ) .EQ. M3DBLE ) THEN
            WRBUF3 = ( 0 .NE. BUFPUT3D( FID, VID, SIZE, IDUM, TSTEP, BUFFER ) )
        ELSE IF ( VTYPE3( VID,FID ) .EQ. M3INT8 ) THEN
            WRBUF3 = ( 0 .NE. BUFPUT3D( FID, VID, SIZE, IDUM, TSTEP, BUFFER ) )
        ELSE
            WRITE( MESG, '( A, I9 )' ) 'Unsupported variable-type', VTYPE3( VID,FID )
            CALL M3MESG( MESG )
            WRBUF3 = .FALSE.
        END IF

        RETURN

    ELSE    !  "all-variables" write request

        I = 1       !  starting subscript for buffer-slice being written

        OKFLAG = .TRUE.

        DO  99  V = 1, NVARS3( FID )

            WFLAG = .TRUE.

            IF ( LDATE3( V,FID ) .EQ. IMISS3 ) THEN !  first call

                IF ( JSTEP .NE. 0 ) THEN

                    ILAST3( V,FID ) = IDUM
                    LDATE3( V,FID ) = JDATE
                    LTIME3( V,FID ) = JTIME

                ELSE

                    ILAST3( V,FID ) = 0
                    LDATE3( V,FID ) = 0
                    LTIME3( V,FID ) = 0
                    NDATE3( V,FID ) = 0
                    NTIME3( V,FID ) = 0

                END IF

            ELSE IF ( NDATE3( V,FID ) .EQ. ADATE  .AND.     &
                      NTIME3( V,FID ) .EQ. ATIME ) THEN ! advance 1 timestep

                IF ( JSTEP .NE. 0 ) THEN

                    ILAST3( V,FID ) = 1 - IDUM
                    LDATE3( V,FID ) = NDATE3( V,FID )
                    LTIME3( V,FID ) = NTIME3( V,FID )
                    NDATE3( V,FID ) = JDATE
                    NTIME3( V,FID ) = JTIME

                ELSE

                    ILAST3( V,FID ) = 0
                    LDATE3( V,FID ) = 0
                    LTIME3( V,FID ) = 0
                    NDATE3( V,FID ) = 0
                    NTIME3( V,FID ) = 0

                END IF

            ELSE IF ( LDATE3( V,FID ) .EQ. ZDATE  .AND.     &
                      LTIME3( V,FID ) .EQ. ZTIME ) THEN  ! retreat 1 time step

                ILAST3( V,FID ) = 1 - IDUM      !  case must be time-dependent
                NDATE3( V,FID ) = LDATE3( V,FID )
                NTIME3( V,FID ) = LTIME3( V,FID )
                LDATE3( V,FID ) = JDATE
                LTIME3( V,FID ) = JTIME

            ELSE IF ( NDATE3( V,FID ) .EQ. IMISS3 ) THEN  ! second call,
            ! time dependent

                IF (  LDATE3( V,FID ) .EQ. ADATE  .AND.     &
                      LTIME3( V,FID ) .EQ. ATIME ) THEN     !  step forward

                    NDATE3( V,FID ) = JDATE
                    NTIME3( V,FID ) = JTIME

                ELSE IF (  LDATE3( V,FID ) .EQ. ZDATE  .AND.        &
                           LTIME3( V,FID ) .EQ. ZTIME ) THEN ! step backward

                    ILAST3( V,FID ) = IDUM
                    LDATE3( V,FID ) = NDATE3( V,FID )
                    LTIME3( V,FID ) = NTIME3( V,FID )
                    NDATE3( V,FID ) = JDATE
                    NTIME3( V,FID ) = JTIME

                END IF

            ELSE IF ( ( JDATE .NE. LDATE3( V,FID ) .OR.     &
                        JTIME .NE. LTIME3( V,FID ) )        &
                      .AND.                                 &
                      ( JDATE .NE. NDATE3( V,FID ) .OR.     &
                        JTIME .NE. NTIME3( V,FID ) ) ) THEN

                WRITE( MESG, 93020 )                        &
                    'Date/time being written to BUFFERED file ' // FLIST3( FID ) // ':', JDATE, JTIME
                CALL M3MSG2( MESG )
                WRITE( MESG, 93020 ) 'Last date/time  written:', LDATE3( V,FID ), LTIME3( V,FID )
                CALL M3MSG2( MESG )
                MESG = 'Out-of-order write to BUFFERED file '
                CALL M3WARN( 'WRITE3/WRBUF3', JDATE, JTIME, MESG )

                WRBUF3 = .FALSE.
                RETURN

            END IF!  if "advance 1", "retreat 1", or not "last" or "next"

            IF ( VTYPE3( V,FID ) .EQ. M3REAL ) THEN
                WFLAG = ( 0 .NE. BUFPUT3( FID, V, SIZE, IDUM, TSTEP , BUFFER(I) ) )
            ELSE IF ( VTYPE3( V,FID ) .EQ. M3INT ) THEN
                WFLAG = ( 0 .NE. BUFPUT3I( FID, V, SIZE, IDUM, TSTEP , BUFFER(I) ) )
            ELSE IF ( VTYPE3( V,FID ) .EQ. M3DBLE ) THEN
                WFLAG = ( 0 .NE. BUFPUT3D( FID, V, SIZE, IDUM, TSTEP , BUFFER(I) ) )
            ELSE IF ( VTYPE3( V,FID ) .EQ. M3INT8 ) THEN
                WFLAG = ( 0 .NE. BUFPUT3D( FID, V, SIZE, IDUM, TSTEP , BUFFER(I) ) )
            ELSE
                WRITE( MESG, '( A, I9 )' ) 'Unsupported variable-type', VTYPE3( V,FID )
                CALL M3MESG( MESG )
                WFLAG = .FALSE.
            END IF

            IF ( VTYPE3( V,FID ) .EQ. M3DBLE .OR.       &
                 VTYPE3( V,FID ) .EQ. M3INT8 ) THEN
                I = I + 2*SIZE    !  set up for next variable's slice of buffer()
            ELSE
                I = I + SIZE    !  set up for next variable's slice of buffer()
            END IF

            IF( .NOT.WFLAG ) THEN

                MESG = 'Failure writing ' // VLIST3( V,FID ) // ' to ' // FLIST3( FID )
                CALL M3WARN( 'WRITE3/WRBUF3', JDATE, JTIME, MESG )
                OKFLAG = .FALSE.

            END IF  !  if wflag:  bufput failed for this variable

99      CONTINUE            !  end loop on variables in this file

        MXREC3( FID ) = MAX( MXREC3( FID ), STEP )

        WRBUF3 = OKFLAG

    END IF  !  if vid>0 (one-vble request), or not (all-vbles request)

    RETURN

    !******************  FORMAT  STATEMENTS   ******************************

    !...........   Internal buffering formats..... 93xxx

93020 FORMAT ( A, :, I9, ':', I6.6, :, A, :, 2X, I6.6 )

END FUNCTION WRBUF3
