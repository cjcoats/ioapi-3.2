
LOGICAL FUNCTION  XTBUF3( FID, VID,                             &
                          LAY0, LAY1, ROW0, ROW1, COL0, COL1,   &
                          JDATE, JTIME,                         &
                          BUFFER )                              &
                  RESULT( XTFLAG )

    !***********************************************************************
    ! Version "$Id: xtbuf3.f90 203 2021-10-14 18:02:11Z coats $"
    ! EDSS/Models-3 I/O API.
    ! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
    ! (C) 2003-2010 Baron Advanced Meteorological Systems,
    ! (C) 2007-2013,2021 Carlie J. Coats, Jr., and
    ! (C) 2015-2020 UNC Institute for the Environment.
    ! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    ! See file "LGPL.txt" for conditions of use.
    !.........................................................................
    !  function body starts at line  91
    !
    !  FUNCTION:  reads data from Models-3 BUFFERED "file" with M3 file
    !       index FID for variable with index VID and indicated col-row-layer
    !       window for the date and time JDATE (coded YYYYDDD) JTIME (HHMMSS).
    !       For time-independent files, JDATE:JTIME are ignored.
    !       If VNAME is 'ALL', reads all variables; if LAYER is -1,
    !       reads all layers.
    !
    !  RETURN VALUE:  TRUE iff the operation succeeds (and the data is available)
    !
    !  PRECONDITIONS REQUIRED:
    !       FNAME is a Models-3 BUFFERED "file" already opened by OPEN3().
    !       Should only be called via XTRACT3().
    !       For ALLLVARS3 xtracts, all variables must be of type M3REAL
    !
    !  SUBROUTINES AND FUNCTIONS CALLED:
    !       JSTEP3, BUFXTR3, BUFXTR3D, BUFXTR3I
    !
    !  REVISION  HISTORY:
    !       Prototype 7/1994 by CJC
    !
    !	Modified 10/1994 by CJC to work with XTRACT3() having granularity
    !	at the level of individual variables.
    !
    !       Modified 5/2002 to support types other than REAL
    !
    !       Modified 03/2010 by CJC
    !
    !       Modified 02/2015 by CJC for I/O API 3.2: Support for INTEGER*8,
    !       USE M3UTILIO
    !
    !       Bug-fixes 04/2020 from Fahim Sidi, US EPA
    !
    !       Version 10/2021 by CJC:  free ".f90" source format for IOAPI-4.0
    !***********************************************************************

    USE M3UTILIO

    IMPLICIT NONE

    !...........   INCLUDES:

    INCLUDE 'STATE3.EXT'

    !...........   ARGUMENTS and their descriptions:

    INTEGER, INTENT(IN   ) :: FID             !  file     id for STATE3 arrays
    INTEGER, INTENT(IN   ) :: VID             !  variable id  "     "     "
    INTEGER, INTENT(IN   ) :: LAY0            !  lower layer  bound
    INTEGER, INTENT(IN   ) :: LAY1            !  upper layer  bound
    INTEGER, INTENT(IN   ) :: ROW0            !  lower row    bound
    INTEGER, INTENT(IN   ) :: ROW1            !  upper row    bound
    INTEGER, INTENT(IN   ) :: COL0            !  lower column bound
    INTEGER, INTENT(IN   ) :: COL1            !  upper column bound
    INTEGER, INTENT(IN   ) :: JDATE           !  date, formatted YYYYDDD
    INTEGER, INTENT(IN   ) :: JTIME           !  time, formatted HHMMSS
    REAL   , INTENT(  OUT) :: BUFFER(*)       !  output buffer array

    !...........   EXTERNAL FUNCTIONS and their descriptions:

    INTEGER, EXTERNAL :: BUFXTR3, BUFXTR3D, BUFXTR3I    !  from bufint3.c


    !...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER       VAR, STEP, SIZE, IOFF       !  loop counter (over variables)
    CHARACTER*256 MESG


    !***********************************************************************
    !   begin body of function  XTBUF3

    IF ( VID .GT. 0 ) THEN		!  xtract on just this variable

        IF ( TSTEP3( FID ) .EQ. 0 ) THEN

            IF( LDATE3( VID,FID ) .EQ. 0 ) THEN
                STEP = ILAST3( VID,FID )
            ELSE
                XTFLAG = .FALSE.
                RETURN
            END IF

        ELSE IF ( JDATE .EQ. LDATE3( VID,FID ) .AND.        &
                  JTIME .EQ. LTIME3( VID,FID ) ) THEN

            STEP = ILAST3( VID,FID )

        ELSE IF ( JDATE .EQ. NDATE3( VID,FID ) .AND.        &
                  JTIME .EQ. NTIME3( VID,FID ) ) THEN

            STEP = 1 - ILAST3( VID,FID )        !  formula swaps 0 and 1

        ELSE

            WRITE( MESG, '( 4A )' )                         &
                'Date and time not available for ',         &
                VLIST3( VID,FID ), ' in ', FLIST3( FID )
            CALL M3WARN( 'XTRACT3/XTBUF3', JDATE, JTIME, MESG )

            XTFLAG = .FALSE.
            RETURN

        END IF

        IF ( VTYPE3( VID,FID ) .EQ. M3REAL ) THEN
            XTFLAG = ( 0 .NE. BUFXTR3( FID, VID,                                    &
                                       LAY0, LAY1, ROW0, ROW1, COL0, COL1,          &
                                       NLAYS3( FID ), NROWS3( FID ), NCOLS3( FID ), &
                                       STEP, BUFFER ) )
        ELSE IF ( VTYPE3( VID,FID ) .EQ. M3INT ) THEN
            XTFLAG = ( 0 .NE. BUFXTR3I( FID, VID,                                   &
                                        LAY0, LAY1, ROW0, ROW1, COL0, COL1,         &
                                        NLAYS3( FID ), NROWS3( FID ), NCOLS3( FID ),&
                                        STEP, BUFFER ) )
        ELSE IF ( VTYPE3( VID,FID ) .EQ. M3DBLE ) THEN
            XTFLAG = ( 0 .NE. BUFXTR3D( FID, VID,                                   &
                                        LAY0, LAY1, ROW0, ROW1, COL0, COL1,         &
                                        NLAYS3( FID ), NROWS3( FID ), NCOLS3( FID ),&
                                        STEP, BUFFER ) )
        ELSE IF ( VTYPE3( VID,FID ) .EQ. M3INT8 ) THEN
            XTFLAG = ( 0 .NE. BUFXTR3D( FID, VID,                                   &
                                        LAY0, LAY1, ROW0, ROW1, COL0, COL1,         &
                                        NLAYS3( FID ), NROWS3( FID ), NCOLS3( FID ),&
                                        STEP, BUFFER ) )
        ELSE
            MESG = 'NonREAL/nonINT types not supported'
            CALL M3WARN( 'XTRACT3/XTBUF3', JDATE, JTIME, MESG )
            XTFLAG = .FALSE.
            RETURN
        END IF

    ELSE				!  xtract on all variables

        XTFLAG = .TRUE.
        SIZE   = ( LAY1-LAY0+1 )*( ROW1-ROW0+1 )*( COL1-COL0+1 )
        IOFF   = 1

        DO  11  VAR = 1, NVARS3( FID )

            IF ( TSTEP3( FID ) .EQ. 0 ) THEN

                IF( LDATE3( VAR,FID ) .EQ. 0 ) THEN
                    STEP = ILAST3( VAR,FID )
                ELSE
                    XTFLAG = .FALSE.
                    RETURN
                END IF

            ELSE IF ( JDATE .EQ. LDATE3( VAR,FID ) .AND.    &
                      JTIME .EQ. LTIME3( VAR,FID ) ) THEN

                STEP = ILAST3( VAR,FID )

            ELSE IF ( JDATE .EQ. NDATE3( VAR,FID ) .AND.    &
                      JTIME .EQ. NTIME3( VAR,FID ) ) THEN

                STEP = 1 - ILAST3( VAR,FID )        !  formula swaps 0 and 1

            ELSE

                WRITE( MESG, '( 4A )' ) 'Date and time not available for ', VLIST3( VAR,FID ), ' in ', FLIST3( FID )
                CALL M3WARN( 'XTRACT3/XTBUF3', JDATE, JTIME, MESG )
                XTFLAG = .FALSE.
                RETURN

            END IF

            IF ( VTYPE3( VAR,FID ) .EQ. M3REAL ) THEN

                IF ( 0 .EQ. BUFXTR3( FID, VAR,                          &
                                     LAY0, LAY1, ROW0, ROW1,            &
                                     COL0, COL1, NLAYS3( FID ),         &
                                     NROWS3( FID ), NCOLS3( FID ),      &
                                     STEP, BUFFER( IOFF ) ) ) THEN
                    XTFLAG = .FALSE.
                    RETURN
                END IF		!  if bufxtr3() failed.
                IOFF = IOFF + SIZE

            ELSE IF ( VTYPE3( VAR,FID ) .EQ. M3INT ) THEN

                IF ( 0 .EQ. BUFXTR3I( FID, VAR,                         &
                                      LAY0, LAY1, ROW0, ROW1,           &
                                      COL0, COL1, NLAYS3( FID ),        &
                                      NROWS3( FID ), NCOLS3( FID ),     &
                                      STEP, BUFFER( IOFF ) ) ) THEN
                    XTFLAG = .FALSE.
                    RETURN
                END IF		!  if bufxtr3() failed.
                IOFF = IOFF + SIZE

            ELSE IF ( VTYPE3( VAR,FID ) .EQ. M3DBLE ) THEN

                IF ( 0 .EQ. BUFXTR3D( FID, VAR,                         &
                                      LAY0, LAY1, ROW0, ROW1,           &
                                      COL0, COL1, NLAYS3( FID ),        &
                                      NROWS3( FID ), NCOLS3( FID ),     &
                                      STEP, BUFFER( IOFF ) ) ) THEN
                    XTFLAG = .FALSE.
                    RETURN
                END IF		!  if bufxtr3() failed.
                IOFF = IOFF + 2*SIZE

            ELSE IF ( VTYPE3( VAR,FID ) .EQ. M3INT8 ) THEN

                IF ( 0 .EQ. BUFXTR3D( FID, VAR,                         &
                                      LAY0, LAY1, ROW0, ROW1,           &
                                      COL0, COL1, NLAYS3( FID ),        &
                                      NROWS3( FID ), NCOLS3( FID ),     &
                                      STEP, BUFFER( IOFF ) ) ) THEN
                    XTFLAG = .FALSE.
                    RETURN
                END IF		!  if bufxtr3() failed.
                IOFF = IOFF + 2*SIZE

            ELSE

                MESG = 'ALLVAR3 nonREAL/nonINT types not supported'
                CALL M3WARN( 'XTRACT3/XTBUF3', JDATE, JTIME, MESG )
                XTFLAG = .FALSE.
                RETURN

            END IF

11      CONTINUE

    END IF		!  if vid > 0, or not

    RETURN
END FUNCTION  XTBUF3

