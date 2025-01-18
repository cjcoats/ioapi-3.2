
LOGICAL FUNCTION RDBUF3( FID, VID, LAYER, JDATE, JTIME, BUFFER )

    !***********************************************************************
    ! Version "$Id: rdbuf3.f90 203 2021-10-14 18:02:11Z coats $"
    ! EDSS/Models-3 I/O API.
    ! Copyright (C) 1992-2002 MCNC, (C) 1992-2012 Carlie J. Coats, Jr.,
    ! (C) 2003-2011 Baron Advanced Meteorological Systems,
    ! (c) 2004-2007 Baron Advanced Meteorological Systems,
    ! (C) 2010-2013,2021 Carlie J. Coats, Jr., and
    ! (C) 2015-2020 UNC Institute for the Environment
    ! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    ! See file "LGPL.txt" for conditions of use.
    !.........................................................................
    !  function body starts at line  89
    !
    !  FUNCTION:  reads data from Models-3 BUFFEREd "file" with M3 file
    !       index FID for variable with name VNAME and layer LAYER,
    !       for the date and time JDATE (coded YYYYDDD) JTIME (HHMMSS).
    !       For time-independent files, JDATE:JTIME are ignored.
    !       If VNAME is 'ALL', reads all variables; if LAYER is -1,
    !       reads all layers.
    !
    !  RETURN VALUE:  TRUE iff the operation succeeds (and the data is available)
    !
    !  PRECONDITIONS REQUIRED:  FNAME is a Models-3 BUFFERED "file" already
    !       opened by CREATE3().  Should only be called via READ3().
    !       For ALLLVARS3 reads, all variables must be of type M3REAL
    !
    !  SUBROUTINES AND FUNCTIONS CALLED:
    !       JSTEP3, BUFGET3
    !
    !  REVISION  HISTORY:
    !       Prototype 7/1994 by CJC
    !
    !	Modified 10/1994 by CJC to work with WRITE3() having granularity
    !	at the level of individual variables.
    !
    !       Modified 5/2002 to support types other than REAL
    !
    !       Modified 5/2003 by CJC: bugfix by David Wong, US EPA -- wrong
    !       arguments to the BUFGET*()
    !
    !       Modified 03/2010 by CJC: F9x changes for I/O API v3.1; fix by David Wong, US EPA --
    !       wrong arguments to the BUFGET*()
    !
    !       Modified 08/2011 by CJC: bug-fixes for "all-variables" case
    !
    !       Modified 02/2015 by CJC for I/O API 3.2: Support for M3INT8
    !
    !       Version 10/2021 by CJC:  free ".f90" source format for IOAPI-4.0
    !***********************************************************************

    IMPLICIT NONE

    !...........   INCLUDES:

    INCLUDE 'PARMS3.EXT'
    INCLUDE 'STATE3.EXT'

    !...........   ARGUMENTS and their descriptions:

    INTEGER, INTENT(IN   ) :: FID             !  subscript for STATE3 arrays
    INTEGER, INTENT(IN   ) :: VID             !  subscript for STATE3 arrays
    INTEGER, INTENT(IN   ) :: LAYER           !  layer number, or 0
    INTEGER, INTENT(IN   ) :: JDATE           !  date, formatted YYYYDDD
    INTEGER, INTENT(IN   ) :: JTIME           !  time, formatted HHMMSS
    REAL   , INTENT(  OUT) :: BUFFER(*)       !  input buffer array


    !...........   EXTERNAL FUNCTIONS and their descriptions:

    INTEGER, EXTERNAL :: JSTEP3     !  compute time step record numbers
    INTEGER, EXTERNAL :: BUFGET3, BUFGET3D, BUFGET3I


    !...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER         STEP            !  time step record number
    INTEGER         VAR             !  loop counter:  variables
    INTEGER         SIZE
    INTEGER         IOFF
    INTEGER         ISTAT
    CHARACTER*256   MESG


    !***********************************************************************
    !   begin body of function  RDBUF3

    IF ( LAYER .GT. 0 ) THEN
        SIZE = BSIZE3( FID )
    ELSE
        SIZE = BSIZE3( FID ) * NLAYS3( FID )
    END IF


    IF ( VID .GT. 0 ) THEN          !  read just this variable

        IF ( TSTEP3( FID ) .EQ. 0 ) THEN

            IF( LDATE3( VID, FID ) .EQ. 0 ) THEN
                STEP = ILAST3( VID,FID )
            ELSE
                RDBUF3 = .FALSE.
                RETURN
            END IF

        ELSE IF ( JDATE .EQ. LDATE3( VID,FID ) .AND.        &
                  JTIME .EQ. LTIME3( VID,FID ) ) THEN

            STEP = ILAST3( VID,FID )

        ELSE IF ( JDATE .EQ. NDATE3( VID,FID ) .AND.        &
                  JTIME .EQ. NTIME3( VID,FID ) ) THEN

            STEP = 1 - ILAST3( VID,FID )        !  formula swaps 0 and 1

        ELSE

            CALL M3WARN( 'READ3/RDBUF3', JDATE, JTIME,      &
                'Date and time not available in '// FLIST3( FID ) )

            RDBUF3 = .FALSE.
            RETURN

        END IF

        IF ( VTYPE3( VID,FID ) .EQ. M3REAL ) THEN
            RDBUF3 = ( 0 .NE. BUFGET3 ( FID, VID, LAYER,        &
                                        NLAYS3( FID ),          &
                                        BSIZE3( FID ), STEP,    &
                                        BUFFER ) )
        ELSE IF ( VTYPE3( VID,FID ) .EQ. M3INT ) THEN
            RDBUF3 = ( 0 .NE. BUFGET3I( FID, VID, LAYER,        &
                                        NLAYS3( FID ),          &
                                        BSIZE3( FID ), STEP,    &
                                        BUFFER ) )
        ELSE IF ( VTYPE3( VID,FID ) .EQ. M3DBLE ) THEN
            RDBUF3 = ( 0 .NE. BUFGET3D( FID, VID, LAYER,        &
                                        NLAYS3( FID ),          &
                                        BSIZE3( FID ), STEP,    &
                                        BUFFER ) )
        ELSE IF ( VTYPE3( VID,FID ) .EQ. M3INT8 ) THEN
            RDBUF3 = ( 0 .NE. BUFGET3D( FID, VID, LAYER,        &
                                        NLAYS3( FID ),          &
                                        BSIZE3( FID ), STEP,    &
                                        BUFFER ) )
        END IF

    ELSE                            !  read all variables

        IOFF = 1
        DO  11  VAR = 1, NVARS3( FID )

            IF ( TSTEP3( FID ) .EQ. 0 ) THEN

                IF( LDATE3( VAR, FID ) .EQ. 0 ) THEN
                    STEP = ILAST3( VAR,FID )
                ELSE
                    RDBUF3 = .FALSE.
                    RETURN
                END IF

            ELSE IF ( JDATE .EQ. LDATE3( VAR,FID ) .AND.        &
                      JTIME .EQ. LTIME3( VAR,FID ) ) THEN

                STEP = ILAST3( VAR,FID )

            ELSE IF ( JDATE .EQ. NDATE3( VAR,FID ) .AND.        &
                      JTIME .EQ. NTIME3( VAR,FID ) ) THEN

                STEP = 1 - ILAST3( VAR,FID )        !  formula swaps 0 and 1

            ELSE

                CALL M3WARN( 'READ3/RDBUF3', JDATE, JTIME,      &
                    'Date and time not available in '// FLIST3( FID ) )

                RDBUF3 = .FALSE.
                RETURN

            END IF

            IF ( VTYPE3( VAR,FID ) .EQ. M3REAL ) THEN
                ISTAT = BUFGET3( FID, VAR, LAYER, NLAYS3( FID ),    &
                                 BSIZE3( FID ), STEP,               &
                                 BUFFER( IOFF ) )
                IOFF  = IOFF + SIZE
            ELSE IF ( VTYPE3( VAR,FID ) .EQ. M3INT  ) THEN
                ISTAT = BUFGET3I( FID, VAR, LAYER, NLAYS3( FID ),   &
                                  BSIZE3( FID ), STEP,              &
                                  BUFFER( IOFF ) )
                IOFF  = IOFF + SIZE
            ELSE IF ( VTYPE3( VAR,FID ) .EQ. M3DBLE ) THEN
                ISTAT = BUFGET3D( FID, VAR, LAYER, NLAYS3( FID ),   &
                                  BSIZE3( FID ), STEP,              &
                                  BUFFER( IOFF ) )
                IOFF  = IOFF + 2 * SIZE             !!  for double
            ELSE IF ( VTYPE3( VAR,FID ) .EQ. M3INT8 ) THEN
                ISTAT = BUFGET3D( FID, VAR, LAYER, NLAYS3( FID ),   &
                                  BSIZE3( FID ), STEP,              &
                                  BUFFER( IOFF ) )
                IOFF  = IOFF + 2 * SIZE             !!  for INTEGER*8
            ELSE
                CALL M3MESG( 'RDBUF3:  Unsupported variable-type' )
                ISTAT = 0
            END IF

            IF ( ISTAT .EQ. 0  ) THEN

                RDBUF3 = .FALSE.
                RETURN

            END IF

11      CONTINUE

        RDBUF3 = .TRUE.             !  (if you get to here)

    END IF                          !  read one vble, or read all vbles

    RETURN
END FUNCTION RDBUF3

