
LOGICAL FUNCTION RDCUSTOM( FID, VID, LAYER, STEP, BUFFER )

    !***********************************************************************
    ! Version "$Id: rdcustom.f90 203 2021-10-14 18:02:11Z coats $"
    ! EDSS/Models-3 I/O API.
    ! Copyright (C) 1992-2002 MCNC, (C) 1992-2012 Carlie J. Coats, Jr.,
    ! (C) 2003-2011 Baron Advanced Meteorological Systems,
    ! (C) 2015-2020 UNC Institute for the Environment,
    ! (C) 2021 Carlie J. Coats, Jr.
    ! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    ! See file "LGPL.txt" for conditions of use.
    !.........................................................................
    !  function body starts at line  82
    !
    !  FUNCTION:  reads data from Models-3 CUSTOM data file with state-variable
    !             file index FID, for variable VID and layer LAYER, for the
    !             time step record STEP.
    !             If VID is -1, reads all variables; if LAYER is -1,
    !             reads all layers.
    !
    !  RETURN VALUE:  TRUE iff the operation succeeds (and the data is available)
    !
    !  PRECONDITIONS REQUIRED:  Should only be called by READ3(), after it
    !             has checked for file, time step, and layer availability,
    !             and that file type is CUSTOM3.
    !
    !  SUBROUTINES AND FUNCTIONS CALLED:  RDVARS
    !
    !  REVISION  HISTORY:
    !	prototype 3/1992 by CJC
    !
    !       modified  9/1994 by CJC:  argument is now VID, not VNAME
    !
    !       Modified 10/2003 by CJC for I/O API version 3:  support for
    !       native-binary BINFIL3 file type; uses INTEGER NAME2FID
    !
    !       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
    !
    !       Modified 08/2015 by CJC: USE MODNCFIO for I/O API v3.2
    !
    !       Version 10/2021 by CJC:  free ".f90" source format for IOAPI-4.0
    !***********************************************************************

    USE MODNCFIO

    IMPLICIT NONE

    !...........   INCLUDES:

    INCLUDE 'PARMS3.EXT'
    INCLUDE 'STATE3.EXT'


    !...........   ARGUMENTS and their descriptions:

    INTEGER, INTENT(IN   ) :: FID             !  file index within the STATE3 commons
    INTEGER, INTENT(IN   ) :: VID             !  variable index, or 0
    INTEGER, INTENT(IN   ) :: LAYER           !  layer number,   or 0
    INTEGER, INTENT(IN   ) :: STEP            !  time step record number
    REAL   , INTENT(  OUT) :: BUFFER(*)       !  buffer array for input


    !...........   EXTERNAL FUNCTIONS and their descriptions:

    LOGICAL, EXTERNAL :: RDVARS     !  read "variables" part of timestep records
    EXTERNAL          :: INITBLK3   !  block data: initialize I/O state


    !...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER         DELTA           !  d(INDX) / d(NF_GET_VARA_* call)
    INTEGER         DIMS ( 5 )      !  corner arg array for NF_GET_VARA_*()
    INTEGER         DELTS( 5 )      !  corner arg array for NF_GET_VARA_*()


    !***********************************************************************
    !   begin body of function  RDCUSTOM

    !.......   Set up DIMS and DELTS arguments for NCVGT() according
    !.......   to whether request is for all layers or not:

    DIMS ( 1 ) = 1
    DELTS( 1 ) = NCOLS3( FID )

    DIMS ( 3 ) = STEP
    DELTS( 3 ) = 1

    IF ( LAYER .EQ. ALLAYS3 ) THEN

        DIMS ( 2 ) = 1
        DELTS( 2 ) = NLAYS3( FID )

        DELTA = NCOLS3( FID ) * NLAYS3( FID )

    ELSE

        DIMS ( 2 ) = LAYER
        DELTS( 2 ) = 1

        DELTA = NCOLS3( FID )

    END IF

    DIMS ( 4 ) = 0
    DELTS( 4 ) = 0

    DIMS ( 5 ) = 0
    DELTS( 5 ) = 0


    !...........   Perform the reads, according to VNAME

    RDCUSTOM = RDVARS( FID, VID, DIMS, DELTS, DELTA, BUFFER )

    RETURN

END FUNCTION RDCUSTOM

