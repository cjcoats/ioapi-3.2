
LOGICAL FUNCTION RDBNDARY( FID, VID, LAYER, STEP, BUFFER )

    !***********************************************************************
    ! Version "$Id: rdbndary.f90 203 2021-10-14 18:02:11Z coats $"
    ! EDSS/Models-3 I/O API.
    ! Copyright (C) 1992-2002 MCNC, (C) 1992-2012 Carlie J. Coats, Jr.,
    ! (C) 2003-2011 Baron Advanced Meteorological Systems,
    ! (c) 2004-2007 Baron Advanced Meteorological Systems,
    ! (C) 2010-2013,2021 Carlie J. Coats, Jr., and
    ! (C) 2015-2020 UNC Institute for the Environment
    ! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    ! See file "LGPL.txt" for conditions of use.
    !.........................................................................
    !  function body starts at line  83
    !
    !  FUNCTION:  reads data from Models-3 GRDDED data file with state-variable
    !             file index FID, for variable VID and layer LAYER, for the
    !             time step record STEP.
    !             If VID is -1 reads all variables; if LAYER is -1,
    !             reads all layers.
    !
    !  RETURN VALUE:  TRUE iff the operation succeeds (and the data is available)
    !
    !  PRECONDITIONS REQUIRED:  Should only be called by READ3(), after it
    !             has checked for file, time step, and layer availability,
    !             and that file type is BNDARY3
    !
    !  SUBROUTINES AND FUNCTIONS CALLED:  RDVARS
    !
    !  REVISION  HISTORY:
    !	prototype 3/1992 by CJC
    !
    !	modified  9/1994 by CJC:  VID argument, not VNAME
    !
    !       Modified 10/2003 by CJC for I/O API version 3:  RDVARS support for
    !       native-binary BINFIL3 file type.
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
    INTEGER, INTENT(IN   ) :: VID             !  variable index , or ALLAYS3
    INTEGER, INTENT(IN   ) :: LAYER           !  layer number, or 0
    INTEGER, INTENT(IN   ) :: STEP            !  time step record number
    REAL   , INTENT(  OUT) :: BUFFER(*)       !  buffer array for input


    !...........   EXTERNAL FUNCTIONS and their descriptions:

    LOGICAL, EXTERNAL :: RDVARS     !  read "variables" part of timestep records
    EXTERNAL          :: INITBLK3   !  block data: initialize I/O state


    !...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER         PERIM           !  2-D boundary-slice volume (cells)
    INTEGER         DELTA           !  d(INDX) / d( NF_GET_VARA_* call )
    INTEGER         DIMS ( 5 )      !  corner arg array for NF_GET_VARA_*()
    INTEGER         DELTS( 5 )      !  corner arg array for NF_GET_VARA_*()


    !***********************************************************************
    !   begin body of function  RDBNDARY

    !.......   Set up DIMS and DELTS arguments for NF_GET_VARA_*(), according
    !.......   to whether request is to read all layers:

    PERIM = 2 * NTHIK3( FID )
    PERIM = ABS( PERIM )*( NCOLS3( FID ) + NROWS3( FID ) + PERIM )

    DIMS ( 1 ) = 1
    DELTS( 1 ) = PERIM

    DIMS ( 3 ) = STEP
    DELTS( 3 ) = 1

    DIMS ( 4 ) = 0
    DELTS( 4 ) = 0

    DIMS ( 5 ) = 0
    DELTS( 5 ) = 0

    IF ( LAYER .EQ. ALLAYS3 ) THEN

        DIMS ( 2 ) = 1
        DELTS( 2 ) = NLAYS3( FID )
        DELTA = PERIM * NLAYS3( FID )

    ELSE    !  read a specific layer

        DIMS ( 2 ) = LAYER
        DELTS( 2 ) = 1
        DELTA = PERIM

    END IF


    !...........   Perform the reads, according to VID

    RDBNDARY = RDVARS( FID, VID, DIMS, DELTS, DELTA, BUFFER )

    RETURN

END FUNCTION RDBNDARY

