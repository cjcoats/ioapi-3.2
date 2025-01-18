
LOGICAL FUNCTION WRBNDARY( FID, VID, TSTAMP, STEP2, BUFFER )

    !***********************************************************************
    ! Version "$Id: wrbndary.f90 203 2021-10-14 18:02:11Z coats $"
    ! EDSS/Models-3 I/O API.
    ! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
    ! (C) 2003-2010 Baron Advanced Meteorological Systems,
    ! (C) 2007-2013,2021 Carlie J. Coats, Jr., and
    ! (C) 2015 UNC Institute for the Environment.
    ! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    ! See file "LGPL.txt" for conditions of use.
    !.........................................................................
    !  function body starts at line  72
    !
    !  FUNCTION:  writes data from Models-3 BNDARY data file with STATE3
    !             index FID, for alll variables and layers, for time step
    !             record STEP.
    !
    !  RETURN VALUE:  TRUE iff the operation succeeds
    !
    !  PRECONDITIONS REQUIRED:  Should only be called by WRITE3(), after it
    !             has checked that file and time step are available, and that
    !             file type is BNDARY3.
    !
    !  SUBROUTINES AND FUNCTIONS CALLED:  WRVARS
    !
    !  REVISION  HISTORY:
    !       prototype 3/92 by CJC
    !
    !       revised  10/94 by CJC:  allow write-by-variable; record
    !               time-step number as time step flag; restart files.
    !
    !       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
    !
    !       Modified 08/2015 by CJC for I/O API 3.2:  USE MODNCFIO
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
    INTEGER, INTENT(IN   ) :: VID             !  vble index within the STATE3 commons
    INTEGER, INTENT(IN   ) :: TSTAMP( 2 )     !  ( jdate yyyyddd, jtime hhmmss )
    INTEGER, INTENT(IN   ) :: STEP2           !  file record number (maybe mod 2)
    REAL   , INTENT(IN   ) :: BUFFER(*)       !  buffer array for input


    !...........   EXTERNAL FUNCTIONS and their descriptions:

    LOGICAL, EXTERNAL :: WRVARS     !  write "variables" part of timestep record
    EXTERNAL          :: INITBLK3        !!  BLOCK DATA to initialize STATE3 commons


    !...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER         DELTA           !  d(INDX) / d(NCVGTcall)
    INTEGER         DIMS ( 5 )      !  corner arg array for NF_PUT_VARA()
    INTEGER         DELTS( 5 )      !  corner arg array for NF_PUT_VARA()


    !***********************************************************************
    !   begin body of function  WRBNDARY

    !.......   Set up args for WRVARS:

    DELTA = 2 * NTHIK3( FID )
    DELTA = DELTA * ( NCOLS3( FID ) + NROWS3( FID ) + DELTA )

    DIMS ( 1 ) = 1
    DELTS( 1 ) = DELTA

    DIMS ( 2 ) = 1
    DELTS( 2 ) = NLAYS3( FID )

    DIMS ( 3 ) = STEP2
    DELTS( 3 ) = 1

    DELTA = DELTA * NLAYS3( FID )

    DIMS ( 4 ) = 0
    DELTS( 4 ) = 0

    DIMS ( 5 ) = 0
    DELTS( 5 ) = 0


    !...........   Perform the writes:

    WRBNDARY = WRVARS( FID, VID, TSTAMP, STEP2, DIMS, DELTS, DELTA, BUFFER )

    RETURN

END FUNCTION WRBNDARY


