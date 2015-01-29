
        LOGICAL FUNCTION WRBNDARY( FID, VID, TSTAMP, STEP2, BUFFER )

C***********************************************************************
C Version "$Id: wrbndary.f 100 2015-01-16 16:52:16Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (C) 2003-2010 by Baron Advanced Meteorological Systems.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  function body starts at line  72
C
C  FUNCTION:  writes data from Models-3 BNDARY data file with STATE3
C             index FID, for alll variables and layers, for time step
C             record STEP.
C
C  RETURN VALUE:  TRUE iff the operation succeeds
C
C  PRECONDITIONS REQUIRED:  Should only be called by WRITE3(), after it
C             has checked that file and time step are available, and that
C             file type is BNDARY3.
C
C  SUBROUTINES AND FUNCTIONS CALLED:  WRVARS
C
C  REVISION  HISTORY:  
C       prototype 3/92 by CJC
C
C       revised  10/94 by CJC:  allow write-by-variable; record 
C               time-step number as time step flag; restart files.
C
C       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
C
C***********************************************************************

      IMPLICIT NONE

C...........   INCLUDES:

        INCLUDE 'PARMS3.EXT'
        INCLUDE 'STATE3.EXT'
        INCLUDE 'NETCDF.EXT'


C...........   ARGUMENTS and their descriptions:

        INTEGER, INTENT(IN   ) :: FID             !  file index within the STATE3 commons
        INTEGER, INTENT(IN   ) :: VID             !  vble index within the STATE3 commons
        INTEGER, INTENT(IN   ) :: TSTAMP( 2 )     !  ( jdate yyyyddd, jtime hhmmss )
        INTEGER, INTENT(IN   ) :: STEP2           !  file record number (maybe mod 2)
        REAL   , INTENT(IN   ) :: BUFFER(*)       !  buffer array for input


C...........   EXTERNAL FUNCTIONS and their descriptions:

        LOGICAL, EXTERNAL :: WRVARS     !  write "variables" part of timestep record
        EXTERNAL          :: INITBLK3        !!  BLOCK DATA to initialize STATE3 commons


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         DELTA           !  d(INDX) / d(NCVGTcall)
        INTEGER         DIMS ( 5 )      !  corner arg array for NCVGT()
        INTEGER         DELTS( 5 )      !  corner arg array for NCVGT()


C***********************************************************************
C   begin body of function  WRBNDARY

C.......   Set up args for WRVARS:

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


C...........   Perform the writes:

        WRBNDARY = WRVARS( FID, VID, TSTAMP, STEP2, 
     &                     DIMS, DELTS, DELTA, BUFFER )

        RETURN

        END FUNCTION WRBNDARY


