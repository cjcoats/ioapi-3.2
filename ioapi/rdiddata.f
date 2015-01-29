
        LOGICAL FUNCTION RDIDDATA( FID, VID, LAYER, STEP, BUFFER )

C***********************************************************************
C Version "$Id: rdiddata.f 100 2015-01-16 16:52:16Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (C) 2003-2010 by Baron Advanced Meteorological Systems.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  function body starts at line  78
C
C  FUNCTION:  reads data from Models-3 IDDATA data file with state-variable
C             index FID, for variable VID and layer LAYER, for the
C             time step record STEP.
C             If VID is -1=ALLAYS3, reads all variables; if LAYER is -1,
C             reads all layers.
C
C  RETURN VALUE:  TRUE iff the operation succeeds (and the data is available)
C
C  PRECONDITIONS REQUIRED:  Should only be called by READ3(), after it
C             has checked for file, time step, and layer availability,
C             and that the file type is IDDATA3.
C
C  SUBROUTINES AND FUNCTIONS CALLED:  RDVARS
C
C  REVISION  HISTORY:  
C       prototype 3/1992 by CJC
C
C       Modified  9/1994 by CJC:  argument VID instead of VNAME
C
C       Modified 10/2003 by CJC for I/O API version 3:  support for
C       native-binary BINFIL3 file type; uses INTEGER NAME2FID
C
C       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
C***********************************************************************

      IMPLICIT NONE

C...........   INCLUDES:

        INCLUDE 'PARMS3.EXT'
        INCLUDE 'STATE3.EXT'
        INCLUDE 'NETCDF.EXT'


C...........   ARGUMENTS and their descriptions:

        INTEGER, INTENT(IN   ) :: FID             !  file index within the STATE3 commons
        INTEGER, INTENT(IN   ) :: VID             !  variable index or -1 == ALL
        INTEGER, INTENT(IN   ) :: LAYER           !  layer number,  or -1 == ALL
        INTEGER, INTENT(IN   ) :: STEP            !  time step record number
        REAL   , INTENT(  OUT) :: BUFFER(*)       !  buffer array for input


C...........   EXTERNAL FUNCTIONS and their descriptions:

        LOGICAL, EXTERNAL :: RDVARS
        EXTERNAL          :: INITBLK3        !!  BLOCK DATA to initialize STATE3 commons


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         IERR            !  netCDF error status return
        INTEGER         INDX            !  subscript location in BUFFER(*)
        INTEGER         DELTA           !  d(INDX) / d(NCVGTcall)
        INTEGER         DIMS ( 5 )      !  corner arg array for NCVGT()
        INTEGER         DELTS( 5 )      !  corner arg array for NCVGT()
        CHARACTER*80    MESG


C***********************************************************************
C   begin body of function  RDIDDATA

C.......   Read the site count for this time step

        IF ( CDFID3( FID ) .GE. 0 ) THEN        !  netCDF file

            DIMS ( 1 ) = STEP

!$OMP CRITICAL( S_NC )
            CALL NCVGT1( CDFID3( FID ), NINDX3( FID ),
     &                   DIMS, BUFFER, IERR )
!$OMP END CRITICAL( S_NC )
            IF ( IERR .NE. 0 ) THEN
                WRITE( MESG,91010 ) 'netCDF error number', IERR
                CALL M3MSG2( MESG )
                MESG = 'Error reading count for file '//FLIST3( FID )
                CALL M3WARN( 'READ3/RDIDDATA', 0, 0, MESG )
                RDIDDATA = .FALSE.
                RETURN
            END IF          !  ierr nonzero:  NCVGTC() failed


C.......   Read the site ID list for this time step

            DIMS ( 1 ) = 1
            DELTS( 1 ) = NROWS3( FID )

            DIMS ( 2 ) = STEP
            DELTS( 2 ) = 1

!$OMP CRITICAL( S_NC )
            CALL NCVGT( CDFID3( FID ), SINDX3( FID ),
     &                  DIMS, DELTS, BUFFER( 2 ), IERR )
!$OMP END CRITICAL( S_NC )
            IF ( IERR .NE. 0 ) THEN
                WRITE( MESG,91010 ) 'netCDF error number', IERR
                CALL M3MSG2( MESG )
                MESG = 'Error reading ID list for file '//FLIST3( FID )
                CALL M3WARN( 'READ3/RDIDDATA', 0, 0, MESG )
                RDIDDATA = .FALSE.
                RETURN
            END IF          !  ierr nonzero:  NCVGTC() failed

            INDX = 2 + NROWS3( FID )

        ELSE

            INDX = 1

        END IF          !  if netCDF file


C.......   Set up DIMS and DELTS arguments for NCVGT() according to
C.......   whether or not request is for is all layers:

        DIMS ( 1 ) = 1
        DELTS( 1 ) = NROWS3( FID )

        DIMS ( 3 ) = STEP
        DELTS( 3 ) = 1

        DIMS ( 4 ) = 0
        DELTS( 4 ) = 0

        DIMS ( 5 ) = 0
        DELTS( 5 ) = 0
        
        IF ( LAYER .EQ. ALLAYS3 ) THEN

            DIMS ( 2 ) = 1
            DELTS( 2 ) = NLAYS3( FID )

            DELTA = NROWS3( FID ) * NLAYS3( FID )

        ELSE         !  read a specific layer

            DIMS ( 2 ) = LAYER
            DELTS( 2 ) = 1

            DELTA = NROWS3( FID )

        END IF          !  if layer == alllays or not


C...........   Perform the reads, according to VID

        RDIDDATA = RDVARS( FID, VID, DIMS, DELTS, DELTA,
     &                     BUFFER( INDX ) )

        RETURN

C******************  FORMAT  STATEMENTS   ******************************

C...........   Error and warning message formats..... 91xxx

91010   FORMAT ( A , : , I5 )


        END FUNCTION RDIDDATA

