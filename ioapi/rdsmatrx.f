
        LOGICAL FUNCTION RDSMATRX( FID, VID, STEP, BUFFER )

C***********************************************************************
C Version "$Id: rdsmatrx.f 100 2015-01-16 16:52:16Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (C) 2003-2012 by Baron Advanced Meteorological Systems.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  function body starts at line  72
C
C  FUNCTION:  reads data from Models-3 SMATRX3 data file with state-variable
C             index FID, for variable VID for the time step record STEP.
C             If VID is -1=ALLAYS3, reads all variables.
C
C  RETURN VALUE:  TRUE iff the operation succeeds (and the data is available)
C
C  PRECONDITIONS REQUIRED:  Should only be called by READ3(), after it
C             has checked for file, time step, and variable availability,
C             and that the file type is SMATRX3.
C
C  SUBROUTINES AND FUNCTIONS CALLED:  RDVARS
C
C  REVISION  HISTORY:  
C       prototype 2/1995 by CJC
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
        INTEGER, INTENT(IN   ) :: STEP            !  time step record number
        REAL   , INTENT(  OUT) :: BUFFER(*)       !  buffer array for input


C...........   EXTERNAL FUNCTIONS and their descriptions:

        LOGICAL, EXTERNAL :: RDVARS


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         IERR            !  netCDF error status return
        INTEGER         INDX            !  subscript location in BUFFER(*)
        INTEGER         DELTA           !  d(INDX) / d(NCVGTcall)
        INTEGER         DIMS ( 5 )      !  corner arg array for NCVGT()
        INTEGER         DELTS( 5 )      !  corner arg array for NCVGT()
        CHARACTER*80    MESG


C***********************************************************************
C   begin body of function  RDSMATRX

C.......   Read the max-col-count array for this time step

        DIMS ( 1 ) = 1
        DELTS( 1 ) = NROWS3( FID )

        DIMS ( 2 ) = STEP
        DELTS( 2 ) = 1

        DIMS ( 3 ) = 0
        DELTS( 3 ) = 0

        DIMS ( 4 ) = 0
        DELTS( 4 ) = 0

        DIMS ( 5 ) = 0
        DELTS( 5 ) = 0
        
        IF ( CDFID3( FID ) .GE. 0 ) THEN                !   netcdf file:

!$OMP CRITICAL( S_NC )
            CALL NCVGT( CDFID3( FID ), SINDX3( FID ),
     &                  DIMS, DELTS, BUFFER, IERR )
!$OMP END CRITICAL( S_NC )

            IF ( IERR .NE. 0 ) THEN
                WRITE( MESG,91010 ) 'netCDF error number', IERR
                CALL M3MSG2( MESG )
                MESG = 'Error reading MAXROW for file ' // FLIST3( FID )
                CALL M3WARN( 'READ3/RDSMATRX', 0, 0, MESG )
                RDSMATRX = .FALSE.
                RETURN
            END IF          !  ierr nonzero:  NCVGTC() failed


C.......   Read the column-index array for this time step

            DIMS ( 1 ) = 1
            DELTS( 1 ) = NCOLS3( FID )

            INDX = NROWS3( FID ) + 1

!$OMP CRITICAL( S_NC )
            CALL NCVGT( CDFID3( FID ), LINDX3( FID ),
     &                  DIMS, DELTS, BUFFER( INDX ), IERR )
!$OMP END CRITICAL( S_NC )

            IF ( IERR .NE. 0 ) THEN
                WRITE( MESG,91010 ) 'netCDF error number', IERR
                CALL M3MSG2( MESG )
                MESG = 'Error reading ROW INDEX for file '//FLIST3(FID)
                CALL M3WARN( 'READ3/RDSMATRX', 0, 0, MESG )
                RDSMATRX = .FALSE.
                RETURN
            END IF          !  ierr nonzero:  NCVGTC() failed

C.......   Set up DIMS and DELTS arguments for NCVGT() according to
C.......   whether or not request is for is all layers:

            DELTA = NCOLS3( FID )
            INDX  = INDX + DELTA 

        ELSE            !  fixup buffer-offset

            INDX = 1

        END IF          !  if netcdf file



C...........   Perform the reads, according to VID

111     CONTINUE
        RDSMATRX = RDVARS( FID, VID, DIMS, DELTS, DELTA,
     &                     BUFFER( INDX ) )

        RETURN

C******************  FORMAT  STATEMENTS   ******************************

C...........   Error and warning message formats..... 91xxx

91010   FORMAT ( A , : , I5, :, A )


        END FUNCTION RDSMATRX

