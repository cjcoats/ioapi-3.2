
        LOGICAL FUNCTION RDGRNEST( FID, VID, LAYER, STEP, BUFFER )

C***********************************************************************
C Version "$Id: rdgrnest.F 100 2015-01-16 16:52:16Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (C) 2003-2011 by Baron Advanced Meteorological Systems.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  function body starts at line  96
C
C  MACHINE DEPENDENCY:  Depends upon how much space location variables
C             take up within BUFFER(*):  managed via parameter DBLSIZE
C             which should be  DBLSIZE = 1  for 64-bit machines (e.g., CRAY),
C             and  DBLSIZE = 2  for other machines
C
C  FUNCTION:  reads data from Models-3 GRDDED data file with state-variable
C             index FID, for variable VID and layer LAYER, for the
C             time step record STEP.
C             If VID is -1, reads all variables; if LAYER is -1,
C             reads all layers.
C
C  RETURN VALUE:  TRUE iff the operation succeeds (and the data is available)
C
C  PRECONDITIONS REQUIRED:  Should only be called by READ3(), after it
C             has checked for file, time step, and layer availability,
C             and that file type is GRNEST3.
C
C  SUBROUTINES AND FUNCTIONS CALLED:  RDVARS
C
C  REVISION  HISTORY:  
C       prototype 3/92 by CJC
C
C       Modified  9/94 by CJC:  argument now VID, not VNAME
C
C       Modified  9/99 by CJC:  portability issues
C
C       Modified 10/2003 by CJC for I/O API version 3:  support for
C       native-binary BINFIL3 file type; uses INTEGER NAME2FID
C
C       Modified 03/20010 by CJC: F90 changes for I/O API v3.1
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


C...........   PARAMETER and its description:
C...........   MACHINE DEPENDENCY:

#if  _CRAY || REAL8
        INTEGER, PARAMETER :: DBLSIZE = 1
#endif
#if  ! ( _CRAY || REAL8 )
        INTEGER, PARAMETER :: DBLSIZE = 2
#endif


C...........   EXTERNAL FUNCTIONS and their descriptions:

        LOGICAL, EXTERNAL :: RDVARS     !  read "variables" part of timestep records
        EXTERNAL          :: INITBLK3   !  block data: initialize I/O state


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         IERR            !  netCDF error status return
        INTEGER         INDX            !  subscript location in BUFFER(*)
        INTEGER         DELTA           !  d(INDX) / d(NCVGTcall)
        INTEGER         DIMS ( 5 )      !  corner arg array for NCVGT()
        INTEGER         DELTS( 5 )      !  corner arg array for NCVGT()
        CHARACTER*512   MESG


C***********************************************************************
C   begin body of function  RDGRNEST

C...........   Read the site count for this time step

        IF ( CDFID3( FID ) .GE. 0 ) THEN          !  netcdf file:

            DIMS ( 1 ) = STEP

!$OMP CRITICAL( S_NC )
          CALL NCVGT1( CDFID3( FID ), NINDX3( FID ),
     &                   DIMS, BUFFER, IERR )
!$OMP END CRITICAL( S_NC )
          IF ( IERR .NE. 0 ) THEN
                WRITE( MESG,91010 ) 'netCDF error number', IERR
                CALL M3MSG2( MESG )
                MESG = 'Error reading count for file ' //FLIST3( FID )
                CALL M3WARN( 'READ3/RDGRNEST', 0, 0, MESG )
                RDGRNEST = .FALSE.
                RETURN
            END IF          !  ierr nonzero:  NCVGT1() failed

            INDX  = 2
            DELTA = NROWS3( FID )


C.......   Read the site ID list for this time step

            DIMS ( 1 ) = 1
            DELTS( 1 ) = NROWS3( FID )

            DIMS ( 2 ) = STEP
            DELTS( 2 ) = 1

!$OMP CRITICAL( S_NC )
          CALL NCVGT( CDFID3( FID ), SINDX3( FID ),
     &                  DIMS, DELTS, BUFFER( INDX ), IERR )
!$OMP END CRITICAL( S_NC )
          IF ( IERR .NE. 0 ) THEN
                WRITE( MESG,91010 ) 'netCDF error number', IERR
                CALL M3MSG2( MESG )
                MESG = 'Error reading ID list for file '//FLIST3( FID )
                CALL M3WARN( 'READ3/RDGRNEST', 0, 0, MESG )
                RDGRNEST = .FALSE.
                RETURN
            END IF          !  ierr nonzero:  NCVGT() failed

            INDX = INDX + DELTA


C.......   Read the site profile-count list for this time step

!$OMP CRITICAL( S_NC )
            CALL NCVGT( CDFID3( FID ), LINDX3( FID ),
     &                  DIMS, DELTS, BUFFER( INDX ), IERR )
!$OMP END CRITICAL( S_NC )
            IF ( IERR .NE. 0 ) THEN
                WRITE( MESG,91010 ) 'netCDF error number', IERR
                CALL M3MSG2( MESG )
                MESG = 'Error reading profile count for file '//
     &                 FLIST3( FID )
                CALL M3WARN( 'READ3/RDGRNEST', 0, 0, MESG )
                RDGRNEST = .FALSE.
                RETURN
            END IF          !  ierr nonzero:  NCVGT() failed

            INDX = INDX + DELTA


C.......   Read the site X,Y location lists for this time step

!$OMP CRITICAL( S_NC )
            CALL NCVGT( CDFID3( FID ), XINDX3( FID ),
     &                   DIMS, DELTS, BUFFER( INDX ), IERR )
!$OMP END CRITICAL( S_NC )
            IF ( IERR .NE. 0 ) THEN
                WRITE( MESG,91010 ) 'netCDF error number', IERR
                CALL M3MSG2( MESG )
                MESG = 'Error reading X coord for file '//FLIST3( FID )
                CALL M3WARN( 'READ3/RDGRNEST', 0, 0, MESG )
                RDGRNEST = .FALSE.
                RETURN
            END IF          !  ierr nonzero:  NCVGT() failed

            INDX = INDX + DELTA * DBLSIZE

!$OMP CRITICAL( S_NC )
            CALL NCVGT( CDFID3( FID ), YINDX3( FID ),
     &                   DIMS, DELTS, BUFFER( INDX ), IERR )
!$OMP END CRITICAL( S_NC )
            IF ( IERR .NE. 0 ) THEN
                WRITE( MESG,91010 ) 'netCDF error number', IERR
                CALL M3MSG2( MESG )
                MESG = 'Error reading Y coord for file '//FLIST3( FID )
                CALL M3WARN( 'READ3/RDGRNEST', 0, 0, MESG )
                RDGRNEST = .FALSE.
                RETURN
            END IF          !  ierr nonzero:  NCVGT() failed

            INDX = INDX + DELTA * DBLSIZE


C.......   Read the site cellsize DX,DY lists for this time step

!$OMP CRITICAL( S_NC )
            CALL NCVGT( CDFID3( FID ), DXNDX3( FID ),
     &                  DIMS, DELTS, BUFFER( INDX ), IERR )
!$OMP END CRITICAL( S_NC )
            IF ( IERR .NE. 0 ) THEN
                WRITE( MESG,91010 ) 'netCDF error number', IERR
                CALL M3MSG2( MESG )
                MESG = 'Error reading cell DX for file '//FLIST3( FID )
                CALL M3WARN( 'READ3/RDGRNEST', 0, 0, MESG )
                RDGRNEST = .FALSE.
                RETURN
            END IF          !  ierr nonzero:  NCVGT() failed

            INDX = INDX + DELTA * DBLSIZE

!$OMP CRITICAL( S_NC )
            CALL NCVGT( CDFID3( FID ), DYNDX3( FID ),
     &                  DIMS, DELTS, BUFFER( INDX ), IERR )
!$OMP END CRITICAL( S_NC )
            IF ( IERR .NE. 0 ) THEN
                WRITE( MESG,91010 ) 'netCDF error number', IERR
                CALL M3MSG2( MESG )
                MESG = 'Error reading cell DY for file ' //FLIST3( FID )
                CALL M3WARN( 'READ3/RDGRNEST', 0, 0, MESG )
                RDGRNEST = .FALSE.
                RETURN
            END IF          !  ierr nonzero:  NCVGT() failed

            INDX = INDX + DELTA * DBLSIZE

        ELSE            !  fixup buffer-offset

            INDX = 1

        END IF          !  if netcdf file


C.......   Set up DIMS and DELTS arguments for NCVGT() according to
C.......   whether all layers are requested:

        DIMS ( 1 ) = 1
        DELTS( 1 ) = NCOLS3( FID )

        DIMS ( 2 ) = 1
        DELTS( 2 ) = NROWS3( FID )

        DIMS ( 4 ) = STEP
        DELTS( 4 ) = 1

        DIMS ( 5 ) = 0
        DELTS( 5 ) = 0
        
       IF ( LAYER .EQ. ALLAYS3 ) THEN

            DIMS ( 1 ) = 1
            DELTS( 1 ) = NCOLS3( FID )

            DIMS ( 2 ) = 1
            DELTS( 2 ) = NROWS3( FID )

            DIMS ( 3 ) = 1
            DELTS( 3 ) = NLAYS3( FID )

            DELTA = NCOLS3( FID ) * NROWS3( FID ) * NLAYS3( FID )

        ELSE    !  read a specific layer

            DIMS ( 3 ) = LAYER
            DELTS( 3 ) = 1

            DELTA = NCOLS3( FID ) * NROWS3( FID )

        END IF


C...........   Perform the reads, according to vid

        RDGRNEST = RDVARS( FID, VID, DIMS, DELTS, DELTA,
     &             BUFFER( INDX ) )

        RETURN

C******************  FORMAT  STATEMENTS   ******************************

C...........   Error and warning message formats..... 91xxx

91010   FORMAT ( A , I5 )


        END FUNCTION RDGRNEST

