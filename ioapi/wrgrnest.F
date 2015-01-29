
        LOGICAL FUNCTION WRGRNEST( FID, TSTAMP, STEP2, BUFFER )

C***********************************************************************
C Version "$Id: wrgrnest.F 100 2015-01-16 16:52:16Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (C) 2003-2010 by Baron Advanced Meteorological Systems.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  function body starts at line  92
C
C  MACHINE DEPENDENCY:  Depends upon how much space location variables
C             take up within BUFFER(*):  managed via parameter DBLSIZE
C             which should be  DBLSIZE = 1  for 64-bit machines (e.g., CRAY),
C             and  DBLSIZE = 2  for other machines
C
C  FUNCTION:  writes data from Models-3 GRNEST data file with STATE3
C             index FID, for alll variables and layers, for time step
C             record STEP.
C
C  RETURN VALUE:  TRUE iff the operation succeeds
C
C  PRECONDITIONS REQUIRED:  Should only be called by WRITE3(), after it
C             has checked that file and time step are available, and that
C             file type is GRNEST3.
C
C  SUBROUTINES AND FUNCTIONS CALLED:  WRVARS
C
C  REVISION  HISTORY:
C       prototype 3/1992 by CJC
C
C       revised  10/1994 by CJC:  allow write-by-variable; record 
C               time-step number as time step flag; restart files.
C
C       Modified  5/1999 by CJC for OpenMP log-file thread-safety
C
C       Modified  2/2002 by CJC for OpenMP netCDF thread-safety
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
        INTEGER, INTENT(IN   ) :: TSTAMP( 2 )     !  ( jdate yyyyddd, jtime hhmmss )
        INTEGER, INTENT(IN   ) :: STEP2           !  file record number (maybe mod 2)
        REAL   , INTENT(IN   ) :: BUFFER(*)       !  buffer array for input


C...........   PARAMETER and its description:
C...........   MACHINE DEPENDENCY:

#if  _CRAY || REAL8
        INTEGER, PARAMETER :: DBLSIZE = 1
#endif
#if  ! ( _CRAY || REAL8 )
        INTEGER, PARAMETER :: DBLSIZE = 2
#endif


C...........   EXTERNAL FUNCTIONS and their descriptions:

        LOGICAL, EXTERNAL :: WRVARS


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER       IERR            !  netCDF error status return
        INTEGER       INDX            !  subscript location in BUFFER(*)
        INTEGER       DELTA           !  d(INDX) / d(NCVGTcall)
        INTEGER       DIMS ( 5 )      !  corner arg array for NCVGT()
        INTEGER       DELTS( 5 )      !  corner arg array for NCVGT()
        LOGICAL       EFLAG           !  error flag


C***********************************************************************
C   begin body of function  WRGRNEST

C...........   Write the site count for this time step

       EFLAG = .FALSE.

!$OMP CRITICAL( S_LOGOUT )
!$OMP CRITICAL( S_NC )

        DIMS ( 1 ) = STEP2

        CALL NCVPT1( CDFID3( FID ), NINDX3( FID ), DIMS, BUFFER, IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )
     &          'netCDF error number', IERR,
     &          'Error writing site count to file ' // FLIST3( FID )
            EFLAG = .TRUE.
            GO TO  999
        END IF          !  ierr nonzero:  NCVPT1() failed

        INDX  = 2
        DELTA = NROWS3( FID )


C.......   Write the site ID list for this time step

        DIMS ( 1 ) = 1
        DELTS( 1 ) = NROWS3( FID )

        DIMS ( 2 ) = STEP2
        DELTS( 2 ) = 1

        CALL NCVPT( CDFID3( FID ), SINDX3( FID ),
     &              DIMS, DELTS, BUFFER( INDX ), IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )
     &          'netCDF error number', IERR,
     &          'Error writing ID list to file ' // FLIST3( FID )
            EFLAG = .TRUE.
            GO TO  999
        END IF          !  ierr nonzero:  NCVPT() failed

        INDX = INDX  +  DELTA


C.......   Write the site  column, row, and level count lists for this time step

        CALL NCVPT( CDFID3( FID ), WCNDX3( FID ),
     &              DIMS, DELTS, BUFFER( INDX ), IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )
     &          'netCDF error number', IERR,
     &          'Error writing site column count list to file '
     &          // FLIST3( FID )
            EFLAG = .TRUE.
            GO TO  999
        END IF          !  ierr nonzero:  NCVPT() failed

        INDX = INDX  +  DELTA

C.......   Write the site profile-count list for this time step

        CALL NCVPT( CDFID3( FID ), WRNDX3( FID ),
     &              DIMS, DELTS, BUFFER( INDX ), IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )
     &          'netCDF error number', IERR,
     &          'Error writing site row count list to file '
     &          // FLIST3( FID )
            EFLAG = .TRUE.
            GO TO  999
        END IF          !  ierr nonzero:  NCVPT() failed

        INDX = INDX  +  DELTA

        CALL NCVPT( CDFID3( FID ), LINDX3( FID ),
     &              DIMS, DELTS, BUFFER( INDX ), IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )
     &          'netCDF error number', IERR,
     &          'Error writing level count list to file '
     &          // FLIST3( FID )
            EFLAG = .TRUE.
            GO TO  999
        END IF          !  ierr nonzero:  NCVPT() failed

        INDX = INDX  +  DELTA


C.......   Write the site X,Y,Z location lists for this time step

        CALL NCVPT( CDFID3( FID ), XINDX3( FID ),
     &              DIMS, DELTS, BUFFER( INDX ), IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )
     &          'netCDF error number', IERR,
     &          'Error writing X-coordinate list to file '
     &          // FLIST3( FID )
            EFLAG = .TRUE.
            GO TO  999
        END IF          !  ierr nonzero:  NCVPT() failed

        INDX = INDX  +  DELTA * DBLSIZE

        CALL NCVPT( CDFID3( FID ), YINDX3( FID ),
     &              DIMS, DELTS, BUFFER( INDX ), IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )
     &          'netCDF error number', IERR,
     &          'Error writing Y-coordinate list to file '
     &          // FLIST3( FID )
            EFLAG = .TRUE.
            GO TO  999
        END IF          !  ierr nonzero:  NCVPT() failed

        INDX = INDX  +  DELTA * DBLSIZE

        CALL NCVPT( CDFID3( FID ), DXNDX3( FID ),
     &              DIMS, DELTS, BUFFER( INDX ), IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )
     &          'netCDF error number', IERR,
     &          'Error writing cellsize DX list to file '
     &          // FLIST3( FID )
            EFLAG = .TRUE.
            GO TO  999
        END IF          !  ierr nonzero:  NCVPT() failed

        INDX = INDX  +  DELTA * DBLSIZE

        CALL NCVPT( CDFID3( FID ), DYNDX3( FID ),
     &              DIMS, DELTS, BUFFER( INDX ), IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )
     &          'netCDF error number', IERR,
     &          'Error writing cellsize DY list to file '
     &          // FLIST3( FID )
            EFLAG = .TRUE.
            GO TO  999
        END IF          !  ierr nonzero:  NCVPT() failed

        INDX = INDX  +  DELTA * DBLSIZE

999     CONTINUE

!$OMP END CRITICAL( S_NC )
!$OMP END CRITICAL( S_LOGOUT )

        IF ( EFLAG ) THEN
            WRGRNEST = .FALSE.
            RETURN
	END IF

C...........   Perform the writes, according to VNAME

        DIMS ( 1 ) = 1
        DELTS( 1 ) = NCOLS3( FID )

        DIMS ( 2 ) = 1
        DELTS( 2 ) = NROWS3( FID )

        DIMS ( 3 ) = 1
        DELTS( 3 ) = NLAYS3( FID )

        DIMS ( 4 ) = STEP2
        DELTS( 4 ) = 1

        DELTA = NCOLS3( FID ) * NROWS3( FID ) * NLAYS3( FID )

        WRGRNEST = WRVARS( FID, ALLAYS3, TSTAMP, STEP2, 
     &                     DIMS, DELTS, DELTA,
     &                     BUFFER( INDX ) )

        RETURN

C******************  FORMAT  STATEMENTS   ******************************

C...........   Error and warning message formats..... 91xxx

91010   FORMAT ( //5X , '>>> WARNING in subroutine WRGRNEST <<<',
     &            2 ( /5X , A , : ) , I5, // )


        END FUNCTION WRGRNEST

