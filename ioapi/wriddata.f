
        LOGICAL FUNCTION WRIDDATA( FID, TSTAMP, STEP2, BUFFER )

C***********************************************************************
C Version "$Id: wriddata.f 100 2015-01-16 16:52:16Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (C) 2003-2010 by Baron Advanced Meteorological Systems.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  function body starts at line  74
C
C  FUNCTION:  writes data from Models-3 IDDATA data file with STATE3
C             index FID, for alll variables and layers, for time step
C             record TSTAMP.
C
C  RETURN VALUE:  TRUE iff the operation succeeds
C
C  PRECONDITIONS REQUIRED:  Should only be called by WRITE3(), after it
C             has checked that file and time step are available, and that
C             file type is IDDATA3.
C
C  SUBROUTINES AND FUNCTIONS CALLED:  WRVARS
C
C  REVISION  HISTORY:
C       prototype 3/1992 by CJC
C
C       revised  10/1994 by CJC:  allow write-by-variable; record 
C       	time-step number as time step flag; restart files.
C
C       revised  2/2002 by CJC:  OpenMP thread-safety
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


C...........   EXTERNAL FUNCTIONS and their descriptions:

        LOGICAL, EXTERNAL :: WRVARS     !  write "variables" part of timestep record


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER       IERR            !  netCDF error status return
        INTEGER       INDX            !  subscript location in BUFFER(*)
        INTEGER       DELTA           !  d(INDX) / d(NCVPTcall)
        INTEGER       DIMS ( 5 )      !  corner arg array for NCVPT()
        INTEGER       DELTS( 5 )      !  corner arg array for NCVPT()
	LOGICAL       EFLAG


C***********************************************************************
C   begin body of function  WRIDDATA

C.......   Write the site count for this

       EFLAG = .FALSE.

!$OMP CRITICAL( S_LOGOUT )

        DIMS ( 1 ) = STEP2
        DELTS( 1 ) = 1

!$OMP   CRITICAL( S_NC )
        CALL NCVPT1( CDFID3( FID ), NINDX3( FID ), DIMS, BUFFER, IERR )
!$OMP   END CRITICAL( S_NC )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )
     &          'netCDF error number', IERR,
     &          'Error writing site count to file ' // FLIST3( FID )
            EFLAG = .TRUE.
            GO TO  999
        END IF          !  ierr nonzero:  NCVPT1() failed


C.......   Write the site ID list for this time step

        DIMS ( 1 ) = 1
        DELTS( 1 ) = NROWS3( FID )

        DIMS ( 2 ) = STEP2
        DELTS( 2 ) = 1

        INDX = 2

!$OMP   CRITICAL( S_NC )
        CALL NCVPT( CDFID3( FID ), SINDX3( FID ),
     &              DIMS, DELTS, BUFFER( INDX ), IERR )
!$OMP   END CRITICAL( S_NC )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )
     &          'netCDF error number', IERR,
     &          'Error writing site ID list to file ' // FLIST3( FID )
            EFLAG = .TRUE.
            GO TO  999
        END IF          !  ierr nonzero:  NCVPTC() failed

        INDX = INDX  +  NROWS3( FID )

999     CONTINUE

!$OMP END CRITICAL( S_LOGOUT )

        IF ( EFLAG ) THEN
            WRIDDATA = .FALSE.
            RETURN
	END IF

C...........   Perform the writes of the "variables" part of the data:

        DIMS ( 1 ) = 1
        DELTS( 1 ) = NROWS3( FID )

        DIMS ( 2 ) = 1
        DELTS( 2 ) = NLAYS3( FID )

        DIMS ( 3 ) = STEP2
        DELTS( 3 ) = 1

        DELTA = NROWS3( FID ) * NLAYS3( FID )

        WRIDDATA = WRVARS( FID, ALLAYS3, TSTAMP, STEP2,
     &                     DIMS, DELTS, DELTA,
     &                     BUFFER ( INDX ) )

        RETURN

C******************  FORMAT  STATEMENTS   ******************************

C...........   Error and warning message formats..... 91xxx

91010   FORMAT ( //5X , '>>> WARNING in subroutine WRIDDATA <<<',
     &            2 ( /5X , A , : ) , I5, // )


        END FUNCTION WRIDDATA

