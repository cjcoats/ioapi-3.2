
        LOGICAL FUNCTION CLOSE3 ( FNAME )

C***********************************************************************
C Version "$Id: close3.f 100 2015-01-16 16:52:16Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
C (C) 2003-2010 Baron Advanced Meteorological Systems
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  function body starts at line  74
C
C  FUNCTION:
C       Flushes and closes down file with logical name FNAME
C
C  RETURN VALUE:
C       TRUE iff it succeeds.
C
C  PRECONDITIONS REQUIRED:
C
C   **  NOT TO BE USED IN MODELING CODES  **
C   **  by order of Joan Novak, EPA ORD, and Ed Bilicki, MCNC EMC **
C
C       FNAME is not virtual, does exist, and has been opened
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       CLOSEBIN3, M3MSG2, NAME2FID
C
C  REVISION  HISTORY:  
C       prototype 8/1995 by CJC
C
C       Modified  1/2000 by CJC for OpenMP thread-safety
C
C       Modified 10/2003 by CJC for I/O API version 3:  support for
C       native-binary BINFIL3 file type; use NAME2FID
C
C       Modified 03/20010 by CJC: F9x changes for I/O API v3.1
C***********************************************************************

      IMPLICIT NONE

C...........   INCLUDES:

        INCLUDE 'PARMS3.EXT'
        INCLUDE 'STATE3.EXT'
        INCLUDE 'NETCDF.EXT'


C...........   ARGUMENTS and their descriptions:

        CHARACTER*(*), INTENT(IN   ) :: FNAME   !  logical name of file to be closed


C...........   EXTERNAL FUNCTIONS and their descriptions:

        INTEGER, EXTERNAL :: CLOSEBIN3       !  for BINFIL3 files
        INTEGER, EXTERNAL :: NAME2FID        !  fname ~~> fid lookup


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         FILE            !  file index
        INTEGER         V               !  vble index
        INTEGER         IERR            !  netCDF error status return
        CHARACTER*256   MESG
        LOGICAL         EFLAG


C***********************************************************************
C   begin body of function  CLOSE3
C.......   Find STATE3 index for the file:

        FILE = NAME2FID( FNAME )
        IF ( FILE .EQ. 0 ) THEN !  file not open.
            MESG = 'File "' // FNAME // '" not currently open'
            CALL M3MSG2( MESG )
            CLOSE3 = .FALSE.
            RETURN
        END IF

        EFLAG = .FALSE.

!$OMP CRITICAL( S_NC )

        IF( CDFID3( FILE ) .GE. 0 ) THEN

            CALL NCCLOS( CDFID3( FILE ), IERR )
            IF ( IERR .NE. 0 ) THEN
                MESG =  'Error closing netCDF file "' // FNAME // '"'
                CALL M3MSG2( MESG )
                WRITE( LOGDEV,91010 ) 
     &                  'netCDF error number', IERR, MESG
                EFLAG = .TRUE.
            END IF      !  if ierr nonzero

        ELSE IF ( CDFID3( FILE ) .EQ. BINFIL3 ) THEN

            IF ( 0 .EQ. CLOSEBIN3( FILE ) ) THEN
                MESG =  'Error closing BINFIL3 file "' // FNAME // '"'
                CALL M3MSG2( MESG )
                EFLAG = .TRUE.
            END IF

        END IF          !  if cdfid(file) positive or not

!$OMP END CRITICAL( S_NC )

        IF ( EFLAG ) THEN
            CLOSE3 = .FALSE.
            RETURN
        END IF

        CALL BUFDEL3( FILE )
        FLIST3( FILE ) = CMISS3
        DO  11 V = 1, NVARS3( FILE )
            VLIST3( V,FILE ) = CMISS3
            ILAST3( V,FILE ) = IMISS3
            LDATE3( V,FILE ) = IMISS3
            LTIME3( V,FILE ) = IMISS3
            NDATE3( V,FILE ) = IMISS3
            NTIME3( V,FILE ) = IMISS3
11      CONTINUE
        
        MESG = 'Closing file ' // FNAME
        CALL M3MSG2( MESG )
        CLOSE3 = .TRUE.

        RETURN

C******************  FORMAT  STATEMENTS   ******************************

C...........   Error and warning message formats..... 91xxx

91010   FORMAT ( //5X , '>>> WARNING in subroutine CLOSE3 <<<',
     &            3 ( /5X , A , : ) , I5, // )


        END FUNCTION CLOSE3

