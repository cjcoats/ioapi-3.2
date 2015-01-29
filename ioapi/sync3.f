
      LOGICAL FUNCTION SYNC3( FNAME )

C***********************************************************************
C Version "$Id: sync3.f 100 2015-01-16 16:52:16Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
C (C) 2003-2010 Baron Advanced Meteorological Systems
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  function body starts at line  66
C
C  FUNCTION:
C       Performs disk synchronization for file FNAME
C
C  RETURN VALUE:
C       TRUE iff the operation succeeds (and the data is available)
C
C  PRECONDITIONS REQUIRED:
C       FNAME is a netCDF-based Models-3 data file already opened by OPEN3()
C
C  REVISION  HISTORY:  
C       prototype 3/2002 by Carlie J. Coats, Jr., MCNC-EMC
C
C       Modified 7/2003 by CJC:  bugfix -- clean up critical sections
C       associated with INIT3()
C
C       Modified 7/2007 by CJC:  bugfix -- format at line 120
C
C       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
C***********************************************************************

      IMPLICIT NONE

C...........   INCLUDES:

        INCLUDE 'PARMS3.EXT'
        INCLUDE 'STATE3.EXT'
        INCLUDE 'NETCDF.EXT'


C...........   ARGUMENTS and their descriptions:

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !  logical file name


C...........   EXTERNAL FUNCTIONS and their descriptions:

        INTEGER, EXTERNAL :: INIT3      !  initialize I/O API
        INTEGER, EXTERNAL :: INDEX1     !  look up names in name tables


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         FID             !  subscript  for STATE3 arrays
        INTEGER         IERR            !  netCDF status return
        LOGICAL         EFLAG
        CHARACTER*16    FIL16           !  scratch file-name buffer
        CHARACTER*256   MESG

C***********************************************************************
C   begin body of function  SYNC3
C.......   Check that Models-3 I/O has been initialized:
 
        EFLAG = .FALSE.
!$OMP   CRITICAL( S_INIT )
        IF ( .NOT. FINIT3 ) THEN
            LOGDEV = INIT3()
            EFLAG = .TRUE.
        END IF          !  if not FINIT3
!$OMP   END CRITICAL( S_INIT )
        IF ( EFLAG ) THEN
            SYNC3 = .FALSE.
            CALL M3MSG2(  'SYNC3:  I/O API not yet initialized.' )
            RETURN
        END IF

C...........   Check length of name arguments; copy into length=16 buffers

        EFLAG = .FALSE.
        IF ( LEN_TRIM( FNAME ) .GT. NAMLEN3 ) THEN
            EFLAG = .TRUE.
            MESG  = 'File "' // FNAME // '"'
            CALL M3MSG2( MESG )
            WRITE( MESG, '( A , I10 )' )
     &          'Max file name length 16; actual:', LEN_TRIM( FNAME )
            CALL M3MSG2( MESG )
        END IF          !  if len( fname ) > 16

        IF ( EFLAG ) THEN
            MESG = 'Invalid variable or file name arguments'
            CALL M3WARN( 'SYNC3', 0,0, MESG )
	    SYNC3 = .FALSE.
            RETURN
        END IF          !  if len( fname ) > 16, or if len( vname ) > 16

        FIL16 = FNAME
        FID   = INDEX1( FIL16, COUNT3, FLIST3 )

        IF ( FID .EQ. 0 ) THEN  !  file not available

            MESG = 'File:  '//FIL16// ' not yet opened.'
            CALL M3WARN( 'SYNC3', 0,0, MESG )
            SYNC3 = .FALSE.

        ELSE IF ( CDFID3( FID ) .GE. 0 ) THEN   !  netCDF file

!$OMP       CRITICAL( S_NC )
            CALL NCSNC( CDFID3( FID ), IERR )
!$OMP       END CRITICAL( S_NC )

            IF ( IERR .EQ. 0 ) THEN
                SYNC3 = .TRUE.
            ELSE

                WRITE( MESG, '( A , I5, 2X, A, 2X, A )' )
     &              'netCDF error number', IERR,
     &              'with disk synchronization for file:', FIL16
                CALL M3WARN( 'SYNC3', 0,0, MESG )
                SYNC3 = .FALSE.
            END IF      !  if ncsnc() failed

        ELSE            !  not a netCDF file...default TRUE

            SYNC3 = .TRUE.

        END IF          !  if file not available, or if file is volatile

        RETURN

        END FUNCTION SYNC3


        
