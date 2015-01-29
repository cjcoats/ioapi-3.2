
       LOGICAL FUNCTION KFINDX( FNAME, COL, ROW,
     &                          ECOUNT, SDATES, STIMES, KFLENS, EVENTS )

C***********************************************************************
C Version "$Id: kfindx.f 100 2015-01-16 16:52:16Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (C) 2003-2010 by Baron Advanced Meteorological Systems.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  function body starts at line  80
C
C  FUNCTION:  reads the event descriptions for the indicated cell
C 	from the KF-Cloud-Event file with logical name FNAME.
C
C  RETURN VALUE:  TRUE iff the operation succeeds (and the data is available)
C
C  PRECONDITIONS REQUIRED:  FNAME is a  KF-Cloud-Event file already
C       opened by kfindx()
C
C  REVISION  HISTORY:
C       Adapted  4/1996 by CJC from READ3().
C
C       Modified  5/1998 by CJC for OpenMP thread-safety
C
C       Modified  1/2002 by CJC:  check TRIMLEN() of FNAME
C
C       Modified 7/2003 by CJC:  bugfix -- clean up critical sections
C       associated with INIT3()
C
C       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
C***********************************************************************

      IMPLICIT NONE

C...........   INCLUDES:

        INCLUDE 'PARMS3.EXT'
        INCLUDE 'STATE3.EXT'
        INCLUDE 'NETCDF.EXT'


C...........   ARGUMENTS and their descriptions:

        CHARACTER*(*), INTENT(IN   ) :: FNAME      !  logical file name
        INTEGER      , INTENT(IN   ) :: COL        !  column number for this event
        INTEGER      , INTENT(IN   ) :: ROW        !  row    number for this event
        INTEGER      , INTENT(  OUT) :: ECOUNT     !  # of events for this col-row
        INTEGER      , INTENT(  OUT) :: SDATES(*)  !  starting date,  formatted YYYYDDD
        INTEGER      , INTENT(  OUT) :: STIMES(*)  !  starting time,  formatted HHMMSS
        INTEGER      , INTENT(  OUT) :: KFLENS(*)  !  event duration, formatted HHMMSS
        INTEGER      , INTENT(  OUT) :: EVENTS(*)  !  event numbers


C...........   EXTERNAL FUNCTIONS and their descriptions:

        INTEGER, EXTERNAL :: INIT3      !  initialize I/O API
        INTEGER, EXTERNAL :: INDEX1     !  look up names in name tables


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         FID             !  subscript  for STATE3 arrays
        INTEGER         FNUM            !  CDFID3( FID )
        INTEGER         IERR            !  netCDF error status return
        INTEGER         DIMT( 5 )       !  corner   for NCVGT()
        INTEGER         DELT( 5 )       !  diagonal for NCVGT()
        CHARACTER*16    FIL16           !  scratch file-name buffer
        CHARACTER*256   MESG            !  scratch message buffer
        LOGICAL         EFLAG


C***********************************************************************
C   begin body of function  KFINDX

C.......   Check that Models-3 I/O has been initialized:

        EFLAG = .FALSE.
!$OMP   CRITICAL( S_INIT )
        IF ( .NOT. FINIT3 ) THEN
            LOGDEV = INIT3()
            EFLAG   = .TRUE.
        END IF          !  if not FINIT3
!$OMP   END CRITICAL( S_INIT )
        IF ( EFLAG ) THEN
            CALL M3MSG2(  'KFINDX:  I/O API not yet initialized.' )
            KFINDX = .FALSE.
            RETURN
        END IF

        IF ( LEN_TRIM( FNAME ) .GT. 16 ) THEN
            MESG = 'File "'// FNAME //'"'
            CALL M3MSG2( MESG )
            WRITE( MESG, '( A, I10 )' )
     &          'Max file name length 16; actual:', LEN_TRIM( FNAME )
            CALL M3WARN( 'KFINDX', 0,0, MESG )
            KFINDX = .FALSE.
            RETURN
        END IF          !  if len( fname ) > 16, or if len( vname ) > 16


C.......   Find netCDF index for the file, and check time step availability:

        FIL16 = FNAME   !  fixed-length-16 scratch copy of name
        FID   = INDEX1( FIL16, COUNT3, FLIST3 )

!$OMP   CRITICAL( S_NC )

        IF ( FID .EQ. 0 ) THEN  !  file not available

            MESG = 'File:  '//FIL16// ' not yet opened.'
            CALL M3WARN( 'KFINDX', 0,0, MESG )
            KFINDX = .FALSE.
            GO TO  999        !  return from kfindx()

        ELSE IF ( VOLAT3( FID ) ) THEN     !  volatile file:  synch with disk

            CALL NCSNC( CDFID3( FID ), IERR )
            IF ( IERR .NE. 0 ) THEN

                WRITE( MESG,91010 )
     &              'netCDF error number', IERR,
     &              'Error with disk synchronization for file:  '
     &              // FIL16
                CALL M3WARN( 'KFINDX', 0,0, MESG )
                KFINDX = .FALSE.
                GO TO  999        !  return from kfindx()

            END IF      !  if ncsnc() failed

        END IF          !  if file not available, or if file is volatile


C.......   Read number of events for this cell:

        FNUM = CDFID3( FID )

        IF      ( COL .LT. 1 .OR. COL .GT. NCOLS3( FID ) ) THEN
            WRITE( MESG,91010 )
     &              'Column requested:    ', COL,
     &              'out of range; max is ', NCOLS3( FID )
            CALL M3WARN( 'KFINDX', 0,0, MESG )
            KFINDX = .FALSE.
            GO TO  999        !  return from kfindx()
        ELSE IF ( ROW .LT. 1 .OR. ROW .GT. NROWS3( FID ) ) THEN
            WRITE( MESG,91010 )
     &              'Row requested:       ', ROW,
     &              'out of range; max is ', NROWS3( FID )
            CALL M3WARN( 'KFINDX', 0,0, MESG )
            KFINDX = .FALSE.
            GO TO  999        !  return from kfindx()
        END IF

        DIMT( 1 ) = COL
        DIMT( 2 ) = ROW
        CALL NCVGT1( FNUM,  NINDX3( FID ), DIMT, ECOUNT, IERR )
        IF ( IERR .NE. 0 ) THEN

            WRITE( MESG,91010 )
     &      'Requested col:  ', COL,
     &      'Requested row:  ', ROW
            CALL M3MESG( MESG )
            WRITE( MESG,91010 )
     &          'netCDF error number', IERR,
     &          'netCDF ID:  ', FNUM
            CALL M3MESG( MESG )
            MESG = 'Error reading KFCOUNT from ' // FIL16
            CALL M3WARN( 'KFINDX', 0,0, MESG )
            KFINDX = .FALSE.
            GO TO  999        !  return from kfindx()

        END IF


C.......   Read the event-specification parameters for ths cell:

        DIMT( 1 ) = 1
        DELT( 1 ) = ECOUNT
        DIMT( 2 ) = COL
        DELT( 2 ) = 1
        DIMT( 3 ) = ROW
        DELT( 3 ) = 1

        CALL NCVGT( FNUM, SINDX3( FID ), DIMT, DELT, EVENTS, IERR )
        IF ( IERR .NE. 0 ) THEN     !  timestep flag not yet written

            WRITE( MESG,91010 )
     &          'Requested col:  ', COL,
     &          'Requested row:  ', ROW
            CALL M3MESG( MESG )
            WRITE( MESG,91010 )
     &          'netCDF error number', IERR,
     &          'netCDF ID:  ', FNUM
            CALL M3MESG( MESG )
            MESG = 'Error reading KFEVENT from ' // FIL16
            CALL M3WARN( 'KFINDX', 0,0, MESG )
            KFINDX = .FALSE.
            GO TO  999        !  return from kfindx()

        END IF          !  if ierr bad or if timestep flags bad

        CALL NCVGT( FNUM, LINDX3( FID ), DIMT, DELT, SDATES, IERR )
        IF ( IERR .NE. 0 ) THEN     !  timestep flag not yet written

            WRITE( MESG,91010 )
     &          'Requested col:  ', COL,
     &          'Requested row:  ', ROW
            CALL M3MESG( MESG )
            WRITE( MESG,91010 )
     &          'netCDF error number', IERR,
     &          'netCDF ID:  ', FNUM
            CALL M3MESG( MESG )
            MESG = 'Error reading KFSDATE from ' // FIL16
            CALL M3WARN( 'KFINDX', 0,0, MESG )
            KFINDX = .FALSE.
            GO TO  999        !  return from kfindx()

        END IF          !  if ierr bad or if timestep flags bad

        CALL NCVGT( FNUM, XINDX3( FID ), DIMT, DELT, STIMES, IERR )
        IF ( IERR .NE. 0 ) THEN     !  timestep flag not yet written

            WRITE( MESG,91010 )
     &      'Requested col:  ', COL,
     &      'Requested row:  ', ROW
            CALL M3MESG( MESG )
            WRITE( MESG,91010 )
     &          'netCDF error number', IERR,
     &          'netCDF ID:  ', FNUM
            CALL M3MESG( MESG )
            MESG = 'Error reading KFSTIME from ' // FIL16
            CALL M3WARN( 'KFINDX', 0,0, MESG )
            KFINDX = .FALSE.
            GO TO  999        !  return from kfindx()

        END IF          !  if ierr bad or if timestep flags bad

        CALL NCVGT( FNUM, YINDX3( FID ), DIMT, DELT, KFLENS, IERR )
        IF ( IERR .NE. 0 ) THEN     !  timestep flag not yet written

            WRITE( MESG,91010 )
     &      'Requested col:  ', COL,
     &      'Requested row:  ', ROW
            CALL M3MESG( MESG )
            WRITE( MESG,91010 )
     &          'netCDF error number', IERR,
     &          'netCDF ID:  ', FNUM
            CALL M3MESG( MESG )
            MESG = 'Error reading KFLNGTH from ' // FIL16
            CALL M3WARN( 'KFINDX', 0,0, MESG )
            KFINDX = .FALSE.
            GO TO  999        !  return from kfindx()

        END IF          !  if ierr bad or if timestep flags bad

        KFINDX = .TRUE.

999     CONTINUE        !  target of "exit from routine"

!$OMP   END CRITICAL( S_NC )

        RETURN

C******************  FORMAT  STATEMENTS   ******************************

C...........   Error and warning message formats..... 91xxx

91010   FORMAT ( 5 ( A , :, I5, :, 2X ) )

        END FUNCTION KFINDX

