
      LOGICAL FUNCTION KFREAD( FNAME, VNAME, EVENT,
     &                         COL, ROW, JDATE, JTIME, KFLEN, VBLE )

C***********************************************************************
C Version "$Id: kfread.F 100 2015-01-16 16:52:16Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (C) 2003-2010 by Baron Advanced Meteorological Systems.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  function body starts at line  98
C
C  FUNCTION:  reads the indicated event from the KF-Cloud-Event file
C 	with logical name FNAME for variable with name VNAME.
C       If VNAME = ALLVAR3 = 'ALL', reads all variables.
C
C  RETURN VALUE:  TRUE iff the operation succeeds (and the data is available)
C
C  PRECONDITIONS REQUIRED:  FNAME is a  KF-Cloud-Event file already
C       opened by KFOPEN()
C
C  REVISION  HISTORY:
C       Adapted   4/1996 by CJC from READ3().
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
        CHARACTER*(*), INTENT(IN   ) :: VNAME      !  variable name, or 'ALL'
        INTEGER      , INTENT(IN   ) :: EVENT      !  KF-cloud event number
        INTEGER      , INTENT(  OUT) :: COL        !  column number for this event
        INTEGER      , INTENT(  OUT) :: ROW        !  row    number for this event
        INTEGER      , INTENT(  OUT) :: JDATE      !  starting date, formatted YYYYDDD
        INTEGER      , INTENT(  OUT) :: JTIME      !  starting time, formatted HHMMSS
        INTEGER      , INTENT(  OUT) :: KFLEN      !  event duration, formatted HHMMSS
        REAL         , INTENT(  OUT) :: VBLE(*)    !  array of returned values for VNAME


C...........   MACHINE DEPENDENCY !!

#if _CRAY || REAL8
        INTEGER, PARAMETER :: TYPSIZE( 6 ) = (/ 1, 1, 1, 1, 1, 1 /)
#endif
#if ! ( _CRAY || REAL8 )
        INTEGER, PARAMETER :: TYPSIZE( 6 ) = (/ 1, 1, 1, 1, 1, 2 /)
#endif


C...........   EXTERNAL FUNCTIONS and their descriptions:

        INTEGER, EXTERNAL :: INIT3      !  initialize I/O API
        INTEGER, EXTERNAL :: INDEX1     !  look up names in name tables


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         FID             !  subscript  for STATE3 arrays
        INTEGER         TDIM            !  netCDF ID for record dimension
        INTEGER         VID, VAR        !  subscripts for STATE3 arrays
        INTEGER         FNUM            !  CDFID3( FID )
        INTEGER         INDX            !  subscript into VBLE( * )
        INTEGER         IERR            !  netCDF error status return
        INTEGER         DIMT( 5 )       !  corner   for NCVGT()
        INTEGER         DELT( 5 )       !  diagonal for NCVGT()
        INTEGER         FLAGS( 5 )      !  event COL-ROW-DATE-TIME-DURATION
        CHARACTER*16    FIL16           !  scratch file-name buffer
        CHARACTER*16    VAR16           !  scratch vble-name buffer
        CHARACTER*256   MESG
        CHARACTER*(MAXNCNAM) TNAME      !  dummy arg for NCDINQ
        LOGICAL EFLAG


C***********************************************************************
C   begin body of function  KFREAD

C.......   Check that Models-3 I/O has been initialized:

        EFLAG = .FALSE.
!$OMP   CRITICAL( S_INIT )
        IF ( .NOT. FINIT3 ) THEN
            LOGDEV = INIT3()
            EFLAG  = .TRUE.
        END IF          !  if not FINIT3
!$OMP   END CRITICAL( S_INIT )
        IF ( EFLAG ) THEN
            CALL M3MSG2(  'KFREAD:  I/O API not yet initialized.' )
            KFREAD = .FALSE.
            RETURN
        END IF

        IF ( LEN_TRIM( FNAME ) .GT. NAMLEN3 ) THEN
            EFLAG = .TRUE.
            MESG  = 'File "'// FNAME// '" Variable "'// VNAME//'"'
            CALL M3MSG2( MESG )
            WRITE( MESG, '( A , I10 )' )
     &          'Max file name length 16; actual:', LEN_TRIM( FNAME )
            CALL M3MSG2( MESG )
        END IF          !  if len( fname ) > 16

        IF ( LEN_TRIM( VNAME ) .GT. NAMLEN3 ) THEN
            EFLAG = .TRUE.
            MESG  = 'File "'// FNAME// '" Variable "'// VNAME//'"'
            CALL M3MSG2( MESG )
            WRITE( MESG, '( A, I10 )'  )
     &          'Max vble name length 16; actual:', LEN_TRIM( VNAME )
            CALL M3MSG2( MESG )
        END IF          !  if len( vname ) > 16

        IF ( EFLAG ) THEN
            MESG = 'Invalid variable or file name arguments'
            CALL M3WARN( 'KFREAD', 0, 0, MESG )
            KFREAD = .FALSE.
            RETURN
        END IF          !  if len( fname ) > 16, or if len( vname ) > 16


C.......   Find netCDF index for the file, and check time step availability:

        FIL16 = FNAME   !  fixed-length-16 scratch copy of name
        FID   = INDEX1( FIL16, COUNT3, FLIST3 )

!$OMP  CRITICAL( S_NC )

        IF ( FID .EQ. 0 ) THEN  !  file not available

            MESG = 'File:  '//FIL16// ' not yet opened.'
            CALL M3WARN( 'KFREAD', 0, 0, MESG )
            KFREAD = .FALSE.
            GO TO  999        !  return from kfread()

        ELSE IF ( VOLAT3( FID ) ) THEN

            !!  volatile file:  synch with disk,
            !!  Since such files may also be used as active channels
            !!  of communication between coupled models, we must also
            !!  re-evaluate the maximum record-number in the file.

            CALL NCSNC( CDFID3( FID ), IERR )
            IF ( IERR .NE. 0 ) THEN

                WRITE( MESG,91010 )
     &              'netCDF error number', IERR,
     &              'Error with disk synchronization for file:  '
     &              // FIL16
                CALL M3WARN( 'KFREAD', 0, 0, MESG )
                KFREAD = .FALSE.
                GO TO  999        !  return from kfread()

            END IF      !  if ncsnc() failed

            TDIM = NCDID( CDFID3( FID ), 'TSTEP', IERR )
            IF ( IERR .NE. 0 ) THEN
                WRITE( MESG,91010 )
     &              'netCDF error number', IERR
                CALL M3MSG2( MESG )
                MESG =
     &          'Error reading netCDF time-dimension ID for file '
     &              // FLIST3( FID )
                CALL M3WARN( 'KFREAD', 0, 0, MESG )
                KFREAD = .FALSE.
                GO TO  999        !  return
            END IF              !  ierr nonzero:  NCVDEF() failed

            CALL NCDINQ( CDFID3( FID ), TDIM, TNAME,
     &                   MXREC3( FID ), IERR )
            IF ( IERR .NE. 0 ) THEN
                WRITE( LOGDEV,91010 )
     &              'Error reading maximum timestep record number',
     &              'File name:  ' // FLIST3( FID ) ,
     &              'netCDF error number', IERR
                WRITE( MESG,91010 )
     &              'netCDF error number', IERR
                CALL M3MSG2( MESG )
                MESG =
     &          'Error reading maximum timestep record number ' //
     &              'for file ' // FLIST3( FID )
                CALL M3WARN( 'KFREAD', 0, 0, MESG )
                KFREAD = .FALSE.
                GO TO  999        !  return
            END IF              !  ierr nonzero:  NCVDEF() failed

        END IF          !  if file not available, or if file is volatile


C.......   Check availability of requested  layer, variable:

        VAR16 = VNAME   !  fixed-length-16 scratch copy of name

        IF ( FTYPE3( FID ) .NE. KFEVNT3 ) THEN

            WRITE( MESG,91010 )
     &      'File type', IERR, 'is not KFEVNT3=-3'
            CALL M3WARN( 'KFREAD', 0, 0, MESG )
            KFREAD = .FALSE.
            GO TO  999        !  return from kfread()

        ELSE IF ( EVENT .GT. MXREC3( FID ) .OR.
     &            EVENT .LE. 0 ) THEN

            WRITE( MESG,91010 )
     &      'Event number', EVENT, 'not available in file ' // FIL16
            CALL M3WARN( 'KFREAD', 0, 0, MESG )
            KFREAD = .FALSE.
            GO TO  999        !  return from kfread()

        ELSE IF ( VAR16 .EQ. ALLVAR3 ) THEN

            VID = ALLAYS3

        ELSE

            VID = INDEX1 ( VAR16, NVARS3( FID ), VLIST3( 1,FID ) )

            IF ( VID .EQ. 0 ) THEN
                WRITE( MESG,91010 )
     &              'Requested variable ' //  VAR16 //
     &              'not available in file ' // FLIST3( FID )
                CALL M3WARN( 'KFREAD', 0, 0, MESG )
                KFREAD = .FALSE.
                GO TO  999        !  return from kfread()
            END IF

        END IF          !  end check on VNAME


C.......   Read contents of the variable(s):

        DIMT( 1 ) = 1
        DELT( 1 ) = NLAYS3( FID )

        DIMT( 2 ) = EVENT
        DELT( 2 ) = 1

        FNUM = CDFID3( FID )

        IF ( VID .GT. 0 ) THEN  !  reading just one variable

            CALL NCVGT( FNUM, VINDX3( VID,FID ),
     &                  DIMT, DELT, VBLE, IERR )
            IF ( IERR .NE. 0 ) THEN     !  variable not yet written

                WRITE( MESG,91010 )
     &          'netCDF error number', IERR,
     &          'Error reading ' // VLIST3( VID,FID ) //
     &          ' from ' // FIL16
                CALL M3WARN( 'KFREAD', 0, 0, MESG )
                KFREAD = .FALSE.
                GO TO  999        !  return from kfread()

            END IF          !  if ierr bad or if variable bad

        ELSE            !  reading all variables

            INDX = 1

            DO  11  VAR = 1, NVARS3( FID )

                CALL NCVGT( FNUM, VINDX3( VAR,FID ),
     &                      DIMT, DELT, VBLE( INDX ), IERR )
                IF ( IERR .NE. 0 ) THEN     !  variable not yet written

                    WRITE( MESG,91010 )
     &                  'netCDF error number', IERR,
     &                  'Error reading ' // VLIST3( VAR,FID ) //
     &                  ' from ' // FIL16
                    CALL M3WARN( 'KFREAD', 0, 0, MESG )
                    KFREAD = .FALSE.
                    GO TO  999        !  return from kfread()

                END IF          !  if ierr bad or if variable bad

                INDX = INDX + NLAYS3( FID )*TYPSIZE( VTYPE3( VAR,FID ) )

11          CONTINUE

        END IF


C.......   Read record header:

        DIMT( 1 ) = 1
        DELT( 1 ) = 5

        DIMT( 2 ) = EVENT
        DELT( 2 ) = 1

        CALL NCVGT( FNUM, TINDX3( FID ),
     &              DIMT, DELT, FLAGS, IERR )
        IF ( IERR .NE. 0 ) THEN     !  record header not yet written

            WRITE( MESG,91010 )
     &          'netCDF error number', IERR,
     &          'Error reading TFLAG from ' // FIL16
            CALL M3WARN( 'KFREAD', 0, 0, MESG )
            KFREAD = .FALSE.
            GO TO  999        !  return from kfread()

        END IF          !  if ierr bad or if record header bad

        COL   = FLAGS( 1 )
        ROW   = FLAGS( 2 )
        JDATE = FLAGS( 3 )
        JTIME = FLAGS( 4 )
        KFLEN = FLAGS( 5 )

        KFREAD = .TRUE.

999     CONTINUE        !  target of "exit from routine"

!$OMP END CRITICAL( S_NC )

        RETURN

C******************  FORMAT  STATEMENTS   ******************************

C...........   Error and warning message formats..... 91xxx

91001   FORMAT ( 5A, 2X , A , I5 )

91010   FORMAT ( 3 ( A , :, I5, :, 2X ) )

        END FUNCTION KFREAD

