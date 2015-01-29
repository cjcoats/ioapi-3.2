
        INTEGER FUNCTION KFWRITE( FNAME, COL, ROW, JDATE, JTIME, KFLEN,
     &                            BUFFER )

C***********************************************************************
C Version "$Id: kfwrite.F 100 2015-01-16 16:52:16Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (C) 2003-2010 by Baron Advanced Meteorological Systems.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  function body starts at line  107
C
C  FUNCTION:  writes all the data from BUFFER() for for the event at
C	COL:ROW:JDATE:JTIME and having duration KFLEN (formatted YYYYDDD
C	and HHMMSS) to the KF-Cloud Event file with logical name FNAME.
C
C  RETURN VALUE:  record number at which the event written, or -1 for failure
C
C  PRECONDITIONS REQUIRED:  FNAME is a KF-Cloud Event file already opened
C                 for write access by KFOPEN()
C
C  POSTCONDITIONS REQUIRED:  subsequent call to SHUT3() to flush the
C                 file to disk.
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C                 INDEX1, INIT3
C
C  REVISION  HISTORY:
C       adapted   4/1996 by CJC from WRITE3()
C
C       Modified  5/1998 by CJC for OpenMP thread-safety
C
C       Modified  5/1999 by ALT for coupling-mode operation
C
C       Modified  9/1999 by CJC unification with KFOPEN()
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

        CHARACTER*(*), INTENT(IN   ) ::  FNAME      !  logical file name
        INTEGER      , INTENT(IN   ) ::  COL        !  column number for this event
        INTEGER      , INTENT(IN   ) ::  ROW        !  row    number for this event
        INTEGER      , INTENT(IN   ) ::  JDATE      !  date, formatted YYYYDDD
        INTEGER      , INTENT(IN   ) ::  JTIME      !  date, formatted HHMMSS
        INTEGER      , INTENT(IN   ) ::  KFLEN      !  time, formatted HHMMSS
        REAL         , INTENT(IN   ) ::  BUFFER(*)  !  output buffer array


C...........   EXTERNAL FUNCTIONS and their descriptions:

        INTEGER, EXTERNAL :: INDEX1     !  look up names in name tables
        INTEGER, EXTERNAL :: INIT3      !  initialize I/O system files.

        EXTERNAL          :: INITBLK3        !!  BLOCK DATA to initialize STATE3 commons

C...........   MACHINE DEPENDENCY !!

#if _CRAY || REAL8
        INTEGER, PARAMETER :: TYPSIZE( 6 ) = (/ 1, 1, 1, 1, 1, 1 /)
#endif
#if ! ( _CRAY || REAL8 )
        INTEGER, PARAMETER :: TYPSIZE( 6 ) = (/ 1, 1, 1, 1, 1, 2 /)
#endif


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER       IDUM            !  holds return value for INIT3()
        INTEGER       CREC            !  record number for this cell
        INTEGER       IREC            !  record number for file
        INTEGER       FID             !  file-subscript for STATE3 arrays
        INTEGER       FNUM            !  CDFID3( FID )
        INTEGER       INDX            !  subscript into BUFFER( * )
        INTEGER       V               !  loop counter:  variables
        INTEGER       DIMT ( 5 )      !  corner   for NCVGT()
        INTEGER       DELT ( 5 )      !  diagonal for NCVGT()
        INTEGER       TFLAG( 5 )      !  tuple:  COL-ROW-DATE-TIME-DURATION
        INTEGER       IERR            !  netCDF error status return
        CHARACTER*16  FIL16           !  scratch file-name     buffer
        CHARACTER*256 MESG
        LOGICAL       EFLAG


C***********************************************************************
C   begin body of function  KFWRITE

C.......   Check that Models-3 I/O has been initialized:

        EFLAG = .FALSE.
!$OMP   CRITICAL( S_INIT )
        IF ( .NOT. FINIT3 ) THEN
            IDUM = INIT3()
            EFLAG   = .TRUE.
        END IF          !  if not FINIT3
!$OMP   END CRITICAL( S_INIT )
        IF ( EFLAG ) THEN
            CALL M3MSG2( 'KFWRITE: I/O API not yet initialized.' )
            KFWRITE = -1
            RETURN
        END IF


C.......   Find netCDF index for the file, and check time step availability:

        IF ( LEN_TRIM( FNAME ) .GT. NAMLEN3 ) THEN
            MESG = 'File "'// FNAME// '"'
            CALL M3MSG2( MESG )
            WRITE( MESG, '( 3( A , :, I5, :, 2X ) )' )
     &          'Max file name length 16; actual:', LEN_TRIM( FNAME )
            CALL M3WARN( 'KFWRITE', 0, 0, MESG )
            KFWRITE = -1
            RETURN
        END IF

!$OMP  CRITICAL( S_NC )

        FIL16 = FNAME   !  fixed-length-16 scratch copy of name
        FID   = INDEX1( FIL16, COUNT3, FLIST3 )

        IF ( FID .EQ. 0 ) THEN  !  file not available

            MESG = 'File:  '//FIL16// ' not yet opened.'
            CALL M3WARN( 'KFWRITE', 0, 0, MESG )
            KFWRITE = -1
            GO TO  999        !  return from kfwrite()

        END IF

        FNUM = CDFID3( FID )
        IF ( FTYPE3( FID ) .NE. KFEVNT3 ) THEN

            WRITE( MESG,91030 )
     &          'File:  ' / /FIL16 // ' has type ', FTYPE3( FID ),
     &          'Not a KF-CLOUD-EVENT file'
            CALL M3WARN( 'KFWRITE', 0, 0, MESG )
            KFWRITE = -1
            GO TO  999        !  return from kfwrite()

        ELSE IF ( RONLY3( FID ) ) THEN

            MESG = 'File:  '//FIL16// ' IS READ-ONLY.'
            CALL M3WARN( 'KFWRITE', 0, 0, MESG )
            KFWRITE = -1
            GO TO  999        !  return from kfwrite()

       ELSE IF ( VOLAT3( FID ) ) THEN     !  volatile file:  synch with disk

           CALL NCSNC( FNUM, IERR )
           IF ( IERR .NE. 0 ) THEN

               WRITE( MESG,91030 )
     &              'netCDF error number', IERR,
     &              'Error with disk synchronization for file:  '
     &              // FIL16

               CALL M3WARN( 'KFWRITE', 0, 0, MESG )
               KFWRITE = -1
               GO TO  999        !  return from kfwrite()

            END IF      !  if NCSNC() error

        END IF          !  if file not opened, or if readonly, or if volatile


        DIMT( 1 ) = COL
        DIMT( 2 ) = ROW
        IF ( COL .GT. NCOLS3( FID ) .OR. ROW .GT. NROWS3( FID ) .OR.
     &       COL .LT. 1             .OR. ROW .LT. 1 ) THEN
            WRITE( MESG, '(A, A )' )
     &              'KFWRITE:  file ', FNAME
            CALL M3MSG2( MESG )
            WRITE( MESG, '( A, I9, A, I9, A )' )
     &          'Requested (col,row)=(', COL, ',',ROW, ')'
            CALL M3MSG2( MESG )
            WRITE( MESG, '( A, I9, A, I9, A )' )
     &          'File dimensions (ncols,nrows)=(',
     &          NCOLS3(FID), ',',NROWS3(FID), ')'
            CALL M3MSG2( MESG )
            CALL M3WARN( 'KFWRITE', 0, 0, 'Out-of-bounds request' )
            KFWRITE = -1
            GO TO  999        !  return from kfwrite()
        END IF

        CALL NCVGT1( FNUM,  NINDX3( FID ), DIMT, CREC, IERR )
        IF ( IERR .NE. 0 ) THEN     !  timestep flag not yet written

            WRITE( MESG,91040 )
            WRITE( MESG,91040 )
     &              'netCDF error number', IERR,
     &              'reading KFCOUNT from ' // FIL16

            CALL M3WARN( 'KFWRITE', 0, 0, MESG )
            KFWRITE = -1
            GO TO  999        !  return from kfwrite()

        END IF

        CREC = CREC + 1
        IREC = MXREC3( FID ) + 1

        IF ( CREC .GT. NTHIK3( FID ) ) THEN

            WRITE( MESG,91050 )
     &              'Maximum event count ', NTHIK3( FID ),
     &              'exceeded at column', COL, 'row', ROW,
     &              'date and time', JDATE, JTIME

            CALL M3WARN( 'KFWRITE', 0, 0, MESG )
            KFWRITE = -1
            GO TO  999        !  return from kfwrite()

        END IF


C.......   Write the variables in the event-record:

        DIMT( 1 ) = 1
        DELT( 1 ) = NLAYS3( FID )

        DIMT( 2 ) = IREC
        DELT( 2 ) = 1

        INDX = 1
        DO  11  V = 1, NVARS3( FID )

            CALL NCVPT( FNUM, VINDX3( V,FID ), DIMT, DELT,
     &                  BUFFER( INDX ), IERR )

            IF ( IERR .NE. 0 ) THEN

                WRITE( MESG,91011 )
     &              'Error writing variable ',
     &              VLIST3( V,FID ),
     &              'from file ' // FLIST3( FID ) ,
     &              'netCDF error number', IERR

                CALL M3WARN( 'KFWRITE', 0, 0, MESG )
!$OMP       CRITICAL( S_LOGOUT )
                WRITE( LOGDEV,* ) 'IOAPI ID    ', FID
                WRITE( LOGDEV,* ) 'netCDF ID   ', CDFID3( FID )
                WRITE( LOGDEV,* ) 'vble        ', VINDX3( V,FID )
                WRITE( LOGDEV,* ) 'dims array  ', DIMT
                WRITE( LOGDEV,* ) 'delts array ', DELT
                WRITE( LOGDEV,* ) 'offset      ', INDX
                WRITE( LOGDEV,* )
!$OMP       END CRITICAL( S_LOGOUT )

                KFWRITE = -1
                GO TO  999        !  return from kfwrite()

            END IF                  !  ierr nonzero:  NCVPT() failed

            INDX = INDX  +  NLAYS3( FID ) * TYPSIZE( VTYPE3( V,FID ) )

11      CONTINUE


C.......   Write TFLAG for the event-record:

        TFLAG( 1 ) = COL		!  grid column for this even
        TFLAG( 2 ) = ROW		!  grid row
        TFLAG( 3 ) = JDATE		!  starting date YYYYDDD
        TFLAG( 4 ) = JTIME		!  starting time HHMMSS
        TFLAG( 5 ) = KFLEN		!  duration      HHMMSS

        DIMT( 1 ) = 1
        DELT( 1 ) = 5

        DIMT( 2 ) = IREC
        DELT( 2 ) = 1

        CALL NCVPT( FNUM, TINDX3( FID ), DIMT, DELT, TFLAG, IERR )

        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG,91040 )
     &      'Error ', IERR, 'writing TFLAG to file ' // FLIST3( FID )
            CALL M3WARN( 'KFWRITE', 0, 0, MESG )
            KFWRITE = -1
            GO TO  999        !  return from kfwrite()
       END IF


C.......   update KFCOUNT, KFEVENT, KFSDATE, KFSTIME, KFLNGTH:

        DIMT( 1 ) = CREC
        DIMT( 2 ) = COL
        DIMT( 3 ) = ROW

        CALL NCVPT1( FNUM, SINDX3( FID ), DIMT, IREC, IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG,91040 )
     &      'Error ', IERR, 'writing KFEVENT to file ' //
     &      FLIST3( FID ) // ' at col ', COL, 'row', ROW, 'count', CREC
            CALL M3WARN( 'KFWRITE', 0, 0, MESG )
            KFWRITE = -1
            GO TO  999        !  return from kfwrite()
       END IF

        CALL NCVPT1( FNUM, LINDX3( FID ), DIMT, JDATE, IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG,91040 )
     &      'Error ', IERR, 'writing KFSDATE to file ' //
     &      FLIST3( FID ) // ' at col ', COL, 'row', ROW, 'count', CREC
            CALL M3WARN( 'KFWRITE', 0, 0, MESG )
            KFWRITE = -1
            GO TO  999        !  return from kfwrite()
       END IF

        CALL NCVPT1( FNUM, XINDX3( FID ), DIMT, JTIME, IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG,91040 )
     &      'Error ', IERR, 'writing KFSTIME to file ' //
     &      FLIST3( FID ) // ' at col ', COL, 'row', ROW, 'count', CREC
            CALL M3WARN( 'KFWRITE', 0, 0, MESG )
            KFWRITE = -1
            GO TO  999        !  return from kfwrite()
       END IF

        CALL NCVPT1( FNUM, YINDX3( FID ), DIMT, KFLEN, IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG,91040 )
     &      'Error ', IERR, 'writing KFLNGTH to file ' //
     &      FLIST3( FID ) // ' at col ', COL, 'row', ROW, 'count', CREC
            CALL M3WARN( 'KFWRITE', 0, 0, MESG )
            KFWRITE = -1
            GO TO  999        !  return from kfwrite()
       END IF


C.......   Update KFCOUNT:

        DIMT( 1 ) = COL
        DIMT( 2 ) = ROW

        CALL NCVPT1( FNUM, NINDX3( FID ), DIMT, CREC, IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG,91040 )
     &      'Error ', IERR, 'writing KFCOUNT to file ' //
     &      FLIST3( FID ) // ' at col ', COL, 'row', ROW, 'count', CREC
            CALL M3WARN( 'KFWRITE', 0, 0, MESG )
            KFWRITE = -1
            GO TO  999        !  return from kfwrite()
        END IF

        MXREC3( FID ) = IREC
        KFWRITE       = IREC

999     CONTINUE        !  target of "exit from routine"

        IF ( VOLAT3( FID ) ) THEN     !  volatile file:  synch with disk

           CALL NCSNC( FNUM, IERR )
           IF ( IERR .NE. 0 ) THEN

               WRITE( MESG,91030 )
     &              'netCDF error number', IERR,
     &              'Error with disk synchronization for file:  '
     &              // FIL16

               CALL M3WARN( 'KFWRITE', 0, 0, MESG )
               KFWRITE = -1
               GO TO  999        !  return from kfwrite()

            END IF      !  if NCSNC() error

        END IF          !  if file is volatile

!$OMP END CRITICAL( S_NC )

        RETURN

C******************  FORMAT  STATEMENTS   ******************************

C...........   Error and warning message formats..... 91xxx

91001   FORMAT ( 3A, 2X , A , I7  )

91010   FORMAT ( 3 ( A , :, 2X ) , I7 )

91011    FORMAT ( //5X , '>>> WARNING in subroutine KFWRITE <<<',
     &             ( /5X , 4A , : ) , I7, // )

91020   FORMAT ( //5X , '>>> WARNING in subroutine KFWRITE <<<',
     &            /5X , A ,
     &            2 ( /5X , A , :, I9, :, ':' , I6.6 ),
     &            /5X , A , 10X, I6.6,  // )

91030   FORMAT ( A , I10 , : , 2X , A )

91040   FORMAT ( A , 5 ( I10, :, 2X , A, : ) )

91050   FORMAT ( A , 3 ( I10, :, 2X , A, : ), I9.7, ':', I6.6 )

        END FUNCTION  KFWRITE

