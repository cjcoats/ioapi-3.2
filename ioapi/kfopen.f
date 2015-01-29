
        LOGICAL FUNCTION  KFOPEN( FNAME, FSTATUS, PGNAME, KFCOUNT )

C***********************************************************************
C Version "$Id: kfopen.f 100 2015-01-16 16:52:16Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (C) 2003-2010 by Baron Advanced Meteorological Systems.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  function body starts at line  82
C
C  FUNCTION:  open KF-cloud file with logical name FNAME, with
C             file status FSTATUS = FSREAD3==1 for read-only,
C             FSRDWR3==2 for read/write/update of existing files,
C             FSNEW3==3 for read/write of new files, or FSUNKN3==4
C             for read/write/update of unknown (new vs. old) files.
C             If opened for write, copies scenario description from
C             I/O STATE3.EXT to file's history, and name PGNAME of
C             caller to file's updater-name.  Sets KFCOUNT
C             Returns TRUE if the file is already open.
C
C  RETURN VALUE:  TRUE iff it succeeds in opening the file, reading its
C                 attributes, and storing the relevant ones in STATE3.EXT
C
C  PRECONDITIONS REQUIRED:
C       FSREAD3 or FSRDWR3:  File FNAME already exists.
C       FSNEW3:  file must _not_ already exist.
C       FSNEW3, FSUNKN3:  user must supply file description in FDESC3.EXT
C       COMMONS
C
C  SUBROUTINES AND FUNCTIONS CALLED:  INDEX1, INIT3
C
C  REVISION  HISTORY:
C       Adapted   4/1996 by CJC from OPEN3()
C       Modified  5/1996 by CJC to support new mode FSCREA3 for opening files.
C       Modified  8/1999 by CJC:  OpenMP thread-safe; unified with OPEN3()
C       Modified  5/2003 by CJC:  critical-section change (deadlock-removal)
C       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
C***********************************************************************

      IMPLICIT NONE

C...........   INCLUDES:

        INCLUDE 'PARMS3.EXT'
        INCLUDE 'STATE3.EXT'
        INCLUDE 'NETCDF.EXT'


C...........   ARGUMENTS and their descriptions:

        CHARACTER*(*), INTENT(IN   ) :: FNAME   !  logical name of file to be opened
        INTEGER      , INTENT(IN   ) :: FSTATUS !  read-only, read-write, new, or unknown
        CHARACTER*(*), INTENT(IN   ) :: PGNAME  !  name of calling program
        INTEGER      , INTENT(  OUT) :: KFCOUNT( * )  !  gridded event counts


C...........   EXTERNAL FUNCTIONS and their descriptions:

        INTEGER, EXTERNAL :: INDEX1  !  look up names in tables
        INTEGER, EXTERNAL :: INIT3   !  initialize I/O system files
        LOGICAL, EXTERNAL :: OPEN3


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         I
        INTEGER         FID             !  subscript for STATE3 arrays
        INTEGER         FNUM            !  netCDF file ID
        INTEGER         IERR            !  netCDF error status return
        LOGICAL         AFLAG           !  return value from INQUIRE
        CHARACTER*256   EQNAME          !  environment value of FNAME
        CHARACTER*256   MESG
        INTEGER         DIMS( 5 )      !  corner arg array for NCVGT()
        INTEGER         DELS( 5 )      !  corner arg array for NCVGT()

C.............................................................................
C   begin body of subroutine  KFOPEN

        IF ( LEN( FNAME ) .GT. 16 ) THEN
            WRITE( MESG,91001 )
     &          'File name length bad for "', FNAME, '"',
     &          'Max file name length 16; actual:', LEN( FNAME )
            CALL M3WARN( 'KFOPEN', 0, 0, MESG )
            KFOPEN = .FALSE.
            RETURN
        END IF

        CALL NAMEVAL( FNAME, EQNAME )

        I = MAX ( INDEX( EQNAME, '-v' ) ,
     &            INDEX( EQNAME, '-V' ) )
        IF ( I .GT. 0 ) THEN
            EQNAME( I:I+1 ) = '  '        !  fix the '-v' (etc.)
        END IF


C.......   Find netCDF index for the file, and check time step availability:
C.......   AFLAG indicates whether the file exists before the call to OPEN3,
C.......   or not, hence whether we read KFCOUNT from the file or write it
C.......   to the file.

!$OMP   CRITICAL( S_KFO )

        INQUIRE ( FILE = EQNAME, EXIST = AFLAG )

        IF ( OPEN3( FNAME, FSTATUS, PGNAME ) ) THEN

            FID  = INDEX1( FNAME, COUNT3, FLIST3 )
            FNUM = CDFID3( FID )
            DIMS( 1 ) = 1
            DELS( 1 ) = NCOLS3( FID )
            DIMS( 2 ) = 1
            DELS( 2 ) = NROWS3( FID )
            WRITE( MESG, '(A, I7, 2( 2X, A ) )' )
     &           'NetCDF ID=', FNUM, 'for file', FNAME

            IF ( AFLAG ) THEN

C.......   Read KFCOUNT from FNAME:

                IF ( VOLAT3( FID ) ) THEN     !  volatile file:  synch with disk
                    CALL NCSNC( FNUM, IERR )
                    IF ( IERR .NE. 0 ) THEN
                        CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &                  'Error with input disk synchronization' )
                        KFOPEN = .FALSE.
                        GO TO 999       !  to return
                    END IF              !  if synch failed
                END IF          !  if file is volatile

                CALL NCVGT( FNUM, NINDX3( FID ), DIMS, DELS,
     &                      KFCOUNT, IERR )
                IF ( IERR .EQ. 0 ) THEN
                    KFOPEN = .TRUE.
                ELSE
                    CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &              'Error reading variable KFCOUNT' )
                    KFOPEN = .FALSE.
                END IF          !  ierr nonzero:  NCVGTC() failed, or succeeded

            ELSE

C.......   Initialize KFCOUNT and write it to FNAME:

                DO  I = 1, NCOLS3( FID ) * NROWS3( FID )
                    KFCOUNT( I ) = 0
                END DO

                CALL NCVPT( FNUM, NINDX3( FID ), DIMS, DELS,
     &                      KFCOUNT, IERR )
                IF ( IERR .EQ. 0 ) THEN
                    KFOPEN = .TRUE.
                ELSE
                    CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &              'Error initializing KFCOUNT' )
                    KFOPEN = .FALSE.
                    GO TO 999   !  to return
                END IF          !  ierr nonzero:  NCENDF() failed

                IF ( VOLAT3( FID ) ) THEN     !  volatile file:  synch with disk
                    CALL NCSNC( FNUM, IERR )
                    IF ( IERR .EQ. 0 ) THEN
                        KFOPEN = .TRUE.
                    ELSE
                        CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &                  'Error with output disk synchronization' )
                        KFOPEN = .FALSE.
                    END IF              !  if synch failed
                END IF          !  if file is volatile

            END IF

        ELSE

            KFOPEN = .FALSE.    !  open3() failed

        END IF          !  if open3() succeeded or not

999     CONTINUE

!$OMP   END CRITICAL( S_KFO )

        RETURN

C******************  FORMAT  STATEMENTS   ******************************

C...........   Error and warning message formats..... 91xxx

91001   FORMAT ( 3A, 2X, A , I5 )

91010   FORMAT ( 3 ( A , :, 2X ) , I7 )

        END FUNCTION  KFOPEN

