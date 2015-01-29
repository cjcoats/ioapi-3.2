
        LOGICAL FUNCTION  OPEN3( FNAME, FSTATUS, PGNAME )

C***********************************************************************
C Version "$Id: open3.F 100 2015-01-16 16:52:16Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (C) 2003-2010 by Baron Advanced Meteorological Systems.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  function body starts at line  130
C
C  FUNCTION:
C       open Models-3 file with logical name FNAME, with file status
C       FSTATUS = FSREAD3==1 for read-only,
C       FSRDWR3==2 for read/write/update of existing files,
C       FSNEW3 ==3 for read/write of new files,
C       FSUNKN3==4 for read/write/update of unknown (new vs. old) files, or
C       FSCREA3==5 for delete-and-reopen for write.
C       If opened for write, copies scenario description from 
C       I/O STATE3.EXT to file's history, and name PGNAME of 
C       caller to file's updater-name.  
C       Returns TRUE if the file is already open.
C
C  RETURN VALUE:
C       TRUE iff it succeeds in opening the file, reading its attributes,
C       and storing the relevant ones in STATE3.EXT
C
C  PRECONDITIONS REQUIRED:  
C       FSREAD3 or FSRDWR3:  File FNAME already exists.
C       FSNEW3:  file must _not_ already exist.
C       FSCREA3: file deleted and new file created if it already exist.
C       FSCREA3, FSNEW3, FSUNKN3:  user must supply file description in 
C	FDESC3.EXT COMMONs
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       CKDESC3, CHKFIL3, CLOSE3, CRTBUF3, CRTFIL3, INDEX1, INIT3,
C       OPNFIL3, OPNLIST3, RMFILE
C
C  REVISION  HISTORY:
C       prototype 3/1992 by CJC
C       modified  7/1992 by CJC:  handles coordinate system, grid 
C                               description attributes in file headers,
C                               in an upward-compatible fashion.
C       Modified  9/1994 by CJC:  4-way mode of file opening (read-only, 
C                               read-write, new, unknown, using parameter 
C                               tokens from PARMS3.EXT); 
C                               also BUFFERED virtual files.
C       Modified  8/1995 by CJC to support CLOSE3()
C       Modified  5/1996 by CJC to support new mode FSCREA3 for opening files.
C       Modified  5/1998 by CJC for OpenMP thread-safety
C       Modified  4/1997 by CJC to fix bug in "already-open" warning formats
C       Modified  5/1998 by CJC for OpenMP thread-safety
C       Modified  5/1999 by ALT for coupling-mode operation
C       Modified  9/1999 by CJC unification with KFOPEN()
C       Modified  2/2002 by CJC check TRIMLEN() of FNAME; File-list
C       multi-file input data sets;
C       Modified  3/2002 by CJC:  STATE3V changes
C       Bugfix    5/2003 by CJC:  crtbuf3() is LOGICAL, not INTEGER
C       (correction by David Wong, US EPA)
C       Modified 7/2003 by CJC:  bugfix -- clean up critical sections
C       associated with INIT3()
C       Modified 10/2003 by CJC for I/O API version 3:  support for
C       native-binary BINFIL3 file type; uses INTEGER NAME2FID
C       Modified 12/2004 by CJC: implement NVARS range-checks
C       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
C***********************************************************************

      IMPLICIT NONE

C...........   INCLUDES:

        INCLUDE 'PARMS3.EXT'
        INCLUDE 'STATE3.EXT'
        INCLUDE 'NETCDF.EXT'
#ifdef IOAPICPL
        INCLUDE 'STATE3V.EXT'
#endif


C...........   ARGUMENTS and their descriptions:

        CHARACTER*(*), INTENT(IN   ) :: FNAME   !  logical name of file to be opened
        INTEGER      , INTENT(IN   ) :: FSTATUS !  read-only, read-write, new, or unknown
        CHARACTER*(*), INTENT(IN   ) :: PGNAME  !  name of calling program


C...........   EXTERNAL FUNCTIONS and their descriptions:

        LOGICAL, EXTERNAL :: CKDESC3  !  does header validity checks
        LOGICAL, EXTERNAL :: CHKFIL3  !  does work of checking file def consistency
        LOGICAL, EXTERNAL :: CLOSE3   !  closes files (if FSCREA3)
        LOGICAL, EXTERNAL :: CRTBUF3  !  create "buffered" virtual files
        LOGICAL, EXTERNAL :: CRTFIL3  !  does work of creating new files
        INTEGER, EXTERNAL :: INDEX1   !  look up names in tables
        INTEGER, EXTERNAL :: NAME2FID !  fname~~> fid lookup
        LOGICAL, EXTERNAL :: OPNFIL3  !  does work of opening "old" files
        LOGICAL, EXTERNAL :: OPNLIST3 !   " multi-file FILE-LIST data sets
        INTEGER, EXTERNAL :: RMFILE   !  shell around UNIX "unlink()" system call
        EXTERNAL          :: INITBLK3 !  BLOCK DATA to initialize STATE3 commons


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         IDUM    !  dummy for INIT3 return value
        INTEGER         FID     !  subscript for STATE3 arrays
        INTEGER         RWMODE  !  netCDF mode corresponding to RDONLY
        INTEGER         IERR    !  netCDF error status return
        LOGICAL         AFLAG   !  return value from INQUIRE
        LOGICAL         BINFLAG !  native-binary data set
        LOGICAL         LISTFLAG!  multi-file LIST-FILE data set
        CHARACTER*512   EQNAME  !  environment value of FNAME
        CHARACTER*8     BNAME   !  for "BUFFERED", etc.
        CHARACTER*16    PRG16   !  scratch  pgm-name buffer

        CHARACTER*512   MESG    !  for m3msg2, m3warn

#ifdef IOAPICPL
        LOGICAL  OPEN3V, UPDTVIR3
        EXTERNAL OPEN3V, UPDTVIR3
        CHARACTER *256 PGMNAME
#endif

C.............................................................................
C   begin body of subroutine  OPEN3

C.......   Find STATE3 index for the file:

        FID = NAME2FID( FNAME )

        IF ( FID .NE. 0 ) THEN  !  file already open.  Consistency checks:

            IF ( FSTATUS .EQ. FSNEW3 ) THEN

                MESG =
     &              'File ' // FNAME // ' already opened; ' //
     &              'cannot subsequently create it "NEW".'
                CALL M3WARN( 'OPEN3', 0, 0, MESG )
                OPEN3 = .FALSE.
                RETURN

            ELSE IF ( FSTATUS .EQ. FSUNKN3 ) THEN

                IF( RONLY3( FID ) ) THEN

                    MESG =
     &              'File ' // FNAME // ' already opened READONLY; ' //
     &              'Cannot subsequently open it "UNKNOWN".'
                    CALL M3WARN( 'OPEN3', 0, 0, MESG )
                    OPEN3 = .FALSE.
                    RETURN

                ELSE 

                    OPEN3 = CHKFIL3( FID ) 
                    RETURN

                END IF

            ELSE IF ( RONLY3( FID ) .AND. FSTATUS .EQ. FSRDWR3 ) THEN

                MESG =
     &              'File ' // FNAME // ' already opened READONLY; ' //
     &              'cannot subsequently open it for READ/WRITE.'
                CALL M3WARN( 'OPEN3', 0, 0, MESG )
                OPEN3 = .FALSE.
                RETURN

            ELSE IF ( FSTATUS .EQ. FSCREA3 ) THEN

                IF ( .NOT. CKDESC3( FNAME ) ) THEN

                    MESG = 'Error:  bad file description in FDESC'
                    CALL M3WARN( 'OPEN3', 0, 0, MESG )
                    FLIST3( FID ) = CMISS3
                    OPEN3 = .FALSE.
                    RETURN

                END IF  !  if CKDESC3() failed

                IF ( CLOSE3( FNAME ) ) THEN

                    MESG = 'File ' // FNAME // ' already opened. ' //
     &                     'Closing, deleting, and re-opening it'
                    CALL M3WARN( 'OPEN3', 0, 0, MESG )

                ELSE

                    MESG = 'File ' // FNAME // ' already opened. ' //
     &                  'Could not close to reopen with status FSCREA3'
                    CALL M3WARN( 'OPEN3', 0, 0, MESG )
                    OPEN3 = .FALSE.
                    RETURN

                END IF

            ELSE

                OPEN3 = .TRUE.
                RETURN

            END IF

        ELSE	!  else file not yet open:  check file desc. for consistency

            IF ( FSTATUS .EQ. FSNEW3   .OR.
     &           FSTATUS .EQ. FSUNKN3  .OR.
     &           FSTATUS .EQ. FSCREA3 ) THEN

                IF ( .NOT. CKDESC3( FNAME ) ) THEN

                    MESG = 'Error:   bad file description in FDESC'
                    CALL M3WARN( 'OPEN3', 0, 0, MESG )
                    OPEN3 = .FALSE.
                    RETURN

                END IF

            END IF	!  if status new, unknown, or create/truncate

        END IF  !  if file already open, or not

        FID = INDEX1( CMISS3, MXFILE3, FLIST3 )
        IF ( FID .EQ. 0 ) THEN
            MESG =
     &          'Could not open ' // FNAME //
     &          'Maximum number of files already have been opened.'
            CALL M3WARN( 'OPEN3', 0, 0, MESG )
            OPEN3 = .FALSE.
            RETURN
        END IF


C.......  Find the value EQNAME for logical name FNAME.
C.......  If EQNAME contains "-v" or "-V" mark this file as volatile:

        CALL NAMEVAL( FNAME, EQNAME )

        IDUM = MAX ( INDEX( EQNAME, ' -v' ) ,
     &               INDEX( EQNAME, ' -V' ) )

        IF ( IDUM .GT. 0 ) THEN
            EQNAME( IDUM: ) = '  '              !  fix the '-v' (etc.)
            VOLAT3( FID )   = .TRUE.            !  volatile file
        ELSE
            VOLAT3( FID ) = .FALSE.
        END IF
        
        FLIST3( FID ) = FNAME
        CDFID3( FID ) = 0
        RONLY3( FID ) = ( FSTATUS .EQ. FSREAD3 )


C.......   Open or create new file, according to FSTATUS and existence 
C.......   of the file:

        BNAME = EQNAME( 1:8 )   ! normalize case for "BUFFERED" files
        CALL UPCASE( BNAME )

        IF ( BNAME .EQ. 'BUFFERED' ) THEN

            IF ( CRTBUF3( FID ) ) THEN  !  sets cdfid3( fid ) = buffil3
                OPEN3  = .TRUE.
                CALL OPNLOG3 ( FID , EQNAME, FSTATUS )
                COUNT3 = MAX( COUNT3, FID )
            ELSE
                FLIST3( FID ) = CMISS3
                OPEN3 = .FALSE.
            END IF

            RETURN

        ELSE IF ( BNAME .EQ. 'VIRTUAL ' ) THEN

#ifdef IOAPICPL
           PLIST3(FID) = EQNAME(9:256)
           CDFID3( FID ) = VIRFIL3
           CALL GETARG( 0, PGMNAME )
           OPEN3 = OPEN3V( PLIST3(FID), FSTATUS, PGMNAME )
           IF ( OPEN3 ) THEN
               IF ( UPDTVIR3( FID ) ) THEN
                   CALL OPNLOG3 ( FID , EQNAME, FSTATUS )
                   COUNT3 = MAX( COUNT3, FID )
               ELSE
                   FLIST3( FID ) = CMISS3
                   OPEN3 = .FALSE.
               END IF
           ELSE
               FLIST3( FID ) = CMISS3
           END IF
           RETURN
#endif

#ifndef IOAPICPL
            CALL M3WARN( 'OPEN3', 0, 0, 
     &      'VIRTUAL files not implemented in this I/O API version' )
            FLIST3( FID ) = CMISS3
            OPEN3 = .FALSE.
            RETURN
#endif

        ELSE IF ( BNAME(1:4) .EQ. 'BIN:' ) THEN
            
            CDFID3( FID ) = BINFIL3
            BINFLAG = .TRUE.
            MESG    = EQNAME( 5:512 )     !  strip off the "BIN:"
            EQNAME  = MESG
            INQUIRE ( FILE = EQNAME, EXIST = AFLAG )

        ELSE IF ( BNAME(1:5) .EQ. 'LIST:' ) THEN
            
            IF ( FSTATUS .EQ. FSREAD3 ) THEN
                CDFID3( FID ) = LSTFIL3
                LISTFLAG = .TRUE.
                AFLAG    = .FALSE.
            ELSE
                MESG = FNAME // ':' // TRIM( EQNAME )
                CALL M3MSG2( MESG )
                CALL M3WARN( 'OPEN3', 0, 0, 'LIST-File not readonly.' )
                FLIST3( FID ) = CMISS3
                OPEN3 = .FALSE.
                RETURN
            END IF

        ELSE

            INQUIRE ( FILE = EQNAME, EXIST = AFLAG )
            LISTFLAG = .FALSE.

        END IF        !  if buffered, else if virtual, or not

        PRG16  = PGNAME
        IF ( FSTATUS .EQ. FSREAD3 ) THEN        !  read-only

            RWMODE = NF_NOWRITE

            IF ( AFLAG ) THEN           !  file exists:

                IF ( OPNFIL3( EQNAME, FID, RWMODE, PRG16 ) ) THEN
                    OPEN3 = .TRUE.
                ELSE
                    FLIST3( FID ) = CMISS3
                    OPEN3 = .FALSE.
                    RETURN
                END IF

            ELSE IF ( LISTFLAG ) THEN

                IF ( OPNLIST3( FID, PRG16 ) ) THEN
                    OPEN3 = .TRUE.
                ELSE
                    FLIST3( FID ) = CMISS3
                    OPEN3 = .FALSE.
                    RETURN
                END IF

            ELSE                        !  file does not exist

                MESG = FNAME // ':' // TRIM( EQNAME )
                CALL M3MSG2( MESG )
                CALL M3WARN( 'OPEN3', 0, 0, 'File not available.' )
                FLIST3( FID ) = CMISS3
                OPEN3 = .FALSE.
                RETURN

            END IF      !  if file exist or not (file status read-only)

        ELSE IF ( FSTATUS .EQ. FSRDWR3 ) THEN   !  read-write

            IF ( AFLAG ) THEN           !  if file exists:  open it

                RWMODE = NF_WRITE
                IF ( OPNFIL3( EQNAME, FID, RWMODE, PRG16 ) ) THEN
                    OPEN3 = .TRUE.
                ELSE
                    FLIST3( FID ) = CMISS3
                    OPEN3 = .FALSE.
                    RETURN
                END IF

            ELSE                        !  file does not exist

                MESG = FNAME // ':' // TRIM( EQNAME )
                CALL M3MSG2( MESG )
                CALL M3WARN( 'OPEN3', 0, 0, 'File not available.' )
                FLIST3( FID ) = CMISS3
                OPEN3 = .FALSE.
                RETURN

            END IF      !  if file exists or not (file status read-write)

        ELSE IF ( FSTATUS .EQ. FSNEW3  ) THEN   !  new  (create)

            IF ( AFLAG ) THEN           !  file exists (error)

                FLIST3( FID ) = CMISS3
                OPEN3 = .FALSE.
                RETURN

            ELSE IF ( CRTFIL3( EQNAME, FID, PRG16 ) ) THEN

                OPEN3 = .TRUE.

            ELSE

                FLIST3( FID ) = CMISS3
                OPEN3 = .FALSE.
                RETURN

            END IF     !  if file exists or not (file status create)

        ELSE IF ( FSTATUS .EQ. FSUNKN3 ) THEN   !  unknown

            IF ( AFLAG ) THEN   !  file exists:  open and check consistency

                RWMODE = NF_WRITE
                IF ( OPNFIL3( EQNAME, FID, RWMODE, PRG16 ) ) THEN

                    IF ( CHKFIL3( FID ) ) THEN  !  consistency check

                        OPEN3 = .TRUE.

                    ELSE

                        FLIST3( FID ) = CMISS3
                        OPEN3 = .FALSE.
                        CALL NCCLOS( CDFID3( FID ), IERR )
                        RETURN 

                    END IF      !  if consistency check succeeded or failed

                ELSE            !  OPNFIL3() failed

                    CALL NCCLOS( CDFID3( FID ), IERR )
                    FLIST3( FID ) = CMISS3
                    OPEN3 = .FALSE.
                    RETURN

                END IF          !  if OPNFIL3() succeeded or not

            ELSE IF ( CRTFIL3( EQNAME, FID, PRG16 ) ) THEN

                OPEN3 = .TRUE.

            ELSE    !  CRTFIL3() failed:

                FLIST3( FID ) = CMISS3
                OPEN3 = .FALSE.
                RETURN

            END IF      !  if file exists or can be created or not

        ELSE IF ( FSTATUS .EQ. FSCREA3 ) THEN   !  truncate and create new

            IF ( AFLAG ) THEN   !  file exists:  remove it first.

                IERR = RMFILE( EQNAME )
                IF ( IERR .NE. 0 ) THEN
                    WRITE ( MESG, "( A, I9, 2X, 3A  )" )
     &                  'Error number', IERR,
     &                  'removing ', FNAME, ' from pathname:'
                     CALL M3WARN( 'OPEN3', 0, 0, MESG )
                     CALL M3MSG2( EQNAME )
                    OPEN3 = .FALSE.
                    RETURN
                END IF	!  if RMFILE failed

            END IF  !  if file exists

            IF ( CRTFIL3( EQNAME, FID, PRG16 ) ) THEN

                OPEN3 = .TRUE.

            ELSE    !  CRTFIL3() failed:

                FLIST3( FID ) = CMISS3
                OPEN3 = .FALSE.
                RETURN

            END IF  !  if CRTFIL3() succeeded or not

        ELSE            !  illegal FSTATUS value:

            CALL M3WARN( 'OPEN3', 0, 0, 
     &      'File opening error:  illegal FSTATUS argument.' )
            CALL M3MSG2( 
     &      'Legal values: 1-READONLY, 2-READ/WRITE, 3-NEW, 4-UNKNOWN')
             WRITE ( MESG, 93010 ) 'Value supplied by caller:', FSTATUS
            CALL M3MSG2( MESG )

            FLIST3( FID ) = CMISS3
            OPEN3 = .FALSE.
            RETURN

        END IF  !  read-only, read-write, new, unknown, create/trunc, or illegal


C.......   Update COUNT3 to account for new file, and log it:

        COUNT3 = MAX( COUNT3, FID )
        CALL OPNLOG3 ( FID , EQNAME, FSTATUS )

        OPEN3 = .TRUE.

        RETURN

C******************  FORMAT  STATEMENTS   ******************************

C...........   Error and warning message formats..... 91xxx

91001   FORMAT ( //5X , '>>> WARNING in subroutine OPEN3 <<<',
     &            /5X , 3A,
     &            /5X , A , I5, // )

91010   FORMAT ( //5X , '>>> WARNING in subroutine OPEN3 <<<',
     &            3 ( /5X , A , : ) , I7, // )

C...........   Internal buffering formats..... 93xxx

93010   FORMAT ( 5 ( A, :, I9, :, 2X ) )

        END FUNCTION  OPEN3

