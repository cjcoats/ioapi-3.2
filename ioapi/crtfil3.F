
        LOGICAL FUNCTION CRTFIL3( EQNAME, FID, PGNAME )

C***********************************************************************
C Version "$Id: crtfil3.F 100 2015-01-16 16:52:16Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (C) 2003-2011 Baron Advanced Meteorological Systems, and
C (C) 2011 David Wong
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  subroutine body starts at line 128
C
C  FUNCTION:  Create netCDF file FNAME using info stored in the FDESC3
C             common.  Record history,   Leave file open for update.
C
C  PRECONDITIONS REQUIRED:
C       File definition already put into FDESC3 common.
C       For file type TSERIES3:  extra attributes in ATDSC3 COMMON
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       CRDICT3
C
C  REVISION  HISTORY:
C       prototype 9/1994 by CJC for new version of OPEN3()
C       modified 10/1994 by CJC to support write granularity at the
C       varible-level for gridded, boundary, and custom files:  TFLAG
C       now dimensioned by <variable-number>, <time-step-number>
C
C       Modified 12/1994 by CJC:  TFLAG ~~> TFLAG to contain the
C       date-and-time, dimensioned by 2, <variable-number>, <time-step-number>
C
C       Modified  2/1995 by CJC to support file type SMATRX3.
C
C       Modified 10/1996 by CJC to support file type TSERIES3.
C       Modified 12/1996 by CJC to support file type PTRFLY3.
C       revised   9/1999 by CJC:  Unification of OPEN3(), KFOPEN()
C       revised   2/2002 by CJC:  for volatile, call ncsync() before return
C       Revised   3/2002 by CJC:  OpenMP thread-safety;
C                                 turn off NF_SHARED for Crays
C       Revised   4/2002 by CJC:  treatment of PROFIL3 files
C       Revised   7/2002 by CJC:  fixed KF initialization bug
C       Revised   5/2003 by CJC:  Set NF_SHARE for volatile files
C       Modified 10/2003 by CJC for I/O API version 3:  support for
C       native-binary BINFIL3 file type
C       Modified 12/2004 by CJC:  NVARS3D range check
C       Modified 11/2008 by CJC:  Environment variable IOAPI_OFFSET_64
C       controls whether netCDF opened with NF_64BIT_OFFSET, to support
C       timestep-records > 2GB
C       Modified 10/2009 by CJC:  flags-change for netCDF4  from
C       Martin Otte, US EPA -- remove NF_WRITE.
C       Modified 03/20010 by CJC: F9x changes for I/O API v3.1
C       Revised 4/2011 by David Wong, US EPA, and by CJC, to add state for
C       full buffered-file file descriptions.
C       Revised 12.2014 by CJC:  USE M3UTILIO; USE M3EXTRA; extra-attribute
C       calls 
C***********************************************************************

        USE M3UTILIO
        USE M3ATTS

        IMPLICIT NONE

#ifdef   _AIX
#define  FLUSH flush_
#endif

C...........   INCLUDES:

        INCLUDE 'STATE3.EXT'
        INCLUDE 'ATDSC3.EXT'
        INCLUDE 'NETCDF.EXT'


C...........   ARGUMENTS and their descriptions:

        CHARACTER*(*), INTENT(IN   ) ::   EQNAME  !  physical name of file to be created
        INTEGER      , INTENT(IN   ) ::   FID     !  index into STATE3 arrays
        CHARACTER*(*), INTENT(IN   ) ::   PGNAME  !  name of calling program


C...........   EXTERNAL FUNCTIONS and their descriptions:

        LOGICAL, EXTERNAL :: CRDICT3 !  create variables for dictionary files
        INTEGER, EXTERNAL :: CRTBIN3 !  create native-binary-mode files


C...........   PARAMETERs and their descriptions:

        INTEGER, PARAMETER :: MXCOL = 8192


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         ISCR
        INTEGER         FNUM            !  netCDF file ID from NCCRE()
        INTEGER         IERR            !  netCDF error status return
        INTEGER         ATT             !  loop counter for var. attributes
        INTEGER         VAR             !  loop counter for file variables
        INTEGER         TSIZE           !  time-step mode
        INTEGER         DSIZE           !  D/T width
        INTEGER         CDIM            !  col/level     dim #
        INTEGER         RDIM            !  row/site      dim #
        INTEGER         LDIM            !  layer         dim #
        INTEGER         DDIM            !  timestamp date-and-time dimension #
        INTEGER         EDIM            !  cell-event    dim # (KF only)
        INTEGER         TDIM            !  timestep      dim #
        INTEGER         VDIM            !  variable-flag dim #
        INTEGER         NDIMS           !  number of dims for NCVDEF()
        INTEGER         DIMS( 5 )       !  array of dims for NCVDEF()
        INTEGER         DELS( 5 )       !  array of dims for NCVPT()
        INTEGER         C, R, L
        LOGICAL         EFLAG, AFLAG
        CHARACTER*80    DSCBUF          !  scratch buffer for descriptions
        CHARACTER*80    DSCBU2          !  scratch buffer for descriptions
        CHARACTER*512   NAMBUF          !  scratch buffer to upcase EQNAME in.
        INTEGER         KFBUF( MXCOL )  !  scratch buffer for KFCOUNT

        LOGICAL, SAVE :: FIRSTIME = .TRUE.
        LOGICAL, SAVE :: OFFSET64 = .FALSE.
        INTEGER, SAVE :: FMODE           !  netCDF file-opening mode


C.............................................................................
C   begin body of subroutine  CRTFIL3
C...........  Create the netCDF file, and put it into definition mode.

!$OMP   CRITICAL( WRITE3_INIT )
        IF ( FIRSTIME ) THEN

            OFFSET64 = ENVYN( 'IOAPI_OFFSET_64',
     &                        'Use NF_64BIT_OFFSET or not',
     &                        .FALSE.,
     &                         IERR )
            IF ( IERR .GT. 0 ) THEN
                EFLAG = .TRUE.
                CALL M3MSG2( 'Bad environment vble "IOAPI_LOG_WRITE"' )
            END IF

            FMODE = IOR( NF_NOCLOBBER, NF_SHARE )
            IF ( OFFSET64 ) THEN
                FMODE = IOR( FMODE, NF_64BIT_OFFSET )
            END IF
            
            AFLAG = ENVYN( 'IOAPI_CFMETA',
     &                     'Generate CF-convention metadata or not?',
     &                     .FALSE., IERR )
            IF ( IERR .GT. 0 ) THEN
                CALL M3MSG2( 'Bad environment vble "IOAPI_CFMETA"' )
            ELSE IF ( AFLAG ) THEN
                CALL INITCF()
            END IF
            
            AFLAG = ENVYN( 'IOAPI_CMAQMETA',
     &                     'Generate CMAQ-convention metadata or not?',
     &                     .FALSE., IERR )
            IF ( IERR .GT. 0 ) THEN
                CALL M3MSG2( 'Bad environment vble "IOAPI_CMAQMETA"' )
            ELSE IF ( AFLAG ) THEN
                CALL INITCMAQ( CMISS3 )
            END IF

            FIRSTIME = .FALSE.

        END IF		!  firstime
!$OMP   END CRITICAL( WRITE3_INIT )

        IF ( NVARS3D .GT. MXVARS3 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &                    'Max NVARS for this build exceeded' )
            CRTFIL3 = .FALSE.
            RETURN
        END IF

        EFLAG = .FALSE.

!$OMP CRITICAL( S_NC )

C...........  If BINIO3-mode file, do CRTBIN3 and return

        IF ( CDFID3( FID ) .EQ. BINFIL3 ) THEN
            EFLAG = ( 0 .EQ. CRTBIN3( EQNAME, FID, PGNAME ) )
            GO TO 999
        END IF

C...........  Else create netCDF file and proceed...

        IERR = NF_CREATE( EQNAME, FMODE, FNUM )
        IF ( IERR .NE. 0 ) THEN
            NAMBUF = EQNAME
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &                    'Error creating netCDF file' )
            CALL M3MSG2( EQNAME )
            EFLAG = .TRUE.
            CALL NCCLOS( FNUM, IERR )
            GO TO 999
        END IF          !  ierr nonzero:  NCCRE() failed

        CDFID3( FID ) = FNUM


C...........   Set attributes valid for all file types:
C.......   IOAPI_VERSION:  I/O API version and date

        CALL NCAPTC( FNUM, NCGLOBAL, 'IOAPI_VERSION', NCCHAR,
     &               LEN_TRIM( DSCBUF ), VERSN3, IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &      'Error creating netCDF file attribute IOAPI_VERSION' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCAPTC) failed

C.......   EXEC_ID:  execution ID

        CALL NCAPTC( FNUM, NCGLOBAL, 'EXEC_ID', NCCHAR,
     &               MXDLEN3, EXECN3, IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &          'Error creating netCDF file attribute EXEC_ID ' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCAPTC) failed

C.......   FTYPE:  file type ID

        FTYPE3( FID ) = FTYPE3D
        CALL NCAPT( FNUM, NCGLOBAL, 'FTYPE', NCLONG,
     &              1, FTYPE3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &          'Error creating netCDF file attribute FTYPE' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCAPT() failed

C.......   CDATE:  creation date

        CALL NCAPT( FNUM, NCGLOBAL, 'CDATE', NCLONG,
     &              1, CURDATE, IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &          'Error creating netCDF file attribute CDATE' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCAPT() failed

C.......   CTIME:  creation time

        CALL NCAPT( FNUM, NCGLOBAL, 'CTIME', NCLONG,
     &              1, CURTIME, IERR )

        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &          'Error creating netCDF file attribute CTIME' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCAPT() failed

C.......   WDATE:  date of last update

        CALL NCAPT( FNUM, NCGLOBAL, 'WDATE', NCLONG,
     &              1, CURDATE, IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &          'Error creating netCDF file attribute WDATE' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCAPT() failed

C.......   WTIME:  time of last update

        CALL NCAPT( FNUM, NCGLOBAL, 'WTIME', NCLONG,
     &              1, CURTIME, IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &          'Error creating netCDF file attribute WTIME' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCAPT() failed


C...........   Set up attributes and variables valid for dictionary
C...........   files only:

        IF ( FTYPE3D .EQ. DCTNRY3 ) THEN

            CRTFIL3 = CRDICT3( FID )
            GO TO 999

        END IF          !  ftype is dictionary


C...............   Set up remaining data file attributes:
C.......   SDATE:STIME  starting date&time (normalized by NEXTIME(:,:,0) )

        SDATE3( FID ) = SDATE3D
        STIME3( FID ) = STIME3D
        CALL NEXTIME( SDATE3( FID ), STIME3( FID ), 0 )

        CALL NCAPT( FNUM, NCGLOBAL, 'SDATE', NCLONG,
     &              1, SDATE3( FID ), IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &          'Error creating netCDF file attribute SDATE' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCAPT() failed

        CALL NCAPT( FNUM, NCGLOBAL, 'STIME', NCLONG,
     &              1, STIME3( FID ), IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &      'Error creating netCDF file attribute STIME' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCAPT() failed

C.......   TSTEP:  time step

        TSTEP3( FID ) = TSTEP3D
        CALL NCAPT( FNUM, NCGLOBAL, 'TSTEP', NCLONG,
     &              1, TSTEP3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &      'Error creating netCDF file attribute TSTEP' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCAPT() failed

C.......   MXREC:  maximum existing time step number

        MXREC3( FID ) = 0

C.......   NTHIK:  thickness of perimeter (cells) for BOUNDARY files

        NTHIK3( FID ) = NTHIK3D
        CALL NCAPT( FNUM, NCGLOBAL, 'NTHIK', NCLONG,
     &              1, NTHIK3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &      'Error creating netCDF file attribute NTHIK' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCAPT() failed

C.......   NCOLS:  number of grid columns/profile levels

        NCOLS3( FID ) = NCOLS3D
        CALL NCAPT( FNUM, NCGLOBAL, 'NCOLS', NCLONG,
     &              1, NCOLS3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &      'Error creating netCDF file attribute NCOLS' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCAPT() failed

C.......   NROWS:  number of grid rows/data sites

        NROWS3( FID ) = NROWS3D
        CALL NCAPT( FNUM, NCGLOBAL, 'NROWS', NCLONG,
     &              1, NROWS3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &      'Error creating netCDF file attribute NROWS' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCAPT() failed

C.......   NLAYS:  number of layers

        NLAYS3( FID ) = NLAYS3D
        CALL NCAPT( FNUM, NCGLOBAL, 'NLAYS', NCLONG,
     &              1, NLAYS3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &          'Error creating netCDF file attribute NLAYS' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCAPT() failed

C.......   NVARS:  number of variables

        NVARS3( FID ) = NVARS3D
        CALL NCAPT( FNUM, NCGLOBAL, 'NVARS', NCLONG,
     &              1, NVARS3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &          'Error creating netCDF file attribute NVARS' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCAPT() failed

C.......   GDTYP:  grid type ID (lat-lon, UTM, RADM, etc...)

        GDTYP3( FID ) = GDTYP3D
        CALL NCAPT( FNUM, NCGLOBAL, 'GDTYP', NCLONG,
     &              1, GDTYP3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &          'Error creating netCDF file attribute GDTYP' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCAPT() failed

C.......   P_ALP:  first map-projection descriptive angle

        P_ALP3( FID ) = P_ALP3D
        CALL NCAPT( FNUM, NCGLOBAL, 'P_ALP', NCDOUBLE,
     &              1, P_ALP3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &          'Error creating netCDF file attribute P_ALP ' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCAPT() failed

C.......   P_BET:  second map-projection descriptive angle

        P_BET3( FID ) = P_BET3D
        CALL NCAPT( FNUM, NCGLOBAL, 'P_BET', NCDOUBLE,
     &              1, P_BET3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &          'Error creating netCDF file attribute P_BET ' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCAPT() failed

C.......   P_GAM:  third map-projection descriptive angle

        P_GAM3( FID ) = P_GAM3D
        CALL NCAPT( FNUM, NCGLOBAL, 'P_GAM', NCDOUBLE,
     &              1, P_GAM3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &          'Error creating netCDF file attribute P_GAM ' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCAPT() failed

C.......   XCENT:  lon of coordinate-system origin

        XCENT3( FID ) = XCENT3D
        CALL NCAPT( FNUM, NCGLOBAL, 'XCENT', NCDOUBLE,
     &              1, XCENT3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &          'Error creating netCDF file attribute XCENT ' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCAPT() failed

C.......   YCENT:  lat of coordinate-system origin

        YCENT3( FID ) = YCENT3D
        CALL NCAPT( FNUM, NCGLOBAL, 'YCENT', NCDOUBLE,
     &              1, YCENT3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &          'Error creating netCDF file attribute YCENT ' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCAPT() failed

C.......   XORIG:  X-coord of grid origin

        XORIG3( FID ) = XORIG3D
        CALL NCAPT( FNUM, NCGLOBAL, 'XORIG', NCDOUBLE,
     &              1, XORIG3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &          'Error creating netCDF file attribute XORIG ' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCAPT() failed

C.......   YORIG:  Y-coord of grid origin

        YORIG3( FID ) = YORIG3D
        CALL NCAPT( FNUM, NCGLOBAL, 'YORIG', NCDOUBLE,
     &              1, YORIG3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &          'Error creating netCDF file attribute YORIG ' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCAPT() failed

C.......   XCELL:  cell width (X direction)

        XCELL3( FID ) = XCELL3D
        CALL NCAPT( FNUM, NCGLOBAL, 'XCELL', NCDOUBLE,
     &              1, XCELL3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &          'Error creating netCDF file attribute XCELL ' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCAPT() failed

C.......   YCELL:  cell width (Y direction)

        YCELL3( FID ) = YCELL3D
        CALL NCAPT( FNUM, NCGLOBAL, 'YCELL', NCDOUBLE,
     &              1, YCELL3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &          'Error creating netCDF file attribute YCELL ' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCAPT() failed

C.......   VGTYP:  vertical coordinate type (VGSIGP3, ... ) or IMISS3

        CALL NCAPT( FNUM, NCGLOBAL, 'VGTYP', NCLONG,
     &              1, VGTYP3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &          'Error creating netCDF file attribute VGTYP ' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCAPT() failed
        VGTYP3( FID ) = VGTYP3D

C.......   VGTOP:

        CALL NCAPT( FNUM, NCGLOBAL, 'VGTOP', NCFLOAT,
     &              1, VGTOP3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &          'Error creating netCDF file attribute VGTOP ' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCAPT() failed
        VGTOP3( FID ) = VGTOP3D

C.......   VGLVS( 1, ..., NLAYS3D+1 ):  vertical coordinate values
C.......   (trapped against MXLAYS3 to prevent possible array overflow)

        CALL NCAPT( FNUM, NCGLOBAL, 'VGLVLS', NCFLOAT,
     &              MIN( NLAYS3D+1, MXLAYS3), VGLVS3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &          'Error creating netCDF file attribute VGLVLS ' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCAPT() failed
        DO L = 1, MIN( NLAYS3D+1, MXLAYS3)
           VGLVS3( L,FID ) = VGLVS3D( L )
        END DO

C.......   GDNAM:  grid name

        GDNAM3( FID ) = GDNAM3D
        CALL NCAPTC( FNUM, NCGLOBAL, 'GDNAM', NCCHAR,
     &              NAMLEN3, GDNAM3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &          'Error creating netCDF file attribute GDNAM ' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCAPTC) failed

C.......   UPNAM:  name of last program to update the file

        CALL NCAPTC( FNUM, NCGLOBAL, 'UPNAM', NCCHAR,
     &               NAMLEN3, PGNAME, IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &          'Error creating netCDF file attribute UPNAM ' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCAPTC() failed

C.......   Variables-list for the file:

        IF( NVARS3( FID ) .GT. 0 ) THEN
            CALL NCAPTC( FNUM, NCGLOBAL, 'VAR-LIST', NCCHAR,
     &                   NAMLEN3 * NVARS3( FID ), VNAME3D, IERR )
            IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &              'Error creating attribute VAR-LIST ' )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !  ierr nonzero:  NCAPTC() failed
        END IF              !  if nvars > 0

C.......   FILEDESC:  file description

        CALL NCAPTC( FNUM, NCGLOBAL, 'FILEDESC', NCCHAR,
     &               MXDLEN3 * MXDESC3, FDESC3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &          'Error creating netCDF file attribute FILEDESC ' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCAPTC() failed

C.......   HISTORY:  update description

        CALL NCAPTC( FNUM, NCGLOBAL, 'HISTORY', NCCHAR,
     &               MXDLEN3 * MXDESC3, SCNDSC, IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &          'Error creating netCDF file attribute HISTORY ' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCAPTC() failed



C...............   Now define dimensions for the file:
C...........   First:  time-step and layer-number dimensions
C...........   (valid for all file types)

        IF ( FTYPE3D .EQ. KFEVNT3 ) THEN

            TSIZE = NCUNLIM
            DSIZE = 5               !  for ddim

        ELSE
            IF ( TSTEP3D .GT. 0 ) THEN
                TSIZE = NCUNLIM
            ELSE IF ( TSTEP3D .LT. 0 ) THEN    !  restart (circular-buffer) file
                TSIZE = 2
            ELSE                               !  time-independent-data file
                TSIZE = 1
            END IF          !  kfevent, or not

            DSIZE = 2               !  for ddim

        END IF          !  kfevent, or tstep > 0, or < 0, or = 0

        TDIM = NCDDEF( FNUM, 'TSTEP', TSIZE, IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &          'Error creating netCDF dimension TSTEP ' )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !  ierr nonzero:  NCDDEF() failed

        DDIM = NCDDEF( FNUM, 'DATE-TIME', DSIZE, IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &          'Error creating netCDF dimension DATE-TIME' )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !  ierr nonzero:  NCDDEF() failed

        LDIM = NCDDEF( FNUM, 'LAY', NLAYS3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &          'Error creating netCDF dimension LAY' )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !  ierr nonzero:  NCDDEF() failed

        VDIM = NCDDEF( FNUM, 'VAR', MAX( NVARS3D, 1 ), IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &          'Error creating netCDF dimension VAR ' )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !  ierr nonzero:  NCDDEF() failed

        IF ( FTYPE3D .EQ. KFEVNT3 ) THEN

            NDIMS = 2               !  for tindx(fid)
            DIMS( 1 ) = DDIM        !  date-time-length-col-row field
            DIMS( 2 ) = TDIM        !  time step dimension
            DSCBUF = '<YYYYDDD,HHMMSS,C,R,H*MMSS>'
            DSCBU2 = 'Event-valid flags:  ' //
     &          'start YYYYDDD,HHMMSS; COL,ROW; duration H*MMSS>'

        ELSE

            NDIMS = 3               !  for tindx(fid)
            DIMS( 1 ) = DDIM        !  date field or time field
            DIMS( 2 ) = VDIM        !  variable number
            DIMS( 3 ) = TDIM        !  time step dimension
            DSCBUF = '<YYYYDDD,HHMMSS>'
            DSCBU2 = 'Timestep-valid flags:  (1) YYYYDDD or (2) HHMMSS'

        END IF          !  kfevent, or tstep > 0, or < 0, or = 0


C...........   Create timestep-available-flag variable:

        TINDX3( FID ) = NCVDEF( FNUM, 'TFLAG',
     &                          NCLONG, NDIMS, DIMS, IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &          'Error creating netCDF variable TFLAG ' )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !  ierr nonzero:  NCVDEF() failed

        CALL NCAPTC( FNUM, TINDX3( FID ), 'units', NCCHAR,
     &               NAMLEN3, DSCBUF( 1:NAMLEN3 ), IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &          'Error creating FLAG attribute "units"' )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !  ierr nonzero:  NCAPTC() failed

        DSCBUF = 'TFLAG'
        CALL NCAPTC( FNUM, TINDX3( FID ), 'long_name', NCCHAR,
     &               NAMLEN3, DSCBUF( 1:NAMLEN3 ), IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &          'Error creating FLAG attribute "long_name" ' )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !  ierr nonzero:  NCAPTC() failed

        CALL NCAPTC( FNUM, TINDX3( FID ), 'var_desc', NCCHAR,
     &               MXDLEN3, DSCBU2, IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &          'Error creating FLAG attribute VAR_DESC ')
            EFLAG = .TRUE.
            GO TO 999
        END IF              !  ierr nonzero:  NCAPTC() failed


C...........   Now, other dimensions (depend upon file type):

        IF ( FTYPE3D .EQ. CUSTOM3 ) THEN        !  other dimensions not known

            CDIM = NCDDEF( FNUM, 'COL', NCOLS3D, IERR )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &              'Error creating netCDF dimension COL' )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !  ierr nonzero:  NCDDEF() failed

            BSIZE3( FID ) = NCOLS3( FID )
            DIMS( 1 ) = CDIM    !  blob-size
            DIMS( 2 ) = LDIM
            DIMS( 3 ) = TDIM

            NDIMS = 3

        ELSE IF ( FTYPE3D .EQ. GRDDED3 ) THEN

            BSIZE3( FID ) = NCOLS3( FID ) * NROWS3( FID )
            RDIM = NCDDEF( FNUM, 'ROW', NROWS3D, IERR )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &              'Error creating netCDF dimension ROW ' )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !  ierr nonzero:  NCDDEF() failed

            CDIM = NCDDEF( FNUM, 'COL', NCOLS3D, IERR )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &              'Error creating netCDF dimension COL' )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !  ierr nonzero:  NCDDEF() failed

            DIMS( 1 ) = CDIM
            DIMS( 2 ) = RDIM
            DIMS( 3 ) = LDIM
            DIMS( 4 ) = TDIM

            NDIMS = 4

        ELSE IF ( FTYPE3D .EQ. BNDARY3 ) THEN

            BSIZE3( FID ) = 2 * ABS( NTHIK3( FID ) )
     &                        * ( NCOLS3( FID ) + NROWS3( FID )
     &                          + 2 * NTHIK3( FID ) )
            ISCR = 2 * NTHIK3D
            ISCR = ISCR * ( NCOLS3D + NROWS3D + ISCR )
            RDIM = NCDDEF( FNUM, 'PERIM', ISCR, IERR )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &              'Error creating netCDF dimension PERIM' )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !  ierr nonzero:  NCDDEF() failed

            DIMS( 1 ) = RDIM
            DIMS( 2 ) = LDIM
            DIMS( 3 ) = TDIM

            NDIMS = 3

        ELSE IF ( FTYPE3D .EQ. IDDATA3 ) THEN

            BSIZE3( FID ) = NROWS3( FID )
            RDIM = NCDDEF( FNUM, 'SITE-NO', NROWS3D, IERR )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &              'Error creating netCDF dimension SITE-NO' )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !  ierr nonzero:  NCDDEF() failed

            DIMS( 1 ) = TDIM

            NINDX3( FID ) = NCVDEF( FNUM, 'ID-COUNT', NCLONG,
     &                              1, DIMS, IERR )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &              'Error creating netCDF variable ID-COUNT' )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !  ierr nonzero:  NCVDEF() failed

            DIMS( 1 ) = RDIM
            DIMS( 2 ) = TDIM

            SINDX3( FID ) = NCVDEF( FNUM, 'ID', NCLONG, 2, DIMS, IERR )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &              'Error creating netCDF variable ID' )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !  ierr nonzero:  NCVDEF() failed

            DIMS( 1 ) = RDIM
            DIMS( 2 ) = LDIM
            DIMS( 3 ) = TDIM

            NDIMS = 3

        ELSE IF ( FTYPE3D .EQ. PROFIL3 ) THEN

            BSIZE3( FID ) = NCOLS3( FID ) * NROWS3( FID )
            RDIM = NCDDEF( FNUM, 'SITE-NO', NROWS3D, IERR )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &              'Error creating netCDF dimension SITE-NO' )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !  ierr nonzero:  NCDDEF() failed

            CDIM = NCDDEF( FNUM, 'LEVEL', NCOLS3D, IERR )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &              'Error creating netCDF dimension LEVEL' )
                EFLAG = .TRUE.
                 GO TO 999
            END IF              !  ierr nonzero:  NCDDEF() failed

            DIMS( 1 ) = TDIM

            NINDX3( FID ) = NCVDEF( FNUM, 'ID-COUNT', NCLONG,
     &                              1, DIMS, IERR )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &              'Error creating netCDF variable ID-COUNT' )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !  ierr nonzero:  NCVDEF() failed

            DIMS( 1 ) = RDIM
            DIMS( 2 ) = TDIM

            SINDX3( FID ) = NCVDEF( FNUM, 'ID', NCLONG, 2, DIMS, IERR )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &              'Error creating netCDF variable ID' )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !  ierr nonzero:  NCVDEF() failed

            LINDX3( FID ) = NCVDEF( FNUM, 'LEVEL-COUNT', NCLONG,
     &                              2, DIMS, IERR )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &              'Error creating netCDF variable LEVEL-COUNT' )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !  ierr nonzero:  NCVDEF() failed

            XINDX3( FID ) = NCVDEF( FNUM, 'X', NCFLOAT, 2, DIMS, IERR )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &              'Error creating netCDF variable X' )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !  ierr nonzero:  NCVDEF() failed

            YINDX3( FID ) = NCVDEF( FNUM, 'Y', NCFLOAT, 2, DIMS, IERR )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &              'Error creating netCDF variable Y' )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !  ierr nonzero:  NCVDEF() failed

            ZINDX3( FID ) = NCVDEF( FNUM, 'Z', NCFLOAT, 2, DIMS, IERR )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &              'Error creating netCDF variable Z' )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !  ierr nonzero:  NCVDEF() failed

            DIMS( 1 ) = CDIM
            DIMS( 2 ) = RDIM
            DIMS( 3 ) = LDIM
            DIMS( 4 ) = TDIM

            NDIMS = 4

        ELSE IF ( FTYPE3D .EQ. GRNEST3 ) THEN

            BSIZE3( FID ) = NCOLS3( FID ) * NROWS3( FID )
            RDIM = NCDDEF( FNUM, 'SITE-NO', NROWS3D, IERR )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &              'Error creating netCDF dimension SITE-NO' )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !  ierr nonzero:  NCDDEF() failed

            CDIM = NCDDEF( FNUM, 'LEVEL', NCOLS3D, IERR )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &              'Error creating netCDF dimension LEVEL' )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !  ierr nonzero:  NCDDEF() failed

            DIMS( 1 ) = TDIM

            NINDX3( FID ) = NCVDEF( FNUM, 'ID-COUNT', NCLONG,
     &                              1, DIMS, IERR )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &              'Error creating netCDF variable ID-COUNT' )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !  ierr nonzero:  NCVDEF() failed

            DIMS( 1 ) = RDIM
            DIMS( 2 ) = TDIM

            SINDX3( FID ) = NCVDEF( FNUM, 'ID', NCLONG, 2, DIMS, IERR )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &              'Error creating netCDF variable ID' )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !  ierr nonzero:  NCVDEF() failed

            WCNDX3( FID ) = NCVDEF( FNUM, 'WIN-COLS', NCLONG,
     &                              2, DIMS, IERR )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &              'Error creating netCDF variable WIN-ROWS' )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !  ierr nonzero:  NCVDEF() failed

            WRNDX3( FID ) = NCVDEF( FNUM, 'WIN-ROWS', NCLONG,
     &                              2, DIMS, IERR )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &              'Error creating netCDF variable WIN-ROWS' )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !  ierr nonzero:  NCVDEF() failed

            LINDX3( FID ) = NCVDEF( FNUM, 'WIN-LEVS', NCLONG,
     &                              2, DIMS, IERR )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &              'Error creating netCDF variable WIN-LEVS' )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !  ierr nonzero:  NCVDEF() failed

            XINDX3( FID ) = NCVDEF( FNUM, 'X', NCDOUBLE, 2, DIMS, IERR )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &              'Error creating netCDF variable X' )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !  ierr nonzero:  NCVDEF() failed

            YINDX3( FID ) = NCVDEF( FNUM, 'Y', NCDOUBLE, 2, DIMS, IERR )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &              'Error creating netCDF variable Y' )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !  ierr nonzero:  NCVDEF() failed

            DXNDX3( FID ) = NCVDEF( FNUM, 'DX', NCDOUBLE, 2, DIMS, IERR)
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &              'Error creating netCDF variable DX' )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !  ierr nonzero:  NCVDEF() failed

            DYNDX3( FID ) = NCVDEF( FNUM, 'DY', NCDOUBLE, 2, DIMS, IERR)
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &              'Error creating netCDF variable DY' )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !  ierr nonzero:  NCVDEF() failed

            DIMS( 1 ) = CDIM
            DIMS( 2 ) = RDIM
            DIMS( 3 ) = TDIM

            NDIMS = 3

        ELSE IF ( FTYPE3D .EQ. SMATRX3 ) THEN

            BSIZE3( FID ) = NCOLS3( FID )
            RDIM = NCDDEF( FNUM, 'ROW-NO', NROWS3D, IERR )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &          'Error creating netCDF dimension ROW-NO' )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !  ierr nonzero:  NCDDEF() failed

            CDIM = NCDDEF( FNUM, 'SP-COEF-NO', NCOLS3D, IERR )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &          'Error creating netCDF dimension SP-COEF-NO' )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !  ierr nonzero:  NCDDEF() failed

            DIMS( 1 ) = RDIM
            DIMS( 2 ) = TDIM

            NDIMS = 2

            SINDX3( FID ) = NCVDEF( FNUM, 'COL-COUNT',
     &                              NCLONG, NDIMS, DIMS, IERR )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &          'Error creating netCDF variable COL-COUNT' )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !  ierr nonzero:  NCVDEF() failed

            DIMS( 1 ) = CDIM
            DIMS( 2 ) = TDIM

            NDIMS = 2

            LINDX3( FID ) = NCVDEF( FNUM, 'COEFF-INDEX',
     &                              NCLONG, NDIMS, DIMS, IERR )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &          'Error creating netCDF variable COEFF-INDEX' )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !  ierr nonzero:  NCVDEF() failed

        ELSE IF ( FTYPE3D .EQ. TSRIES3 ) THEN

            BSIZE3( FID ) = NCOLS3( FID ) * NROWS3( FID )
            RDIM = NCDDEF( FNUM, 'ROW', NROWS3D, IERR )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &              'Error creating netCDF dimension ROW' )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !  ierr nonzero:  NCDDEF() failed

            CDIM = NCDDEF( FNUM, 'COL', NCOLS3D, IERR )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &              'Error creating netCDF dimension COL' )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !  ierr nonzero:  NCDDEF() failed

            DIMS( 1 ) = CDIM
            DIMS( 2 ) = RDIM
            DIMS( 3 ) = LDIM
            DIMS( 4 ) = TDIM

            NDIMS = 4

        ELSE IF ( FTYPE3D .EQ. PTRFLY3 ) THEN	!  "exotic" gridded type

            BSIZE3( FID ) = NCOLS3( FID ) * NROWS3( FID )
            RDIM = NCDDEF( FNUM, 'ROW', NROWS3D, IERR )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &              'Error creating netCDF dimension ROW' )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !  ierr nonzero:  NCDDEF() failed

            CDIM = NCDDEF( FNUM, 'COL', NCOLS3D, IERR )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &              'Error creating netCDF dimension COL' )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !  ierr nonzero:  NCDDEF() failed

            DIMS( 1 ) = CDIM
            DIMS( 2 ) = RDIM
            DIMS( 3 ) = LDIM
            DIMS( 4 ) = TDIM

            NDIMS = 4

        ELSE IF ( FTYPE3D .EQ. KFEVNT3 ) THEN

            EDIM = NCDDEF( FNUM, 'CELL-EVENT', NTHIK3D, IERR )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &          'Error creating netCDF dimension CELL-EVENT' )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !  ierr nonzero:  NCDDEF() failed

            RDIM = NCDDEF( FNUM, 'ROW', NROWS3D, IERR )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &          'Error creating netCDF dimension ROW' )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !  ierr nonzero:  NCDDEF() failed

            CDIM = NCDDEF( FNUM, 'COL', NCOLS3D, IERR )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &          'Error creating netCDF dimension COL' )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !  ierr nonzero:  NCDDEF() failed

            DIMS( 1 ) = CDIM
            DIMS( 2 ) = RDIM
            NDIMS = 2

            NINDX3( FID ) = NCVDEF( FNUM, 'KFCOUNT', NCLONG,
     &                          2, DIMS, IERR )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &          'Error creating netCDF variable KFCOUNT' )
                 EFLAG = .TRUE.
                GO TO 999
            END IF              !  ierr nonzero:  NCVDEF() failed

            DIMS( 1 ) = EDIM
            DIMS( 2 ) = CDIM
            DIMS( 3 ) = RDIM
            NDIMS = 3

            SINDX3( FID ) = NCVDEF( FNUM, 'KFEVENT',
     &                              NCLONG, 3, DIMS, IERR )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &          'Error creating netcdf variable KFEVENT' )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !  ierr nonzero:  NCVDEF() failed

            LINDX3( FID ) = NCVDEF( FNUM, 'KFSDATE',
     &                              NCLONG, 3, DIMS, IERR )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &          'Error creating netCDF variable KFSDATE' )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !  ierr nonzero:  NCVDEF() failed

            XINDX3( FID ) = NCVDEF( FNUM, 'KFSTIME',
     &                              NCLONG, 3, DIMS, IERR )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &          'Error creating netCDF variable KFSTIME' )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !  ierr nonzero:  NCVDEF() failed

            YINDX3( FID ) = NCVDEF( FNUM, 'KFLNGTH',
     &                              NCLONG, 3, DIMS, IERR )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &          'Error creating netCDF variable KFLNGTH' )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !  ierr nonzero:  NCVDEF() failed

            NDIMS = 2           !  for kfevnt variables
            DIMS( 1 ) = LDIM
            DIMS( 2 ) = TDIM

        ELSE    !  illegal file type

            WRITE( LOGDEV,91010 )
     &          'File creation error for file ' // FLIST3( FID ),
     &          'ILLEGAL FILE TYPE.', FTYPE3D
            EFLAG = .TRUE.
            CALL NCABOR( FNUM, IERR )
            GO TO 999

        END IF  !  if file type CUSTOM3, GRDDED3, BNDARY3, IDDATA3,
                !  PROFIL3, GRNEST3, SMATRIX3, or else none of these.


C.......   Define all the Models-3 variables for this file:

        DO 111  VAR = 1 , NVARS3D

            ISCR = VTYPE3D( VAR )

            IF ( ISCR .NE. M3INT   .AND.
     &           ISCR .NE. M3REAL  .AND.
     &           ISCR .NE. M3DBLE ) THEN
                WRITE( LOGDEV,91010 )
     &          'Error creating netCDF variable ' // VNAME3D( VAR ) //
     &          'for file ' // FLIST3( FID ),
     &          'Illegal data type', ISCR
                EFLAG = .TRUE.
                CALL NCABOR( FNUM, IERR )
                GO TO 999
            END IF

            VLIST3( VAR,FID ) = VNAME3D( VAR )
            UNITS3( VAR,FID ) = UNITS3D( var )
            VTYPE3( VAR,FID ) = ISCR
            VINDX3( VAR,FID ) = NCVDEF( FNUM,
     &                                  VNAME3D( VAR ),
     &                                  VTYPE3D( VAR ),
     &                                  NDIMS, DIMS, IERR )

            IF ( IERR .NE. 0 ) THEN
                DSCBUF =
     &           'Error creating netCDF variable ' // VNAME3D( VAR )
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBUF )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !  ierr nonzero:  NCVDEF() failed

C...............   Now define attributes for the variables:
C...........   Note:  "long_name" and "units" are standard netCDF attributes.

            CALL NCAPTC( FNUM, VINDX3( VAR,FID ), 'long_name',
     &                   NCCHAR, NAMLEN3, VNAME3D( VAR ), IERR )
            IF ( IERR .NE. 0 ) THEN
                DSCBUF =
     &          'Error creating  attribute LONG_NAME for variable ' //
     &          VNAME3D( VAR )
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBUF )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !  ierr nonzero:  NCAPTC() failed

            CALL NCAPTC( FNUM, VINDX3( VAR,FID ), 'units',
     &                   NCCHAR, NAMLEN3, UNITS3D( VAR ), IERR )
            IF ( IERR .NE. 0 ) THEN
                WRITE( LOGDEV,91010 )
     &              'Error creating ' // VNAME3D( VAR ) //
     &                  ' attribute UNITS for file ' // FLIST3( FID ),
     &              'netCDF error number', IERR
                EFLAG = .TRUE.
                CALL NCABOR( FNUM, IERR )
                GO TO 999
            END IF              !  ierr nonzero:  NCAPTC() failed

            CALL NCAPTC( FNUM, VINDX3( VAR,FID ), 'var_desc',
     &                   NCCHAR, MXDLEN3, VDESC3D( VAR ), IERR )
            IF ( IERR .NE. 0 ) THEN
                DSCBUF =
     &          'Error creating  attribute UNITS for variable ' //
     &          VNAME3D( VAR )
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBUF )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !  ierr nonzero:  NCAPTC() failed

111     CONTINUE        ! end loop on VAR, defining variables & attributes


C...........   Additional variable attributes for time series

        IF ( FTYPE3D .EQ. TSRIES3 ) THEN

            DO 120 VAR = 1 , NVARS3D

                CALL NCAPT( FNUM, VINDX3( VAR,FID ), 'natts',
     &                    NCLONG, 1, NATTS3D( VAR ), IERR )
                IF ( IERR .NE. 0 ) THEN
                    DSCBUF =
     &              'Error creating  attribute "natts" for ' //
     &              'variable ' // VNAME3D( VAR )
                    CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBUF )
                    EFLAG = .TRUE.
                    GO TO 999
                END IF

                DO 115 ATT = 1, NATTS3D( VAR )   ! loop for each addtnl var. att.

                    CALL NCAPT( FNUM,
     &                          VINDX3( VAR,FID ), ATNAM3D( ATT,VAR ),
     &                          NCFLOAT, 1, FATTS3D( ATT,VAR ), IERR )

                    IF ( IERR .NE. 0 ) THEN
                        DSCBUF =
     &                    'Error creating  attribute ' //
     &                     ATNAM3D( ATT,VAR ) //
     &                    ' for variable ' // VNAME3D( VAR )
                        CALL M3ABORT( FLIST3(FID), FNUM, IERR, DSCBUF )
                        EFLAG = .TRUE.
                        GO TO 999
                    END IF

115             CONTINUE

120         CONTINUE

        END IF          !   ftype tseries3


C...........   Put FNUM back into data mode:  attributes and variables
C...........   now defined.

        CALL NCENDF( FNUM, IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &              'Error putting netCDF file into data mode.' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCENDF() failed

        IF ( FTYPE3D .EQ. KFEVNT3 ) THEN        !  initialize KFCOUNT

            DO  C = 1, MIN( NCOLS3( FID ), MXCOL )
                KFBUF( C ) = 0
            END DO

            DO R = 1, NROWS3( FID )
            DO C = 1, NCOLS3( FID ), MXCOL

                DIMS( 1 ) = C
                DIMS( 2 ) = R
                DELS( 1 ) = MIN( NCOLS3D, C+MXCOL-1 ) - C + 1
                DELS( 2 ) = 1

                CALL NCVPT( FNUM, NINDX3( FID ), DIMS, DELS,
     &                      KFBUF, IERR )
                IF ( IERR .NE. 0 ) THEN
                    CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &                            'Error initializing KFCOUNT' )
                    EFLAG = .TRUE.
                    GO TO 999
                END IF          !  ierr nonzero:  NCENDF() failed

            END DO
            END DO

        END IF          !  if KF-Event file:         !  if file is volatile


        IF ( CFMETA ) THEN
            IF ( .NOT.SETCF( FID ) ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &          'Error with CF metadata' )
                EFLAG = .TRUE.
                GO TO 999
            END IF
        END IF

        IF ( CMAQMETA ) THEN
            IF ( .NOT.SETCMAQ( FID ) ) THEN
!!                CALL M3ABORT( FLIST3( FID ), FNUM, IERR,      !!  cmaqmeta not yet implemented
!!     &          'Error with CF metadata' )
!!                EFLAG = .TRUE.
!!                GO TO 999
            END IF
        END IF


        IF ( VOLAT3( FID ) ) THEN     !  volatile file:  synch with disk
            CALL NCSNC( FNUM, IERR )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &          'Error with disk synchronization' )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !  if synch failed
        END IF 


C.......   File creation now complete, or else we have a failure-exit
C.......   from this critical section.

999     CONTINUE

!$OMP END CRITICAL( S_NC )

        CRTFIL3 = ( .NOT. EFLAG )

        RETURN

C******************  FORMAT  STATEMENTS   ******************************

C...........   Error and warning message formats..... 91xxx

91010   FORMAT ( //5X , '>>> WARNING in subroutine CRTFIL3 <<<',
     &            2 ( /5X , A , : ) , I5, // )

        END FUNCTION CRTFIL3

