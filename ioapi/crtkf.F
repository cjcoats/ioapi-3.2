
        LOGICAL FUNCTION CRTKF ( EQNAME, FID, PGNAME, BUFFER )

C***********************************************************************
C Version "$Id: crtkf.F 100 2015-01-16 16:52:16Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (C) 2003-2011 Baron Advanced Meteorological Systems, and
C (C) 2011 David Wong
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  subroutine body starts at line 88
C
C  FUNCTION:  Create netCDF KF-Cloud Event file FNAME using info stored
C      in the FDESC3 common.  Record history, Leave file open for update.
C
C  PRECONDITIONS REQUIRED:  File definition already put into FDESC3 common
C
C  SUBROUTINES AND FUNCTIONS CALLED:  M3ABORT()
C
C  REVISION  HISTORY:
C       adapted   4/1996 from CRTFIL3() by CJC for KF-Cloud Event files
C       revised   9/1999 by CJC:  Unification of KFOPEN() with OPEN3()
C       revised   2/2002 by CJC:  for volatile, call ncsync() before return
C       Revised   3/2002 by CJC:  OpenMP thread-safety;
C                                 turn off NF_SHARED for Crays
C       Modified 10/2009 by CJC:  flags-change for netCDF4  from
C       Martin Otte, US EPA -- remove NF_WRITE.
C       Modified 03/20010 by CJC: F9x changes for I/O API v3.1
C       Revised 4/2011 by David Wong, US EPA, and by CJC, to add state for
C       full buffered-file file descriptions.
C***********************************************************************

      IMPLICIT NONE

C...........   INCLUDES:

        INCLUDE 'PARMS3.EXT'
        INCLUDE 'STATE3.EXT'
        INCLUDE 'FDESC3.EXT'
        INCLUDE 'NETCDF.EXT'


C...........   ARGUMENTS and their descriptions:

        CHARACTER*(*), INTENT(IN   ) :: EQNAME  !  physical name of file to be created
        INTEGER      , INTENT(IN   ) :: FID     !  index into STATE3 arrays
        CHARACTER*(*), INTENT(IN   ) :: PGNAME  !  name of calling program
        REAL         , INTENT(  OUT) :: BUFFER( * )


C...........   EXTERNAL FUNCTIONS and their descriptions:

        LOGICAL, EXTERNAL :: ENVYN   !  yes-or-no environment variable value
        INTEGER, EXTERNAL :: INDEX1  !  look up names in tables


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         ISCR
        INTEGER         FNUM            !  netCDF file ID from NCCRE()
        INTEGER         IERR            !  netCDF error status return
        INTEGER         VAR, LVL        !  loop counters for file variables
        INTEGER         TSIZE           !  time-step mode
        INTEGER         CDIM            !  col/level     dim #
        INTEGER         RDIM            !  row/site      dim #
        INTEGER         LDIM            !  layer         dim #
        INTEGER         EDIM            !  cell-event    dim #
        INTEGER         DDIM            !  date-time-duration-cell dim #
        INTEGER         TDIM            !  timestep      dim #
        INTEGER         VDIM            !  variable-flag dim #
        INTEGER         NDIMS           !  number of dims for NCVDEF()
        INTEGER         DIMS( 5 )       !  array of dims for NCVDEF()
        INTEGER         DELS( 5 )       !  array of dims for NCVPUT()
        LOGICAL         EFLAG
        CHARACTER*8     BNAME           !  for "BUFFERED", etc.
        CHARACTER*80    DSCBUF          !  scratch buffer for descriptions

        INTEGER, SAVE :: FMODE           !  netCDF file-opening mode
        LOGICAL, SAVE :: FIRSTIME = .TRUE.
        LOGICAL, SAVE :: OFFSET64 = .FALSE.

C.............................................................................
C   begin body of subroutine  CRTKF
C...........  Create the netCDF file, and put it into definition mode.

        BNAME = EQNAME( 1:8 )   ! normalize case for "BUFFERED" files
        CALL UPCASE( BNAME )
        IF ( BNAME .EQ. 'BUFFERED' ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &                    'Path not a real file:' )
            CALL M3MSG2( EQNAME )
            CRTKF = .FALSE.
            RETURN
        END IF

        EFLAG = .FALSE.

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

            FIRSTIME = .FALSE.

        END IF		!  firstime
!$OMP   END CRITICAL( WRITE3_INIT )


!$OMP CRITICAL( S_NC )

        IERR = NF_CREATE( EQNAME , FMODE, FNUM )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &      'Error creating netCDF file at path:' )
            CALL M3MSG2( EQNAME )
            EFLAG = .TRUE.
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
     &      'Error creating netCDF file attribute EXEC_ID' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCAPTC) failed

C.......   FTYPE:  file type ID

        FTYPE3( FID ) = FTYPE3D
        CALL NCAPT( FNUM, NCGLOBAL, 'FTYPE', NCLONG,
     &              1, FTYPE3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &      'Error creating netCDF file attribute FTYPE' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCAPT() failed

C.......   CDATE:  creation date

        CALL NCAPT( FNUM, NCGLOBAL, 'CDATE', NCLONG,
     &              1, CURDATE, IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &      'Error creating netCDF file attribute CDATE' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCAPT() failed

C.......   CTIME:  creation time

        CALL NCAPT( FNUM, NCGLOBAL, 'CTIME', NCLONG,
     &              1, CURTIME, IERR )

        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &      'Error creating netCDF file attribute CTIME' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCAPT() failed

C.......   WDATE:  date of last update

        CALL NCAPT( FNUM, NCGLOBAL, 'WDATE', NCLONG,
     &              1, CURDATE, IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &      'Error creating netCDF file attribute WDATE' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCAPT() failed

C.......   WTIME:  time of last update

        CALL NCAPT( FNUM, NCGLOBAL, 'WTIME', NCLONG,
     &              1, CURTIME, IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &      'Error creating netCDF file attribute WTIME' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCAPT() failed

C.......   SDATE:  starting date

        SDATE3( FID ) = SDATE3D
        CALL NCAPT( FNUM, NCGLOBAL, 'SDATE', NCLONG,
     &              1, SDATE3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &      'Error creating netCDF file attribute SDATE' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCAPT() failed

C.......   STIME:  starting time:

        STIME3( FID ) = STIME3D
        CALL NCAPT( FNUM, NCGLOBAL, 'STIME', NCLONG,
     &              1, STIME3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &      'Error creating netCDF file attribute STIME' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCAPT() failed

C.......   MXREC:  maximum existing time step number

        MXREC3( FID ) = 0

C.......   NTHIK:  mapped onto cellwise max number of events

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
     &      'Error creating netCDF file attribute NLAYS' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCAPT() failed

C.......   NVARS:  number of variables

        NVARS3( FID ) = NVARS3D
        CALL NCAPT( FNUM, NCGLOBAL, 'NVARS', NCLONG,
     &              1, NVARS3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &      'Error creating netCDF file attribute NVARS' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCAPT() failed

C.......   GDTYP:  grid type ID (lat-lon, UTM, RADM, etc...)

        GDTYP3( FID ) = GDTYP3D
        CALL NCAPT( FNUM, NCGLOBAL, 'GDTYP', NCLONG,
     &              1, GDTYP3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &      'Error creating netCDF file attribute GDTYP' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCAPT() failed

C.......   P_ALP:  first map-projection descriptive angle

        P_ALP3( FID ) = P_ALP3D
        CALL NCAPT( FNUM, NCGLOBAL, 'P_ALP', NCDOUBLE,
     &              1, P_ALP3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &      'Error creating netCDF file attribute P_ALP' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCAPT() failed

C.......   P_BET:  second map-projection descriptive angle

        P_BET3( FID ) = P_BET3D
        CALL NCAPT( FNUM, NCGLOBAL, 'P_BET', NCDOUBLE,
     &              1, P_BET3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &      'Error creating netCDF file attribute P_BET' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCAPT() failed

C.......   P_GAM:  third map-projection descriptive angle

        P_GAM3( FID ) = P_GAM3D
        CALL NCAPT( FNUM, NCGLOBAL, 'P_GAM', NCDOUBLE,
     &              1, P_GAM3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &      'Error creating netCDF file attribute P_GAM' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCAPT() failed

C.......   XCENT:  lon of coordinate-system origin

        XCENT3( FID ) = XCENT3D
        CALL NCAPT( FNUM, NCGLOBAL, 'XCENT', NCDOUBLE,
     &              1, XCENT3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &      'Error creating netCDF file attribute XCENT' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCAPT() failed

C.......   YCENT:  lat of coordinate-system origin

        YCENT3( FID ) = YCENT3D
        CALL NCAPT( FNUM, NCGLOBAL, 'YCENT', NCDOUBLE,
     &              1, YCENT3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &      'Error creating netCDF file attribute YCENT' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCAPT() failed

C.......   XORIG:  X-coord of grid origin

        XORIG3( FID ) = XORIG3D
        CALL NCAPT( FNUM, NCGLOBAL, 'XORIG', NCDOUBLE,
     &              1, XORIG3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &      'Error creating netCDF file attribute XORIG' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCAPT() failed

C.......   YORIG:  Y-coord of grid origin

        YORIG3( FID ) = YORIG3D
        CALL NCAPT( FNUM, NCGLOBAL, 'YORIG', NCDOUBLE,
     &              1, YORIG3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &      'Error creating netCDF file attribute YORIG' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCAPT() failed

C.......   XCELL:  cell width (X direction)

        XCELL3( FID ) = XCELL3D
        CALL NCAPT( FNUM, NCGLOBAL, 'XCELL', NCDOUBLE,
     &              1, XCELL3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &      'Error creating netCDF file attribute XCELL' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCAPT() failed

C.......   YCELL:  cell width (Y direction)

        YCELL3( FID ) = YCELL3D
        CALL NCAPT( FNUM, NCGLOBAL, 'YCELL', NCDOUBLE,
     &              1, YCELL3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &      'Error creating netCDF file attribute YCELL' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCAPT() failed

C.......   VGTYP:  vertical coordinate type (VGSIGP3, ... ) or IMISS3

        CALL NCAPT( FNUM, NCGLOBAL, 'VGTYP', NCLONG,
     &              1, VGTYP3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &      'Error creating netCDF file attribute VGTYP' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCAPT() failed
        VGTYP3( FID ) = VGTYP3D

C.......   VGTOP:

        CALL NCAPT( FNUM, NCGLOBAL, 'VGTOP', NCFLOAT,
     &              1, VGTOP3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &      'Error creating netCDF file attribute VGTOP' )
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
     &      'Error creating netCDF file attribute VGLVLS' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCAPT() failed         !  ierr nonzero:  NCAPT() failed
        DO LVL = 1, MIN( NLAYS3D+1, MXLAYS3 )
           VGLVS3( LVL,FID ) = VGLVS3D( LVL )
        END DO

C.......   GDNAM:  grid name

        GDNAM3( FID ) = GDNAM3D
        CALL NCAPTC( FNUM, NCGLOBAL, 'GDNAM', NCCHAR,
     &              NAMLEN3, GDNAM3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &      'Error creating netCDF file attribute GDNAM' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCAPTC) failed

C.......   UPNAM:  name of last program to update the file

        CALL NCAPTC( FNUM, NCGLOBAL, 'UPNAM', NCCHAR,
     &               NAMLEN3, PGNAME, IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &      'Error creating netCDF file attribute UPNAM' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCAPTC() failed

C.......   Variables-list for the file:

        IF( NVARS3( FID ) .GT. 0 ) THEN
            CALL NCAPTC( FNUM, NCGLOBAL, 'VAR-LIST', NCCHAR,
     &                   NAMLEN3 * NVARS3( FID ), VNAME3D, IERR )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &       'Error creating attribute VAR-LIST for file' )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !  ierr nonzero:  NCAPTC() failed
        END IF              !  if nvars > 0

C.......   FILEDESC:  file description

        CALL NCAPTC( FNUM, NCGLOBAL, 'FILEDESC', NCCHAR,
     &               MXDLEN3 * MXDESC3, FDESC3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &      'Error creating netCDF file attribute FILEDESC' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCAPTC() failed

C.......   HISTORY:  update description

        CALL NCAPTC( FNUM, NCGLOBAL, 'HISTORY', NCCHAR,
     &               MXDLEN3 * MXDESC3, SCNDSC, IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &      'Error creating netCDF file attribute HISTORY' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCAPTC() failed



C...............   Now define dimensions for the file:
C...........   First:  time-step (event) and layer-number dimensions
C...........   (valid for all file types)

        TSIZE = NCUNLIM

        TDIM = NCDDEF( FNUM, 'TSTEP', TSIZE, IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &      'Error creating netCDF dimension TSTEP' )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !  ierr nonzero:  NCDDEF() failed

        DDIM = NCDDEF( FNUM, 'EVENT-LOC', 5, IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &      'Error creating netCDF dimension EVENT-LOC' )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !  ierr nonzero:  NCDDEF() failed

        EDIM = NCDDEF( FNUM, 'CELL-EVENT', NTHIK3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &      'Error creating netCDF dimension CELL-EVENT' )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !  ierr nonzero:  NCDDEF() failed

        LDIM = NCDDEF( FNUM, 'LAY', NLAYS3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &      'Error creating netCDF dimension LAY' )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !  ierr nonzero:  NCDDEF() failed

        VDIM = NCDDEF( FNUM, 'VAR', MAX( NVARS3D, 1 ), IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &      'Error creating netCDF dimension VAR' )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !  ierr nonzero:  NCDDEF() failed


C...........   Now, other dimensions

        RDIM = NCDDEF( FNUM, 'ROW', NROWS3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &      'Error creating netCDF dimension ROW' )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !  ierr nonzero:  NCDDEF() failed

        CDIM = NCDDEF( FNUM, 'COL', NCOLS3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &      'Error creating netCDF dimension COL' )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !  ierr nonzero:  NCDDEF() failed

C...........   Create KFCOUNT event-count variable

        DIMS( 1 ) = CDIM
        DIMS( 2 ) = RDIM
        NDIMS = 2

        NINDX3( FID ) = NCVDEF( FNUM, 'KFCOUNT', NCLONG,
     &                          2, DIMS, IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &      'Error creating netCDF variable KFCOUNT' )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !  ierr nonzero:  NCVDEF() failed

C...........   Create event-description variable:

        DIMS( 1 ) = DDIM        !  date-time-length-col-row field
        DIMS( 2 ) = TDIM        !  time step dimension
        NDIMS = 2

        TINDX3( FID ) = NCVDEF( FNUM, 'TFLAG',
     &                          NCLONG, NDIMS, DIMS, IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &      'Error creating netCDF variable TFLAG' )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !  ierr nonzero:  NCVDEF() failed

        DSCBUF = '<YYYYDDD,HHMMSS,C,R,H*MMSS>'
        CALL NCAPTC( FNUM, TINDX3( FID ), 'units', NCCHAR,
     &               NAMLEN3, DSCBUF( 1:NAMLEN3 ), IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &      'Error creating FLAG attribute "units"' )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !  ierr nonzero:  NCAPTC() failed

        DSCBUF = 'TFLAG'
        CALL NCAPTC( FNUM, TINDX3( FID ), 'long_name', NCCHAR,
     &               NAMLEN3, DSCBUF( 1:NAMLEN3 ), IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &      'Error creating FLAG attribute "long_name"' )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !  ierr nonzero:  NCAPTC() failed

        DSCBUF = 'Event-valid flags:  ' //
     &          'start YYYYDDD,HHMMSS; COL,ROW; duration H*MMSS>'
        CALL NCAPTC( FNUM, TINDX3( FID ), 'var_desc', NCCHAR,
     &               MXDLEN3, DSCBUF, IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &      'Error creating FLAG attribute VAR_DESC' )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !  ierr nonzero:  NCAPTC() failed


        DIMS( 1 ) = EDIM
        DIMS( 2 ) = CDIM
        DIMS( 3 ) = RDIM
        NDIMS = 3

        SINDX3( FID ) = NCVDEF( FNUM, 'KFEVENT', NCLONG, 3, DIMS, IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &      'Error creating netcdf variable KFEVENT' )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !  ierr nonzero:  NCVDEF() failed

        LINDX3( FID ) = NCVDEF( FNUM, 'KFSDATE', NCLONG, 3, DIMS, IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &      'Error creating netCDF variable KFSDATE' )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !  ierr nonzero:  NCVDEF() failed

        XINDX3( FID ) = NCVDEF( FNUM, 'KFSTIME', NCLONG, 3, DIMS, IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &      'Error creating netCDF variable KFSTIME' )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !  ierr nonzero:  NCVDEF() failed

        YINDX3( FID ) = NCVDEF( FNUM, 'KFLNGTH', NCLONG, 3, DIMS, IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &      'Error creating netCDF variable KFLNGTH' )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !  ierr nonzero:  NCVDEF() failed


C.......   Define rest of the variables for this file:

        DIMS( 1 ) = LDIM
        DIMS( 2 ) = TDIM
        NDIMS = 2

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
            UNITS3( VAR,FID ) = UNITS3D( VAR )
            VTYPE3( VAR,FID ) = ISCR
            VINDX3( VAR,FID ) = NCVDEF( FNUM,
     &                                  VNAME3D( VAR ),
     &                                  VTYPE3D( VAR ),
     &                                  NDIMS, DIMS, IERR )

            IF ( IERR .NE. 0 ) THEN
                DSCBUF =
     &          'Error creating netCDF variable ' // VNAME3D( VAR )
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBUF )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !  ierr nonzero:  NCVDEF() failed

C...............   Now define attributes for the variables:
C...........   Note:  "long_name" and "units" are standard netCDF attributes.

            CALL NCAPTC( FNUM, VINDX3( VAR,FID ), 'long_name',
     &                   NCCHAR, NAMLEN3, VNAME3D( VAR ), IERR )
            IF ( IERR .NE. 0 ) THEN
                DSCBUF = 'Error creating attribute LONG_NAME for ' //
     &                   VNAME3D( VAR )
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBUF )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !  ierr nonzero:  NCAPTC() failed

            CALL NCAPTC( FNUM, VINDX3( VAR,FID ), 'units',
     &                   NCCHAR, NAMLEN3, UNITS3D( VAR ), IERR )
            IF ( IERR .NE. 0 ) THEN
                DSCBUF = 'Error creating ttribute UNITS for ' //
     &                   VNAME3D( VAR )
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBUF )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !  ierr nonzero:  NCAPTC() failed

            CALL NCAPTC( FNUM, VINDX3( VAR,FID ), 'var_desc',
     &                   NCCHAR, MXDLEN3, VDESC3D( VAR ), IERR )
            IF ( IERR .NE. 0 ) THEN
                DSCBUF = 'Error creating  attribute VAR_DESC for ' //
     &                   VNAME3D( VAR )
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBUF )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !  ierr nonzero:  NCAPTC() failed

111     CONTINUE        ! end loop on VAR, defining variables & attributes


C...........   Put FNUM back into data mode:  attributes and variables
C...........   now defined.

        CALL NCENDF( FNUM, IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &      'Error putting netCDF file into data mode.' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCENDF() failed

C.......   File creation now complete (control only falls through
C.......   to here in case of success).  Need to initialize KFCOUNT:

        DO  122  VAR = 1, NCOLS3D * NROWS3D
            BUFFER( VAR ) = 0
122     CONTINUE


        DIMS( 1 ) = 1
        DIMS( 2 ) = 1
        DELS( 1 ) = NCOLS3D
        DELS( 2 ) = NROWS3D
        CALL NCVPT( FNUM, NINDX3( FID ), DIMS, DELS, BUFFER, IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &      'Error initializing KFCOUNT' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCENDF() failed

        IF ( VOLAT3( FID ) ) THEN     !  volatile file:  synch with disk
            CALL NCSNC( FNUM, IERR )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &          'Error with disk synchronization' )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !  if synch failed
        END IF          !  if file is volatile


C.......   File creation now complete, or else we have a failure-exit
C.......   from this critical section.

999     CONTINUE

!$OMP END CRITICAL( S_NC )

        CRTKF = ( .NOT. EFLAG )

        RETURN

C******************  FORMAT  STATEMENTS   ******************************

C...........   Error and warning message formats..... 91xxx

91010   FORMAT ( //5X , '>>> WARNING in subroutine CRTKF <<<',
     &            2 ( /5X , A , : ) , I5, // )


        END FUNCTION CRTKF



