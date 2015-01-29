
        LOGICAL FUNCTION  OPNFIL3 ( EQNAME, FID, FSTATUS, PGNAME )

C***********************************************************************
C Version "$Id: opnfil3.F 100 2015-01-16 16:52:16Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
C (C) 2003-2010 Baron Advanced Meteorological Systems
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  function body starts at line  98
C
C  FUNCTION: 
C       open pre-existing Models-3 file with logical name FLIST3( FID ),
C       with readonly/readwrite status FSTATUS.  If opened for write,
C       copy scenario description from I/O STATE3.EXT to file's
C       history, and name PGNAME of caller to file's updater-name.
C       Returns TRUE if the file is already open.
C
C  RETURN VALUE:
C       TRUE iff it succeeds in opening the file, reading its attributes,
C       and storing the relevant ones in STATE3.EXT
C
C  PRECONDITIONS REQUIRED:
C       File FLIST3( FID ) already exists.
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C
C  REVISION  HISTORY:
C       prototype 9/1994 by CJC to go with new version of OPEN3()
C
C	Modified  2/1995 by CJC to handle file type SMATRX3.
C
C	Modified 10/1995 by CJC to handle file type TSRIES3.
C
C	Modified  1/1997 by CJC to handle file type PTRFLY3.
C
C       revised   6/1999 by CJC:  OpenMP thread-safety
C
C       revised   3/2002 by CJC:  error message for bad ncclos() status,
C       for volatile, call ncsync() before return; no NF_SHARED  for CraysC
C
C       revised   3/2002 by CJC:  bug fix with OR-operation on FSTATUS
C
C       revised   3/2002 by CJC:  set NF_SHARE for volatile files
C
C       revised   6/2002 by CJC:  Don't set NF_SHARE for Cray
C
C       Modified 10/2003 by CJC for I/O API version 3:  support for
C       native-binary BINFIL3 file type
C
C       Modified 12/2004 by CJC:  MXVARS3 check
C
C       Modified 10/2009 by CJC:  flags-change for netCDF4  from 
C       Martin Otte, US EPA -- remove NF_NOCLOBBER
C
C       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
C***********************************************************************

      IMPLICIT NONE

C...........   INCLUDES:

        INCLUDE 'PARMS3.EXT'
        INCLUDE 'STATE3.EXT'
        INCLUDE 'NETCDF.EXT'


C...........   ARGUMENTS and their descriptions:

        CHARACTER*(*), INTENT(IN   ) :: EQNAME  !  physical name of file to be opened
        INTEGER      , INTENT(IN   ) :: FID     !  index into STATE3 tables
        INTEGER      , INTENT(IN   ) :: FSTATUS !  read-only/read-write status for NCOPN()
        CHARACTER*(*), INTENT(IN   ) :: PGNAME  !  name of calling program


C...........   EXTERNAL FUNCTIONS and their descriptions:

        LOGICAL, EXTERNAL :: CKFILE3 !  header-consistency check
        INTEGER, EXTERNAL :: OPNBIN3 !  open native-binary-mode files


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER              FNUM       !  netCDF file ID from NCCRE()
        INTEGER              FMODE      !  netCDF file-opening mode
        INTEGER              TDIM       !  netCDF dimension ID from NCDID
        INTEGER              IERR       !  netCDF error status return
        INTEGER              VAR        !  loop counter for file variables
        INTEGER              ND, NA
        INTEGER              D( MAXVDIMS )
        LOGICAL              EFLAG
        CHARACTER*(MAXNCNAM) TNAME      !  dummy arg for NCDINQ
        CHARACTER*512        EQ512      !  fixed-length buffer
        CHARACTER*256        MESG       !  fixed-length buffer

C.............................................................................
C   begin body of subroutine  OPNFIL3

        IF ( CDFID3( FID ) .EQ. BUFFIL3 ) THEN
            MESG = 'Error opening "BUFFERED" file ' // FLIST3( FID )
            CALL M3MSG2( MESG )
            CALL M3MSG2( 'File has not yet been created.' )
            OPNFIL3 = .FALSE.
            RETURN
        END IF

        IF ( FSTATUS .NE. NF_WRITE ) THEN
            FMODE = FSTATUS
        ELSE
#if _CRAY
            FMODE = NF_WRITE
#endif
#if ! ( _CRAY )
           FMODE = IOR( NF_SHARE, NF_WRITE )
#endif
        END IF
#if ! ( _CRAY )
        IF ( VOLAT3( FID ) ) THEN
            FMODE = IOR( FMODE , NF_SHARE )
        END IF
#endif

        EFLAG = .FALSE.

!$OMP CRITICAL( S_NC )

C...........  If BINIO3-mode file, do CRTBIN3 and return

        IF ( CDFID3( FID ) .EQ. BINFIL3 ) THEN
            EFLAG = ( 0 .EQ. OPNBIN3( EQNAME, FID, FSTATUS, PGNAME ) )
            GO TO 999
        END IF

C...........  Else open the netCDF file with that EQNAME....

        EQ512= EQNAME    
        IERR = NF_OPEN( EQNAME, FMODE, FNUM )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 
     &      'Error opening file at path-name:' )
            CALL M3MSG2( EQ512 )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCCRE() failed
        CDFID3( FID ) = FNUM


C...........   Get attributes valid for all file types:
C...........   FTYPE:  file type ID

        CALL NCAGT( FNUM, NCGLOBAL, 'FTYPE', FTYPE3( FID ), IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 
     &      'Error reading netCDF file attribute FTYPE.' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCAGT() failed

C.......   NVARS:  number of variables

        CALL NCAGT( FNUM, NCGLOBAL, 'NVARS', NVARS3( FID ), IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 
     &      'Error reading netCDF file attribute NVARS' )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NVARS3( FID ) .GT. MXVARS3 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 
     &      'Max value of attribute NVARS exceeded for this build' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCAGT() failed

        IF ( FTYPE3( FID ) .EQ. DCTNRY3 ) THEN

C...........   Set up variable FLAG:  "record-written" indicator, valid
C...........   for dictionary files:

            TINDX3( FID ) = NCVID( FNUM, 'FLAG', IERR )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 
     &          'Error reading file attribute FLAG' )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !  ierr nonzero:  NCAGT() failed

        ELSE    ! Set up attributes and variables valid for nondictionary files:

C.......   TSTEP:  time step

            IF ( FTYPE3( FID ) .NE. KFEVNT3 ) THEN
                CALL NCAGT( FNUM, NCGLOBAL,  
     &                      'TSTEP', TSTEP3( FID ), IERR )
                IF ( IERR .NE. 0 ) THEN
                    CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 
     &              'Error reading file attribute TSTEP' )
                    EFLAG = .TRUE.
                    GO TO 999
                END IF          !  ierr nonzero:  NCAGT() failed
            END IF          !  if not kfevnt3

C.......   SDATE:  starting date (Julian date YYYYDDD)

            CALL NCAGT( FNUM, NCGLOBAL, 'SDATE', SDATE3( FID ) , IERR )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 
     &          'Error reading file attribute SDATE' )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !  ierr nonzero:  NCAGT() failed

C.......   STIME:  starting time (HHMMSS)

            CALL NCAGT( FNUM, NCGLOBAL, 'STIME', STIME3( FID ) , IERR )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 
     &          'Error reading file attribute STIME' )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !  ierr nonzero:  NCAGT() failed

C.......   NTHIK:  boundary file perimeter thickness (cells)

            CALL NCAGT( FNUM, NCGLOBAL, 'NTHIK', NTHIK3( FID ), IERR )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 
     &         'Error reading file attribute NTHIK' )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !  ierr nonzero:  NCAGT() failed

C.......   NCOLS:  number of grid columns/profile levels

            CALL NCAGT( FNUM, NCGLOBAL, 'NCOLS', NCOLS3( FID ), IERR )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 
     &          'Error reading file attribute NCOLS' )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !  ierr nonzero:  NCAGT() failed

C.......   NROWS:  number of grid rows/data sites

            CALL NCAGT( FNUM, NCGLOBAL, 'NROWS', NROWS3( FID ), IERR )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 
     &          'Error reading file attribute NROWS' )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !  ierr nonzero:  NCAGT() failed

C.......   NLAYS:  number of layers

            CALL NCAGT( FNUM, NCGLOBAL, 'NLAYS', NLAYS3( FID ), IERR )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 
     &          'Error reading file attribute NLAYS' )
                GO TO 999
            END IF          !  ierr nonzero:  NCAGT() failed

C.......   GDTYP:  grid type ID (lat-lon, UTM, RADM, etc...)

            CALL NCAGT( FNUM, NCGLOBAL, 'GDTYP', GDTYP3( FID ), IERR )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 
     &          'Error reading file attribute GDTYP' )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !  ierr nonzero:  NCAGT() failed


C.......   P_ALP:  first map-projection-description angle               

            CALL NCAGT( FNUM, NCGLOBAL, 'P_ALP', P_ALP3( FID ), IERR )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 
     &          'Error reading file attribute P_ALP' )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !  ierr nonzero:  NCAGT() failed

C.......   P_BET:  second map-projection-description angle              

            CALL NCAGT( FNUM, NCGLOBAL, 'P_BET', P_BET3( FID ), IERR )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 
     &          'Error reading file attribute P_BET' )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !  ierr nonzero:  NCAGT() failed

C.......   P_GAM:  third map-projection-description angle               

            CALL NCAGT( FNUM, NCGLOBAL, 'P_GAM', P_GAM3( FID ), IERR )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 
     &          'Error reading file attribute P_GAM' )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !  ierr nonzero:  NCAGT() failed

C.......   XCENT:  lon of coordinate-system (0,0) origin

            CALL NCAGT( FNUM, NCGLOBAL, 'XCENT', XCENT3( FID ), IERR )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 
     &          'Error reading file attribute XCENT' )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !  ierr nonzero:  NCAGT() failed

C.......   YCENT:  lat of coordinate-system (0,0) origin

            CALL NCAGT( FNUM, NCGLOBAL, 'YCENT', YCENT3( FID ), IERR )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 
     &          'Error reading file attribute YCENT' )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !  ierr nonzero:  NCAGT() failed

C.......   XORIG:  X-coord of grid origin
C.......   (in map units; see FDESC3.EXT for description)

            CALL NCAGT( FNUM, NCGLOBAL, 'XORIG', XORIG3( FID ), IERR )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 
     &          'Error reading file attribute XORIG' )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !  ierr nonzero:  NCAGT() failed

C.......   YORIG:  Y-coord of grid origin
C.......   (in map units; see FDESC3.EXT for description)

            CALL NCAGT( FNUM, NCGLOBAL, 'YORIG', YORIG3( FID ), IERR )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 
     &          'Error reading file attribute YORIG' )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !  ierr nonzero:  NCAGT() failed

C.......   XCELL:  cell width (X direction)
C.......   (in map units; see FDESC3.EXT for description)

            CALL NCAGT( FNUM, NCGLOBAL, 'XCELL', XCELL3( FID ), IERR )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 
     &          'Error reading file attribute XCELL' )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !  ierr nonzero:  NCAGT() failed

C.......   YCELL:  cell width (Y direction)
C.......   (in map units; see FDESC3.EXT for description)

            CALL NCAGT( FNUM, NCGLOBAL, 'YCELL', YCELL3( FID ), IERR )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 
     &          'Error reading file attribute XCELL' )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !  ierr nonzero:  NCAGT() failed

C.......   VGTYP:  vertical coordinate type
           
            CALL NCAGT( FNUM, NCGLOBAL, 'VGTYP', VGTYP3( FID ), IERR )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 
     &                'Error reading file attribute VGTYP.' )
                CALL M3MSG2( MESG )
                GO TO 999
            END IF          !  ierr nonzero:  NCAGT() failed

C.......   VGTOP:  vertical  coordinate sigma-top

            CALL NCAGT( FNUM, NCGLOBAL, 'VGTOP', VGTOP3( FID ), IERR )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 
     &                'Error reading file attribute VGTOP.' )
                CALL M3MSG2( MESG )
                GO TO 999
            END IF          !  ierr nonzero:  NCAGT() failed

C.......   VGLVS:  vertical coordinate values

            CALL NCAGT( FNUM, NCGLOBAL, 'VGLVLS', VGLVS3(1,FID), IERR )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 
     &                'Error reading file attribute VGLVS.' )
                CALL M3MSG2( MESG )
                GO TO 999
            END IF          !  ierr nonzero:  NCAGT() failed

C.......   GDNAM:  grid name

            CALL NCAGTC( FNUM, NCGLOBAL, 'GDNAM',
     &                   GDNAM3( FID ), NAMLEN3, IERR )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 
     &          'Error reading file attribute GDNAM' )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !  ierr nonzero:  NCAGT() failed


C...........   Get ID for timestep-available-flag variable:

            TINDX3( FID ) = NCVID( FNUM, 'TFLAG', IERR )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 
     &          'Error reading file attribute TFLAG' )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !  ierr nonzero:  NCAGT() failed

C...........   Get extent of the time dimension:

            TDIM = NCDID( FNUM, 'TSTEP', IERR )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 
     &          'Error reading time-dimension ID' )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !  ierr nonzero:  NCAGT() failed

            CALL NCDINQ( FNUM, TDIM, TNAME, MXREC3( FID ), IERR )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 
     &          'Error reading maximum timestep number' )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !  ierr nonzero:  NCAGT() failed

C...........   Now, get netCDF variable ID's

            IF ( FTYPE3( FID ) .EQ. CUSTOM3 )  THEN

                BSIZE3( FID ) = NCOLS3( FID )

            ELSE IF ( FTYPE3( FID ) .EQ. GRDDED3 )  THEN

                BSIZE3( FID ) = NCOLS3( FID ) * NROWS3( FID )

            ELSE IF ( FTYPE3( FID ) .EQ. BNDARY3 ) THEN

                BSIZE3( FID ) = 2 * ABS( NTHIK3( FID ) )
     &                            * ( NCOLS3( FID ) + NROWS3( FID ) 
     &                              + 2 * NTHIK3( FID ) )

            ELSE IF ( FTYPE3( FID ) .EQ. IDDATA3 ) THEN

                BSIZE3( FID ) = NROWS3( FID )

                NINDX3( FID ) = NCVID( FNUM, 'ID-COUNT', IERR )
                IF ( IERR .NE. 0 ) THEN
                    CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 
     &              'Error reading ID for variable ID-COUNT' )
                    EFLAG = .TRUE.
                    GO TO 999
                END IF          !  ierr nonzero:  NCAGT() failed

                SINDX3( FID ) = NCVID( FNUM, 'ID', IERR )
                IF ( IERR .NE. 0 ) THEN
                    CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 
     &              'Error reading netcdf ID for variable ID' )
                    EFLAG = .TRUE.
                    GO TO 999
                END IF          !  ierr nonzero:  NCAGT() failed

            ELSE IF ( FTYPE3( FID ) .EQ. PROFIL3 ) THEN

                BSIZE3( FID ) = NCOLS3( FID ) * NROWS3( FID )

                NINDX3( FID ) = NCVID( FNUM, 'ID-COUNT', IERR )
                IF ( IERR .NE. 0 ) THEN
                    CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 
     &              'Error reading netcdf ID for variable ID-COUNT' )
                    EFLAG = .TRUE.
                    GO TO 999
                END IF          !  ierr nonzero:  NCAGT() failed

                SINDX3( FID ) = NCVID( FNUM, 'ID', IERR )
                IF ( IERR .NE. 0 ) THEN
                    CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 
     &              'Error reading netcdf ID for variable ID' )
                    EFLAG = .TRUE.
                    GO TO 999
                END IF          !  ierr nonzero:  NCAGT() failed

                LINDX3( FID ) = NCVID( FNUM, 'LEVEL-COUNT', IERR )
                IF ( IERR .NE. 0 ) THEN
                    CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 
     &              'Error reading ID for variable LEVEL-COUNT' )
                    EFLAG = .TRUE.
                    GO TO 999
                END IF          !  ierr nonzero:  NCAGT() failed

                XINDX3( FID ) = NCVID( FNUM, 'X', IERR )
                IF ( IERR .NE. 0 ) THEN
                    CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 
     &              'Error reading ID for variable X' )
                    EFLAG = .TRUE.
                    GO TO 999
                END IF          !  ierr nonzero:  NCAGT() failed

                YINDX3( FID ) = NCVID( FNUM, 'Y', IERR )
                IF ( IERR .NE. 0 ) THEN
                    CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 
     &              'Error reading ID for variable Y' )
                    EFLAG = .TRUE.
                    GO TO 999
                END IF          !  ierr nonzero:  NCAGT() failed

                ZINDX3( FID ) = NCVID( FNUM, 'Z', IERR )
                IF ( IERR .NE. 0 ) THEN
                    CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 
     &              'Error reading ID for variable Z' )
                    EFLAG = .TRUE.
                    GO TO 999
                END IF          !  ierr nonzero:  NCAGT() failed

            ELSE IF ( FTYPE3( FID ) .EQ. GRNEST3 ) THEN

                BSIZE3( FID ) = NCOLS3( FID ) * NROWS3( FID )

                NINDX3( FID ) = NCVID( FNUM, 'ID-COUNT', IERR )
                IF ( IERR .NE. 0 ) THEN
                    CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 
     &              'Error reading ID for variable ID-COUNT' )
                    EFLAG = .TRUE.
                    GO TO 999
                END IF          !  ierr nonzero:  NCAGT() failed

                SINDX3( FID ) = NCVID( FNUM, 'ID', IERR )
                IF ( IERR .NE. 0 ) THEN
                    CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 
     &              'Error reading netcdf ID for variable "ID"' )
                    EFLAG = .TRUE.
                    GO TO 999
                END IF          !  ierr nonzero:  NCAGT() failed

                WCNDX3( FID ) = NCVID( FNUM, 'WIN-COLS', IERR )
                IF ( IERR .NE. 0 ) THEN
                    CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 
     &              'Error reading ID for variable WIN-COLS' )
                    EFLAG = .TRUE.
                    GO TO 999
                END IF          !  ierr nonzero:  NCAGT() failed

                WRNDX3( FID ) = NCVID( FNUM, 'WIN-ROWS', IERR )
                IF ( IERR .NE. 0 ) THEN
                    CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 
     &              'Error reading ID for variable WIN-ROWST' )
                    EFLAG = .TRUE.
                    GO TO 999
                END IF          !  ierr nonzero:  NCAGT() failed

                LINDX3( FID ) = NCVID( FNUM, 'WIN-LEVS', IERR )
                IF ( IERR .NE. 0 ) THEN
                    CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 
     &              'Error reading ID for variable WIN-LEVS' )
                    MESG = 'File name:  ' // FLIST3( FID )
                    EFLAG = .TRUE.
                    GO TO 999
                END IF          !  ierr nonzero:  NCAGT() failed

                XINDX3( FID ) = NCVID( FNUM, 'X', IERR )
                IF ( IERR .NE. 0 ) THEN
                    CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 
     &              'Error reading ID for variable X' )
                    EFLAG = .TRUE.
                    GO TO 999
                END IF          !  ierr nonzero:  NCAGT() failed

                YINDX3( FID ) = NCVID( FNUM, 'Y', IERR )
                IF ( IERR .NE. 0 ) THEN
                    CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 
     &              'Error reading ID for variable Y' )
                    EFLAG = .TRUE.
                    GO TO 999
                END IF          !  ierr nonzero:  NCAGT() failed

                DXNDX3( FID ) = NCVID( FNUM, 'DX', IERR )
                IF ( IERR .NE. 0 ) THEN
                    CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 
     &              'Error reading ID for variable DX' )
                    EFLAG = .TRUE.
                    GO TO 999
                END IF          !  ierr nonzero:  NCAGT() failed

                DYNDX3( FID ) = NCVID( FNUM, 'DY', IERR )
                IF ( IERR .NE. 0 ) THEN
                    CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 
     &              'Error reading ID for variable DY' )
                    EFLAG = .TRUE.
                    GO TO 999
                END IF          !  ierr nonzero:  NCAGT() failed

            ELSE IF ( FTYPE3( FID ) .EQ. SMATRX3 ) THEN

                BSIZE3( FID ) = NCOLS3( FID )

                SINDX3( FID ) = NCVID( FNUM, 'COL-COUNT', IERR )
                IF ( IERR .NE. 0 ) THEN
                    CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 
     &              'Error reading ID for variable COL-COUNT' )
                    EFLAG = .TRUE.
                    GO TO 999
                END IF          !  ierr nonzero:  NCAGT() failed

                LINDX3( FID ) = NCVID( FNUM, 'COEFF-INDEX', IERR )
                IF ( IERR .NE. 0 ) THEN
                    CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 
     &              'Error reading ID for variable COEFF-INDEX' )
                    EFLAG = .TRUE.
                    GO TO 999
                END IF          !  ierr nonzero:  NCAGT() failed

            ELSE IF ( FTYPE3( FID ) .EQ. TSRIES3 )  THEN
 
                BSIZE3( FID ) = NCOLS3( FID ) * NROWS3( FID )

            ELSE IF ( FTYPE3( FID ) .EQ. PTRFLY3 )  THEN
 
                BSIZE3( FID ) = NCOLS3( FID ) * NROWS3( FID )

            ELSE IF ( FTYPE3( FID ) .EQ. KFEVNT3 ) THEN

                BSIZE3( FID ) = NROWS3( FID )

                NINDX3( FID ) = NCVID( FNUM, 'KFCOUNT', IERR )
                IF ( IERR .NE. 0 ) THEN
                    CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 
     &              'Error reading netCDF ID for variable ID-COUNT' )
                    EFLAG = .TRUE.
                    GO TO 999
                END IF              !  ierr nonzero:  NCVID() failed

                SINDX3( FID ) = NCVID( FNUM, 'KFEVENT', IERR )
                IF ( IERR .NE. 0 ) THEN
                    CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 
     &              'Error reading netCDF variable ID' )
                    EFLAG = .TRUE.
                    GO TO 999
                END IF              !  ierr nonzero:  NCVID() failed

                LINDX3( FID ) = NCVID( FNUM, 'KFSDATE', IERR )
                IF ( IERR .NE. 0 ) THEN
                    CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 
     &              'Error reading netCDF ID for variable LEVEL-COUNT' )
                    EFLAG = .TRUE.
                    GO TO 999
                END IF              !  ierr nonzero:  NCVID() failed

                XINDX3( FID ) = NCVID( FNUM, 'KFSTIME', IERR )
                IF ( IERR .NE. 0 ) THEN
                    CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 
     &              'Error reading netCDF ID for variable KFSDATE' )
                    EFLAG = .TRUE.
                    GO TO 999
                END IF              !  ierr nonzero:  NCVID() failed

                YINDX3( FID ) = NCVID( FNUM, 'KFLNGTH', IERR )
                IF ( IERR .NE. 0 ) THEN
                    CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 
     &              'Error reading netCDF ID for variable KFLNGTH' )
                    EFLAG = .TRUE.
                    GO TO 999
                END IF              !  ierr nonzero:  NCVID() failed

            ELSE    !  illegal file type

                MESG = 'File opening error:  ' // 
     &                 'ILLEGAL FILE TYPE for file ' // FLIST3( FID ) 
                    CALL M3MSG2( MESG )
                WRITE( MESG,91010 ) 'Unknown file type ', FTYPE3( FID )
                    CALL M3MSG2( MESG )
                EFLAG = .TRUE.
                GO TO 999

            END IF      !  if file type custom, gridded, idlist, profile,
                        !  gridnest, smatrix, or none of the above.

        END IF          !  file type not dictionary


C.......   Variables-list for the file:

        IF ( NVARS3( FID ) .GT. 0 ) THEN
            CALL NCAGTC( FNUM, NCGLOBAL, 'VAR-LIST', VLIST3( 1,FID ),
     &                   NAMLEN3 * NVARS3( FID ), IERR )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &          'Error reading attribute VAR-LIST' )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !  ierr nonzero:  NCAGT() failed
        END IF              !  if nvars > 0

        DO  VAR = 1 , NVARS3( FID )

            VINDX3( VAR,FID ) = NCVID( FNUM, VLIST3( VAR,FID ), IERR )
            IF ( IERR .NE. 0 ) THEN
                MESG = 'Error reading IDs for  variable '
     &          // VLIST3( VAR,FID )
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, MESG )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !  ierr nonzero:  NCAGT() failed

            CALL NCVINQ( FNUM, 
     &                   VINDX3( VAR,FID ), TNAME, 
     &                   VTYPE3( VAR,FID ), ND, D, NA, IERR )
            IF ( IERR .NE. 0 ) THEN
                MESG = 'Error reading type for  variable '
     &          // VLIST3( VAR,FID )
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, MESG )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !  ierr nonzero:  NCAGT() failed

            CALL NCAGTC( FNUM, VINDX3( VAR,FID ), 'units',
     &                   UNITS3( VAR,FID ), NAMLEN3, IERR )
            IF ( IERR .NE. 0 ) THEN
                MESG = 'Error reading units for  variable '
     &          // VLIST3( VAR,FID )
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, MESG )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !  ierr nonzero:  NCAGT() failed

        END DO          !  end loop on variables


C.......   If opened for WRITE:  put attribute HISTORY:  update description

        IF ( IAND( FSTATUS, NF_WRITE ) .NE. 0 ) THEN

            CALL NCREDF( FNUM, IERR )
            IF ( IERR .NE. 0 ) THEN
               CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &         'Error opening history for update' )
                GO TO 999
            END IF          !  ierr nonzero:  NCAGT() failed

            CALL NCAPTC( FNUM, NCGLOBAL, 'UPNAM', NCCHAR,
     &                   NAMLEN3, PGNAME, IERR )
            IF ( IERR .NE. 0 ) THEN
               CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &         'Error writing file attribute UPNAM' )
                GO TO 999
            END IF          !  ierr nonzero:  NCAGT() failed

            CALL NCAPTC( FNUM, NCGLOBAL, 'HISTORY', NCCHAR,
     &                   MXDLEN3 * MXDESC3, SCNDSC, IERR )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &          'Error writing file attribute HISTORY' )
                GO TO 999
            END IF          !  ierr nonzero:  NCAGT() failed

            CALL NCENDF( FNUM, IERR )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &          'Error flushing HISTORY to disk' )
                GO TO 999
            END IF          !  ierr nonzero:  NCAGT() failed

        END IF
        
        IF ( VOLAT3( FID ) ) THEN     !  volatile file:  synch with disk
            CALL NCSNC( FNUM, IERR )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &          'Error with disk synchronization' )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !  if synch failed
        END IF          !  if file is volatile

        IF ( .NOT. CKFILE3( FID ) ) THEN
            CALL NCCLOS( FNUM, IERR )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &          'Error NCCLOS-ing file' )
            END IF          !  ierr nonzero:  NCAGT() failed
            EFLAG = .TRUE.
        END IF

999     CONTINUE

!$OMP END CRITICAL( S_NC )

        OPNFIL3 = ( .NOT. EFLAG )

        RETURN

C******************  FORMAT  STATEMENTS   ******************************

C...........   Error and warning message formats..... 91xxx

91010   FORMAT ( A , : I5 )

        END FUNCTION  OPNFIL3

