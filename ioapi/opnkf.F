
        LOGICAL FUNCTION  OPNKF( EQNAME, FID, FSTATUS, PGNAME, BUFFER )

C***********************************************************************
C Version "$Id: opnkf.F 100 2015-01-16 16:52:16Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
C (C) 2003-2010 Baron Advanced Meteorological Systems
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  function body starts at line  87
C
C  FUNCTION:
C       open pre-existing KF-Cloud file with logical name FLIST3( FID ),
C       with readonly/readwrite status FSTATUS.  If opened for
C       write, copy scenario description from I/O STATE3.EXT to
C       file's history, and name PGNAME of caller to file's
C       updater-name.  Returns TRUE if the file is already open.
C       Return gridded event-counts array in BUFFER
C
C  RETURN VALUE:
C       TRUE iff it succeeds in opening the file, reading its
C       attributes, and storing the relevant ones in STATE3.EXT
C
C  PRECONDITIONS REQUIRED:  File FLIST3( FID ) already exists.
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C
C  REVISION  HISTORY:
C       prototype 9/94 by CJC to go with new version of OPEN3()
C
C	Modified  2/95 by CJC to handle file type SMATRX3.
C
C       revised  6/99 by CJC:  OpenMP thread-safety
C
C       revised   3/2002 by CJC:  error message for bad ncclos() status,
C       for volatile, call ncsync() before return; no NF_SHARED  for Crays
C
C       Modified 5/2003 by CJC: bugfix by by David Wong, US EPA;
C       set NF_SHARE for volatile files.
C
C       revised   6/2002 by CJC:  Don't set NF_SHARE for Cray
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
        REAL         , INTENT(  OUT) :: BUFFER( * )


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER              FMODE      !  netCDF file-opening mode
        INTEGER              FNUM       !  netCDF file ID from NCCRE()
        INTEGER              TDIM       !  netCDF dimension ID from NCDID
        INTEGER              IERR       !  netCDF error status return
        INTEGER              VAR        !  loop counter for file variables
        INTEGER              ND, NA
        INTEGER              D( MAXVDIMS )
        LOGICAL              EFLAG
        INTEGER         DIMS ( 5 )      !  corner arg array for NCVGT()
        INTEGER         DELTS( 5 )      !  corner arg array for NCVGT()
        CHARACTER*(MAXNCNAM) TNAME      !  dummy arg for NCDINQ
        CHARACTER*256        MESG       !  fixed-length buffer

C.............................................................................
C   begin body of subroutine  OPNKF

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

C...........  Open the netCDF file with that EQNAME.

        MESG = EQNAME
        IERR  = NF_OPEN( EQNAME, FMODE, FNUM )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 
     &      'Error opening file at path-name:' )
            CALL M3MSG2( EQNAME )
            EFLAG = .TRUE.
            GO TO  999        !  return
        END IF          !  ierr nonzero:  NCCRE() failed
        CDFID3( FID ) = FNUM


C...........   Get attributes
C...........   FTYPE:  file type ID

        CALL NCAGT( FNUM, NCGLOBAL, 'FTYPE', FTYPE3( FID ), IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 
     &      'Error reading netCDF file attribute FTYPE.' )
            CALL M3MSG2( MESG )
            EFLAG = .TRUE.
            GO TO  999        !  return
        ELSE IF ( FTYPE3( FID ) .NE. KFEVNT3 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 
     &      'OPNKF:  file not a KF-Cloud Event file' )
            CALL M3MSG2( MESG )
            EFLAG = .TRUE.
            GO TO  999        !  return
        END IF          !  ierr nonzero:  NCAGT() failed

C.......   NVARS:  number of variables

        CALL NCAGT( FNUM, NCGLOBAL, 'NVARS', NVARS3( FID ), IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 
     &      'Error reading netCDF file attribute NVARS' )
            EFLAG = .TRUE.
            GO TO  999        !  return
        END IF          !  ierr nonzero:  NCAGT() failed
           
                
C.......   SDATE:  starting date (Julian date YYYYDDD)
      
        CALL NCAGT( FNUM, NCGLOBAL, 'SDATE', SDATE3( FID ) , IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 
     &      'Error reading netCDF file attribute SDATE' )
            EFLAG = .TRUE.
            GO TO  999        !  return
        END IF          !  ierr nonzero:  NCAGT() failed
           
C.......   STIME:  starting time (HHMMSS)
      
        CALL NCAGT( FNUM, NCGLOBAL, 'STIME', STIME3( FID ) , IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( A, I10, 2X, A )' ) 
     &          'NetCDF error number', IERR,
     &          'reading netCDF file attribute STIME'
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,MESG )
            EFLAG = .TRUE.
            GO TO  999        !  return
        END IF          !  ierr nonzero:  NCAGT() failed
           
C.......   NTHIK:  mapped onto cellwise max number of events
      
        CALL NCAGT( FNUM, NCGLOBAL, 'NTHIK', NTHIK3( FID ), IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 
     &      'Error reading netCDF file attribute NTHIK' )
            EFLAG = .TRUE.
            GO TO  999        !  return
        END IF          !  ierr nonzero:  NCAGT() failed
           
C.......   NCOLS:  number of grid columns/profile levels
      
        CALL NCAGT( FNUM, NCGLOBAL, 'NCOLS', NCOLS3( FID ), IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 
     &      'Error reading netCDF file attribute NCOLS' )
            EFLAG = .TRUE.
            GO TO  999        !  return
        END IF          !  ierr nonzero:  NCAGT() failed
           
C.......   NROWS:  number of grid rows/data sites
      
        CALL NCAGT( FNUM, NCGLOBAL, 'NROWS', NROWS3( FID ), IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 
     &      'Error reading netCDF file attribute NROWS' )
            EFLAG = .TRUE.
            GO TO  999        !  return
        END IF          !  ierr nonzero:  NCAGT() failed
           
C.......   NLAYS:  number of layers
      
        CALL NCAGT( FNUM, NCGLOBAL, 'NLAYS', NLAYS3( FID ), IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 
     &      'Error reading netCDF file attribute NLAYS' )
            EFLAG = .TRUE.
            GO TO  999        !  return
        END IF          !  ierr nonzero:  NCAGT() failed
           
C.......   GDTYP:  grid type ID (lat-lon, UTM, RADM, etc...)
      
        CALL NCAGT( FNUM, NCGLOBAL, 'GDTYP', GDTYP3( FID ), IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 
     &      'Error reading netCDF file attribute GDTYP' )
            EFLAG = .TRUE.
            GO TO  999        !  return
        END IF          !  ierr nonzero:  NCAGT() failed
           
                
C.......   P_ALP:  first map-projection-description angle               
      
        CALL NCAGT( FNUM, NCGLOBAL, 'P_ALP', P_ALP3( FID ), IERR )
        IF ( IERR .NE. 0 ) THEN
            P_ALP3( FID ) = BADVAL3
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 
     &      'WARNING:  Error in netCDF file attribute P_ALP' )
            EFLAG = .TRUE.
            GO TO  999        !  return
        END IF          !  ierr nonzero:  NCAGT() failed
           
C.......   P_BET:  second map-projection-description angle              
      
        CALL NCAGT( FNUM, NCGLOBAL, 'P_BET', P_BET3( FID ), IERR )
        IF ( IERR .NE. 0 ) THEN
            P_BET3( FID ) = BADVAL3
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 
     &      'WARNING:  Error in netCDF file attribute P_BET' )
            EFLAG = .TRUE.
            GO TO  999        !  return
        END IF          !  ierr nonzero:  NCAGT() failed
           
C.......   P_GAM:  third map-projection-description angle               
      
        CALL NCAGT( FNUM, NCGLOBAL, 'P_GAM', P_GAM3( FID ), IERR )
        IF ( IERR .NE. 0 ) THEN
            P_GAM3( FID ) = BADVAL3
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 
     &      'WARNING:  Error in netCDF file attribute P_GAM' )
            EFLAG = .TRUE.
            GO TO  999        !  return
        END IF          !  ierr nonzero:  NCAGT() failed
           
C.......   XCENT:  lon of coordinate-system (0,0) origin
      
        CALL NCAGT( FNUM, NCGLOBAL, 'XCENT', XCENT3( FID ), IERR )
        IF ( IERR .NE. 0 ) THEN
            XCENT3( FID ) = BADVAL3
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 
     &      'WARNING:  Error in netCDF file attribute XCENT' )
            EFLAG = .TRUE.
            GO TO  999        !  return
        END IF          !  ierr nonzero:  NCAGT() failed
           
C.......   YCENT:  lat of coordinate-system (0,0) origin
      
        CALL NCAGT( FNUM, NCGLOBAL, 'YCENT', YCENT3( FID ), IERR )
        IF ( IERR .NE. 0 ) THEN
            YCENT3( FID ) = BADVAL3
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 
     &      'WARNING: Error in netCDF file attribute YCENT' )
           EFLAG = .TRUE.
            GO TO  999        !  return
        END IF          !  ierr nonzero:  NCAGT() failed
           
C.......   XORIG:  X-coord of grid origin
C.......   (in map units; see FDESC3.EXT for description)
      
        CALL NCAGT( FNUM, NCGLOBAL, 'XORIG', XORIG3( FID ), IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 
     &      'Error reading netCDF file attribute XORIG' )
            EFLAG = .TRUE.
            GO TO  999        !  return
        END IF          !  ierr nonzero:  NCAGT() failed
           
C.......   YORIG:  Y-coord of grid origin
C.......   (in map units; see FDESC3.EXT for description)
      
        CALL NCAGT( FNUM, NCGLOBAL, 'YORIG', YORIG3( FID ), IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 
     &      'Error reading netCDF file attribute YORIG' )
            EFLAG = .TRUE.
            GO TO  999        !  return
        END IF          !  ierr nonzero:  NCAGT() failed
           
C.......   XCELL:  cell width (X direction)
C.......   (in map units; see FDESC3.EXT for description)
      
        CALL NCAGT( FNUM, NCGLOBAL, 'XCELL', XCELL3( FID ), IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 
     &      'Error reading netCDF file attribute XCELL' )
            EFLAG = .TRUE.
            GO TO  999        !  return
        END IF          !  ierr nonzero:  NCAGT() failed
           
C.......   YCELL:  cell width (Y direction)
C.......   (in map units; see FDESC3.EXT for description)
      
        CALL NCAGT( FNUM, NCGLOBAL, 'YCELL', YCELL3( FID ), IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 
     &      'Error reading netCDF file attribute YCELL' )
            EFLAG = .TRUE.
            GO TO  999        !  return
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

        CALL NCAGT( FNUM, NCGLOBAL, 'VGLVLS', VGLVS3( 1,FID ), IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 
     &                'Error reading file attribute VGLVS.' )
            CALL M3MSG2( MESG )
            GO TO 999
        END IF          !  ierr nonzero:  NCAGT() failed

C.......   GDNAM:  grid name

        CALL NCAGTC( FNUM, NCGLOBAL, 'GDNAM',
     &               GDNAM3( FID ), NAMLEN3, IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 
     &      'Error reading netCDF file attribute GDNAM' )
            EFLAG = .TRUE.
            GO TO  999        !  return
        END IF          !  ierr nonzero:  NCAGTC) failed


C...........   Get ID for timestep-available-flag variable:

        TINDX3( FID ) = NCVID( FNUM, 'TFLAG', IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 
     &      'Error reading netCDF ID for variable TFLAG' )
            EFLAG = .TRUE.
            GO TO  999        !  return
        END IF              !  ierr nonzero:  NCVDEF() failed
           
C...........   Get extent of the time dimension:
      
        TDIM = NCDID( FNUM, 'TSTEP', IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 
     &      'Error reading netCDF time-dimension ID' )
            EFLAG = .TRUE.
            GO TO  999        !  return
        END IF              !  ierr nonzero:  NCVDEF() failed
           
        CALL NCDINQ( FNUM, TDIM, TNAME, MXREC3( FID ), IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 
     &      'Error reading maximum timestep record number' )
            EFLAG = .TRUE.
            GO TO  999        !  return
        END IF              !  ierr nonzero:  NCVDEF() failed


C...........   Now, get netCDF variable ID's

        BSIZE3( FID ) = NROWS3( FID )
       
        NINDX3( FID ) = NCVID( FNUM, 'KFCOUNT', IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 
     &      'Error reading netCDF ID for variable ID-COUNT' )
            EFLAG = .TRUE.
            GO TO  999        !  return
        END IF              !  ierr nonzero:  NCVID() failed
       
        SINDX3( FID ) = NCVID( FNUM, 'KFEVENT', IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 
     &      'Error reading netCDF variable ID' )
            EFLAG = .TRUE.
            GO TO  999        !  return
        END IF              !  ierr nonzero:  NCVID() failed
     
        LINDX3( FID ) = NCVID( FNUM, 'KFSDATE', IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 
     &      'Error reading netCDF ID for variable LEVEL-COUNT' )
            EFLAG = .TRUE.
            GO TO  999        !  return
        END IF              !  ierr nonzero:  NCVID() failed
      
        XINDX3( FID ) = NCVID( FNUM, 'KFSTIME', IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 
     &      'Error reading netCDF ID for variable KFSDATE' )
            EFLAG = .TRUE.
            GO TO  999        !  return
        END IF              !  ierr nonzero:  NCVID() failed
       
        YINDX3( FID ) = NCVID( FNUM, 'KFLNGTH', IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 
     &      'Error reading netCDF ID for variable KFLNGTH' )
            EFLAG = .TRUE.
            GO TO  999        !  return
        END IF              !  ierr nonzero:  NCVID() failed
      

C.......   Variables-list for the file:

        IF ( NVARS3( FID ) .GT. 0 ) THEN
            CALL NCAGTC( FNUM, NCGLOBAL, 'VAR-LIST', VLIST3( 1,FID ),
     &                   NAMLEN3 * NVARS3( FID ), IERR )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 
     &          'Error reading attribute VAR-LIST' )
                EFLAG = .TRUE.
                GO TO  999        !  return
            END IF              !  ierr nonzero:  NCAGTC() failed
        END IF              !  if nvars > 0

        DO  VAR = 1 , NVARS3( FID )

            VINDX3( VAR,FID ) = NCVID( FNUM, VLIST3( VAR,FID ), IERR )
            IF ( IERR .NE. 0 ) THEN
                MESG = 'Error reading ID for netCDF variable '
     &                  // VLIST3( VAR,FID )
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, MESG )
                EFLAG = .TRUE.
                GO TO  999        !  return
            END IF              !  ierr nonzero:  NCVID() failed

            CALL NCVINQ( FNUM, 
     &                   VINDX3( VAR,FID ), TNAME, 
     &                   VTYPE3( VAR,FID ), ND, D, NA, IERR )
            IF ( IERR .NE. 0 ) THEN
                MESG = 'Error reading type for netCDF variable '
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, MESG )
                EFLAG = .TRUE.
                GO TO  999        !  return
            END IF              !  ierr nonzero:  NCVID() failed

            CALL NCAGTC( FNUM, VINDX3( VAR,FID ), 'units',
     &                   UNITS3( VAR,FID ), NAMLEN3, IERR )
            IF ( IERR .NE. 0 ) THEN
                MESG = 'Error reading units for netCDF variable '
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, MESG )
                EFLAG = .TRUE.
                GO TO  999        !  return
            END IF              !  ierr nonzero:  NCVID() failed

        END DO          !  end loop on variables


C.......   If opened for WRITE:  put attribute HISTORY:  update description

        IF ( IAND( FSTATUS, NF_WRITE ) .NE. 0 ) THEN

            CALL NCREDF( FNUM, IERR )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &          'Error opening history for update' )
                EFLAG = .TRUE.
                GO TO  999        !  return
            END IF          !  ierr nonzero:  NCREDF() failed

            CALL NCAPTC( FNUM, NCGLOBAL, 'UPNAM', NCCHAR,
     &                   NAMLEN3, PGNAME, IERR )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &          'Error writing netCDF file attribute UPNAM' )
                EFLAG = .TRUE.
                GO TO  999        !  return
            END IF          !  ierr nonzero:  NCAPTC() failed

            CALL NCAPTC( FNUM, NCGLOBAL, 'HISTORY', NCCHAR,
     &                   MXDLEN3 * MXDESC3, SCNDSC, IERR )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &          'Error writing netCDF file attribute HISTORY' )
                EFLAG = .TRUE.
                GO TO  999        !  return
            END IF          !  ierr nonzero:  NCAPTC() failed

            CALL NCENDF( FNUM, IERR )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &          'Error flushing history to disk' )
                EFLAG = .TRUE.
                GO TO  999        !  return
            END IF          !  ierr nonzero:  NCENDF() failed

        END IF
        
        DIMS ( 1 ) = 1
        DELTS( 1 ) = NCOLS3( FID )

        DIMS ( 2 ) = 1
        DELTS( 2 ) = NROWS3( FID )

        CALL NCVGT( FNUM, NINDX3( FID ), DIMS, DELTS, BUFFER, IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,
     &      'Error reading variable KFCOUNT' )
            EFLAG = .TRUE.
            GO TO  999        !  return
        END IF          !  ierr nonzero:  NCVGTC() failed, or succeeded
        
        OPNKF = .TRUE.        !  (if you get to here)

999     CONTINUE

!$OMP END CRITICAL( S_NC )

        OPNKF = ( .NOT. EFLAG )

        RETURN
        END FUNCTION  OPNKF

