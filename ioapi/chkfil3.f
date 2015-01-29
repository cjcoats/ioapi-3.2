
        LOGICAL FUNCTION CHKFIL3( FID )

C***********************************************************************
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (C) 2003-2010 by Baron Advanced Meteorological Systems.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  function body starts at line  95
C
C  FUNCTION:  Check consistency between file description in FDESC3
C             structures and STATE3 tables for new version of OPEN3()
C             Report inconsistencies to the log.
C
C  RETURN VALUE:  TRUE iff consistent
C
C  PRECONDITIONS REQUIRED:  call set up correctly by OPEN3()
C
C  SUBROUTINES AND FUNCTIONS CALLED:  TRIMLEN
C
C  REVISION  HISTORY:
C       prototype 9/94 by CJC for new version of OPEN3()
C       Bug fix  10/96 by CJC to *ERR() -- avoid overflow for BADVAL3 args.
C       Modified 2/97 by CJC:  Additional check for legality of variable-names
C       revised  6/99 by CJC:  OpenMP thread-safety
C       revised  8/2000 by CJC:  format-bug at lines 431-432
C       revised 10/2000 by CJC:  weakened the SDATE:STIME checking --
C               SDATE3D:STIME3D must be consistent with the timestep
C               sequence of the file.
C       Modified  2/2004 by CJC for I/O API version 3:  support for
C       native-binary BINFIL3 file type.
C       Modified  8/2007 by CJC:  support same-set/different-order
C       variables-lists.
C       Modified 11/2007 by CJC:  Revert from 8/2007:  same-set/different-order
C       is bogus
C       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
C***********************************************************************

      IMPLICIT NONE

C...........   INCLUDES:

        INCLUDE 'PARMS3.EXT'
        INCLUDE 'STATE3.EXT'
        INCLUDE 'FDESC3.EXT'
        INCLUDE 'NETCDF.EXT'


C...........   ARGUMENTS and their descriptions:

        INTEGER, INTENT(IN   ) :: FID             !  subscript for STATE3 arrays


C...........   EXTERNAL FUNCTIONS and their descriptions:

        LOGICAL, EXTERNAL :: CKNAME
        INTEGER, EXTERNAL :: INDEX1  !  name-table lookup
        INTEGER, EXTERNAL :: JSTEP3  !  record number within a timestep sequence.


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        LOGICAL		OKFLAG
        INTEGER         IREC              !  timestep record number
        INTEGER         IERR              !  netCDF error status return
        INTEGER         VAR, LVL, V, I    !  loop counters
        INTEGER         VGTYP             !  vertical coordinate type
        REAL            VGTOP             !  model-top (sigma only)
        REAL            VGLVLS( MXLAYS3 ) !  vertical level values

C.......   Statement functions for evaluating relative error: TRUE
C.......   iff P,Q (X,Y) are significantly different (compares
C.......   normalized difference against tolerance)

        LOGICAL         DBLERR, FLTERR
        REAL*8          P, Q
        REAL            X, Y

        DBLERR( P,Q ) = 
     &  ( P .GT. AMISS3 ) .AND. ( Q .GT. AMISS3 ) .AND.
     &  ( ( DABS( DBLE( P - Q ) ) 
     &    / DSQRT( DBLE( P*P + Q*Q + 1.0D-5 ) ) ) .GT. 1.0D-6 )
        
        FLTERR( X,Y ) = 
     &  ( X .GT. AMISS3 ) .AND. ( Y .GT. AMISS3 ) .AND.
     &  ( ( ABS( X - Y ) / SQRT( X*X + Y*Y + 1.0E-5 ) ) .GT. 1.0E-5 )
        
C.............................................................................
C   begin body of subroutine  CHKFIL3

!$OMP CRITICAL( S_LOGOUT )

        OKFLAG = .TRUE.

C...........   Check attributes valid for all file types:
C...........   FTYPE:  file type ID

        IF ( FTYPE3D .NE. FTYPE3( FID ) ) THEN
            WRITE( LOGDEV,91020 )
     &          'Inconsistent file attribute FTYPE for file ' 
     &          // FLIST3( FID ),
     &          'Value from file:  ', FTYPE3( FID ),
     &          'Value from caller:', FTYPE3D
            OKFLAG = .FALSE.
        END IF

C.......   NVARS:  number of variables

        IF ( NVARS3D .NE. NVARS3( FID ) ) THEN
            WRITE( LOGDEV,91020 )
     &          'Inconsistent file attribute NVARS for file ' 
     &          // FLIST3( FID ),
     &          'Value from file:  ', NVARS3( FID ),
     &          'Value from caller:', NVARS3D
            OKFLAG = .FALSE.
        END IF

C.......   This completes checks for dictionary files.
            
        IF ( FTYPE3( FID ) .EQ. DCTNRY3 ) THEN
            OKFLAG = .TRUE.
            GO TO  999
        END IF    
     

C.......   Continue checks for data files -- TSTEP:  time step

        IF ( TSTEP3D .NE. TSTEP3( FID ) ) THEN
                WRITE( LOGDEV,91020 )
     &          'Inconsistent file attribute TSTEP for file ' 
     &          // FLIST3( FID ),
     &          'Value from file:  ', TSTEP3( FID ),
     &          'Value from caller:', TSTEP3D
            OKFLAG = .FALSE.
        END IF
        
C.......   SDATE:  starting date (Julian date YYYYDDD)
C.......   STIME:  starting time (HHMMSS)

        IF ( TSTEP3D .NE. 0 ) THEN
            IREC = JSTEP3( SDATE3D,       STIME3D, 
     &                     SDATE3( FID ), STIME3( FID ), TSTEP3D )
            IF ( IREC .LT. 0 ) THEN
                WRITE( LOGDEV, 91021 )
     &          'Inconsistent SDATE:STIME for file ' // FLIST3( FID ),
     &          'Value from file:  ', SDATE3( FID ), ':', STIME3( FID ),
     &          'Value from caller:', SDATE3D, ':', STIME3D
            OKFLAG = .FALSE.
            END IF
        END IF
      
C.......   NTHIK:  perimeter thickness (cells; boundary files only)
      
        IF( FTYPE3D .EQ. BNDARY3 ) THEN
            IF ( NTHIK3D .NE. NTHIK3( FID ) ) THEN
                WRITE( LOGDEV,91020 )
     &              'Inconsistent file attribute NTHIK for file '
     &              // FLIST3( FID ),
     &              'Value from file:  ', NTHIK3( FID ),
     &              'Value from caller:', NTHIK3D
                OKFLAG = .FALSE.
            END IF
        END IF
      
C.......   NCOLS:  number of grid columns/profile levels (not used for IDDATA)
      
        IF ( FTYPE3D .NE. IDDATA3 ) THEN
            IF ( NCOLS3D .NE. NCOLS3( FID ) ) THEN
                WRITE( LOGDEV,91020 )
     &              'Inconsistent file attribute NCOLS for file ' 
     &              // FLIST3( FID ),
     &              'Value from file:  ', NCOLS3( FID ),
     &              'Value from caller:', NCOLS3D
                OKFLAG = .FALSE.
            END IF
        END IF
      
C.......   NROWS:  number of grid rows/data sites.  Not used for CUSTOM.
      
        IF ( FTYPE3D .NE. CUSTOM3 ) THEN
            IF ( NROWS3D .NE. NROWS3( FID ) ) THEN
                WRITE( LOGDEV,91020 )
     &              'Inconsistent file attribute NROWS for file ' 
     &              // FLIST3( FID ),
     &              'Value from file:  ', NROWS3( FID ),
     &              'Value from caller:', NROWS3D
                OKFLAG = .FALSE.
            END IF
        END IF
      
C.......   NLAYS:  number of layers
      
        IF ( NLAYS3D .NE. NLAYS3( FID ) ) THEN
            WRITE( LOGDEV,91020 )
     &          'Inconsistent file attribute NLAYS for file ' 
     &          // FLIST3( FID ),
     &          'Value from file:  ', NLAYS3( FID ),
     &          'Value from caller:', NLAYS3D
            OKFLAG = .FALSE.
        END IF
      
C.......   GDTYP:  grid type ID (lat-lon, UTM, RADM, etc...)
      
        IF ( GDTYP3D .NE. GDTYP3( FID ) ) THEN
            WRITE( LOGDEV,91020 )
     &          'Inconsistent file attribute GDTYP for file ' 
     &          // FLIST3( FID ),
     &          'Value from file:  ', GDTYP3( FID ),
     &          'Value from caller:', GDTYP3D
            OKFLAG = .FALSE.
        END IF
      
C.......   P_ALP:  first map-projection-description angle               
      
        IF ( DBLERR( P_ALP3D , P_ALP3( FID ) ) ) THEN
            WRITE( LOGDEV,91030 )
     &          'Inconsistent file attribute P_ALP for file ' 
     &          // FLIST3( FID ),
     &          'Value from file:  ', P_ALP3( FID ),
     &          'Value from caller:', P_ALP3D
            OKFLAG = .FALSE.
        END IF
      
C.......   P_BET:  second map-projection-description angle              
      
        IF ( DBLERR( P_BET3D , P_BET3( FID ) ) ) THEN
            WRITE( LOGDEV,91030 )
     &          'Inconsistent file attribute P_BET for file ' 
     &          // FLIST3( FID ),
     &          'Value from file:  ', P_BET3( FID ),
     &          'Value from caller:', P_BET3D
            OKFLAG = .FALSE.
        END IF
      
C.......   P_GAM:  third map-projection-description angle               
      
        IF ( DBLERR( P_GAM3D , P_GAM3( FID ) ) ) THEN
            WRITE( LOGDEV,91030 )
     &          'Inconsistent file attribute P_GAM for file ' 
     &          // FLIST3( FID ),
     &          'Value from file:  ', P_GAM3( FID ),
     &          'Value from caller:', P_GAM3D
            OKFLAG = .FALSE.
        END IF
      
C.......   XCENT:  lon of coordinate-system (0,0) origin
      
        IF ( DBLERR( XCENT3D , XCENT3( FID ) ) ) THEN
            WRITE( LOGDEV,91030 )
     &          'Inconsistent file attribute XCENT for file ' 
     &          // FLIST3( FID ),
     &          'Value from file:  ', XCENT3( FID ),
     &          'Value from caller:', XCENT3D
            OKFLAG = .FALSE.
        END IF
      
C.......   YCENT:  lat of coordinate-system (0,0) origin
      
        IF ( DBLERR( YCENT3D , YCENT3( FID ) ) ) THEN
            WRITE( LOGDEV,91030 )
     &          'Inconsistent file attribute YCENT for file ' 
     &          // FLIST3( FID ),
     &          'Value from file:  ', YCENT3( FID ),
     &          'Value from caller:', YCENT3D
            OKFLAG = .FALSE.
        END IF
      
C.......   XORIG:  X-coord of grid origin
C.......   (in map units; see FDESC3.EXT for description)
      
        IF ( DBLERR( XORIG3D , XORIG3( FID ) ) ) THEN
            WRITE( LOGDEV,91030 )
     &          'Inconsistent file attribute XORIG for file ' 
     &          // FLIST3( FID ),
     &          'Value from file:  ', XORIG3( FID ),
     &          'Value from caller:', XORIG3D
            OKFLAG = .FALSE.
        END IF
      
C.......   YORIG:  Y-coord of grid origin
C.......   (in map units; see FDESC3.EXT for description)
      
        IF ( DBLERR( YORIG3D , YORIG3( FID ) ) ) THEN
            WRITE( LOGDEV,91030 )
     &          'Inconsistent file attribute YORIG for file ' 
     &          // FLIST3( FID ),
     &          'Value from file:  ', YORIG3( FID ),
     &          'Value from caller:', YORIG3D
            OKFLAG = .FALSE.
        END IF
      
C.......   XCELL:  cell width (X direction)
C.......   (in map units; see FDESC3.EXT for description)
      
        IF ( DBLERR( XCELL3D , XCELL3( FID ) ) ) THEN
            WRITE( LOGDEV,91030 )
     &          'Inconsistent file attribute XCELL for file ' 
     &          // FLIST3( FID ),
     &          'Value from file:  ', XCELL3( FID ),
     &          'Value from caller:', XCELL3D
            OKFLAG = .FALSE.
        END IF
      
C.......   YCELL:  cell width (Y direction)
C.......   (in map units; see FDESC3.EXT for description)
      
        IF ( DBLERR( YCELL3D , YCELL3( FID ) ) ) THEN
            WRITE( LOGDEV,91030 )
     &          'Inconsistent file attribute YCELL for file ' 
     &          // FLIST3( FID ),
     &          'Value from file:  ', YCELL3( FID ),
     &          'Value from caller:', YCELL3D
            OKFLAG = .FALSE.
        END IF
         
C.......   VGTYP:  grid type ID (lat-lon, UTM, RADM, etc...)
C...........   (Not stored for BUFFERED or VIRTUAL files)
C...........   VG attributes may not be present for old-version files.

        IF ( CDFID3( FID ) .NE. BUFFIL3  .AND.
     &       CDFID3( FID ) .NE. VIRFIL3  .AND.
     &       CDFID3( FID ) .NE. BINFIL3 ) THEN

            IF ( NLAYS3D .GT. 1 ) THEN
            
                CALL NCAGT( CDFID3( FID ), NCGLOBAL, 'VGTYP', 
     &                      VGTYP, IERR )
                
                IF ( IERR .EQ. 0  .AND. VGTYP3D .NE. VGTYP ) THEN
                    WRITE( LOGDEV,91020 )
     &                  'Inconsistent file attribute VGTYP for file ' 
     &                  // FLIST3( FID ),
     &                  'Value from file:  ', VGTYP,
     &                  'Value from caller:', VGTYP3D
                    OKFLAG = .FALSE.
                END IF
         
                CALL NCAGT( CDFID3( FID ), NCGLOBAL, 'VGTOP', 
     &                      VGTOP, IERR )
                
                IF ( FLTERR( VGTOP3D, VGTOP ) ) THEN
                    WRITE( LOGDEV,91040 )
     &                  'Inconsistent file attribute VGTOP for file ' 
     &                  // FLIST3( FID ),
     &                  'Value from file:  ', VGTOP,
     &                  'Value from caller:', VGTOP3D
                    OKFLAG = .FALSE.
                END IF
         
C...........   VGLVS( 1, ..., NLAYS3D+1 ):  vertical coordinate values
        
                CALL NCAGT( CDFID3( FID ), NCGLOBAL, 'VGLVLS', 
     &                      VGLVLS, IERR )
            
                IF ( IERR .EQ. 0 ) THEN
            
                    DO  100 LVL = 1, MIN( NLAYS3D+1, MXLAYS3 )
            
                        IF ( FLTERR( VGLVS3D( LVL ), 
     &                               VGLVLS ( LVL ) ) ) THEN
            
                            WRITE( LOGDEV,91040 )
     &                      'Inconsistent attribute VGLVLS for file '
     &                      // FLIST3( FID ),
     &                      'Value from file:  ', VGLVLS ( LVL ),
     &                      'Value from caller:', VGLVS3D( LVL ),
     &                      'Error at level    ', LVL
                            OKFLAG = .FALSE.
            
                        END IF  !  if flterr()
            
100                 CONTINUE    !  end loop on LVL
            
                END IF  !  if NCAGT() succeeded (upward comatibility check)
            
            END IF              !  if #( layers ) > 1
            
        END IF          !  if not BUFFERED or VIRTUAL file

C.......   GDNAM:  grid name

        IF ( GDNAM3D .NE. GDNAM3( FID ) ) THEN
            WRITE( LOGDEV,91010 )
     &          'Inconsistent grid name for file '// FLIST3( FID ),
     &          'Value from file:  ' // GDNAM3( FID ) ,
     &          'Value from caller:' // GDNAM3D
            OKFLAG = .FALSE.
        END IF
      
           
C.......   Variables-list for the file:
      
        DO 200  V = 1 , NVARS3( FID )

            IF ( .NOT. CKNAME( VNAME3D( V ) ) ) THEN
                OKFLAG = .FALSE.
                WRITE( LOGDEV,91010 )
     &          'Illegal variable name "' // VNAME3D( V ) // 
     &          '" in file ' // FLIST3( FID )
            ELSE IF ( VNAME3D(V) .NE. VLIST3(V,FID) ) THEN
                OKFLAG = .FALSE.
                WRITE( LOGDEV,91010 )
     &              'Inconsistent variable list for file '//FLIST3(FID),
     &              'Value from file:   '// VLIST3(V,FID),
     &              'Value from caller: '// VNAME3D( V )
            ELSE IF ( VTYPE3( V,FID ) .NE. VTYPE3D( V ) ) THEN
                OKFLAG = .FALSE.
                WRITE( LOGDEV,91020 )
     &              'Inconsistent type for variable ' // VNAME3D( V )
     &              // ' from file ' // FLIST3( FID ),
     &              'Value from file:  ', VTYPE3( V,FID ),
     &              'Value from caller:', VTYPE3D( V )
            END IF

200     CONTINUE
           
999     CONTINUE
           
        CHKFIL3 = OKFLAG

!$OMP END CRITICAL( S_LOGOUT )

C.......   Control only falls through to here if everything succeeds:

        RETURN
     
C******************  FORMAT  STATEMENTS   ******************************

C...........   Error and warning message formats..... 91xxx

91010   FORMAT ( //5X , '>>> WARNING in subroutine CHKFIL3 <<<',
     &            3 ( /5X , A , : ) , I5, // )

91020   FORMAT ( //5X , '>>> WARNING in subroutine CHKFIL3 <<<',
     &            /5X , A, 
     &            2 ( /5X , A , I9, : ) )

91021   FORMAT ( //5X , '>>> WARNING in subroutine CHKFIL3 <<<',
     &            /5X , A, 
     &            2 ( /5X , A , I9, A, I6.6, : ) )

91030   FORMAT ( //5X , '>>> WARNING in subroutine CHKFIL3 <<<',
     &            /5X , A, 
     &        2 ( /5X , A , 1PD24.17 , : ) )

91040   FORMAT ( //5X , '>>> WARNING in subroutine CHKFIL3 <<<',
     &            /5X , A, 
     &        2 ( /5X , A , 1PE12.5 , : ),
     &            /5X , A , I9 )

        END FUNCTION CHKFIL3

 
