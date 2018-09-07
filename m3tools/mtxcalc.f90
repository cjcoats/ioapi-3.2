
PROGRAM MTXCALC

    !!***********************************************************************
    !! Version "$Id: mtxcalc.f90 108 2018-09-07 18:59:37Z coats $"
    !! EDSS/Models-3 M3TOOLS.
    !! Copyright (C) 1992-2002 MCNC,
    !! (C) 1995-2002, 2005-2013 Carlie J. Coats, Jr.,
    !! (C) 2002-2010 Baron Advanced Meteorological Systems. LLC., and
    !! (C) 2015 UNC Institute for the Environment.
    !! Distributed under the GNU GENERAL PUBLIC LICENSE version 2
    !! See file "GPL.txt" for conditions of use.
    !!.........................................................................
    !!  program body starts at line  156
    !!
    !!  DESCRIPTION:
    !!      Take GRIDDESC names for input and output grids, and produce
    !!      a sparse matrix file that has the transform matrix to convert
    !!      data from input to output, based on input-area fractions.
    !!
    !!  PRECONDITIONS REQUIRED:
    !!      setenv <logical names>  <path-names>
    !!      input and output grids have same coordinate system
    !!      output grid is entirely contained in the input grid
    !!
    !!  SUBROUTINES AND FUNCTIONS CALLED:
    !!      I/O API
    !!
    !!  REVISION  HISTORY:
    !!      Prototype 11/2000 by Carlie J. Coats, Jr., NCSC
    !!      Revised   11/2001 by CJC for I/O API Version 2.1
    !!      Revised   10/2005 by CJC for I/O API Version 2.1
    !!      Version   10/2002 by CJC for I/O API Version 2.2:  support for
    !!      many additional coordinate transformations, direct calls to GCTP
    !!      Version   10/2005 by CJC:  handle negative XCELL, YCELL
    !!      Version   11/2005 by CJC:  eliminate unused vbles
    !!      Version    1/2006 by CJC:  Bug-fix to off-by-one-half error
    !!      reported by Dr. Michael Bane, UManchester, UK, at approx
    !!      Lines 732-2
    !!      Version   12/2006 by CJC:  Bug-fix to GTYPES menu
    !!      Version    6/2008 by CJC:  Albers support
    !!      Version    8/2008 by CJC:  USE M3UTILIO, MODATTS3 to put
    !!      grid-attributes into matrix.
    !!      Version   12/2008 by CJC:  heuristic to compensate for WMO screw-up
    !!      that declares all longitudes should be positive
    !!      Version 02/2010 by CJC for I/O API v3.1:  Fortran-90 only;
    !!      USE M3UTILIO, and related changes.
    !!      Version 12/2014 by CJC for I/O API v3.2:  USE MODGCTP::GRID2XY(),
    !!      USE MODATTS3::SETMTXATT()
    !!      Version  02/2015 by CJC for I/O API v3.2:  F90 free-format source,
    !!      in-this-code (but external) BLDMATRIX; new environment variable
    !!      SCALEFAC; USE routine GRID2XY from MODULE MODGCTP; USE routine
    !!      SETMTXATT from MODULE MODATTS3; use generics for "GET*()", "ENV*()"
    !!      Version  02/2016 by CJC:  bug-fix at lines 411&ff"  XLOC- XORFIG etc.
    !!      Version  03/2018 by CJC:  Need to calculate NUMERX, NUMERY, 
    !!      DENOMX, DENOMY
    !!***********************************************************************

    USE M3UTILIO
    USE MODATTS3
    USE MODGCTP

    IMPLICIT NONE

    !!...........   PARAMETERS and their descriptions:

    CHARACTER*24, PARAMETER :: GTYPES( 0:10 ) = &
          (/ 'Unknown/Invalid       ',          &
             'Latitude-Longitude    ',          &
             'Lambert               ',          &
             'General Mercator      ',          &
             'General Stereographic ',          &
             'UTM                   ',          &
             'Polar Stereographic   ',          &
             'Equatorial Mercator   ',          &
             'Transverse Mercator   ',          &
             'Albers Equal-Area     ',          &
             'Unknown/Invalid       ' /)

    REAL*8, PARAMETER :: PI     = 3.141592653589793238462643383279d0
    REAL*8, PARAMETER :: PI180  = PI / 180.0d0
    REAL*8, PARAMETER :: REARTH = 6367333.0d0
    REAL*8, PARAMETER :: DG2M   = REARTH * PI180

    CHARACTER*16, PARAMETER :: PNAME = 'MTXCALC'
    CHARACTER*72, PARAMETER :: BAR   =  &
    '-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-'

    !!...........   EXTERNAL FUNCTIONS and their descriptions:

    INTEGER :: IARGC

    !!...........   LOCAL VARIABLES and their descriptions:

    INTEGER         LOGDEV  !  unit number for log file
    INTEGER         ARGCNT  !  number of command-line args, from IARGC()
    INTEGER         STATUS
    INTEGER         MATDEV  ! optional ASCII output file

    CHARACTER*16    IGRID   !  GRIDDESC name, parameters for  input grid
    INTEGER         NCOLS1
    INTEGER         NROWS1
    INTEGER         NTHIK1
    INTEGER         GDTYP1
    REAL*8          P_ALP1      ! first, second, third map
    REAL*8          P_BET1      ! projection descriptive
    REAL*8          P_GAM1      ! parameters.
    REAL*8          XCENT1      ! lon for coord-system X=0
    REAL*8          YCENT1      ! lat for coord-system Y=0
    REAL*8          XORIG1      ! X-coordinate origin of grid (map units)
    REAL*8          YORIG1      ! Y-coordinate origin of grid
    REAL*8          XCELL1      ! X-coordinate cell dimension
    REAL*8          YCELL1      ! Y-coordinate cell dimension

    INTEGER         NCOLS3      ! for refined grud
    INTEGER         NROWS3
    REAL*8          XCELL3
    REAL*8          YCELL3

    CHARACTER*16    OGRID   !  GRIDDESC name, parameter for output grid
    INTEGER         NCOLS2
    INTEGER         NROWS2
    INTEGER         NGRID2
    INTEGER         NTHIK2
    INTEGER         GDTYP2
    REAL*8          P_ALP2      ! first, second, third map
    REAL*8          P_BET2      ! projection descriptive
    REAL*8          P_GAM2      ! parameters.
    REAL*8          XCENT2      ! lon for coord-system X=0
    REAL*8          YCENT2      ! lat for coord-system Y=0
    REAL*8          XORIG2      ! X-coordinate origin of grid (map units)
    REAL*8          YORIG2      ! Y-coordinate origin of grid
    REAL*8          XCELL2      ! X-coordinate cell dimension
    REAL*8          YCELL2      ! Y-coordinate cell dimension
    REAL*8          DDX2        ! 1/xcell
    REAL*8          DDY2        ! 1/ycell

    INTEGER         NX, NY, NFRACS
    INTEGER         I, J, K, L, N, C, R, CC, RR
    INTEGER         COL, ROW
    INTEGER         COLMAX, ROWMAX
    INTEGER         COLMIN, ROWMIN
    INTEGER         ISTAT
    LOGICAL         EFLAG
    CHARACTER*16    SCRBUF
    CHARACTER*256   MESG

    REAL            SCALEFAC
    REAL            DDXY
    REAL            NUMERX, DENOMX      !  worst-case XCELL1,2 in meters
    REAL            NUMERY, DENOMY      !  worst-case YCELL1,2 in meters
    REAL*8          DX, DY
    REAL*8          X0, Y0, X, Y, XADJ

    INTEGER, ALLOCATABLE :: ISCR( :, : )
    INTEGER, ALLOCATABLE :: ICEL( : )
    INTEGER, ALLOCATABLE :: OCEL( : )
    REAL,    ALLOCATABLE :: FRAC( : )
    REAL,    ALLOCATABLE :: MATX( : )
    REAL*8,  ALLOCATABLE :: XLOC( :,: )
    REAL*8,  ALLOCATABLE :: YLOC( :,: )

    !!***********************************************************************
    !!   begin body of program MTXCALC

    LOGDEV = INIT3()
    ARGCNT = IARGC()
    EFLAG = .FALSE.
    WRITE( *, '( 5X, A )' ) ' ', BAR, ' ',                                      &
'Program MTXCALC to produce a sparse grid-to-grid transform matrix for',        &
'specified input and output grids, and write the result to a sparse-matrix',    &
'file.  The algorithm employed is to sub-sample each input cell using a',       &
'subsampled grid of size COL_REFINEMENT by ROW_REFINEMENT in each input',       &
'grid cell, where COL_REFINEMENT and ROW_REFINEMENT are environment',           &
'variables set by the user (with defaults to a 100 by 100 refinement,',         &
'in case they are not set).',                                                   &
'',                                                                             &
'Usage:  MTXCALC [INGRID OUTGRID] <and follow the prompts>',                    &
'',                                                                             &
'where <INGRID> and <OUTGRID> are the GRIDDESC names of the input and',         &
'output grids for the transformation.  If these are not entered at the',        &
'command line,the program will prompt you for them.',                           &
'',                                                                             &
'PRECONDITIONS REQUIRED:',                                                      &
'',                                                                             &
'    setenv COL_REFINEMENT  <col-sampling factor> (default 100)',               &
'    setenv ROW_REFINEMENT  <row-sampling factor> (default 100)',               &
'    setenv SCALEFAC        <scale factor> [default 1.0]; may be',              &
'                            used to fix-up units, etc.',                       &
'    setenv GRIDDESC        <path name for grid descriptions file>',            &
'    setenv MATRIX          <path name for I/O API matrix file>',               &
'    setenv MATTXT          <path name for  ASCII  matrix file,',               &
'                            or "NONE" to suppress ASCII output>',              &
'',                                                                             &
'The following input and output coordinate systems are supported:',             &
'',                                                                             &
'    Latitude-Longitude    ',                                                   &
'    Lambert Conformal Conic',                                                  &
'    UTM                   ',                                                   &
'    Polar Stereographic   ',                                                   &
'    Transverse Mercator   ',                                                   &
'    Equatorial Mercator   ',                                                   &
'    Albers Equal-Area Conic',                                                  &
'',                                                                             &
'See URL',                                                                      &
'',                                                                             &
'    https://www.cmascenter.org/ioapi/documentation/3.1/html#tools',            &
' ',                                                                            &
'Program copyright (C) 1992-2002 MCNC, (C) 1995-2013',                          &
'Carlie J. Coats, Jr., (C) 2002-2010 Baron Advanced',                           &
'Meteorological Systems, LLC., and (C) 2015 UNC Institute',                     &
'for the Environment.  Released under Version 2 of the',                        &
'GNU General Public License. See enclosed GPL.txt,',                            &
'or URL',                                                                       &
'',                                                                             &
'    https://www.gnu.org/licenses/old-licenses/gpl-2.0.html',                   &
'',                                                                             &
'Comments and questions are welcome and can be sent to',                        &
'',                                                                             &
'    Carlie J. Coats, Jr.    carlie@jyarborough.com',                           &
'or',                                                                           &
'    UNC Institute for the Environment',                                        &
'    100 Europa Dr., Suite 490 Rm 405',                                         &
'    Campus Box 1105',                                                          &
'    Chapel Hill, NC 27599-1105',                                               &
'',                                                                             &
'',                                                                             &
'Program version: ',                                                            &
'$Id: mtxcalc.f90 108 2018-09-07 18:59:37Z coats $',&
' '

    IF ( .NOT. GETVAL( 'Continue with program?', .TRUE. ) ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'Program terminated at user request', 2 )
    ELSE IF ( ARGCNT .EQ. 2 ) THEN
        CALL GETARG( 1, IGRID )
        CALL GETARG( 2, OGRID )
    ELSE IF ( ARGCNT .EQ. 0 ) THEN
        CALL GETSTR( 'Enter name for  input grid', 'IGRID', IGRID )
        CALL GETSTR( 'Enter name for output grid', 'OGRID', OGRID )
    ELSE
        MESG= 'USAGE ERROR:  incorrect number of arguments'
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
    END IF

    MATDEV = PROMPTFFILE( 'Enter ASCII MATRIX file name, or "NONE"', .FALSE., .TRUE., 'MATTXT', PNAME )
    IF ( MATDEV .EQ. -1 ) THEN
        MESG= 'ERROR opening ASCII matrix file'
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
    ELSE IF ( MATDEV .GT. 0 ) THEN
        WRITE( MATDEV, '(A)' ) '# Fractions for sparse transform matrix'
        WRITE( MATDEV, '(A)' ) '# FRAC=Area(INCELL OUTCELL)/Area(INCELL)'
        WRITE( MATDEV, '(A)' ) '# INGRID    ' // IGRID
        WRITE( MATDEV, '(A)' ) '# OUTGRID   ' // OGRID
    END IF


    !!.......   Read the input and output grid parameters from GRIDDESC:

    IF ( .NOT. DSCGRID( IGRID, SCRBUF, GDTYP1,                  &
                        P_ALP1, P_BET1,P_GAM1, XCENT1, YCENT1,  &
                        XORIG1, YORIG1, XCELL1, YCELL1,         &
                        NCOLS1, NROWS1, NTHIK1 ) ) THEN

        MESG   = 'Grid "' // TRIM( IGRID ) // '" not found in GRIDDESC file'
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )

    END IF  ! if dscgrid() failed for input grid

    I = MAX( 0, MIN( GDTYP1, 9 ) )
    MESG = 'Input grid:  "' // TRIM( IGRID ) // '" is of type "' // TRIM( GTYPES( I ) ) // '"'
    CALL M3MESG( MESG )
    WRITE( MESG, '( A, 2X, 3(   F15.7, 2X ) )' ) 'Defining angles  ', P_ALP1, P_BET1,P_GAM1
    CALL M3MESG( MESG )
    WRITE( MESG, '( A, 2X, 2(   F15.7, 2X ) )' ) 'Coordinate Origin', XCENT1, YCENT1
    CALL M3MESG( MESG )
    WRITE( MESG, '( A, 2X, 2( 1PE15.7, 2X ) )' ) 'SW Grid Corner   ', XORIG1, YORIG1
    CALL M3MESG( MESG )
    WRITE( MESG, '( A, 2X, 2( 1PE15.7, 2X ) )' ) 'NE Grid Corner   ', XORIG1+DBLE(NCOLS1)*XCELL1, YORIG1+DBLE(NROWS1)*YCELL1
    CALL M3MESG( MESG )
    WRITE( MESG, '( A, 2X, 2( 1PE15.7, 2X ) )' ) 'Cell Size        ', XCELL1, YCELL1
    CALL M3MESG( MESG )
    WRITE( MESG, '( A, 2X, 2( I6, 2X, A ) )'   ) 'Dimensions:      ', NCOLS1, 'cols', NROWS1, 'rows'
    CALL M3MESG( MESG )

    IF ( .NOT. DSCGRID( OGRID, SCRBUF, GDTYP2,                    &
                        P_ALP2, P_BET2,P_GAM2, XCENT2, YCENT2,    &
                        XORIG2, YORIG2, XCELL2, YCELL2,           &
                        NCOLS2, NROWS2, NTHIK2 ) ) THEN

        MESG   = 'Grid "' // TRIM( OGRID ) // '" not found in GRIDDESC file'
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )

    END IF                  ! if dscgrid() failed for output grid
    
    NGRID2 = NCOLS2 * NROWS2

    CALL M3MESG( ' ' )
    I = MAX( 0, MIN( GDTYP2, 9 ) )
    MESG = 'Output grid:  "' // TRIM( OGRID ) // '" is of type "' // TRIM( GTYPES( I ) ) // '"'
    CALL M3MESG( MESG )
    WRITE( MESG, '( A, 2X, 3(   F15.7, 2X ) )' ) 'Defining angles  ', P_ALP2, P_BET2,P_GAM2
    CALL M3MESG( MESG )
    WRITE( MESG, '( A, 2X, 2(   F15.7, 2X ) )' ) 'Coordinate Origin', XCENT2, YCENT2
    CALL M3MESG( MESG )
    WRITE( MESG, '( A, 2X, 2( 1PE15.7, 2X ) )' ) 'SW Grid Corner   ', XORIG2, YORIG2
    CALL M3MESG( MESG )
    WRITE( MESG, '( A, 2X, 2( 1PE15.7, 2X ) )' ) 'NE Grid Corner   ', XORIG2+DBLE(NCOLS2)*XCELL2, YORIG2+DBLE(NROWS2)*YCELL2
    CALL M3MESG( MESG )
    WRITE( MESG, '( A, 2X, 2( 1PE15.7, 2X ) )' ) 'Cell Size        ', XCELL2, YCELL2
    CALL M3MESG( MESG )
    WRITE( MESG, '( A, 2X, 2( I6, 2X, A ) )'   ) 'Dimensions:      ', NCOLS2, 'cols', NROWS2, 'rows'
    CALL M3MESG( MESG )
    CALL M3MESG( ' ' )


    !!.......   Compute longitude adjustment to compensate for WMO violation
    !!.......   of ISO Standard 6709 ("-180 < LON <= 180"), where WMO
    !!.......   declares longitude range 0 <= LON < 360:

    XADJ = 0.0D0
    IF       ( GDTYP1.EQ.LATGRD3 .AND. XORIG1.GT.0.0D0 ) THEN           !  WMO LL input
        IF ( ( GDTYP2.EQ.LATGRD3 .AND. XORIG2.LT.0.0D0 ) .OR.       &
             ( GDTYP2.GT.0       .AND. XCENT2.LT.0.0D0 ) ) THEN
            XADJ = 360.0D0
            CALL M3MESG( 'Longitude adjustment:  360.0D0' )
        END IF
    ELSE IF  ( GDTYP2 .EQ. LATGRD3 .AND. XORIG2 .GT. 0.0D0 ) THEN       !  WMO LL output
        IF ( ( GDTYP1 .EQ. LATGRD3 .AND. XORIG1 .LT. 0.0D0 ) .OR.    &
             ( GDTYP1 .GT. 0       .AND. XCENT1 .LT. 0.0D0 ) ) THEN
            XADJ = -360.0D0
            CALL M3MESG( 'Longitude adjustment: -360.0D0' )
        END IF
    END IF

!!.......   Gewt environment; compute sampling ratios:

    IF ( XCELL1 .LE. XCELL2 ) THEN
        C = 100
    ELSE
        C = 100 * ( 1 + NINT( XCELL2 / XCELL1 ) )
    END IF

    IF ( YCELL1 .LE. YCELL2 ) THEN
        R = 100
    ELSE
        R = 100 * ( 1 + NINT( YCELL2 / YCELL1 ) )
    END IF
    NX = ENVGET( 'COL_REFINEMENT', 'column-factor', C, STATUS )
    IF ( STATUS .GT. 0 ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'ERROR:  Bad environment variable "COL_REFINEMENT"', 2 )
    END IF

    NY = ENVGET( 'ROW_REFINEMENT','row-factor', R, STATUS )
    IF ( STATUS .GT. 0 ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'ERROR:  Bad environment variable "ROW_REFINEMENT"', 2 )
    END IF

    SCALEFAC = ENVGET( 'SCALEFAC', 'Scale factor for transform matrix', 1.0, STATUS )
    IF ( STATUS .GT. 0 ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'ERROR:  Bad environment variable "SCALEFAC"', 2 )
    END IF

    !!........  Sampling-grid parameters:

    NCOLS3 = NCOLS1*NX
    NROWS3 = NROWS1*NY
    XCELL3 = XCELL1 / DBLE( NX )
    YCELL3 = YCELL1 / DBLE( NY )

    CALL M3MESG( ' ' )
    WRITE( MESG, '( A, 2( I8, 2X, A ) )' ) 'Sampling ratios:', NX, 'per input column', NY, 'per input row'
    CALL M3MESG( MESG )
    IF ( GDTYP1 .EQ. LATGRD3 ) THEN
        WRITE( MESG, '( A, 2( 1PD15.7, 2X, A ) )' ) 'Sampling cellsize:', XCELL3, 'by', YCELL3, 'degrees'
    ELSE
        WRITE( MESG, '( A, 2( 1PD15.7, 2X, A ) )' ) 'Sampling cellsize:', XCELL3, 'by', YCELL3, 'meters'
    END IF
    CALL M3MESG( MESG )

    !!.......   Allocate scratch buffers:

    IF ( GDTYP1 .EQ. LATGRD3 ) THEN
        Y      = YORIG1 + 0.5D0*DBLE( NCOLS1 )*YCELL1
        DENOMX = SNGL( DG2M * XCELL1 * COS( Y * PI180 ) )
        DENOMY = SNGL( DG2M * YCELL1 )
    ELSE
        DENOMX = SNGL( XCELL1 )
        DENOMY = SNGL( YCELL1 )
    END IF
    IF ( GDTYP2 .EQ. LATGRD3 ) THEN
        Y      = YORIG2 + 0.5D0*DBLE( NCOLS2 )*YCELL2
        NUMERX = SNGL( DG2M * XCELL2 * COS( Y * PI180 ) )
        NUMERY = SNGL( DG2M * YCELL2 )
    ELSE
        NUMERX = SNGL( XCELL2 )
        NUMERY = SNGL( YCELL2 )
    END IF

    NFRACS = ( 3 + 3*CEILING( ABS( NUMERX / DENOMX ) ) )*               &
             ( 3 + 3*CEILING( ABS( NUMERY / DENOMY ) ) )*               &
             NGRID2

    CALL M3MESG( MESG )

    ALLOCATE( ISCR( NCOLS2, NROWS2 ),       &
              ICEL( NFRACS ),               &
              OCEL( NFRACS ),               &
              FRAC( NFRACS ),               &
              XLOC( NCOLS3, NROWS3 ),       &
              YLOC( NCOLS3, NROWS3 ), STAT = ISTAT )

    IF ( ISTAT .NE. 0 ) THEN
        MESG   = 'Work-buffer allocation failure'
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
    END IF                  ! if allocate failed

    CALL GRID2XY( GDTYP2, P_ALP2, P_BET2, P_GAM2, XCENT2, YCENT2,       &
                  GDTYP1, P_ALP1, P_BET1, P_GAM1, XCENT1, YCENT1,       &
                  NCOLS3, NROWS3, XORIG1, YORIG1, XCELL3, YCELL3,       &
                  XLOC, YLOC )

    !!.......   Compute fractions:

    DO  ROW = 1, NROWS2
    DO  COL = 1, NCOLS2
        ISCR( COL,ROW ) = 0
    END DO
    END DO

    DDX2 = 1.0 / XCELL2
    DDY2 = 1.0 / YCELL2
    DDXY = SCALEFAC / FLOAT( NX * NY )
    N    = 0
    K    = 0

    DO  R = 1, NROWS1         !  traverse input grid:
    DO  C = 1, NCOLS1         !  SERIAL LOOP:  dependency on ISCR, N

        K = K + 1

        COLMAX = 0
        ROWMAX = 0
        COLMIN = NCOLS2 + 1
        ROWMIN = NROWS2 + 1
        DO  J = 1, NY
        DO  I = 1, NX

            CC = I + NX * ( C - 1 )
            RR = J + NY * ( R - 1 )
            X = XLOC(CC,RR)  -  XORIG2  -  XADJ
            Y = YLOC(CC,RR)  -  YORIG2
            IF ( X .GE. 0.0D0  .AND. Y .GE. 0.0 ) THEN
                COL = 1 + INT( DDX2 * X )
                ROW = 1 + INT( DDY2 * Y )
                IF ( COL .LE. NCOLS2 .AND. ROW .LE. NROWS2 ) THEN
                    ISCR( COL,ROW ) = ISCR( COL,ROW ) + 1
                    IF ( COL .GT. COLMAX ) COLMAX = COL
                    IF ( COL .LT. COLMIN ) COLMIN = COL
                    IF ( ROW .GT. ROWMAX ) ROWMAX = ROW
                    IF ( ROW .LT. ROWMIN ) ROWMIN = ROW
                END IF
            END IF

        END DO      !  end loop on sub-sampled cols in input grid
        END DO      !  end loop on sub-sampled rows in input grid

        DO ROW = ROWMIN, ROWMAX
        DO COL = COLMIN, COLMAX
            IF ( ISCR( COL,ROW ) .GT. 0 ) THEN
                N = N + 1
                IF ( N .LE. NFRACS ) THEN
                    FRAC( N ) = DDXY * FLOAT( ISCR( COL,ROW ) )
                    ICEL( N ) =  C  +  ( R  - 1 ) * NCOLS1  !  input cell #
                    OCEL( N ) = COL + ( ROW - 1 ) * NCOLS2  ! output cell #
                    ISCR( COL,ROW ) = 0   ! reset to zero for next pass
                END IF
            END IF
        END DO
        END DO

    END DO
    END DO          !  end traversal of input grid

    IF ( N .GT. NFRACS ) THEN
        WRITE( MESG, '( A, I12, 2X, A, I12 )' ) 'ERROR:  Size-allocation overflow:  allocated', NFRACS, 'actual', N
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
    ELSE IF ( N .EQ. 0 ) THEN
        MESG = 'ERROR:  NO INTERSECTION FOUND:  Number of coeffs = 0'
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
    END IF
    WRITE( MESG, '( A, I12 )' ) 'Number of coeffs', N
    CALL M3MESG( MESG )


    !!.......   Allocate and organize the output sparse matrix:
    !!.......   (writing ASCII output file, if requested)

    ALLOCATE( MATX( NGRID2 + 2*N ), STAT = ISTAT )

    IF ( ISTAT .NE. 0 ) THEN
        MESG   = 'Output-buffer allocation failure'
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
    END IF                  ! if allocate failed

    CALL BLDMATRIX( NGRID2, N, ICEL, OCEL, FRAC,    &
                    MATDEV, NCOLS1, NCOLS2,         &
                    MATX,                           &   !!  matrix-row counts
                    MATX( NGRID2+1 ),               &   !!  indices inti input grid
                    MATX( NGRID2+N+1 ) )                !!  coeffs

    !!.......   Open output file and write out the output sparse matrix:

    GDNAM3D = OGRID
    NROWS3D = NGRID2
    NCOLS3D = N
    FTYPE3D = SMATRX3
    NVARS3D = 1
    NLAYS3D = 1
    VGTYP3D = IMISS3
    VGTOP3D = BADVAL3
    VGLVS3D( 1 ) = BADVAL3
    VGLVS3D( 2 ) = BADVAL3

    SDATE3D = 0
    STIME3D = 0
    TSTEP3D = 0

    VNAME3D( 1 ) = 'COEF'
    VTYPE3D( 1 ) = M3REAL
    UNITS3D( 1 ) = 'n/a'
    VDESC3D( 1 ) = 'Sparse matrix coefficient'
    FDESC3D( 1 ) = 'Sparse transform matrix'
    FDESC3D( 2 ) = '#INGRID  ' // IGRID
    FDESC3D( 3 ) = '#OUTGRID ' // OGRID
    DO  N = 4, MXDESC3
        FDESC3D( N ) = ' '
    END DO

    IF ( .NOT. OPEN3( 'MATRIX', FSUNKN3, PNAME ) ) THEN

        EFLAG = .TRUE.
        MESG = 'ERROR:  Could not open output SPARSE MATRIX file'
        CALL M3MESG( MESG )

    ELSE IF ( .NOT. SETMTXATT( 'MATRIX', IGRID, GDTYP1,                 &
                                P_ALP1, P_BET1, P_GAM1, XCENT1, YCENT1, &
                                XORIG1, YORIG1, XCELL1, YCELL1,         &
                                NCOLS1, NROWS1,                         &
                                         OGRID, GDTYP2,                 &
                                P_ALP2, P_BET2, P_GAM2, XCENT2, YCENT2, &
                                XORIG2, YORIG2, XCELL2, YCELL2,         &
                                NCOLS2, NROWS2 ) ) THEN
        EFLAG = .TRUE.
        MESG  = 'Could not set grid description attributes for "MATRIX"'
        CALL M3MESG( MESG )

    ELSE IF ( .NOT. WRITE3( 'MATRIX', ALLVAR3, 0, 0, MATX ) ) THEN

        EFLAG = .TRUE.
        MESG  = 'ERROR:  Could not write output SPARSE MATRIX'
        CALL M3MESG( MESG )

    END IF


    !!...............  Shut down program:

    IF ( EFLAG ) THEN
        MESG  = 'Failure in program MTXCALC'
        ISTAT = 2
    ELSE
        MESG  = 'Success in program MTXCALC'
        ISTAT = 0
    END IF
    CALL M3EXIT( PNAME, 0, 0, MESG, ISTAT )


END PROGRAM MTXCALC


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
    !!  Can not be a CONTAINed routine because of the implied
    !!  equivalences between INTEGER MATX(:) in the caller, and 
    !!  <INTEGER ICNT(:), INTEGER INDX(:), REAL COEF(:)>
    !!  in this routine.
    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

    SUBROUTINE  BLDMATRIX( M, N, ICEL, OCEL, FRAC,  &
                           MATDEV, NCOLS1, NCOLS2,  &
                           ICNT, INDX, COEF )

        IMPLICIT NONE

        INTEGER, INTENT(IN   ) :: M               !  number of output rows
        INTEGER, INTENT(IN   ) :: N               !  number of input coefficients
        INTEGER, INTENT(IN   ) :: ICEL( N )       !  list of in-cells
        INTEGER, INTENT(IN   ) :: OCEL( N )       !  list of out-cells
        REAL   , INTENT(IN   ) :: FRAC( N )       !  list of coefficients
        INTEGER, INTENT(IN   ) :: MATDEV          !  optional ASCII output unit
        INTEGER, INTENT(IN   ) :: NCOLS1          !  input  grid-column
        INTEGER, INTENT(IN   ) :: NCOLS2          !  output grid-column
        INTEGER, INTENT(  OUT) :: ICNT( M )       !  matrix: output row-count
        INTEGER, INTENT(  OUT) :: INDX( N )       !  matrix: output row-index
        REAL   , INTENT(  OUT) :: COEF( N )       !  matrix: coefficients

        INTEGER   I, J, K, L, JJ

        !!..................  body of BLDMATRIX  ...........................

        K = 0

        IF ( MATDEV .LT. 0 ) THEN   !  then do NOT write ASCII coeffs

            DO  I = 1, M

                L = 0
                DO  J = 1, N
                    IF ( OCEL( J ) .EQ. I ) THEN
                        K = K + 1
                        L = L + 1
                        INDX( K ) = ICEL( J )
                        COEF( K ) = FRAC( J )
                    END IF
                END DO          !  end loop on coeffs J
                ICNT( I ) = L

            END DO              !  end loop on output rows I

        ELSE        !  matdev > 0:  write ASCII coeffs-file to unit MATDEV

            WRITE( MATDEV, '( A4, 5A12 )' )     &
                '#    ', 'OUTROW', 'OUTCOL', 'INROW', 'INCOL', 'FRAC'

            DO  I = 1, M

                L = 0
                DO  J = 1, N
                    IF ( OCEL( J ) .EQ. I ) THEN
                        K = K + 1
                        L = L + 1
                        JJ = ICEL( J )
                        INDX( K ) = JJ
                        COEF( K ) = FRAC( J )
                        WRITE( MATDEV, '( 4I12, 2X, 1PE13.6 )' )    &
                                  1 + (I -1)/NCOLS2,                &   ! output row #
                                  1 + MOD(  I-1, NCOLS2 ),          &   ! output col #
                                  1 + (JJ-1)/NCOLS1,                &   !  input row #
                                  1 + MOD( JJ-1, NCOLS1 ),          &   !  input col #
                                  FRAC( J )
                    END IF
                END DO          !  end loop on coeffs J
                ICNT( I ) = L

            END DO              !  end loop on output rows I

        END IF          !  if matdev < 0, or not

        RETURN

    END SUBROUTINE BLDMATRIX

