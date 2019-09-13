
PROGRAM FINDWNDW

    !!***************************************************************
    !!  Version "$Id: findwndw.f90 130 2019-09-13 20:42:32Z coats $"
    !!  Copyright (c) 2016 UNC Institute for the Environment
    !!  All rights reserved.
    !!..............................................................
    !!  Program body starts at line  89
    !!
    !!  DESCRIPTION:
    !!      See splash screen
    !!
    !!  PRECONDITIONS:
    !!      See splash screen
    !!
    !!  REVISION  HISTORY:
    !!      Prototype  3/2016 by Carlie J. Coats, Jr., UNC IE
    !!      Version  09/2019 by CJC:  call INITSPHERES() before using MODGCTP transforms
    !!***************************************************************

    USE M3UTILIO, M3U_INITSPHERES => INITSPHERES
    USE MODGCTP

    IMPLICIT NONE

    !!......  PARAMETERS and their descriptions:


    CHARACTER*16, PARAMETER ::  PNAME = 'FINDWNDW'
    CHARACTER*16, PARAMETER ::  BLANK = ' '
    CHARACTER*16, PARAMETER ::  COMMA = ','
    CHARACTER*64, PARAMETER ::  BAR   = &
  '-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-'


    !!......  LOCAL VARIABLES and their descriptions:

    LOGICAL :: EFLAG   = .FALSE.
    LOGICAL :: WMOFLAG = .FALSE.

    INTEGER         LDEV, ISTAT
    INTEGER         C, R
    INTEGER         CMAX1, CMIN1
    INTEGER         RMAX1, RMIN1
    REAL*8          XMAX1, XMIN1
    REAL*8          YMAX1, YMIN1
    REAL*8          XYTMP, Z

    !!    grid name, parameters for FILE1, FILE2

    INTEGER         NCOLS1      ! dimensions of dot=pt grid
    INTEGER         NROWS1
    INTEGER         GDTYP1
    REAL*8          P_ALP1      ! first, second, third map
    REAL*8          P_BET1      ! projection descriptive
    REAL*8          P_GAM1      ! parameters.
    REAL*8          XCENT1      ! lon for coord-system X=0
    REAL*8          YCENT1      ! lat for coord-system Y=0
    REAL*8          XORIG1      ! X-coordinate origin of dot-pt grid (map units)
    REAL*8          YORIG1      ! Y-coordinate origin of dot-pt grid
    REAL*8          XCELL1      ! X-coordinate cell dimension
    REAL*8          YCELL1      ! Y-coordinate cell dimension

    INTEGER         NCOLS2
    INTEGER         NROWS2
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
    REAL*8          XFINL2      ! X-coordinate origin of grid (map units)
    REAL*8          YFINL2      ! Y-coordinate origin of grid

    REAL*8          XORIGW, YORIGW
    INTEGER         NCOLSW, NROWSW

    REAL*8 , ALLOCATABLE :: XLOC1( :,: )    !! FILE1 dot=pt grid nodes in FILE2 coords
    REAL*8 , ALLOCATABLE :: YLOC1( :,: )
				
    CHARACTER*120   MESG

    !!--------------------------------------------------------------
    !!   begin body of program FINDWNDW

    LDEV  = INIT3()
    EFLAG = .FALSE.

    WRITE( LDEV, '( 5X, A )' )  BLANK, BAR,                                     &
'Program FINDWNDW to read headers from two files FILE1 and FILE2, and',         &
'compute the smallest window into FILE2 that covers FILE1, if any.',            &
'Program reports failure if FILE2-grid does not completely cover',              &
'the FILE1-grid.',                                                              &
'',                                                                             &
'Log-output includes column and row ranges for the FILE2-window,',              &
'XORIG and YORIG for the FILE2-window, and a GRIDDESC-line for it.',            &
'',                                                                             &
'PRECONDITIONS REQUIRED:',                                                      &
'    setenv FILE1   <path name>"',                                              &
'    setenv FILE2   <path name>"',                                              &
'',                                                                             &
'    ${FILE1} and ${FILE2} are of data-type GRIDDED.',                          &
'    ${FILE2} has a positively oriented grid.',                                 &
'',                                                                             &
'The following coordinate systems are supported',                               &
'',                                                                             &
'    Latitude-Longitude (must comply with ISO Standard 6709)',                  &
'    Lambert Conformal Conic',                                                  &
'    General Mercator      ',                                                   &
'    General Stereographic ',                                                   &
'    UTM                   ',                                                   &
'    Polar Stereographic   ',                                                   &
'    Transverse Mercator   ',                                                   &
'    Equatorial Mercator   ',                                                   &
'    Albers Conic Equal Area',                                                  &
'',                                                                             &
'Program version:',                                                             &
'$Id: findwndw.f90 130 2019-09-13 20:42:32Z coats $',&
'',                                                                             &
'Copyright (C) 2016 UNC Institute for the Environment',                         &
'All rights reserved.',                                                         &
''

    IF ( .NOT. GETYN( 'Continue with program?', .TRUE. ) ) THEN
        MESG = 'Program terminated at user request'
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
    END IF


    !!...............  Open input files:

    IF ( .NOT.OPEN3( 'FILE1', FSRDWR3, PNAME ) ) THEN
        EFLAG = .TRUE.
        MESG  = 'Could not open "FILE1"'
        CALL M3MESG( MESG )
    ELSE IF ( .NOT.DESC3( 'FILE1' ) ) THEN
        EFLAG = .TRUE.
        MESG  = 'Could not get description for "FILE1"'
        CALL M3MESG( MESG )
    ELSE IF ( FTYPE3D .NE. GRDDED3 ) THEN
        EFLAG = .TRUE.
        MESG  = 'Input file FILE1:  type not GRIDDED"'
        CALL M3MESG( MESG )
    ELSE
        NCOLS1 = NCOLS3D+1
        NROWS1 = NROWS3D+1
        GDTYP1 = GDTYP3D
        P_ALP1 = P_ALP3D
        P_BET1 = P_BET3D
        P_GAM1 = P_GAM3D
        XCENT1 = XCENT3D
        YCENT1 = YCENT3D
        XORIG1 = XORIG3D - 0.5D0 * XCELL3D
        YORIG1 = YORIG3D - 0.5D0 * YCELL3D
        XCELL1 = XCELL3D
        YCELL1 = YCELL3D
    END IF                          !  if  open3( FILE1 )

    IF ( .NOT.OPEN3( 'FILE2', FSRDWR3, PNAME ) ) THEN
        EFLAG = .TRUE.
        MESG  = 'Could not open ' // 'FILE2'
        CALL M3MESG( MESG )
    ELSE IF ( .NOT.DESC3( 'FILE2' ) ) THEN
        EFLAG = .TRUE.
        MESG  = 'Could not get description for ' // 'FILE2'
        CALL M3MESG( MESG )
    ELSE IF ( FTYPE3D .NE. GRDDED3 ) THEN
        EFLAG = .TRUE.
        MESG  = 'Input file FILE2:  type not GRIDDED"'
        CALL M3MESG( MESG )
    ELSE
        NCOLS2 = NCOLS3D
        NROWS2 = NROWS3D
        GDTYP2 = GDTYP3D
        P_ALP2 = P_ALP3D
        P_BET2 = P_BET3D
        P_GAM2 = P_GAM3D
        XCENT2 = XCENT3D
        YCENT2 = YCENT3D
        XORIG2 = XORIG3D
        YORIG2 = YORIG3D
        XCELL2 = XCELL3D
        YCELL2 = YCELL3D
        XFINL2 = XORIG3D + DBLE( NCOLS2 )*XCELL2
        YFINL2 = YORIG3D + DBLE( NROWS2 )*YCELL2
        
        WMOFLAG = ( ( GDTYP2 .EQ. LATGRD3 ) .AND. ( XFINL2 .GT. 180.0D0 ) )

        !!........  Force positive grid orientation

        IF ( XCELL2 .LT. 0.0D0 ) THEN
            EFLAG = .TRUE.
            CALL M3MESG( 'Negative X-orientation for FILE2 grid.' )
        END IF

        IF ( YCELL2 .LT. 0.0D0 ) THEN
            EFLAG = .TRUE.
            CALL M3MESG( 'Negative Y-orientation for FILE2 grid.' )
        END IF
    END IF                         !  if  open3( inpfile )


    IF ( EFLAG ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'Fatal input-file error(s)', 2 )
    END IF

    

    !!..........  Allocate coordinate buffers:

    ALLOCATE( XLOC1( NCOLS1,NROWS1 ),           &
              YLOC1( NCOLS1,NROWS1 ),  STAT = ISTAT )

    IF ( ISTAT .NE. 0 ) THEN
        EFLAG = .TRUE.
        WRITE( MESG, '( A, I10 )' ) 'Buffer allocation failed:  STAT=', ISTAT
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
    END IF


    !!..........  Calculate grid-to-grid transform:

    IF ( .NOT. INITSPHERES() ) THEN
        CALL M3EXIT( PNAME, 0,0, 'INITSPHERES() failure', 2 )
    END IF
    CALL GRID2XY( GDTYP2, P_ALP2, P_BET2, P_GAM2, XCENT2, YCENT2,    &
                  GDTYP1, P_ALP1, P_BET1, P_GAM1, XCENT1, YCENT1,    &
                  NCOLS1, NROWS1, XORIG1, YORIG1, XCELL1, YCELL1,    &
                  XLOC1,  YLOC1 )


    !!..........  Calculate max and min of XLOC1, YLOC1

    XMAX1 = XLOC1(1,1)
    XMIN1 = XLOC1(1,1)
    YMAX1 = YLOC1(1,1)
    YMIN1 = YLOC1(1,1)
    
    IF ( WMOFLAG ) THEN
    
        CALL M3MESG( 'FILE2 seems to violate ISO Standard 6709 for "Lat-Lon"' )

!$OMP   PARALLEL DO                                             &
!$OMP&          DEFAULT( NONE ),                                &
!$OMP&           SHARED( NCOLS1, NROWS1, NCOLS2, NROWS2,        &
!$OMP&                   XORIG2, YORIG2, XFINL2, YFINL2 ),      &
!$OMP&          PRIVATE( C, R, Z )                              &
!$OMP&        REDUCTION( MAX:   XMAX1, YMAX1 ),                 &
!$OMP&        REDUCTION( MIN:   XMIN1, YMIN1 ),                 &
!$OMP&        REDUCTION( .OR.:  EFLAG )

        DO R = 1, NROWS1
        DO C = 1, NCOLS1

            Z = MOD( XLOC1( C,R ) + 360.0D0 , 360.0D0 )

            IF ( Z .LT. XORIG2 ) THEN
                EFLAG = .TRUE.
            ELSE IF ( Z .GT. XFINL2 ) THEN
                EFLAG = .TRUE.
            ELSE IF ( Z .GT. XMAX1 ) THEN
                XMAX1 = Z
            ELSE IF ( Z .LT. XMIN1 ) THEN
                XMIN1 = Z
            END IF

            Z = YLOC1( C,R )

            IF ( Z .LT. YORIG2 ) THEN
                EFLAG = .TRUE.
            ELSE IF ( Z .GT. YFINL2 ) THEN
                EFLAG = .TRUE.
            ELSE IF ( Z .GT. YMAX1 ) THEN
                YMAX1 = Z
            ELSE IF ( Z .LT. YMIN1 ) THEN
                YMIN1 = Z
            END IF

        END DO
        END DO

    ELSE

!$OMP   PARALLEL DO                                             &
!$OMP&          DEFAULT( NONE ),                                &
!$OMP&           SHARED( NCOLS1, NROWS1, NCOLS2, NROWS2,        &
!$OMP&                   XORIG2, YORIG2, XFINL2, YFINL2 ),      &
!$OMP&          PRIVATE( C, R, Z )                              &
!$OMP&        REDUCTION( MAX:   XMAX1, YMAX1 ),                 &
!$OMP&        REDUCTION( MIN:   XMIN1, YMIN1 ),                 &
!$OMP&        REDUCTION( .OR.:  EFLAG )

        DO R = 1, NROWS1
        DO C = 1, NCOLS1

            Z = XLOC1( C,R )

            IF ( Z .LT. XORIG2 ) THEN
                EFLAG = .TRUE.
            ELSE IF ( Z .GT. XFINL2 ) THEN
                EFLAG = .TRUE.
            ELSE IF ( Z .GT. XMAX1 ) THEN
                XMAX1 = Z
            ELSE IF ( Z .LT. XMIN1 ) THEN
                XMIN1 = Z
            END IF

            Z = YLOC1( C,R )

            IF ( Z .LT. YORIG2 ) THEN
                EFLAG = .TRUE.
            ELSE IF ( Z .GT. YFINL2 ) THEN
                EFLAG = .TRUE.
            ELSE IF ( Z .GT. YMAX1 ) THEN
                YMAX1 = Z
            ELSE IF ( Z .LT. YMIN1 ) THEN
                YMIN1 = Z
            END IF

        END DO
        END DO

    END IF      !!  if wmoflag, or not

    CALL M3MESG( BAR )
    WRITE( MESG, '( 2( A, 1PD24.16 ) )' ) 'Window XMIN:XMAX:', XMIN1, ':', XMAX1
    CALL M3MESG( MESG )
    WRITE( MESG, '( 2( A, 1PD24.16 ) )' ) 'Window YMIN:YMAX:', YMIN1, ':', YMAX1
    CALL M3MESG( MESG )

    IF ( EFLAG ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'Grid-coverage error(s)', 2 )
    END IF
    
    CMAX1  = 1 + INT( XMAX1 / XCELL2 )
    CMIN1  = 1 + INT( XMIN1 / XCELL2 )
    RMAX1  = 1 + INT( YMAX1 / YCELL2 )
    RMIN1  = 1 + INT( YMIN1 / YCELL2 )
    XORIGW = XORIG2 + DBLE( CMIN1-1 )*XCELL2
    YORIGW = YORIG2 + DBLE( RMIN1-1 )*YCELL2
    NCOLSW = CMAX1 - CMIN1 + 1
    NROWSW = RMAX1 - RMIN1 + 1

    CALL M3MESG( BAR )
    WRITE( MESG, '( A, I5, A, I5 )' ) 'Window cols:', CMIN1, ':', CMAX1
    CALL M3MESG( MESG )

    WRITE( MESG, '( A, I5, A, I5 )' ) 'Window rows:', RMIN1, ':', RMAX1
    CALL M3MESG( MESG )

    WRITE( MESG, '( A, 1PD24.16 )' ) 'Window XORIG:', XORIGW
    CALL M3MESG( MESG )

    WRITE( MESG, '( A, 1PD24.16 )' ) 'Window YORIG:', YORIGW
    CALL M3MESG( MESG )
    CALL M3MESG( 'GRIDDESC line:' )
    WRITE( MESG, '( A, 4( A, 1PD24.16 ), 3( A, I8 ) )' )      &
        "'WINDOW_PROJ'", COMMA, XORIGW, COMMA, YORIGW, COMMA, XCELL2, COMMA, YCELL2, COMMA, NCOLSW, COMMA, NROWSW, COMMA, 1
    CALL M3MESG( MESG )
    CALL M3MESG( BAR )

    CALL M3EXIT( PNAME, 0, 0, 'Success in program', 0 )


END PROGRAM FINDWNDW

