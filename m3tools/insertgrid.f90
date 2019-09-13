
PROGRAM INSERTGRID

    !!***************************************************************
    !!  Version "$Id: insertgrid.f90 130 2019-09-13 20:42:32Z coats $"
    !!  Copyright (c) 2015 UNC Institute for the Environment
    !!  All rights reserved.
    !!..............................................................
    !!  Program body starts at line  134
    !!
    !!  DESCRIPTION:
    !!
    !!  PRECONDITIONS:
    !!
    !!  REVISION  HISTORY:
    !!      Prototype  7/2015 by Carlie J. Coats, Jr., UNC IE
    !!      Version  09/2019 by CJC:  call INITSPHERES() before using MODGCTP transforms
    !!***************************************************************

    USE M3UTILIO, AVOID_INITSPHERES => INITSPHERES
    USE MODGCTP

    IMPLICIT NONE

    !!......  PARAMETERS and their descriptions:


    CHARACTER*16, PARAMETER ::  PNAME = 'INSERTGRID'
    CHARACTER*16, PARAMETER ::  BLANK = ' '
    CHARACTER*64, PARAMETER ::  BAR   = &
  '-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-'


    !!......  LOCAL VARIABLES and their descriptions:

    INTEGER         LDEV, ISTAT
    INTEGER         C, R, CC, RR, C2, R2, I, J, K, L, M, N, V
    INTEGER         SDATE, STIME, TSTEP, TSECS, NRECS, NSAMP
    INTEGER         JDATE, JTIME

    LOGICAL         EFLAG

    REAL*8          SUM, GFAC, GG

    CHARACTER*16    VNAME
    CHARACTER*256   MESG

    !!     name, parameters for input grid

    CHARACTER*16    GDNAM1
    INTEGER         NCOLS1
    INTEGER         NROWS1
    INTEGER         NGRID1
    INTEGER         NLAYS1
    INTEGER         NVARS1
    INTEGER         NRECS1
    INTEGER         GDTYP1
    INTEGER         VGTYP1
    INTEGER         SDATE1
    INTEGER         STIME1
    INTEGER         TSTEP1
    INTEGER         EDATE1
    INTEGER         ETIME1
    REAL*8          P_ALP1      ! first, second, third map
    REAL*8          P_BET1      ! projection descriptive
    REAL*8          P_GAM1      ! parameters.
    REAL*8          XCENT1      ! lon for coord-system X=0
    REAL*8          YCENT1      ! lat for coord-system Y=0
    REAL*8          XORIG1      ! X-coordinate origin of grid (map units)
    REAL*8          YORIG1      ! Y-coordinate origin of grid
    REAL*8          XCELL1      ! X-coordinate cell dimension
    REAL*8          YCELL1      ! Y-coordinate cell dimension
    REAL            VGTOP1
    REAL            VGLVS1( MXLAYS3+1 )
    CHARACTER*16    VNAME1( MXVARS3 )

    !!     GRIDDESC name, parameter for output grid and file

    CHARACTER*16    GDNAM2
    INTEGER         GDTYP2
    INTEGER         NCOLS2
    INTEGER         NROWS2
    INTEGER         NGRID2
    INTEGER         NVARS2
    REAL*8          P_ALP2
    REAL*8          P_BET2
    REAL*8          P_GAM2
    REAL*8          XCENT2
    REAL*8          YCENT2
    REAL*8          XORIG2
    REAL*8          YORIG2
    REAL*8          XCELL2
    REAL*8          YCELL2

    !!     parameters for sampling grid:

    INTEGER         NCOLS3
    INTEGER         NROWS3
    INTEGER         NGRID3
    REAL*8          XCELL3
    REAL*8          YCELL3

    REAL*8 , ALLOCATABLE :: XLOC3( :,: )
    REAL*8 , ALLOCATABLE :: YLOC3( :,: )
    INTEGER, ALLOCATABLE :: NCNT1( : )
    INTEGER, ALLOCATABLE :: NDEX2( : )

    REAL   , ALLOCATABLE :: ABUF1( :,: )
    REAL   , ALLOCATABLE :: ABUF2( :,: )


    !!........  Statement Functions:

    LOGICAL         FLTERR
    REAL            PP, QQ

    FLTERR( PP, QQ ) = ( (PP - QQ)**2  .GT.  1.0E-9*( PP*PP + QQ*QQ + 1.0E-5 ) )

    !!--------------------------------------------------------------
    !!   begin body of program INSERTGRID

    LDEV  = INIT3()
    EFLAG = .FALSE.

    WRITE( LDEV, '( 5X, A )' )  BLANK, BAR,                                     &
'Program INSERTGRID to read data from two files FILE1 and FILE2, and',          &
'for FILE1 cells completely within the coverage of FILE2, to aggregate',        &
'REAL FILE2 data over those cells, and write the resulting data back',          &
'to FILE1.',                                                                    &
'',                                                                             &
'PRECONDITIONS REQUIRED:',                                                      &
'    setenv FILE1   <path name>"',                                              &
'    setenv FILE2   <path name>"',                                              &
'',                                                                             &
'    setenv GRID_REFINEMENT  <re-sampling factor along each coordinate>',       &
'    setenv GRID_SCALE       <re-gridding scale factor> [from FILE1,FILE2]',    &
'',                                                                             &
'    setenv SDATE   <starting date (YYYYDDD)> [from FILE1]',                    &
'    setenv STIME   <starting time  (HHMMSS)> [from FILE1]',                    &
'    setenv TSTEP   <time step      (HHMMSS)> [from FILE1]',                    &
'    setenv EDATE   <final    date (YYYYDDD)> [from FILE1]',                    &
'    setenv ETIME   <final    time  (HHMMSS)> [from FILE1]',                    &
'',                                                                             &
'    ${FILE1} and ${FILE2} are of data-type GRIDDED and cover the',             &
'    requested time-period [SDATE:STIME - EDATE:ETIME]',                        &
'',                                                                             &
'',                                                                             &
'',                                                                             &
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
'$Id: insertgrid.f90 130 2019-09-13 20:42:32Z coats $',&
'',                                                                             &
'Copyright (C) 2015 UNC Institute for the Environment',                         &
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

        NCOLS1 = NCOLS3D
        NROWS1 = NROWS3D
        NLAYS1 = NLAYS3D
        NGRID1 = NCOLS1*NROWS1
        NRECS1 = MXREC3D
        SDATE1 = SDATE3D
        STIME1 = STIME3D
        TSTEP1 = TSTEP3D
        GDTYP1 = GDTYP3D
        VGTYP1 = VGTYP3D
        P_ALP1 = P_ALP3D
        P_BET1 = P_BET3D
        P_GAM1 = P_GAM3D
        XCENT1 = XCENT3D
        YCENT1 = YCENT3D
        XORIG1 = XORIG3D
        YORIG1 = YORIG3D
        XCELL1 = XCELL3D
        YCELL1 = YCELL3D
        VGTOP1 = VGTOP3D
        VGLVS1 = VGLVS3D

        L = 0
        DO V = 1, NVARS3D
            IF ( VTYPE3D( V ) .EQ. M3REAL ) THEN
                L = L + 1
                VNAME1( L ) = VNAME3D( V )
            END IF
        END DO
        NVARS1 = L

        CALL LASTTIME( SDATE1, STIME1, TSTEP1, NRECS1, EDATE1, ETIME1 )

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
    ELSE IF ( NLAYS3D .NE. NLAYS1 ) THEN
        EFLAG = .TRUE.
        MESG  = 'Inconsistent NLAYS dimension for ' // 'FILE2'
        CALL M3MESG( MESG )
    ELSE IF ( .NOT.GRDCHK3( 'FILE2',                                        &
                            P_ALP3D,  P_BET3D,  P_GAM3D,  XCENT3D, YCENT3D, &
                            XORIG3D,  YORIG3D,  XCELL3D,  YCELL3D,          &
                            NLAYS1,   VGTYP1,   VGTOP1,   VGLVS1 ) ) THEN
        EFLAG = .TRUE.
        MESG  = 'Inconsistent coordinates for "GRID_CRO_2D"'
        CALL M3MESG( MESG )
    ELSE

        NCOLS2 = NCOLS3D
        NROWS2 = NROWS3D
        NGRID2 = NCOLS2*NROWS2
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

        IF ( NLAYS1 .GT. 1 .AND. VGTYP1 .NE. IMISS3 ) THEN

            IF ( VGTYP1 .NE. VGTYP3D ) THEN
                CALL M3MSG2( 'VGTYP mismatch' )
                EFLAG = .TRUE.
            END IF

            IF ( VGTYP1 .EQ. VGSGPH3 .OR.   &
                 VGTYP1 .EQ. VGSGPN3 .OR.   &
                 VGTYP1 .EQ. VGSIGZ3 .OR.   &
                 VGTYP1 .EQ. VGWRFEM ) THEN

                IF ( FLTERR( VGTOP1, VGTOP3D ) ) THEN
                    CALL M3MSG2( 'VGTOP mismatch' )
                    EFLAG = .TRUE.
                END IF

            END IF      !  if "sigma" vertical coordinate type

            DO  L = 1, NLAYS1+1
                IF ( FLTERR( VGLVS1(L), VGLVS3D(L) ) ) THEN
                    WRITE( MESG, '( A, I4, 1X, 2 ( A, 1PE14.7 ) )' )    &
                    'Layer', L, 'mismatch: ', VGLVS1(L), ' vs ', VGLVS3D(L) 
                    CALL M3MSG2( MESG )
                    EFLAG = .TRUE.
                END IF
            END DO

        END IF          ! if nlays > 1 and vgtyp not "missing"

        DO V = 1, NVARS1
            L = INDEX1( VNAME1(V), NVARS3D, VNAME3D )
            IF ( L .LE. 0 ) THEN
                EFLAG = .TRUE.
                MESG  = 'Variable "' // TRIM( VNAME1(V) ) // '" not available in FILE2'
                CALL M3MESG( MESG )
            ELSE IF ( VTYPE3D(L) .NE. M3REAL ) THEN
                EFLAG = .TRUE.
                MESG  = 'Variable "' // TRIM( VNAME1(V) ) // '" in FILE2 not of type REAL'
                CALL M3MESG( MESG )
            END IF
        END DO

    END IF                          !  if  not open3( 'FILE2' ) else...                        !  if  open3( inpfile )


    IF ( EFLAG ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'Fatal input-file error(s)', 2 )
    END IF


    !!...............  Get environment
    !!  default 3::1 resampling of fine grid:

    N     = CEILING( 3.0 * GRID_RATIO( GDTYP1, XCELL1, YCELL1, YORIG1,  &
                                       GDTYP2, XCELL2, YCELL2, YORIG2 ) )
    NSAMP = ENVINT( 'GRID_REFINEMENT', 'refinement-factor for aggregation', N, ISTAT )
    IF ( ISTAT .GT. 0 ) THEN
        EFLAG = .TRUE.
        MESG  = 'Bad environment variable "GRID_REFINEMENT"'
        CALL M3MESG( MESG )
    ELSE
        NCOLS3 = NSAMP * NCOLS1
        NROWS3 = NSAMP * NROWS1
        NGRID3 = NCOLS3 * NROWS3
        XCELL3 = XCELL1 / DBLE( NSAMP )
        YCELL3 = YCELL1 / DBLE( NSAMP )
    END IF
    
    GG   = XCELL1*YCELL1 / ( XCELL2*YCELL2 )
    GFAC = ENVDBLE( 'GRID_SCALE', 'Re-gridding scale factor', GG, ISTAT ) / DBLE( NSAMP**2 )
    IF ( ISTAT .GT. 0 ) THEN
        EFLAG = .TRUE.
        MESG  = 'Bad environment variable "GRID_SCALE"'
        CALL M3MESG( MESG )
    END IF


    !!...............  Get time step sequence from environment:

    IF ( TSTEP1 .EQ. 0 ) THEN 
    
        NRECS = 1
        
    ELSE
    
        SDATE = ENVINT( 'SDATE', 'Start date [YYYYDD]', SDATE1, ISTAT )
        IF ( ISTAT .GT. 0 ) THEN
            EFLAG = .TRUE.
            MESG  = 'Bad environment variable "SDATE"'
            CALL M3MESG( MESG )
        END IF

        STIME = ENVINT( 'STIME', 'Start TIME  [HHMMSS]', STIME1, ISTAT )
        IF ( ISTAT .GT. 0 ) THEN
            EFLAG = .TRUE.
            MESG  = 'Bad environment variable "STIME"'
            CALL M3MESG( MESG )
        END IF

        JDATE = ENVINT( 'EDATE', 'Final date [YYYYDD]', EDATE1, ISTAT )
        IF ( ISTAT .GT. 0 ) THEN
            EFLAG = .TRUE.
            MESG  = 'Bad environment variable "EDATE"'
            CALL M3MESG( MESG )
        END IF

        JTIME = ENVINT( 'ETIME', 'Final TIME  [HHMMSS]', ETIME1, ISTAT )
        IF ( ISTAT .GT. 0 ) THEN
            EFLAG = .TRUE.
            MESG  = 'Bad environment variable "ETIME"'
            CALL M3MESG( MESG )
        END IF

        TSTEP = ENVINT( 'TSTEP', 'TIME STEP  [HHMMSS]', TSTEP1, ISTAT )
        IF ( ISTAT .GT. 0 ) THEN
            EFLAG = .TRUE.
            MESG  = 'Bad environment variable "TSTEP"'
            CALL M3MESG( MESG )
        END IF

        NRECS = CURREC( JDATE, JTIME, SDATE, STIME, TSTEP, L, N )

    END IF

    IF ( NRECS .LT. 0 ) THEN
        EFLAG = .TRUE.
        CALL M3MESG( 'Bad time step sequence' )
    END IF


    IF ( EFLAG ) THEN
        MESG = 'Bad input environment'
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
    END IF


    !!..........  Allocate coordinate buffers:

    ALLOCATE( XLOC3( NCOLS3,NROWS3 ),           &
              YLOC3( NCOLS3,NROWS3 ),           &
            NCNT1( 0:NGRID1 ),                  &
              NDEX2( NGRID3 ),                  &
              ABUF1( NGRID1,NLAYS1 ),           &
              ABUF2( NGRID2,NLAYS1 ),  STAT = ISTAT )

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
                  NCOLS3, NROWS3, XORIG1, YORIG1, XCELL3, YCELL3,    &
                  XLOC3,  YLOC3 )

    NCNT1(0) = 0
    M = 0
    DO R = 0, NCOLS1-1
    DO C = 0, NROWS1-1

        N = M

        DO J = 1, NSAMP
        DO I = 1, NSAMP
            CC = I + NSAMP * C
            RR = J + NSAMP * R
            IF ( XLOC3( CC,RR ) .LT. XORIG2 )  CYCLE
            IF ( YLOC3( CC,RR ) .LT. YORIG2 )  CYCLE
            C2 = 1 + ( XLOC3( CC,RR ) - XORIG2 ) / XCELL2
            R2 = 1 + ( YLOC3( CC,RR ) - YORIG2 ) / YCELL2
            IF ( C2 .GT. NCOLS2 )  CYCLE
            IF ( R2 .GT. NROWS2 )  CYCLE
            M  = M + 1
            NDEX2( M ) = C2 + NCOLS2 * ( R2 - 1 )
        END DO
        END DO

        !!  only count these if all <XLOC,YLOC>s are in the grid:

        IF ( M .NE. N + NSAMP*NSAMP ) THEN
            M = N
        END IF

        I = 1 + C + NCOLS1*R
        NCNT1( I ) = M

    END DO
    END DO


    !!..........  Process time step sequence:

    JDATE = SDATE
    JTIME = STIME

    DO N = 1, NRECS

        WRITE( MESG, '( A, I9.7, A, I6.6 )' ) 'Processing', JDATE, ':', JTIME
        CALL M3MESG( MESG )

        DO  V = 1, NVARS1

            VNAME = VNAME1( V )

            IF ( .NOT.READ3( 'FILE1', VNAME, ALLAYS3, JDATE, JTIME, ABUF1 ) ) THEN
                EFLAG = .TRUE.
                MESG  = 'Could not read "' // VNAME // '" from FILE1'
                CALL M3MESG( MESG )
                CYCLE
            END IF

            IF ( .NOT.READ3( 'FILE2', VNAME, ALLAYS3, JDATE, JTIME, ABUF2 ) ) THEN
                EFLAG = .TRUE.
                MESG  = 'Could not read "' // VNAME // '" from FILE1'
                CALL M3MESG( MESG )
                CYCLE
            END IF

            DO L = 1, NLAYS1

                DO I = 1, NGRID1

                    IF ( NCNT1( I ) .GT. NCNT1( I-1 ) ) THEN
                        SUM = 0.0D0
                        DO J = NCNT1( I-1 ) +1, NCNT1( I )
                            K   = NDEX2( J )
                            SUM = SUM + ABUF2( K,L )
                        END DO
                        ABUF1( I,L ) = SUM
                    END IF

                END DO

            END DO

            IF ( .NOT.WRITE3( 'FILE1', VNAME, JDATE, JTIME, ABUF1 ) ) THEN
                EFLAG = .TRUE.
                MESG  = 'Could not write "' // VNAME // '" to FILE1'
                CALL M3MESG( MESG )
                CYCLE
            END IF

        END DO          !!  end loop on REAL variables V

        CALL NEXTIME( JDATE, JTIME, TSTEP )

    END DO              !!  end loop on time steps N


    IF ( EFLAG ) THEN
        MESG  = 'Failure in program'
        ISTAT = 2
    ELSE
        MESG  = 'Success in program'
        ISTAT = 0
    END IF

    CALL M3EXIT( PNAME, 0, 0, MESG, ISTAT )


CONTAINS


    REAL FUNCTION GRID_RATIO( GDTYP1, XCELL1, YCELL1, YORIG1,     &
                              GDTYP2, XCELL2, YCELL2, YORIG2 )

        INCLUDE 'CONST3.EXT'

        INTEGER, INTENT( IN ) :: GDTYP1, GDTYP2
        REAL*8 , INTENT( IN ) :: XCELL1, YCELL1, YORIG1
        REAL*8 , INTENT( IN ) :: XCELL2, YCELL2, YORIG2

        REAL  DX1, DX2, DY1, DY2

        IF ( GDTYP1 .EQ. LATGRD3 ) THEN
            DX1 = DG2M * REAL( XCELL1 ) * COS( PI180*REAL( YORIG1 ) )
            DY1 = DG2M * REAL( YCELL1 )
        ELSE
            DX1 = REAL( XCELL1 )
            DY1 = REAL( YCELL1 )
        END IF

        IF ( GDTYP2 .EQ. LATGRD3 ) THEN
            DX2 = DG2M * REAL( XCELL2 ) * COS( PI180*REAL( YORIG2 ) )
            DY2 = DG2M * REAL( YCELL2 )
        ELSE
            DX2 = REAL( XCELL2 )
            DY2 = REAL( YCELL2 )
        END IF

        GRID_RATIO = MAX( ABS( DY1/DY2 ), ABS( DX1/DX2 ), 1.0 )

        RETURN

    END FUNCTION GRID_RATIO



END PROGRAM INSERTGRID

