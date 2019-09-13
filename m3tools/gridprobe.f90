
PROGRAM GRIDPROBE

    !!***************************************************************
    !! Version "$Id: gridprobe.f90 130 2019-09-13 20:42:32Z coats $"
    !! EDSS/Models-3 M3TOOLS.
    !! Copyright (C) 1992-2002 MCNC, 
    !! (C) 1995-2002, 2005-2013 Carlie J. Coats, Jr.,
    !! (C) 2002-2010 Baron Advanced Meteorological Systems. LLC., and 
    !! (C) 2015 UNC Institute for the Environment.
    !! Distributed under the GNU GENERAL PUBLIC LICENSE version 2
    !! See file "GPL.txt" for conditions of use.
    !!..............................................................
    !!  program body starts at line  99
    !!
    !!  DESCRIPTION:
    !!      See splash screen
    !!
    !!  PRECONDITIONS:
    !!      REQUIRES 01/09/2013 or later version of M3UTILIO
    !!      See splash screen
    !!
    !!  REVISION  HISTORY:
    !!      Prototype  11/2015 by CJC for I/O API v3.2
    !!      Version    09/2019 by CJC:  call INITSPHERES() before using MODGCTP transforms
    !!***************************************************************

    USE M3UTILIO, M3U_INITSPHERES => INITSPHERES
    USE MODGCTP
    IMPLICIT NONE


    !!......  PARAMETERS and their descriptions:

    INTEGER,      PARAMETER ::  MXPNT = 256

    CHARACTER*16, PARAMETER ::  PNAME = 'GRIDPROBE'
    CHARACTER*16, PARAMETER ::  BLANK = ' '
    CHARACTER*64, PARAMETER ::  BAR   = &
    '-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-'


    !!......  LOCAL VARIABLES and their descriptions:

    INTEGER         LDEV, ISTAT, IMISS
    INTEGER         V, K, N, C, R, L
    INTEGER         K1, K2, K3, K4, C0, R0, NC, NR
    INTEGER         NMAX, UMAX
    INTEGER         RDEV, PDEV          !  for ${REPORT}, ${POINTS}
    INTEGER         SDATE, STIME, EDATE, ETIME, NRECS, NVARS
    INTEGER         JDATE, JTIME, TSTEP

    INTEGER         NPNTS
    INTEGER         LAY2
    REAL*8          XLON( MXPNT )
    REAL*8          YLAT( MXPNT )
    REAL*8            XX( MXPNT )
    REAL*8            YY( MXPNT )
    INTEGER         INDX( MXPNT )
    REAL            XFAC( MXPNT )
    REAL            YFAC( MXPNT )
    REAL            VBUF( MXPNT )
    CHARACTER*16    VNAME

    LOGICAL         EFLAG, HEADER

    CHARACTER*72    PROMPT
    CHARACTER*144   MENU( MXVARS3 )
    CHARACTER*256   MESG

    !!     GRIDDESC name, parameters for output grid

    CHARACTER*16    GDNAM1
    INTEGER         FTYPE1
    INTEGER         GDTYP1
    INTEGER         NCOLS1
    INTEGER         NROWS1
    INTEGER         NLAYS1
    INTEGER         NTHIK1
    INTEGER         NVARS1
    INTEGER         NSIZE1
    REAL*8          P_ALP1
    REAL*8          P_BET1
    REAL*8          P_GAM1
    REAL*8          XCENT1
    REAL*8          YCENT1
    REAL*8          XORIG1
    REAL*8          YORIG1
    REAL*8          XCELL1
    REAL*8          YCELL1

    REAL     , ALLOCATABLE :: RBUF( : )
    INTEGER  , ALLOCATABLE :: IBUF( : )
    REAL*8   , ALLOCATABLE :: DBUF( : )
    INTEGER*8, ALLOCATABLE :: LBUF( : )

    !!--------------------------------------------------------------
    !!   begin body of program GRIDPROBE

    LDEV  = INIT3()
    EFLAG = .FALSE.

    WRITE( LDEV, '( 5X, A )' )  BLANK, BAR,                                 &
'Program GRIDPROBE to interpolate time series for a specified layer',       &
'of a REAL variable to a specified set of Lat-Lon points from a GRIDDED',   &
'input file, and write the resulting time series as columns to a',          &
'(human-readable) ASCII REPORT-file.',                                      &
'',                                                                         &
'PRECONDITIONS REQUIRED:',                                                  &
'    setenv INFILE     <path name for  input gridded file>',                &
'    setenv POINTS     <path name for  input points  file>',                &
'    setenv REPORT     <path name for output ASCII   file>',                &
'    setenv HEADER     <Header-lines on output file? (Y/N) [N]',            &
'',                                                                         &
'    at most 256 points in the set of points.',                             &
'',                                                                         &
'    ${POINTS} is list-formatted with one line per point, structured as',   &
'',                                                                         &
'        <latitude for site>  <longitude for site>',                        &
'',                                                                         &
'THE PROGRAM WILL PROMPT YOU for starting and ending date&time for the',    &
'report period, time step, and name of the variable to interpolate.',       &
'For multi-layer files, the program will also prompt you for the',          &
'layer to extract.',                                                        &
'',                                                                         &
'SEE ALSO program "m3probe" for subscript based time series output.',       &
'',                                                                         &
'See URL',                                                                  &
'',                                                                         &
'   https://www.cmascenter.org/ioapi/documentation/3.1/html/AA.html#tools', &
'',                                                                         &
'Copyright (C) 2015 UNC Institute for the Environment.',                    &
'Released under Version 2 of the GNU General Public License.',              &
'See enclosed GPL.txt, or URL',                                             &
''  ,                                                                       &
'    https://www.gnu.org/licenses/old-licenses/gpl-2.0.html',               &
''  ,                                                                       &
'Comments and questions are welcome and can be sent to'  ,                  &
'',                                                                         &
'    Carlie J. Coats, Jr.    carlie@jyarborough.com',                       &
'or',                                                                       &
'    UNC Institute for the Environment',                                    &
'    100 Europa Dr., Suite 490 Rm 405',                                     &
'    Campus Box 1105',                                                      &
'    Chapel Hill, NC 27599-1105',                                           &
'',                                                                         &
'Program version: ',                                                        &
'$Id: gridprobe.f90 130 2019-09-13 20:42:32Z coats $',&
''

    IF ( .NOT. GETVAL( 'Continue with program?', .TRUE. ) ) THEN
        MESG = 'Program terminated at user request'
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
    END IF


    !!...............  Open files:

    IF ( .NOT. INITSPHERES() ) THEN
        CALL M3EXIT( PNAME, 0,0, 'INITSPHERES() failure', 2 )
    END IF

    IF ( .NOT.OPEN3( 'INFILE', FSREAD3, PNAME ) ) THEN
        EFLAG = .TRUE.
        MESG  = 'ERROR:  Could not open "INFILE"'
        CALL M3MESG( MESG )
    ELSE IF ( .NOT.DESC3( 'INFILE' ) ) THEN
        EFLAG = .TRUE.
        MESG  = 'ERROR:  Could not get description for "INFILE"'
        CALL M3MESG( MESG )
    ELSE IF ( FTYPE3D .NE. GRDDED3 ) THEN
        EFLAG = .TRUE.
        MESG  = 'ERROR:   "INFILE" not GRIDDED'
        CALL M3MESG( MESG )
    ELSE

        GDNAM1 = GDNAM3D
        FTYPE1 = FTYPE3D
        GDTYP1 = GDTYP3D
        NCOLS1 = NCOLS3D
        NROWS1 = NROWS3D
        NLAYS1 = NLAYS3D
        NTHIK1 = NTHIK3D
        NVARS1 = NVARS3D
        GDTYP1 = GDTYP3D
        P_ALP1 = P_ALP3D
        P_BET1 = P_BET3D
        P_GAM1 = P_GAM3D
        XCENT1 = XCENT3D
        YCENT1 = YCENT3D
        XORIG1 = XORIG3D
        YORIG1 = YORIG3D
        XCELL1 = XCELL3D
        YCELL1 = YCELL3D

        NSIZE1 = NCOLS3D * NROWS1

        NMAX = 0
        UMAX = 0
        DO V = 1, NVARS1
            NMAX = MAX( NMAX, LEN_TRIM( VNAME3D( V ) ) )
            UMAX = MAX( UMAX, LEN_TRIM( UNITS3D( V ) ) )
        END DO

        DO V = 1, NVARS1
            MENU( V )(           1:NMAX        ) = VNAME3D( V )( 1:NMAX )
            MENU( V )( NMAX+1     :NMAX+2      ) = '('
            MENU( V )( NMAX+3     :NMAX+UMAX+2 ) = UNITS3D( V )( 1:UMAX )
            MENU( V )( NMAX+UMAX+3:NMAX+UMAX+5 ) = ') :'
            MENU( V )( NMAX+UMAX+6:            ) = VDESC3D( V )
            MENU( V )(          70:         72 ) = '...'
        END DO

    END IF              !  if not.open3(INFILE...); else...

    PDEV = GETEFILE( 'POINTS', .TRUE., .TRUE., PNAME )
    IF ( PDEV .LT. 0 ) THEN
        EFLAG = .TRUE.
        CALL M3MESG( 'ERROR:  Could not open file "REPORT"'  )
    ELSE

        NPNTS = 0
        DO K = 1, MXPNT

            READ( PDEV, *, END=99, IOSTAT=ISTAT ) YLAT(K), XLON(K)
            IF ( ISTAT .NE. 0 ) THEN
                EFLAG = .TRUE.
                WRITE( MESG, '( A, I9, 2X, A, I4 )' ) 'Error IOSTAT=', ISTAT, 'reading POINTS at line', K
                CALL M3MESG( MESG )
            END IF
                    
            NPNTS = K

        END DO
99      CONTINUE

        CALL XY2XY( GDTYP1, P_ALP1, P_BET1, P_GAM1, XCENT1, YCENT1,      &
                    LATGRD3, 0.0D0,  0.0D0,  0.0D0,  0.0D0,  0.0D0,      &
                    NPNTS, XLON, YLAT, XX, YY )

        CALL M3MESG( BAR )
        CALL M3MESG( 'Interpolation Points and Grid-Normal coordinates' )
        DO K = 1, NPNTS
            XX(K) = ( XX(K) - XORIG1 )/XCELL1       !! grid-normal coords
            YY(K) = ( YY(K) - YORIG1 )/YCELL1
            WRITE( MESG, '( I3, A, 4( F14.7, :, 2X, A ) )' ) K, ': LAT=', YLAT(K), 'LON=', XLON(K), 'C=', XX, 'R=', YY(K)
            CALL M3MESG( MESG )
        END DO
        CALL M3MESG( BAR )

        IF ( NPNTS .EQ. 0 ) THEN
            EFLAG = .TRUE.
            CALL M3MESG( 'ERROR:  No points entered' )
        END IF

    END IF


    RDEV = GETEFILE( 'REPORT', .FALSE., .TRUE., PNAME )
    IF ( RDEV .LT. 0 ) THEN
        EFLAG = .TRUE.
        CALL M3MESG( 'ERROR:  Could not open file "REPORT"'  )
    END IF

    HEADER = ENVYN( 'HEADER', 'ASCII header on ${REPORT}?', .FALSE., ISTAT )
    IF ( ISTAT .GT. 0 ) THEN
        EFLAG = .TRUE.
        CALL M3MESG( 'Bad environment variable "HEADER"' )
    END IF


    IF ( EFLAG ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'Fatal file-related error(s)', 2 )
    END IF


    ALLOCATE( RBUF( NSIZE1 ),  STAT = ISTAT )
    IF ( ISTAT .NE. 0 ) THEN
        WRITE( MESG, '( A, I10 )' ) 'ERROR:  Allocation failure for work array:  STATUS=', ISTAT
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
    END IF


    !!...............  Get date&time, probe-point specs:

    CALL RUNSPEC( 'INFILE', .FALSE., SDATE, STIME, TSTEP, NRECS )

    CALL M3MESG( BLANK )
    CALL M3MESG( 'Available variables are:' )
    CALL M3PARAG( NVARS1, MENU )
    CALL GETSTR( 'Enter name of requested variable', VNAME3D( 1 ), VNAME )
    V = INDEX1( VNAME, NVARS1, VNAME3D )
    IF ( V .LE. 0 ) THEN
        EFLAG = .TRUE.
        MESG  = 'ERROR:  Variable "' // TRIM( VNAME ) // '" not available from "INFILE"'
        CALL M3MESG( MESG )
    ELSE IF ( VTYPE3D( V ) .NE. M3REAL ) THEN
        EFLAG = .TRUE.
        MESG  = 'ERROR:  Variable "' // TRIM( VNAME ) // '" not of type REAL'
        CALL M3MESG( MESG )
    END IF

    CALL M3MESG( BLANK )
    IF ( NLAYS1 .GT. 1 ) THEN
        LAY2 = GETVAL( 1, NLAYS1, 1, 'Enter LAYER for this point' )
    ELSE
        LAY2 = 1
    END IF



    IF ( EFLAG) THEN
        CALL M3EXIT( PNAME, 0, 0, 'ERROR:  Bad set-up', 2 )
    END IF


    !!...............  Compute bilinear transform-matrix:

    CALL PNTS2INDX( GDTYP1, P_ALP1, P_BET1, P_GAM1, XCENT1, YCENT1,      &
                    LATGRD3, 0.0D0,  0.0D0,  0.0D0,  0.0D0,  0.0D0,      &
                    NCOLS1, NROWS1, XORIG1, YORIG1, XCELL1, YCELL1,      &
                    NPNTS, XLON, YLAT, INDX, XFAC, YFAC )

    !!...............  Processing loop

    IF ( HEADER ) THEN
        JDATE = SDATE
        JTIME = STIME
        WRITE( RDEV, '(1X, A)' ) BAR
        WRITE( RDEV, '(1X, A)' ) 'Interpolation Points and Grid-Normal Coordinates'
        DO K = 1, NPNTS
            WRITE( RDEV, '( I4, A, 4( F14.7, :, 2X, A ) )' ) K, ': LAT=', YLAT(K), 'LON=', XLON(K), 'C=', XX, 'R=', YY(K)
        END DO
        WRITE( RDEV, '( 1X, A)' ) BAR
        WRITE( RDEV, '( 16X, 256( 2X, I14, : ) )' ) ( K, K = 1, NPNTS )
    END IF

    DO N = 1, NRECS

        WRITE( MESG, '( A, I9.7, A, I6.6 )' ) 'Processing', JDATE, ':', JTIME
        CALL M3MESG( MESG )

        IF ( .NOT.READ3( 'INFILE', VNAME, ALLAYS3, JDATE, JTIME, RBUF ) ) THEN
            EFLAG = .TRUE.
        ELSE
            CALL INDXMULT( NPNTS, NCOLS1, NROWS1, INDX, XFAC, YFAC, VBUF, RBUF )
            WRITE( RDEV, '( I9.7, A, I6.6, 256( 2X, 1PE14.6, : ) )' )     &
                JDATE, ':', JTIME, ( RBUF( K ), K = 1, NPNTS )
        END IF

        CALL NEXTIME( JDATE, JTIME, TSTEP )

    END DO          !!  end loop on time steps for this variable


    IF ( EFLAG ) THEN
        MESG  = 'Failure in program'
        ISTAT = 2
    ELSE
        MESG  = 'Success in program'
        ISTAT = 0
    END IF

    CALL M3EXIT( PNAME, 0, 0, MESG, ISTAT )


END PROGRAM GRIDPROBE


