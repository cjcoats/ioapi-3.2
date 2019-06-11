
PROGRAM WNDWDESC

    !!***************************************************************
    !! Version "$Id: wndwdesc.f90 115 2019-06-11 21:11:40Z coats $"
    !! EDSS/Models-3 M3TOOLS.
    !! Copyright (C) 1992-2002 MCNC, 
    !! (C) 1995-2002, 2005-2013 Carlie J. Coats, Jr.,
    !! (C) 2002-2010 Baron Advanced Meteorological Systems. LLC., and 
    !! (C) 2015- UNC Institute for the Environment.
    !! Distributed under the GNU GENERAL PUBLIC LICENSE version 2
    !! See file "GPL.txt" for conditions of use.
    !!..............................................................
    !!  program body starts at line  99
    !!
    !!  DESCRIPTION:
    !!      See splash screen
    !!
    !!  PRECONDITIONS:
    !!      See splash screen
    !!
    !!  REVISION  HISTORY:
    !!      Prototype  06/20019 by Carlie J. Coats, Jr.,UNC IE
    !!***************************************************************

    USE M3UTILIO
    USE MODGCTP
    IMPLICIT NONE


    !!......  PARAMETERS and their descriptions:

    CHARACTER*16, PARAMETER ::  PNAME = 'WNDWDESC'
    CHARACTER*16, PARAMETER ::  QUOTE = "'"
    CHARACTER*16, PARAMETER ::  BLANK = ' '
    CHARACTER*64, PARAMETER ::  BAR   = &
    '-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-'

    !!...........   EXTERNAL FUNCTIONS and their descriptions:

    INTEGER :: IARGC              !  may be intrinsic...


    !!......  LOCAL VARIABLES and their descriptions:

    INTEGER         ARGCNT, LDEV, GDEV, ISTAT, IMISS
    INTEGER         COL1, ROW1, COL2, ROW2, K
    REAL*8          LAT1, LON1, LAT2, LON2, XX1, YY1, XX2, YY2
    LOGICAL         EFLAG

    CHARACTER*16    GNAME      ! grid name
    CHARACTER*16    WNAME      ! window sub-grid name
    CHARACTER*16    CNAME      ! coordinate system name
    INTEGER         NCOLS      ! number of grid columns
    INTEGER         NROWS      ! number of grid rows
    INTEGER         NTHIK      ! bdy thickness
    INTEGER         GDTYP      ! grid type:  1=LAT-LON, 2=Lambert, ...
    REAL*8          P_ALP      ! first, second, third map
    REAL*8          P_BET      ! projection descriptive
    REAL*8          P_GAM      ! parameters.
    REAL*8          XCENT      ! lon for coord-system X=0
    REAL*8          YCENT      ! lat for coord-system Y=0
    REAL*8          XORIG      ! X-coordinate origin of grid (map units)
    REAL*8          YORIG      ! Y-coordinate origin of grid
    REAL*8          XCELL      ! X-coordinate cell dimension
    REAL*8          YCELL      ! Y-coordinate cell dimension

    INTEGER         NCOLSW, NROWSW
    REAL*8          XORIGW, YORIGW
    CHARACTER*256   MESG        !  buffer for m3exit(), etc
    CHARACTER*256   ENVBUF      !  value from command line arguments

    !!--------------------------------------------------------------
    !!   begin body of program WNDWDESC

    LDEV   = INIT3()
    ARGCNT = IARGC()
    EFLAG  = .FALSE.

    WRITE( LDEV, '( 5X, A )' )  BLANK, BAR,                                 &
'Program WNDWDESC to compute corners and write grid description for a',     &
'Lat-Lon described window ${WNDWNAME} into an I/O API grid ${GRIDNAME}.',   &
'',                                                                         &
'PRECONDITIONS REQUIRED:',                                                  &
'    setenv GRIDDESC   <path name for  input grid description file>',       &
'    setenv WNDWDESC   <path name for output grid description file>',       &
'',                                                                         &
'USAGE:',                                                                   &
'    wndwdesc LAT1 LON1 LAT2 LON2 GRIDNAME WNDWNAME',                       &
'',                                                                         &
'',                                                                         &
'See URL',                                                                  &
'',                                                                         &
'   https://www.cmascenter.org/ioapi/documentation/3.1/html/AA.html#tools', &
'',                                                                         &
'Copyright (C) 2019 UNC Institute for the Environment.',                    &
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
'$Id: wndwdesc.f90 115 2019-06-11 21:11:40Z coats $',&
''

    IF ( ARGCNT .NE. 6 ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'USAGE ERROR', 2 )
    END IF

    CALL GETARG( 1, ENVBUF )
    READ( ENVBUF, *, IOSTAT=ISTAT )LAT1
    IF ( ISTAT .NE. 0 ) THEN
        WRITE( MESG, '( A, I10 )' ) 'Error reading LAT1.  ISTAT=', ISTAT
        CALL M3MESG( MESG )
        EFLAG = .TRUE.
    END IF

    CALL GETARG( 2, ENVBUF )
    READ( ENVBUF, *, IOSTAT=ISTAT )LON1
    IF ( ISTAT .NE. 0 ) THEN
        WRITE( MESG, '( A, I10 )' ) 'Error reading LON1.  ISTAT=', ISTAT
        CALL M3MESG( MESG )
        EFLAG = .TRUE.
    END IF

    CALL GETARG( 3, ENVBUF )
    READ( ENVBUF, *, IOSTAT=ISTAT )LAT2
    IF ( ISTAT .NE. 0 ) THEN
        WRITE( MESG, '( A, I10 )' ) 'Error reading LAT2.  ISTAT=', ISTAT
        CALL M3MESG( MESG )
        EFLAG = .TRUE.
    END IF

    CALL GETARG( 4, ENVBUF )
    READ( ENVBUF, *, IOSTAT=ISTAT )LON2
    IF ( ISTAT .NE. 0 ) THEN
        WRITE( MESG, '( A, I10 )' ) 'Error reading LON2.  ISTAT=', ISTAT
        CALL M3MESG( MESG )
        EFLAG = .TRUE.
    END IF

    CALL GETARG( 5, GNAME )
    CALL GETARG( 6, WNAME )
    IF ( .NOT.DSCGRID( GNAME, CNAME,                                    &
                       GDTYP, P_ALP, P_BET, P_GAM, XCENT, YCENT,        &
                       XORIG, YORIG, XCELL, YCELL, NCOLS, NROWS, NTHIK ) ) THEN
        MESG = 'Grid "'// TRIM( GNAME ) // '" not found in GRIDDESC file'
        CALL M3MESG( MESG )
        EFLAG = .TRUE.
    END IF

    IF ( EFLAG ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'Fatal setup error(s)', 2 )
    END IF


    CALL XY2XY( GDTYP, P_ALP, P_BET, P_GAM, XCENT, YCENT,       &
                LATGRD3, 0.0D0,  0.0D0,  0.0D0,  0.0D0,  0.0D0, &
                LON1, LAT1, XX1, YY1 )

    XX1  = ( XX1 - XORIG ) / XCELL
    COL1 = INT( XX1 )
    IF ( XX1 .LT. 0.0d0 ) THEN
        CALL M3MESG( 'LON1 > XORIG1 out-of-bounds ' )
        EFLAG = .TRUE.
    ELSE IF ( COL1 .GT. NCOLS ) THEN
        CALL M3MESG( 'LON1 > XORIG1+(NCOLS-1)*XCELL out-of-bounds ' )
        EFLAG = .TRUE.
    END IF

    YY1  = ( YY1 - YORIG ) / YCELL
    ROW1 = INT( YY1 )
    IF ( YY1 .LT. 0.0d0 ) THEN
        CALL M3MESG( 'LON1 > YORIG1 out-of-bounds ' )
        EFLAG = .TRUE.
    ELSE IF ( ROW1 .GT. NROWS ) THEN
        CALL M3MESG( 'LON1 > YORIG1+(NROWS-1)*YCELL out-of-bounds ' )
        EFLAG = .TRUE.
    END IF

    CALL XY2XY( GDTYP, P_ALP, P_BET, P_GAM, XCENT, YCENT,       &
                LATGRD3, 0.0D0,  0.0D0,  0.0D0,  0.0D0,  0.0D0, &
                LON2, LAT2, XX2, YY2 )

    XX2  = ( XX2 - XORIG ) / XCELL
    COL2 = INT( XX2 )
    IF ( XX2 .LT. 0.0d0 ) THEN
        CALL M3MESG( 'LON2 > XORIG2 out-of-bounds ' )
        EFLAG = .TRUE.
    ELSE IF ( COL2 .GT. NCOLS ) THEN
        CALL M3MESG( 'LON2 > XORIG2+(NCOLS-2)*XCELL out-of-bounds ' )
        EFLAG = .TRUE.
    END IF

    YY2  = ( YY2 - YORIG ) / YCELL
    ROW2 = INT( YY2 )
    IF ( YY2 .LT. 0.0d0 ) THEN
        CALL M3MESG( 'LON2 > YORIG2 out-of-bounds ' )
        EFLAG = .TRUE.
    ELSE IF ( ROW2 .GT. NROWS ) THEN
        CALL M3MESG( 'LON2 > YORIG2+(NROWS-2)*YCELL out-of-bounds ' )
        EFLAG = .TRUE.
    END IF

    IF ( EFLAG ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'Fatal cordinate error(s)', 2 )
    END IF

    IF ( COL1 .GT. COL2 ) THEN
        K    = COL1
        COL1 = COL2
        COL2 = K
    END IF

    IF ( ROW1 .GT. ROW2 ) THEN
        K    = ROW1
        ROW1 = ROW2
        ROW2 = K
    END IF
    XORIGW = XORIG + DBLE( COL1-1 )*XCELL
    YORIGW = YORIG + DBLE( ROW1-1 )*YCELL
    NCOLSW = COL2 - COL1 + 1
    NROWSW = ROW2 - ROW1 + 1

    WRITE( LDEV, '(A, 2I10 )' ) 'SW CORNER (C,R)=', COL1, ROW1
    WRITE( LDEV, '(A, 2I10 )' ) 'NE CORNER (C,R)=', COL2, ROW2
    
    GDEV = GETEFILE( 'WNDWDESC', .FALSE., .TRUE., PNAME )
    IF ( GDEV .LT. 0 ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'Error opening "'//TRIM( WNAME ) // '"', 2 )
    END IF
    WRITE( GDEV, '( A, /, 3A, /, I4, 5 (1PD24.17) )' )                              &
        '!  coords --2 lines:  name; type, P-alpha, P-beta, P-gamma, xcent, ycent', &
        QUOTE, TRIM( CNAME ), QUOTE,                                                &
        GDTYP, P_ALP, P_BET, P_GAM, XCENT, YCENT
    WRITE( GDEV, '(4A, /)' )                                    &
        QUOTE, BLANK, QUOTE, '                   ! end coords'

    WRITE( GDEV, '(3A, /, 5(1PD24.17), 3I8 )' )                 &
        QUOTE, TRIM( WNAME) , QUOTE,                            &
        XCELL, YCELL, XORIGW, YORIGW, NCOLSW, NROWSW, NTHIK
    WRITE( GDEV, '(4A, /)' )                                    &                       
        QUOTE, BLANK, QUOTE, '                   ! end grids'

    IF ( EFLAG ) THEN
        MESG  = 'Failure in program'
        ISTAT = 2
    ELSE
        MESG  = 'Success in program'
        ISTAT = 0
    END IF

    CALL M3EXIT( PNAME, 0, 0, MESG, ISTAT )


END PROGRAM WNDWDESC
