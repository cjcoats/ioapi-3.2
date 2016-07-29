
PROGRAM M3MASK

    !!***********************************************************************
    !!  Program body starts at line   62
    !!
    !!  DESCRIPTION:
    !!      Read and grid ASCII "mask" file, that contains <col,row>
    !!      locations of mask cells.
    !!
    !!  PRECONDITIONS REQUIRED:
    !!       see splash screen
    !!
    !!  REVISION  HISTORY:
    !!      Prototype  7/2016 by Carlie J. Coats, Jr., UNC IE
    !!***********************************************************************

    USE M3UTILIO
    USE MODGCTP

    IMPLICIT NONE

    !!...........   PARAMETERS and their descriptions:

    CHARACTER*16, PARAMETER ::   PNAME  = 'M3MASK'   
    CHARACTER*16, PARAMETER ::   BLANK  = ' '
    CHARACTER*64, PARAMETER ::   BAR    = &
    '-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-'

    !!...........   LOCAL VARIABLES and their descriptions:

    INTEGER         LDEV, ISTAT
    INTEGER         COL, ROW, L
    REAL*8          YLAT, XLON, XX, YY, DDX1, DDY1
    LOGICAL         COLROW
    LOGICAL         EFLAG
    CHARACTER*256   MESG

    INTEGER         NCOLS1      ! number of grid columns
    INTEGER         NROWS1      ! number of grid rows
    INTEGER         NLAYS1      ! number of layers
    INTEGER         NTHIK1      ! bdy thickness
    INTEGER         GDTYP1      ! grid type:  1=LAT-LON, 2=UTM, ...
    REAL*8          P_ALP1      ! first, second, third map
    REAL*8          P_BET1      ! projection descriptive
    REAL*8          P_GAM1      ! parameters.
    REAL*8          XCENT1      ! lon for coord-system X=0
    REAL*8          YCENT1      ! lat for coord-system Y=0
    REAL*8          XORIG1      ! X-coordinate origin of grid (map units)
    REAL*8          YORIG1      ! Y-coordinate origin of grid
    REAL*8          XCELL1      ! X-coordinate cell dimension
    REAL*8          YCELL1      ! Y-coordinate cell dimension

    
    !!  Structures for mask data:

    INTEGER, ALLOCATABLE :: MASK( :,: )

    !!***********************************************************************
    !!   begin body of program M3MASK

    LDEV = INIT3()
    EFLAG  = .FALSE.         !!  no errors found yet
    WRITE( *, '( 5X, A )' )  BLANK, BAR,                                        &
'Program "M3MASK" to read ASCII <COL,ROW> or <LON.LAT> mask-cell data and',     &
'a grid-definition from the header of a GRIDDED M3IO input file LLFILE. and',   &
'create a "MASK" output file on the same grid as LLFILE.',                      &
'',                                                                             &
'PRECONDITIONS REQUIRED: ',                                                     &
'      setenv COLROW      <"Y" for col-row input, "N" for Lat-Lon input>',      &
'      setenv MASKDATA    <path-name for ASCII mask   input file>',             &
'      setenv LLFILE      <path-name for M3IO gridded input file>',             &
'                         (e.g., GRID_CRO_2D)',                                 &
'      setenv MASKFILE    <path-name for M3IO output file',                     &
'',                                                                             &
'      If COLROW, then MASKDATA is list-formatted, and has lines of the form:', &
'',                                                                             &
'           <col> <row>',                                                       &
'',                                                                             &
'      Otherwise, MASKDATA has lines of the form',                              &
'',                                                                             &
'           <LAT> <LON> ',                                                      &
'',                                                                             &
'      If LLFILE has Lat-Lon coordinates, then they MUST BE COMPLIANT WITH',    &
'      the ISO STANDARD 6709 (-180 <=LON <= 180), instead of following the',    &
'      WMO so-called "standard".',                                              &
'',                                                                             &
'Program copyright (C) 2016 UNC Institute for the Environment.',                &
'Released under Version 2 of the GNU General Public License. See',              &
'enclosed GPL.txt, or URL',                                                     &
''  ,                                                                           &
'    https://www.gnu.org/licenses/old-licenses/gpl-2.0.html',                   &
''  ,                                                                           &
'Comments and questions are welcome and can be sent to'  ,                      &
'',                                                                             &
'    Carlie J. Coats, Jr.    cjcoats@email.unc.edu',                            &
'    UNC Institute for the Environment',                                        &
'    100 Europa Dr., Suite 490 Rm 405',                                         &
'    Campus Box 1105',                                                          &
'    Chapel Hill, NC 27599-1105',                                               &
'',                                                                             &
'Program version: ',                                                            &
'$Id: m3mask.f90 407 2016-07-29 20:06:16Z coats $',&
BAR, ''

    IF ( .NOT. GETYN( 'Continue with program?', .TRUE. ) ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'Program terminated at user request', 2 )
    END IF


    !!...........   Open and describe input files:

    COLROW = ENVYN( 'COLROW', 'Input MASKDATA has COL-ROW, not LAT-LONM?', .TRUE., ISTAT )
    IF ( ISTAT .GT. 0 ) THEN
        EFLAG = .TRUE.
        CALL M3MESG( 'Bad environment variable "COLROW"' )
    END IF

    LDEV = GETEFILE( 'MASKDATA', .TRUE., .TRUE., PNAME )
    IF ( LDEV .LT. 0 ) THEN
        EFLAG = .TRUE.
        CALL M3MESG( 'Could not open "MASKDATA"' )
    END IF

    IF ( .NOT. OPEN3( 'LLFILE', FSREAD3, PNAME ) ) THEN
        EFLAG = .TRUE.
        CALL M3WARN( PNAME, 0, 0, 'Failure opening "LLFILE"' )
    ELSE IF ( .NOT. DESC3( 'LLFILE' ) ) THEN
        EFLAG = .TRUE.
        CALL M3WARN( PNAME, 0, 0, 'Failure getting file description for "LLFILE"' )
    ELSE IF ( FTYPE3D .NE. GRDDED3 ) THEN
        EFLAG = .TRUE.
        CALL M3WARN( PNAME, 0, 0, 'Error:  non-GRIDDED "LLFILE"' )
    ELSE
        NCOLS1 = NCOLS3D
        NROWS1 = NROWS3D
        NLAYS1 = NLAYS3D
        NTHIK1 = NTHIK3D
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
        DDX1   = 1.0D0 / XCELL1
        DDY1   = 1.0D0 / YCELL1
    END IF


    IF ( EFLAG ) THEN
        MESG = 'Fatal input file error(s)'
        CALL M3EXIT( PNAME, 0, 0,MESG, 2 )
    END IF


    !!...........   Allocate working arrays:  dims from DESC3() call

    ALLOCATE( MASK( NCOLS3D, NROWS3D ), STAT = ISTAT )

    IF ( ISTAT .NE. 0 ) THEN
        WRITE( MESG, '( A, I10 )' ) 'Allocation failure:  STATUS=', ISTAT
        EFLAG = .TRUE.
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
    END IF

    VNAME3D( 1 ) = 'MASK'
    VTYPE3D( 1 ) = M3INT
    UNITS3D( 1 ) = 'none'
    VDESC3D( 1 ) = '1==mask region, 0==outside-mask region'
    NVARS3D = 1
    SDATE3D = 0
    STIME3D = 0
    TSTEP3D = 0
    NLAYS3D = 1

    IF ( .NOT. OPEN3( 'MASKFILE', FSUNKN3, PNAME ) ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'Failure opening "MASKFILE"', 2 )
    END IF


    !!...........   Create mask:  read and process input file

    MASK = 0        !!  default, array assignment
    
    
    IF ( COLROW ) THEN

        DO L = 1, 999999999

            READ( LDEV, *, END=99, IOSTAT=ISTAT )  COL, ROW
            IF ( ISTAT .NE. 0 ) THEN
                WRITE( MESG, '( A, I9, A, I10 )' ) 'Error reading COL ROW from "MASKDATA" at line', L, ': STATUS=', ISTAT
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
                CYCLE
            END IF

            IF ( COL .LT. 1 .OR. COL .GT. NCOLS3D .OR.     &
                 ROW .LT. 1 .OR. ROW .GT. NROWS3D  ) THEN
                WRITE( MESG, '( A, I5, 2X, A, I5 )' ) '<COL=', COL, 'ROW=', ROW, '> out of grid bounds'
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            ELSE
                MASK( COL,ROW ) = 1
            END IF

        END DO
    
    ELSE        !!  Lat-Lon input data:

        DO L = 1, 999999999

            READ( LDEV, *, END=99, IOSTAT=ISTAT )  YLAT, XLON
            IF ( ISTAT .NE. 0 ) THEN
                WRITE( MESG, '( A, I9, A, I10 )' ) 'Error reading LAT LON from "MASKDATA" at line', L, ': STATUS=', ISTAT
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
                CYCLE
            END IF

            CALL XY2XY( GDTYP1, P_ALP1, P_BET1, P_GAM1, XCENT1, YCENT1,     &
                        LATGRD3, 0.0D0,  0.0D0,  0.0D0,  0.0D0,  0.0D0,     &
                        XLON, YLAT, XX, YY )
            XX  = DDX1 * ( XX - XORIG1 ) 
            YY  = DDY1 * ( YY - YORIG1 )
            COL = 1 + INT( XX )
            ROW = 1 + INT( YY )

            IF ( COL .LT. 1 .OR. COL .GT. NCOLS3D .OR.     &
                 ROW .LT. 1 .OR. ROW .GT. NROWS3D  ) THEN
                WRITE( MESG, '( A, F11.6, 2X, A, F11.6, A )' )  &
                    'WARNING:  <LAT=', YLAT, 'LON=', XLON, '> out of grid bounds'
                CALL M3MESG( MESG )
            ELSE
                MASK( COL,ROW ) = 1
            END IF

        END DO
    
    END IF
    
99  CONTINUE        !!  target of "END="  loop exit


    IF ( EFLAG ) THEN
        MESG = 'Fatal input data error(s)'
        CALL M3EXIT( PNAME, 0, 0,MESG, 2 )
    END IF
    
    IF ( .NOT. WRITE3( 'MASKFILE', 'MASK', 0, 0, MASK )  ) THEN
        MESG = 'Fatal output error(s)'
        CALL M3EXIT( PNAME, 0, 0,MESG, 2 )
    END IF


    CALL M3EXIT( PNAME, 0, 0, 'Success in program', 0 )


END PROGRAM M3MASK


    
