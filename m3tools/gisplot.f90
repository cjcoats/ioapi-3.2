

PROGRAM GISPLOT

    !!*******************************************************************
    !!  Version "$Id: gisplot.f90 280 2025-04-12 15:34:39Z coats $"
    !!  Copyright (c) 2010 Baron Advanced Meteorological Systems.
    !!  and (C) 2013-2025 Carlie J. Coats, Jr.,
    !!  Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2
    !!  See file "LGPL.txt" for conditions of use.
    !!..................................................................
    !!  program body        starts at line   224
    !!  subroutine RDCONFIG starts at line   706
    !!  subroutine RDFILE   starts at line  1255
    !!
    !!  DESCRIPTION:
    !!      Create tile-plots for variables in a sequence of gridded GIS files
    !!      Plot types may be GIF, JPG, or PNG
    !!
    !!  PRECONDITIONS:
    !!      Parameter-consistency with "gdplot.c"
    !!      For use, see splash screen.
    !!
    !!  REVISION  HISTORY:
    !!      Prototype  8/2010 by Carlie J. Coats, Jr., BAMS:
    !!
    !!      Versions  10/2010 by CJC:
    !!      TITLE* bugfix; change TITLE* init to support blank title lines;
    !!      compatible with vector/obs/contour-plot version of "gdplot.c"
    !!
    !!      Version   09/2012 by CJC:   map-thickness capability
    !!
    !!      Version    4/2025 by CJC for I/O API M3Tools version 4.0
    !!      mods for all I/O API supported map projections
    !!
    !!      Bug-fix 4/16/2025:  F Sidi points out double declaration of
    !!      PARAMETER BLANK
    !!*******************************************************************


    USE M3UTILIO
    IMPLICIT NONE

    !!......  PARAMETERS and their descriptions:

    INTEGER, PARAMETER :: MXSCALE = 250
    INTEGER, PARAMETER :: MXBIN   =  50

    INTEGER, PARAMETER :: ALLPLT  =   0
    INTEGER, PARAMETER :: XSIZED  =   1
    INTEGER, PARAMETER :: YSIZED  =   2

    INTEGER, PARAMETER :: GIFIMG  =   1
    INTEGER, PARAMETER :: JPGIMG  =   2
    INTEGER, PARAMETER :: PNGIMG  =   3

    INTEGER, PARAMETER :: TILED   =   1
    INTEGER, PARAMETER :: SMOOTH  =   2

    INTEGER, PARAMETER :: LINSCL  =   1
    INTEGER, PARAMETER :: LOGSCL  =   2
    INTEGER, PARAMETER :: EXPSCL  =   3

    CHARACTER*1,  PARAMETER :: QUOTD = '"'      !  "double-quote"
    CHARACTER*1,  PARAMETER :: QUOTE = ''''     !  "single-quote"
    CHARACTER*1,  PARAMETER :: BLANK = ' '

    INTEGER, PARAMETER :: BIL   =  1
    INTEGER, PARAMETER :: BIL2  =  2
    INTEGER, PARAMETER :: BIL4  =  3
    INTEGER, PARAMETER :: FLT   =  4
    INTEGER, PARAMETER :: ZBIL  =  5
    INTEGER, PARAMETER :: ZBIL2 =  6
    INTEGER, PARAMETER :: ZBIL4 =  7
    INTEGER, PARAMETER :: ZFLT  =  8
    INTEGER, PARAMETER :: ARCI  =  9
    INTEGER, PARAMETER :: ARCR  = 10
    INTEGER, PARAMETER :: ASCI  = 11
    INTEGER, PARAMETER :: ASCR  = 12

    INTEGER     , PARAMETER ::  NTYPES = 12
    CHARACTER*16, PARAMETER ::  ATYPES( NTYPES ) = (/ 'BIL  ', 'BIL2 ', 'BIL4 ', 'FLT  ',           &
                                                      'ZBIL ', 'ZBIL2', 'ZBIL4', 'ZFLT ',           &
                                                      'ARCI ', 'ARCR ', 'ASCI ', 'ASCR '  /)
    INTEGER     , PARAMETER ::  VTYPES( NTYPES ) = (/  M3INT,   M3INT,   M3INT,  M3REAL,            &
                                                       M3INT,   M3INT,   M3INT,  M3REAL,            &
                                                       M3INT,   M3REAL,  M3INT,  M3REAL   /)

    CHARACTER*16, PARAMETER :: PNAME = 'GISPLOT'
    CHARACTER*64, PARAMETER :: BAR   = '-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-'


    !!......  LOCAL VARIABLES and their descriptions:

    INTEGER     LDEV, ISTAT
    INTEGER     C, R, F,  I, J, K, L, N, V, V1
    INTEGER     JDATE, JTIME, TSTEP
    LOGICAL     EFLAG, AFLAG

    CHARACTER*16    CNAME

    !!  Grid description:

    CHARACTER*16    GDNAM1      ! grid name
    INTEGER         NCOLS1      ! number of input-grid columns
    INTEGER         NROWS1      ! number of input-grid rows
    INTEGER         NLAYS1      ! number of input-grid layers
    INTEGER         NTHIK1
    INTEGER         NSIZE1      ! number of input 2D-grid cells
    INTEGER         GDTYP1      ! grid type:  1=LAT-LON, 2=UTM, ...
    REAL*8          P_ALP1      ! first, second, third map
    REAL*8          P_BET1      ! projection descriptive
    REAL*8          P_GAM1      ! parameters.
    REAL*8          XCENT1      ! lon for coord-system X=0
    REAL*8          YCENT1      ! lat for coord-system Y=0
    REAL*8          XORIG1      ! X-coordinate origin of grid (map units)
    REAL*8          YORIG1      ! Y-coordinate origin of grid
    REAL*8          XFINL1      ! X-coordinate origin of grid (map units)
    REAL*8          YFINL1      ! Y-coordinate origin of grid
    REAL*8          XCELL1      ! X-coordinate cell dimension
    REAL*8          YCELL1      ! Y-coordinate cell dimension

    !! Run-control variables:

    INTEGER ::  PLOTONLY = ALLPLT
    INTEGER ::  IMGTYPE  = GIFIMG       !  GIFIMG | JPGIMG | PNGIMG
    INTEGER ::  IMGMTHD  = TILED        !  TILED | SMOOTHED

        INTEGER ::  COL0,  COL1,  ROW0,  ROW1
    INTEGER ::  NXPIX = 960     !  width  of total image
    INTEGER ::  NYPIX = 720     !  height of total image
    INTEGER ::  NXGRD, NYGRD    !  size of tile-grid within image
    INTEGER ::  XGRD0, YGRD0    !  upper-left corner of tile-grid within image

    LOGICAL ::  DTFLAG

    INTEGER ::  NRGB = 10
    INTEGER ::  RGB( 3, MXSCALE )
    DATA RGB( 1:3,1:10 ) /   0,   0, 204,       &
                             0, 102, 255,       &
                             0, 204, 255,       &
                             0, 255, 255,       &
                             0, 204,  51,       &
                           153, 102,   0,       &
                           255, 255,   0,       &
                           255, 204,   0,       &
                           255, 153,   0,       &
                           255,   0,   0   /
    INTEGER ::  MISS( 3 ) = (/ 127, 127, 127 /)
    INTEGER ::  RGB1( 3 ) = (/   0,   0,   0 /)
    INTEGER ::  RGB2( 3 ) = (/  31,  31,  31 /)
    INTEGER ::  RGB3( 3 ) = (/  63,  63,  63 /)
    INTEGER ::  THK1      =      1
    INTEGER ::  THK2      =      1
    INTEGER ::  THK3      =      1

    INTEGER ::  NVRGB = 2
    INTEGER ::  VRGB( 3, MXSCALE ) = 0

    INTEGER ::  NZRGB = 2
    INTEGER ::  ZRGB( 3, MXSCALE ) = 0

    INTEGER :: NVARS   = 0
    LOGICAL :: VARPLOT = .FALSE.
    REAL    :: VMISS   = BADVAL3       !  scale-max for variable
    REAL    :: VMAX    = -9.999E36     !  scale-max for variable
    REAL    :: VMIN    =  9.999E36     !  scale-min for variable
    REAL    :: VFAC    =  1.0          !  scale-factor for variable

    REAL    :: SMIN  =  9.999E36
    REAL    :: SMAX  = -9.999E36
    REAL    :: SBAR  =  BADVAL3
    INTEGER :: CVMIN = IMISS3
    INTEGER :: CVMAX = IMISS3
    INTEGER :: RVMIN = IMISS3
    INTEGER :: RVMAX = IMISS3

    REAL    :: ZMIN  =  9.999E36
    REAL    :: ZMAX  = -9.999E36
    REAL    :: ZBAR  =  BADVAL3
    INTEGER :: CZMIN = IMISS3
    INTEGER :: CZMAX = IMISS3
    INTEGER :: RZMIN = IMISS3
    INTEGER :: RZMAX = IMISS3

    INTEGER  :: VTYPE   = 0

    CHARACTER*16  :: VNAME   = BLANK
    CHARACTER*16  :: UNITS   = BLANK
    CHARACTER*16  :: ATYPE   = 'PNG'
    INTEGER       :: FTYPE   = 0
    CHARACTER*16  :: BINFMT  = '%g'

    INTEGER ::  NBINS = 6
    REAL    ::  BINS( MXBIN )

    REAL        S, Y, Y0, Y1, UFAC, YFAC, AFAC
    REAL        YMAX, YMIN, YBAR
    REAL*8      YSUM
    INTEGER     IMIN, IMAX, CMAX, CMIN, RMAX, RMIN, NCNT, OCNT

    CHARACTER*4   :: SUFFIX
    CHARACTER*24  :: DTBUF
    CHARACTER*64  :: BASENAME
    CHARACTER*32  :: MAPNAME1 = 'OUTLSTATES3000'
    CHARACTER*32  :: MAPNAME2 = 'OUTLCOUNTIES'
    CHARACTER*32  :: MAPNAME3 = BLANK
    CHARACTER*256 :: MESG, LINE1, LINE2
    CHARACTER*512 :: EQNAME, EQROOT, MAPDIR, INFILE

    INTEGER                    :: NFILES = 0
    INTEGER      , ALLOCATABLE :: JDATES( : )
    INTEGER      , ALLOCATABLE :: JTIMES( : )
    CHARACTER*16 , ALLOCATABLE :: FNAMES( : )
    CHARACTER*256, ALLOCATABLE :: TITLE1( : )
    CHARACTER*256, ALLOCATABLE :: TITLE2( : )
    CHARACTER*256, ALLOCATABLE :: TITLE3( : )

    REAL,      ALLOCATABLE ::  RBUF( :,: )
    INTEGER  , ALLOCATABLE ::  IBUF( :,: )
    INTEGER*1, ALLOCATABLE ::  JBUF( :,: )
    INTEGER*1, ALLOCATABLE ::  KBUF( :,: )


    !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

    LDEV  = INIT3()
    EFLAG = .FALSE.         !  no errors found yet

    WRITE( LDEV, '( 5X, A )' ) BLANK, BAR,                                      &
'Program GISPLOT to create tile-plots of REAL variables from GRIDDED GIS',      &
'files and save the results as image files, using the "GD" image library.',     &
'',                                                                             &
'Supported input file types are:',                                              &
'',                                                                             &
'    (uncompressed)     BIL  , BIL2 , BIL4 , FLT',                              &
'    (GZIP compressed)  ZBIL , ZBIL2, ZBIL4, ZFLT',                             &
'    (ASCII)            ARCI , ARCR , ASCI , ASCR',                             &
'',                                                                             &
'NOTE:  ARCI, ARCR have header lines with grid parameters',                     &
'',                                                                             &
'            "north", "south" "east", "west", "rows", and "cols",',             &
'',                                                                             &
'while ASCI, ASCR have header lines with',                                      &
'',                                                                             &
'            "ncols", "nrows", "xllcorner", "yllcorner", "cellsize"',           &
'            and optionally "NODATA_value"',                                    &
'',                                                                             &
'Output file formats are GIF, JPEG, and PNG; output file names are of',         &
'the form "${BASENAME}.<VNAME>[.<LLL>].<YYYYDDDHHMMSS.[gif | jpg | png ]"',     &
'where ${BASENAME} may be a partial file-name, a directory, or blank,',         &
'VNAME is the variable-name,  and YYYYDDDHHMMSS is the Julian date&time.',      &
'',                                                                             &
'PRECONDITIONS REQUIRED:',                                                      &
'     setenv GRIDDESC  <path name>',                                            &
'     setenv GRIDNAME  <GRIDDESC name for input grid>',                         &
'     setenv CONFIG    <path name for configuration file>',                     &
'     setenv BASENAME  <base name or directory for output images>',             &
'                      If directory, should end with "/"',                      &
'                      LEN( final image-file names ) < 512',                    &
'     setenv MAPDIR    <directory for maps, or "NONE"> [${EDSS_MAPDIR}]',       &
'     setenv IMGDATES  <use date-&-time instead of record-numbers> [Yes]',      &
''


    WRITE( LDEV, '( 5X, A )' )                                                  &
'PROGRAM CONTROL:',                                                             &
'${CONFIG} is ASCII , with lines to control the program options, of the form',  &
'',                                                                             &
'    "<NAME>        <value(s)>":',                                              &
'',                                                                             &
'with the folowing fields:',                                                    &
'',                                                                             &
'    IMGFORMAT      <GIF | JPG | PNG>  [GIF]',                                  &
'    IMGMETHOD      <TILED | SMOOTHED> [TILED]',                                &
'    WIDTH          <number of pixels wide for the entire image> [960]',        &
'    HEIGHT         <number of pixels high for the entire image> [720]',        &
'    WINDOW_COLS    <first and last grid-col for the plot> [entire grid]',      &
'    WINDOW_ROWS    <first and last grid-row for the plot> [entire grid]',      &
'                   (minimum image-size 240x280)',                              &
'    PLOTONLY       <WIDTH | HEIGHT>  [WIDTH]',                                 &
'                   to suppress legend, and generate tiled-image only,',        &
'                   with image dimensions from WIDTH or HEIGHT.',               &
'    LEGEND_BINS    <number of color-labels on the color-scale legend> [6]',    &
'    COLOR          <index> <red> <green> <blue> for tile/smooth map',          &
'                   where index is consecutive for some range 0 ... N <253 ',   &
'                   and color values are in [0...255]',                         &
'                   [default tile palettye is a 10-color scale,',               &
'                   vector palette is 1-color (black) scale.]',                 &
'    MISSING_COLOR  <red> <green> <blue> [127 127 127]',                        &
'',                                                                             &
'    Map-control variables, for up to 3 maps:',                                 &
'    MAPNAME1       <"states" map-file name in ${MAPDIR} [OUTLHRES]>',          &
'    MAPNAME2       <"county" map-file name in ${MAPDIR} [OUTLCOUNTIES]>',      &
'    MAPNAME3       <"extra"  map-file name in ${MAPDIR} [<blank>]>',           &
'                   Use blank values to suppress these maps.',                  &
'    MAPCOLOR1      <red> <green> <blue> [  0   0   0]',                        &
'    MAPCOLOR2      <red> <green> <blue> [ 31  31  31]',                        &
'    MAPCOLOR3      <red> <green> <blue> [ 63  63  63]',                        &
'',                                                                             &
'    MAPWIDTH1      <line-width (pixels) [1]',                                  &
'    MAPWIDTH2      <line-width (pixels) [1]',                                  &
'    MAPWIDTH3      <line-width (pixels) [1]',                                  &
'',                                                                             &
'    VARIABLE       <vble name>',                                               &
'    UNITS          <units-name> <scale factor> [<units from file> 1]',         &
'    MISSING_VALUE  <"missing" value for these files>',                         &
'    SCALE_RANGE    <min> <max>  for the tile color-scale [data-min,data-max]', &
'    FORMAT_BINS    <"C" format for color-scale legend-labels> ["%g"]',         &
'    GIS_FORMAT     < BIL  | BIL2  | BIL4  |  FLT | ',                          &
'                     ZBIL | ZBIL2 | ZBIL4 | ZFLT |',                           &
'                     ARCI | ARCR  | ASCI  | ASCR >',                           &
'',                                                                             &
'with repeated sections, per timestep plotted:',                                &
'',                                                                             &
'    INFILE         <logical name> <YYYYDDD or 0 [0]>  <HHMMSS or 0 [0]>',      &
'    TITLE1         <First  legend-header line> [VNAME]',                       &
'    TITLE2         <Second legend-header line> [VDESC]',                       &
'    TITLE3         <Third  legend-header line> [${INFILE}]',                   &
'',                                                                             &
'    for each INFILE:',                                                         &
'        setenv <file> <path-name>',                                            &
'        path-names are of the form <root>.[ flt | bil | hdr ] [.gz]',          &
'',                                                                             &
'    Total number of colors in ${CONFIG} should not exceed 253.',               &
'',                                                                             &
'    Input files already on the output map-projection and grid.',               &
'',                                                                             &
'Copyright (C) 2010 Baron Advanced Meteorological Systems, LLC.',               &
'and (c) 2013-2025 Carlie J. Coats, Jr.',                                       &
'Released under Version 2 of the GNU General Public License.',                  &
'See enclosed GPL.txt, or URL',                                                 &
'https://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html',                    &
' ',                                                                            &
'Comments and questions are welcome and can be sent to',                        &
' ',                                                                            &
'    Carlie J. Coats, Jr.    carlie@jyarborough.com',                           &
'',                                                                             &
'Program version:',                                                             &
'$Id: gisplot.f90 280 2025-04-12 15:34:39Z coats $',&
''

    !!...............  Get environment variable :

    CALL ENVSTR( 'GRIDNAME', 'GRIDDESC name for input grid', 'LL_LSM1', GDNAM1, ISTAT )
    IF ( ISTAT .GT. 0 ) THEN
        EFLAG = .TRUE.
        CALL M3MESG( 'Bad environment variable "GRIDNAME"' )
    ELSE IF ( .NOT.DSCGRID( GDNAM1, CNAME, GDTYP1,                      &
                            P_ALP1, P_BET1, P_GAM1, XCENT1, YCENT1,     &
                            XORIG1, YORIG1, XCELL1, YCELL1,             &
                            NCOLS1, NROWS1, NTHIK1 ) ) THEN
        EFLAG = .TRUE.
        CALL M3MESG( '"OUTGRID" not found in GRIDDESC file' )
    ELSE

        COL0 = 1                        !  default window to plot
        COL1 = NCOLS1
        ROW0 = 1
        ROW1 = NROWS1

        NSIZE1 = NCOLS1*NROWS1
        WRITE( MESG, '( 5( A, :, I6      ) )'  ) '"Map projection type=', GDTYP1
        CALL M3MESG( MESG )
        WRITE( MESG, '( 5( A, :, 1PD24.17 ) )' ) '<P_ALP:P_BET:P_GAM>=<', P_ALP1, ':', P_BET1, ':', P_GAM1, '>'
        CALL M3MESG( MESG )
        WRITE( MESG, '( 5( A, :, 1PD24.17 ) )' ) '<XCENT:YCENT>      =<', XCENT1, ':', YCENT1, '>'
        CALL M3MESG( MESG )
        WRITE( MESG, '( 5( A, :, 1PD24.17 ) )' ) '<XORIG:YORIG>      =<', XORIG1, ':', YORIG1, '>'
        CALL M3MESG( MESG )
        WRITE( MESG, '( 5( A, :, 1PD24.17 ) )' ) '<XCELL:YCELL>      =<', XCELL1, ':', YCELL1, '>'
        CALL M3MESG( MESG )
        WRITE( MESG, '( 5( A, :, I6       ) )' ) '<NC   :   NR>      =<', NCOLS1, ':', NROWS1, '>'
        CALL M3MESG( MESG )
        
        CALL INITXY( GDTYP1, P_ALP1, P_BET1, P_GAM1, XCENT1, YCENT1 )

    END IF      !  if envstr() failed; else if dscgrid() failed; else...


    CALL ENVSTR( 'BASENAME', 'base-name for output images', ' ', EQROOT, ISTAT )
    IF ( ISTAT .GT. 0 ) THEN
        EFLAG = .TRUE.
        CALL M3MESG( 'Bad environment variable "BASENAME"' )
    ELSE
        L = LEN_TRIM( EQROOT )
        IF ( EQROOT( L:L ) .NE. '/' ) THEN
            EQROOT( L+1:L+1 ) = '.'
        END IF
    END IF


    DTFLAG = ENVYN( 'IMGDATES', 'Use dates (instead of record numbers) for output-file indexing?', .TRUE., ISTAT )
    IF ( ISTAT .GT. 0 ) THEN
        EFLAG = .TRUE.
        CALL M3MESG( 'Bad environment variable "IMGDATES"' )
    END IF


    CALL NAMEVAL( 'EDSS_MAPDIR', EQNAME )
    CALL ENVSTR( 'MAPDIR', 'Directory for maps', EQNAME, MAPDIR, ISTAT )
    IF ( ISTAT .GT. 0 ) THEN
        EFLAG = .TRUE.
        CALL M3MESG( 'Bad environment variable "MAPDIR"' )
    ELSE IF ( MAPDIR .NE. 'NONE' ) THEN
        L = LEN_TRIM( MAPDIR )
        IF ( MAPDIR( L:L ) .NE. '/' ) MAPDIR( L+1:L+1 ) = '/'
    END IF


    !!...............  Process configuration file:

    CALL RDCONFIG( EFLAG )
    IF ( NXPIX .LT. 280 .OR. NYPIX .LT. 240 ) THEN
        EFLAG = .TRUE.
        CALL M3MESG( 'Image size less than minimum 280x240' )
    END IF


    IF ( EFLAG ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'Fatal config / environment error(s)', 2 )
    END IF


    !!...............  Initialize graphics; set up maps:

    CALL INITPLOT( PLOTONLY, 0,                                         &
                   NXPIX, NYPIX, NRGB, RGB, NVRGB, VRGB, NZRGB, ZRGB,   &
                   MISS, NCOLS1, NROWS1, COL0,  COL1,  ROW0,  ROW1,     &
                   GDTYP1, P_ALP1, P_BET1, P_GAM1, XCENT1, YCENT1,      &
                   XORIG1, YORIG1, XCELL1, YCELL1, ISTAT )

    IF ( ISTAT .NE. 0 ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'Plot-initialization error', 2 )
    END IF


    IF ( MAPDIR .NE. 'NONE' ) THEN

        IF ( MAPNAME1 .NE. BLANK ) THEN
            EQNAME = TRIM( MAPDIR ) // TRIM( MAPNAME1 )
            MESG   = 'Initializing map ' // TRIM( EQNAME )
            CALL M3MESG( MESG )
            CALL INITMAP( 1, TRIM( EQNAME ), RGB1, THK1, ISTAT )
            IF ( ISTAT .NE. 0 )  EFLAG = .TRUE.
        END IF

        IF ( MAPNAME2 .NE. BLANK ) THEN
            EQNAME = TRIM( MAPDIR ) // TRIM( MAPNAME2 )
            MESG   = 'Initializing map ' // TRIM( EQNAME )
            CALL M3MESG( MESG )
            CALL INITMAP( 2, TRIM( EQNAME ), RGB2, THK2, ISTAT )
            IF ( ISTAT .NE. 0 )  EFLAG = .TRUE.
        END IF

        IF ( MAPNAME3 .NE. BLANK ) THEN
            EQNAME = TRIM( MAPDIR ) // TRIM( MAPNAME3 )
            MESG   = 'Initializing map ' // TRIM( EQNAME )
            CALL M3MESG( MESG )
            CALL INITMAP( 3, TRIM( EQNAME ), RGB3, THK3, ISTAT )
            IF ( ISTAT .NE. 0 )  EFLAG = .TRUE.
        END IF

        IF ( EFLAG ) THEN
            CALL M3EXIT( PNAME, 0, 0, 'Map-initialization error', 2 )
        END IF

    END IF


    !!...............  Allocate buffers:

    ALLOCATE( RBUF( NCOLS1,NROWS1 ),    &
              IBUF( NCOLS1,NROWS1 ), STAT = ISTAT )
    IF ( ISTAT .NE. 0 ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'Grid-buffer allocation error', 2 )
    END IF
    CALL M3MESG( BAR )


    !!...............  Get scale max/min where needed

    IF ( VMIN .GT. VMAX ) THEN

        DO F = 1, NFILES

            CALL RDFILE( FNAMES( F ), AFLAG )
            IF ( AFLAG ) THEN
                EFLAG = .TRUE.
                CYCLE
            END IF

            IF ( VTYPE .EQ. M3INT ) THEN

                DO R = ROW0, ROW1
                DO C = COL0, COL1
                    Y = FLOAT( IBUF( C,R ) )
                    IF ( FLTSAME( Y , VMISS ) )  CYCLE
                    Y = VFAC * Y
                    IF ( Y .LT. YMIN   )  YMIN = Y
                    IF ( Y .GT. YMAX   )  YMAX = Y
                END DO
                END DO

            ELSE IF ( VTYPE .EQ. M3REAL ) THEN

                DO R = ROW0, ROW1
                DO C = COL0, COL1
                    Y = RBUF( C,R )
                    IF ( FLTSAME( Y , VMISS ) )  CYCLE
                    Y = VFAC * Y
                    IF ( Y .LT. YMIN   )  YMIN = Y
                    IF ( Y .GT. YMAX   )  YMAX = Y
                END DO
                END DO

            END IF

        END DO

        VMAX = YMAX
        VMIN = YMIN

    END IF



    !!........  Compute scaling factors:

    DO F = 1, NFILES

        CALL RDFILE( FNAMES( F ), AFLAG )
        IF ( AFLAG ) THEN
            EFLAG = .TRUE.
            CYCLE
        END IF

        YMAX = -9.999E36
        YMIN =  9.999E36
        YSUM =  0.0D0
        NCNT =  0

        IF ( VTYPE .EQ. M3INT ) THEN

            DO R = ROW0, ROW1
            DO C = COL0, COL1
                Y = FLOAT( IBUF( C,R ) )
                IF ( FLTSAME( Y , VMISS ) )  CYCLE
                Y = VFAC * Y
                YSUM = YSUM + Y
                NCNT = NCNT + 1
                IF ( Y .LT. YMIN   ) then
                    YMIN = Y
                    CMIN = C
                    RMIN = R
                END IF
                IF ( Y .GT. YMAX   )THEN
                    YMAX = Y
                    CMAX = C
                    RMAX = R
                END IF
                RBUF( C,R ) = Y
            END DO
            END DO

        ELSE IF ( VTYPE .EQ. M3REAL ) THEN

            DO R = ROW0, ROW1
            DO C = COL0, COL1
                Y = RBUF( C,R )
                IF ( FLTSAME( Y , VMISS ) )  CYCLE
                Y = VFAC * Y
                YSUM = YSUM + Y
                NCNT = NCNT + 1
                IF ( Y .LT. YMIN   ) then
                    YMIN = Y
                    CMIN = C
                    RMIN = R
                END IF
                IF ( Y .GT. YMAX   )THEN
                    YMAX = Y
                    CMAX = C
                    RMAX = R
                END IF
                RBUF( C,R ) = Y
            END DO
            END DO

        ELSE

            WRITE( MESG, '( A, I10 )' ) 'Unrecognized VTYPE=', VTYPE
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            CYCLE

        END IF

        IF ( TITLE1( F ) .EQ. CMISS3 )  TITLE1( F ) = VNAME
        IF ( TITLE3( F ) .EQ. CMISS3 )  CALL NAMEVAL( FNAMES( F ), TITLE3( F ) )


        !!........  Compute  bin-label values

        AFAC = ( VMAX - VMIN ) / FLOAT( NBINS-1 )
        DO R = 1, NBINS
            BINS( R ) = VMIN + AFAC * FLOAT( R-1 )
        END DO


        !!........  Compute scaling factors:  rescaled V should have range [0 , NSCALE-1 ]

        YFAC = FLOAT( NRGB ) / ( VMAX - VMIN )
        IF ( NCNT .GT. 0 ) THEN
            YBAR = YSUM / DBLE( NCNT )
        ELSE
            YBAR = BADVAL3
        END IF
        PRINT *, '     ', TRIM( VNAME ), ': YMIN=', YMIN, '  YMAX=', YMAX, '  YBAR=', YBAR


        !!........  Write the legend and scale bar:

        J = JDATES( F )
        K = JTIMES( F )
        IF ( J*J + K*K  .NE. 0 ) THEN
            DTBUF = DT2STR( J, K )
        ELSE
            DTBUF = BLANK
        END IF
                
        CALL IMGCLR()   !!  Clear this-step image storage

        CALL  LEGEND( NBINS, BINS,                                      &
                      YMAX,  CMAX,  RMAX, YMIN,  CMIN,  RMIN, YBAR,     &
                      SMAX, CVMAX, RVMAX, SMIN, CVMIN, RVMIN, SBAR,     &
                      1.0,                                              &
                      ZMAX, CZMAX, RZMAX, ZMIN, CZMIN, RZMIN, ZBAR,     &
                      0.0, 1.0,                                         &
                      TRIM( VNAME ), TRIM( UNITS ), BLANK, BLANK,       &
                      TRIM( DTBUF ),                                    &
                      TRIM( TITLE1( F ) ), TRIM( TITLE2( F ) ),         &
                      TRIM( TITLE3( F ) ), TRIM( BINFMT ) )


        !!........  Draw either tile plot or smoothed plot

        IF ( IMGMTHD .EQ. TILED ) THEN
            CALL IMGTILE( RBUF, Y0, YFAC )
        ELSE IF ( IMGMTHD .EQ. SMOOTH ) THEN
            CALL IMGSMTH( RBUF, Y0, YFAC )
        ELSE
            EFLAG = .TRUE.
            CALL M3MESG( 'Unrecognized IMGMETHOD' )
        END IF


        !!........  Draw maps in reverse order, so MAP1 ends up "on top"

        IF ( MAPDIR .NE. 'NONE' ) THEN

            IF ( MAPNAME3 .NE. BLANK ) THEN
                CALL IMGMAP( 3 )
            END IF

            IF ( MAPNAME2 .NE. BLANK ) THEN
                CALL IMGMAP( 2 )
            END IF

            IF ( MAPNAME1 .NE. BLANK ) THEN
                CALL IMGMAP( 1 )
            END IF

        END IF

        IF ( JDATES( F ) .EQ. 0 .AND. JTIMES( F ) .EQ. 0 ) THEN
            EQNAME = TRIM( EQROOT ) // TRIM( VNAME ) // SUFFIX
        ELSE IF ( DTFLAG ) THEN
            WRITE( EQNAME, '( 3A, I7.7, I6.6, A )' ) TRIM( EQROOT ), TRIM( VNAME ), '.', JDATES(F), JTIMES(F), SUFFIX
        ELSE
            WRITE( EQNAME, '( 3A, I4.4, A )' ) TRIM( EQROOT ), TRIM( VNAME ), '.', F-1, SUFFIX
        END IF

        CALL IMGWRITE( IMGTYPE, EQNAME, ISTAT )
        IF ( ISTAT .NE. 0 ) THEN
            EFLAG = .TRUE.
            WRITE( MESG, '( 3A, I9.7, A, I6.6 )' )      &
                'Error writing image-file for variable "', TRIM( VNAME ), ' date%time', JDATE, ':', JTIME
            CALL M3MESG( MESG )
        END IF

    END DO


    IF ( EFLAG ) THEN
        MESG  = 'Failure in program'
        ISTAT = 2
    ELSE
        MESG  = 'Success in program'
        ISTAT = 0
    END IF

    CALL M3EXIT( PNAME, 0, 0, MESG, ISTAT )



CONTAINS    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-



    SUBROUTINE RDCONFIG( EFLAG )

        LOGICAL, INTENT( INOUT ) :: EFLAG

        INTEGER IDEV, ISTAT, F, I, K, L, M, N, V, NLINES
        REAL    XX, YY
        LOGICAL BFLAG, CFLAG

        CHARACTER*1     A1

        CHARACTER*256   MESG
        CHARACTER*512   LINE
        CHARACTER*32    FIELD( 6 )

        !!-----------  body of subroutine:  -----------------------

        BFLAG = .FALSE.
        CFLAG = .FALSE.

        IDEV  = GETEFILE( 'CONFIG', .TRUE., .TRUE., 'M3PLOT/RDCONFIG' )

        IF ( IDEV .LT. 0 ) THEN
            EFLAG = .TRUE.
            CALL M3MESG( 'Could not open "CONFIG"' )
            RETURN
        END IF


        L = 0
        DO

            READ( IDEV, '( A )', END=11, IOSTAT=ISTAT ) A1
            IF ( ISTAT .LT. 0 ) THEN
                WRITE( MESG, '( A, I9, 2X, A, I9 )' ) 'Error=', ISTAT, 'reading "CONFIG" at line', L+1
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            ELSE IF ( BLKORCOMMENT( LINE ) ) THEN
                CYCLE
            ELSE
                L = L +1
            END IF

        END DO

11      CONTINUE        !  EOF-exit from loop

        NLINES = L
        REWIND( IDEV )

        ALLOCATE( FNAMES( NLINES ),     &
                  JDATES( NLINES ),     &
                  JTIMES( NLINES ),     &
                  TITLE1( NLINES ),     &
                  TITLE2( NLINES ),     &
                  TITLE3( NLINES ), STAT = ISTAT )
        IF ( ISTAT .NE. 0 ) THEN
            CALL M3EXIT( 'GISPLOT/RDCONFIG', 0, 0, 'Name-buffer allocation error', 2 )
        ELSE
            FNAMES = BLANK
            TITLE1 = CMISS3
            TITLE2 = CMISS3
            TITLE3 = CMISS3
            JDATES = 0
            JTIMES = 0
        END IF


        F = 0

        DO L = 1, NLINES

            READ( IDEV, '( A )', END=99, IOSTAT=ISTAT ) LINE
            IF ( ISTAT .LT. 0 ) THEN
                WRITE( MESG, '( A, I9, 2X, A, I9 )' ) 'Error=', ISTAT, 'reading "CONFIG" at line', L
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
                CYCLE            
            ELSE IF ( BLKORCOMMENT( LINE ) ) THEN
                CYCLE
            END IF

            CALL SPLITLINE( LINE, 6, N, FIELD, BFLAG )
            IF ( BFLAG ) THEN
                WRITE( MESG, '( A, I7 )' ) 'Badly-formatted fields in "CONFIG" at line', L
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
                CYCLE
            END IF

            IF ( N .EQ. 0 ) CYCLE

            CALL UPCASE( FIELD( 1 ) )

            IF ( FIELD( 1 ) .EQ. 'IMGFORMAT' ) THEN

                CALL UPCASE( FIELD( 2 ) )

                IF ( N .NE. 2 ) THEN
                    BFLAG = .TRUE.
                    WRITE( MESG, '( A, I7 )' ) 'Bad number of fields for "IMGFORMAT" in "CONFIG" at line', L
                    CALL M3MESG( MESG )
                ELSE IF ( FIELD( 2 ) .EQ. 'GIF' ) THEN
                    IMGTYPE = GIFIMG
                    SUFFIX  = '.gif'
                ELSE IF ( FIELD( 2 ) .EQ. 'JPG' ) THEN
                    IMGTYPE = JPGIMG
                    SUFFIX  = '.jpg'
                ELSE IF ( FIELD( 2 ) .EQ. 'JPEG' ) THEN
                    IMGTYPE = JPGIMG
                    SUFFIX  = '.jpg'
                ELSE IF ( FIELD( 2 ) .EQ. 'PNG' ) THEN
                    IMGTYPE = PNGIMG
                    SUFFIX  = '.png'
                ELSE
                    BFLAG = .TRUE.
                    WRITE( MESG, '( A, I7 )' ) 'Unrecognized value for "IMGFORMAT" in "CONFIG" at line', L
                    CALL M3MESG( MESG )
                END IF

            ELSE IF ( FIELD( 1 ) .EQ. 'IMGMETHOD' ) THEN

                CALL UPCASE( FIELD( 2 ) )

                IF ( N .NE. 2 ) THEN
                    BFLAG = .TRUE.
                    WRITE( MESG, '( A, I7 )' ) 'Bad number of fields for "IMGMETHOD" in "CONFIG" at line', L
                    CALL M3MESG( MESG )
                ELSE IF ( FIELD( 2 ) .EQ. 'TILED' ) THEN
                    IMGMTHD = TILED
                ELSE IF ( FIELD( 2 ) .EQ. 'SMOOTHED' ) THEN
                    IMGMTHD = SMOOTH
                ELSE
                    BFLAG = .TRUE.
                    WRITE( MESG, '( A, I7 )' ) 'Unrecognized value for "IMGMETHOD" in "CONFIG" at line', L
                    CALL M3MESG( MESG )
                END IF

            ELSE IF ( FIELD( 1 ) .EQ. 'PLOTONLY' ) THEN

                CALL UPCASE( FIELD( 2 ) )

                IF ( N .NE. 2 ) THEN
                    BFLAG = .TRUE.
                    WRITE( MESG, '( A, I7 )' ) 'Bad number of fields for "PLOTONLY" in "CONFIG" at line', L
                    CALL M3MESG( MESG )
                ELSE IF ( FIELD( 2 ) .EQ. 'WIDTH' ) THEN
                    PLOTONLY = XSIZED
                ELSE IF ( FIELD( 2 ) .EQ. 'HEIGHT' ) THEN
                    PLOTONLY = YSIZED
                ELSE
                    BFLAG = .TRUE.
                    WRITE( MESG, '( A, I7 )' ) 'Unrecognized value for "PLOTONLY" in "CONFIG" at line', L
                    CALL M3MESG( MESG )
                END IF

            ELSE IF ( FIELD( 1 ) .EQ. 'WIDTH' ) THEN

                IF ( N .NE. 2 ) THEN
                    BFLAG = .TRUE.
                    WRITE( MESG, '( A, I7 )' ) 'Bad number of fields for "WIDTH" in "CONFIG" at line', L
                    CALL M3MESG( MESG )
                ELSE
                    NXPIX = STR2INT( FIELD( 2 ) )
                END IF

            ELSE IF ( FIELD( 1 ) .EQ. 'HEIGHT' ) THEN

                IF ( N .NE. 2 ) THEN
                    BFLAG = .TRUE.
                    WRITE( MESG, '( A, I7 )' ) 'Bad number of fields for "HEIGHT" in "CONFIG" at line', L
                    CALL M3MESG( MESG )
                ELSE
                    NYPIX = STR2INT( FIELD( 2 ) )
                END IF

            ELSE IF ( FIELD( 1 ) .EQ. 'WINDOW_COLS' ) THEN

                IF ( N .NE. 3 ) THEN
                    BFLAG = .TRUE.
                    WRITE( MESG, '( A, I7 )' ) 'Bad number of fields for "WINDOW_COLS" in "CONFIG" at line', L
                    CALL M3MESG( MESG )
                ELSE
                    COL0 = STR2INT( FIELD( 2 ) )
                    COL1 = STR2INT( FIELD( 3 ) )
                END IF

            ELSE IF ( FIELD( 1 ) .EQ. 'WINDOW_ROWS' ) THEN

                IF ( N .NE. 3 ) THEN
                    BFLAG = .TRUE.
                    WRITE( MESG, '( A, I7 )' ) 'Bad number of fields for "WINDOW_ROWS" in "CONFIG" at line', L
                    CALL M3MESG( MESG )
                ELSE
                    ROW0 = STR2INT( FIELD( 2 ) )
                    ROW1 = STR2INT( FIELD( 3 ) )
                END IF

            ELSE IF ( FIELD( 1 ) .EQ. 'MAPNAME1' ) THEN

                IF ( N .GT. 2 ) THEN
                    BFLAG = .TRUE.
                    WRITE( MESG, '( A, I7 )' ) 'Bad number of fields for "MAPNAME1" in "CONFIG" at line', L
                    CALL M3MESG( MESG )
                ELSE IF ( MAPDIR .EQ. 'NONE' ) THEN
                    BFLAG = .TRUE.
                    WRITE( MESG, '( A, I7 )' ) '${MAPDIR}="NONE" turns off "MAPNAME1" in "CONFIG" at line', L
                    CALL M3MESG( MESG )
                ELSE IF ( N .EQ. 2 ) THEN
                    MAPNAME1 = FIELD( 2 )
                ELSE
                    MAPNAME1 = BLANK
                END IF

            ELSE IF ( FIELD( 1 ) .EQ. 'MAPNAME2' ) THEN

                IF ( N .GT. 2 ) THEN
                    BFLAG = .TRUE.
                    WRITE( MESG, '( A, I7 )' ) 'Bad number of fields for "MAPNAME2" in "CONFIG" at line', L
                    CALL M3MESG( MESG )
                ELSE IF ( MAPDIR .EQ. 'NONE' ) THEN
                    BFLAG = .TRUE.
                    WRITE( MESG, '( A, I7 )' ) '${MAPDIR}="NONE" turns off "MAPNAME2" in "CONFIG" at line', L
                    CALL M3MESG( MESG )
                ELSE IF ( N .EQ. 2 ) THEN
                    MAPNAME2 = FIELD( 2 )
                ELSE
                    MAPNAME2 = BLANK
                END IF

            ELSE IF ( FIELD( 1 ) .EQ. 'MAPNAME3' ) THEN

                IF ( N .GT. 2 ) THEN
                    BFLAG = .TRUE.
                    WRITE( MESG, '( A, I7 )' ) 'Bad number of fields for "MAPNAME3" in "CONFIG" at line', L
                    CALL M3MESG( MESG )
                ELSE IF ( MAPDIR .EQ. 'NONE' ) THEN
                    BFLAG = .TRUE.
                    WRITE( MESG, '( A, I7 )' ) '${MAPDIR}="NONE" turns off "MAPNAME3" in "CONFIG" at line', L
                    CALL M3MESG( MESG )
                ELSE IF ( N .EQ. 2 ) THEN
                    MAPNAME3 = FIELD( 2 )
                ELSE
                    MAPNAME3 = BLANK
                END IF

            ELSE IF ( FIELD( 1 ) .EQ. 'MAPCOLOR1'   ) THEN

                IF ( N .NE. 4 ) THEN
                    BFLAG = .TRUE.
                    WRITE( MESG, '( A, I7 )' ) 'Bad number of fields for "MAPCOLOR1" in "CONFIG" at line', L
                    CALL M3MESG( MESG )
                ELSE
                    RGB1( 1 ) = STR2INT( FIELD( 2 ) )
                    RGB1( 2 ) = STR2INT( FIELD( 3 ) )
                    RGB1( 3 ) = STR2INT( FIELD( 4 ) )
                END IF

            ELSE IF ( FIELD( 1 ) .EQ. 'MAPCOLOR2'   ) THEN

                IF ( N .NE. 4 ) THEN
                    BFLAG = .TRUE.
                    WRITE( MESG, '( A, I7 )' ) 'Bad number of fields for "MAPCOLOR2" in "CONFIG" at line', L
                    CALL M3MESG( MESG )
                ELSE
                    RGB2( 1 ) = STR2INT( FIELD( 2 ) )
                    RGB2( 2 ) = STR2INT( FIELD( 3 ) )
                    RGB2( 3 ) = STR2INT( FIELD( 4 ) )
                END IF

            ELSE IF ( FIELD( 1 ) .EQ. 'MAPCOLOR3'   ) THEN

                IF ( N .NE. 4 ) THEN
                    BFLAG = .TRUE.
                    WRITE( MESG, '( A, I7 )' ) 'Bad number of fields for "MAPCOLOR3" in "CONFIG" at line', L
                    CALL M3MESG( MESG )
                ELSE
                    RGB3( 1 ) = STR2INT( FIELD( 2 ) )
                    RGB3( 2 ) = STR2INT( FIELD( 3 ) )
                    RGB3( 3 ) = STR2INT( FIELD( 4 ) )
                END IF

            ELSE IF ( FIELD( 1 ) .EQ. 'MAPWIDTH1'   ) THEN

                IF ( N .NE. 2 ) THEN
                    AFLAG = .TRUE.
                    WRITE( MESG, '( A, I7 )' ) 'Bad number of fields for "MAPWIDTH1" in "CONFIG" at line', L
                    CALL M3MESG( MESG )
                ELSE
                    THK1 = STR2INT( FIELD( 2 ) )
                END IF

            ELSE IF ( FIELD( 1 ) .EQ. 'MAPWIDTH2'   ) THEN

                IF ( N .NE. 2 ) THEN
                    AFLAG = .TRUE.
                    WRITE( MESG, '( A, I7 )' ) 'Bad number of fields for "MAPWIDTH2" in "CONFIG" at line', L
                    CALL M3MESG( MESG )
                ELSE
                    THK2 = STR2INT( FIELD( 2 ) )
                END IF

            ELSE IF ( FIELD( 1 ) .EQ. 'MAPWIDTH3'   ) THEN

                IF ( N .NE. 2 ) THEN
                    AFLAG = .TRUE.
                    WRITE( MESG, '( A, I7 )' ) 'Bad number of fields for "MAPWIDTH3" in "CONFIG" at line', L
                    CALL M3MESG( MESG )
                ELSE
                    THK3 = STR2INT( FIELD( 2 ) )
                END IF

            ELSE IF ( FIELD( 1 ) .EQ. 'COLOR' ) THEN    !!  tile-color

                IF ( N .NE. 5 ) THEN
                    BFLAG = .TRUE.
                    WRITE( MESG, '( A, I7 )' ) 'Bad number of fields for "COLOR" in "CONFIG" at line', L
                    CALL M3MESG( MESG )
                ELSE
                    I = STR2INT( FIELD( 2 ) )
                    IF ( I .GT. MXSCALE .OR. I .LT. 1 ) THEN
                        BFLAG = .TRUE.
                        WRITE( MESG, '( A, I7 )' ) 'Bad index for "COLOR" in "CONFIG" at line', L
                        CALL M3MESG( MESG )
                    ELSE
                        IF ( CFLAG ) THEN
                            NRGB = MAX( I, NRGB )   !  accumulate colors
                        ELSE
                            CFLAG = .TRUE.              !  starting a color-scale:  use current I
                            NRGB = I
                        END IF
                        RGB( 1,I ) = STR2INT( FIELD( 3 ) )
                        RGB( 2,I ) = STR2INT( FIELD( 4 ) )
                        RGB( 3,I ) = STR2INT( FIELD( 5 ) )
                    END IF
                END IF

            ELSE IF ( FIELD( 1 ) .EQ. 'VARIABLE' ) THEN         !  VARIABLE <VNAME> <mode: LIN | LOG | EXP >

                VARPLOT = .TRUE.

                IF ( N .EQ. 2 ) THEN
                    VNAME = FIELD( 2 )
                ELSE
                    BFLAG = .TRUE.
                    WRITE( MESG, '( A, I7 )' ) 'Bad number of fields for "VARIABLE" in "CONFIG" at line', L
                    CALL M3MESG( MESG )
                END IF

            ELSE IF ( FIELD( 1 ) .EQ. 'UNITS' ) THEN

                IF      ( N .EQ. 3 ) THEN
                    UNITS = FIELD( 2 )
                    VFAC  = STR2REAL( FIELD( 3 ) )
                ELSE IF ( N .EQ. 2 ) THEN
                    UNITS = FIELD( 2 )
                    VFAC  = 1.0
                ELSE
                    BFLAG = .TRUE.
                    WRITE( MESG, '( A, I7 )' ) 'Bad number of fields for "UNITS" in "CONFIG" at line', L
                    CALL M3MESG( MESG )
                END IF

            ELSE IF ( FIELD( 1 ) .EQ. 'GIS_FORMAT' ) THEN

                IF ( N .NE. 2 ) THEN
                    BFLAG = .TRUE.
                    WRITE( MESG, '( A, I7 )' ) 'Bad number of fields for "GIS_FORMAT" in "CONFIG" at line', L
                    CALL M3MESG( MESG )
                ELSE
                    ATYPE = FIELD( 2 )
                END IF

            ELSE IF ( FIELD( 1 ) .EQ. 'MISSING_VALUE' ) THEN

                IF ( N .NE. 2 ) THEN
                    BFLAG = .TRUE.
                    WRITE( MESG, '( A, I7 )' ) 'Bad number of fields for "MISSING_VALUE" in "CONFIG" at line', L
                    CALL M3MESG( MESG )
                ELSE
                    VMISS = STR2REAL( FIELD( 2 ) )
                END IF

            ELSE IF ( FIELD( 1 ) .EQ. 'SCALE_RANGE' ) THEN

                IF ( N .NE. 3 ) THEN
                    BFLAG = .TRUE.
                    WRITE( MESG, '( A, I7 )' ) 'Bad number of fields for "SCALE_RANGE" in "CONFIG" at line', L
                    CALL M3MESG( MESG )
                ELSE
                    XX = STR2REAL( FIELD( 2 ) )
                    YY = STR2REAL( FIELD( 3 ) )
                    VMIN = MIN( XX, YY )
                    VMAX = MAX( XX, YY )
                END IF

            ELSE IF ( FIELD( 1 ) .EQ. 'LEGEND_BINS' ) THEN

                IF ( N .EQ. 2 ) THEN
                    NBINS = STR2INT( FIELD( 2 ) )
                ELSE
                    BFLAG = .TRUE.
                    WRITE( MESG, '( A, I7 )' ) 'Bad number of fields for "LEGEND_BINS" in "CONFIG" at line', L
                    CALL M3MESG( MESG )
                END IF

            ELSE IF ( FIELD( 1 ) .EQ. 'MISSING_COLOR'   ) THEN

                IF ( N .NE. 4 ) THEN
                    BFLAG = .TRUE.
                    WRITE( MESG, '( A, I7 )' ) 'Bad number of fields for "MISSING_COLOR" in "CONFIG" at line', L
                    CALL M3MESG( MESG )
                ELSE
                    MISS( 1 ) = STR2INT( FIELD( 2 ) )
                    MISS( 2 ) = STR2INT( FIELD( 3 ) )
                    MISS( 3 ) = STR2INT( FIELD( 4 ) )
                END IF

            ELSE IF ( FIELD( 1 ) .EQ. 'INFILE' ) THEN

                IF ( N .EQ. 4 ) THEN
                    F = F + 1
                    J = STR2INT( FIELD( 3 ) )
                    K = STR2INT( FIELD( 4 ) )
                    FNAMES( F ) = FIELD( 2 )
                    JDATES( F ) = J
                    JTIMES( F ) = K
                ELSE IF ( N .EQ. 2 ) THEN
                    F = F + 1
                    FNAMES( F ) = FIELD( 2 )
                    JDATES( F ) = 0
                    JTIMES( F ) = 0
                ELSE
                    BFLAG = .TRUE.
                    WRITE( MESG, '( A, I7 )' ) 'Bad number of fields for "INFILE" in "CONFIG" at line', L
                    CALL M3MESG( MESG )
                END IF

                IF ( J .LT. 0 .OR. K .LT. 0 ) THEN
                    BFLAG = .TRUE.
                    WRITE( MESG, '( A, I7 )' ) 'Bad date/time fields for "INFILE" in "CONFIG" at line', L
                    CALL M3MESG( MESG )
                END IF

           ELSE IF ( FIELD( 1 ) .EQ. 'TITLE1' ) THEN

                IF ( F .EQ. 0 ) THEN
                    BFLAG = .TRUE.
                    WRITE( MESG, '( A, I7 )' ) '"INFILE" not yet set for "TITLE1" in "CONFIG" at line', L
                    CALL M3MESG( MESG )
                ELSE
                    M = INDEX( 'TITLE1', LINE ) + 7
                    N = M + LBLANK( LINE( M: ) )
                    TITLE1( F ) = LINE( N: )
                END IF

            ELSE IF ( FIELD( 1 ) .EQ. 'TITLE2' ) THEN

                IF ( F .EQ. 0 ) THEN
                    BFLAG = .TRUE.
                    WRITE( MESG, '( A, I7 )' ) '"VARIABLE" not yet set for "TITLE2" in "CONFIG" at line', L
                    CALL M3MESG( MESG )
                ELSE
                    M = INDEX( 'TITLE2', LINE ) + 7
                    N = M + LBLANK( LINE( M: ) )
                    TITLE2( F ) = LINE( N: )
                END IF

            ELSE IF ( FIELD( 1 ) .EQ. 'TITLE3' ) THEN

                IF ( F .EQ. 0 ) THEN
                    BFLAG = .TRUE.
                    WRITE( MESG, '( A, I7 )' ) '"VARIABLE" not yet set for "TITLE3" in "CONFIG" at line', L
                    CALL M3MESG( MESG )
                ELSE
                    M = INDEX( 'TITLE3', LINE ) + 7
                    N = M + LBLANK( LINE( M: ) )
                    TITLE3( F ) = LINE( N: )
                END IF

            ELSE

                WRITE( MESG, '( 3 A, I9 )' ) 'Unrecognized field "', FIELD(1), '" in "CONFIG" at line', L
                CALL M3MESG( MESG )
                BFLAG = .TRUE.
                CYCLE

            END IF              !  if field(1) is...

        END DO

99      CONTINUE        !  EOF-exit from loop

        CLOSE( IDEV )


        IF ( BFLAG ) THEN
            EFLAG = .TRUE.
            CALL M3MESG( 'Error(s) reading/processing file "CONFIG"' )
            RETURN
        ELSE IF ( .NOT.VARPLOT ) THEN
            EFLAG = .TRUE.
            CALL M3MESG( 'Missing "VARIABLE" in file "CONFIG"' )
        ELSE IF ( ATYPE .EQ. BLANK ) THEN
            EFLAG = .TRUE.
            CALL M3MESG( 'Missing "GIS_FORMAT" in "CONFIG"' )
            RETURN
        ELSE IF ( F .LE. 0 ) THEN
            EFLAG = .TRUE.
            CALL M3MESG( 'Missing "INFILE"s in "CONFIG"' )
            RETURN
        END IF

        I     = INDEX1( ATYPE, NTYPES, ATYPES )
        FTYPE = I
        IF ( I .LE. 0 ) THEN
            EFLAG = .TRUE.
            CALL M3MESG( 'Bad "GIS_FORMAT" in "CONFIG"' )
            RETURN
        END IF

        VTYPE  = VTYPES( I )

        IF ( FLTSAME( VMISS, BADVAL3 ) ) THEN
            IF ( FTYPE .EQ. BIL ) THEN
                VMISS = 0.0
            ELSE IF ( FTYPE .EQ. BIL2 ) THEN
                VMISS = 0.0
            ELSE IF ( FTYPE .EQ. BIL4 ) THEN
                VMISS = 0.0
            ELSE IF ( FTYPE .EQ. ARCI ) THEN
                VMISS = FLOAT( IMISS3 )
            ELSE IF ( FTYPE .EQ. ASCI ) THEN
                VMISS = FLOAT( IMISS3 )
            END IF
        END IF

        NFILES = F

        RETURN

    END SUBROUTINE RDCONFIG



    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-



    SUBROUTINE RDFILE( FNAME, AFLAG )

        USE MODGISIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME
        LOGICAL      , INTENT(  OUT) :: AFLAG

        AFLAG = .FALSE.

        IF ( FTYPE .EQ. BIL ) THEN
            IF ( .NOT.RDBIFILE( FNAME, NCOLS1, NROWS1, IBUF ) ) THEN
                EFLAG = .TRUE.
                MESG  = 'Could not read ' // FNAME
                CALL M3MESG( MESG )
            END IF
        ELSE IF ( FTYPE .EQ. BIL2 ) THEN
            IF ( .NOT.RDBI2FILE( FNAME, NCOLS1, NROWS1, IBUF ) ) THEN
                EFLAG = .TRUE.
                MESG  = 'Could not read ' // FNAME
                CALL M3MESG( MESG )
            END IF
        ELSE IF ( FTYPE .EQ. BIL4 ) THEN
            IF ( .NOT.RDBI4FILE( FNAME, NCOLS1, NROWS1, IBUF ) ) THEN
                EFLAG = .TRUE.
                MESG  = 'Could not read ' // FNAME
                CALL M3MESG( MESG )
            END IF
        ELSE IF ( FTYPE .EQ. FLT ) THEN
            IF ( .NOT.RDBRFILE( FNAME, NCOLS1, NROWS1, RBUF ) ) THEN
                EFLAG = .TRUE.
                MESG  = 'Could not read ' // FNAME
                CALL M3MESG( MESG )
            END IF
        ELSE IF ( FTYPE .EQ. ZBIL ) THEN
            IF ( .NOT.RDZBIFILE( FNAME, NCOLS1, NROWS1, IBUF ) ) THEN
                EFLAG = .TRUE.
                MESG  = 'Could not read ' // FNAME
                CALL M3MESG( MESG )
            END IF
        ELSE IF ( FTYPE .EQ. ZBIL2 ) THEN
            IF ( .NOT.RDZBI2FILE( FNAME, NCOLS1, NROWS1, IBUF ) ) THEN
                EFLAG = .TRUE.
                MESG  = 'Could not read ' // FNAME
                CALL M3MESG( MESG )
            END IF
        ELSE IF ( FTYPE .EQ. ZBIL4 ) THEN
            IF ( .NOT.RDZBI4FILE( FNAME, NCOLS1, NROWS1, IBUF ) ) THEN
                EFLAG = .TRUE.
                MESG  = 'Could not read ' // FNAME
                CALL M3MESG( MESG )
            END IF
        ELSE IF ( FTYPE .EQ. ZFLT ) THEN
            IF ( .NOT.RDZBRFILE( FNAME, NCOLS1, NROWS1, RBUF ) ) THEN
                EFLAG = .TRUE.
                MESG  = 'Could not read ' // FNAME
                CALL M3MESG( MESG )
            END IF
        ELSE IF ( FTYPE .EQ. ARCI ) THEN
            IF ( .NOT.RDARCI( FNAME, NCOLS1, NROWS1, XORIG1, YORIG1, XCELL1, IBUF ) ) THEN
                EFLAG = .TRUE.
                MESG  = 'Could not read ' // FNAME
                CALL M3MESG( MESG )
            END IF
        ELSE IF ( FTYPE .EQ. ARCR ) THEN
            IF ( .NOT.RDARCR( FNAME, NCOLS1, NROWS1, XORIG1, YORIG1, XCELL1, RBUF ) ) THEN
                EFLAG = .TRUE.
                MESG  = 'Could not read ' // FNAME
                CALL M3MESG( MESG )
            END IF
        ELSE IF ( FTYPE .EQ. ASCI ) THEN
            IF ( .NOT.RDAIFILE( FNAME, NCOLS1, NROWS1, XORIG1, YORIG1, XCELL1, IBUF ) ) THEN
                EFLAG = .TRUE.
                MESG  = 'Could not read ' // FNAME
                CALL M3MESG( MESG )
            END IF
        ELSE IF ( FTYPE .EQ. ASCR ) THEN
            IF ( .NOT.RDARFILE( FNAME, NCOLS1, NROWS1, XORIG1, YORIG1, XCELL1, RBUF ) ) THEN
                EFLAG = .TRUE.
                MESG  = 'Could not read ' // FNAME
                CALL M3MESG( MESG )
            END IF
        ELSE
            CALL M3EXIT( PNAME, 0,0, 'Unsupported input file type', 2 )
        END IF

        RETURN

    END SUBROUTINE RDFILE


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION BLKORCOMMENT( CH )

        CHARACTER(LEN=*), INTENT( IN ) :: CH

        CHARACTER*1,  PARAMETER :: BANG   = '!'
        CHARACTER*1,  PARAMETER :: POUND  = '#'
        CHARACTER*1,  PARAMETER :: DOLLAR = '$'
        
        IF ( CH .EQ. BLANK )  THEN
            BLKORCOMMENT = .TRUE.
        ELSE
            BLKORCOMMENT = ( ( CH(1:1) .EQ. BANG  ) .OR. ( CH(1:1) .EQ. POUND ) .OR. ( CH(1:1) .EQ. DOLLAR ) )
        END IF
        
        RETURN

    END  FUNCTION BLKORCOMMENT



    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION FLTSAME( X, Y )
        REAL, INTENT( IN ) :: X, Y
        REAL*8  P, Q
        P = X
        Q = Y
        FLTSAME = ( (P - Q)**2  .LE.  1.0D-9*( P*P + Q*Q + 1.0E-5 ) )
    END FUNCTION FLTSAME



END PROGRAM GISPLOT
