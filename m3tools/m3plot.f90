
PROGRAM M3PLOT

    !!*******************************************************************
    !!  Version "$Id: m3plot.f90 280 2025-04-12 15:34:39Z coats $"
    !!  Copyright (c) 2010-2011 Baron Advanced Meteorological Systems.
    !!  and (C) 2013-2025 Carlie J. Coats, Jr.,
    !!  Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2
    !!  See file "LGPL.txt" for conditions of use.
    !!..................................................................
    !!  program body         starts at line   297
    !!  subroutine RDCONFIG  starts at line  1812
    !!  subroutine GRDSMOOTH starts at line  2779
    !!  subroutine LLANGLE   starts at line  2885
    !!  subroutine LLTOXY    starts at line  3022
    !!  subroutine READIT    starts at line  3055
    !!
    !!  DESCRIPTION:
    !!      Create tile-plots for variables in a gridded M3IO file
    !!      with optional vector and observation overlays.
    !!      Plot types may be GIF, JPG, or PNG
    !!
    !!  PRECONDITIONS:
    !!      REQUIRES 01/09/2013 or later version of M3UTILIO
    !!
    !!      Parameter-consistency with "gdplot.c"
    !!      For use, see splash screen.
    !!
    !!  REVISION  HISTORY:
    !!      Prototype 10/2009 by Carlie J. Coats, Jr., BAMS.
    !!
    !!      Version    7/2010 by CJC:  vector and obs overlays.
    !!
    !!      Version   10/2010 by CJC: TITLE* bugfix; change TITLE* init
    !!      to support blank title lines.
    !!
    !!      Version   11/2010 by CJC:   add contour-plot, obs-vector options
    !!
    !!      Version   03/2011 by CJC:   add log-stats option
    !!
    !!      Version   02/2012 by CJC:   add offset B to display formula (A * Y + B)
    !!
    !!      Version   09/2012 by CJC:   map-thickness capability
    !!
    !!      Version   01/2013 by CJC:   Use LASTTIME to compute EDATE:ETIME
    !!
    !!      Version   06/2013 by CJC:   Support for INT, INT8, and REAL*8
    !!
    !!      Version    4/2025 by CJC for I/O API M3Tools version 4.0
    !!      Obs LLTOXY now done in this program.
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

    CHARACTER*16, PARAMETER :: MODES( 3 ) = (/ 'LIN', 'LOG', 'EXP' /)

    CHARACTER*16, PARAMETER :: PNAME = 'M3PLOT'
    CHARACTER*16, PARAMETER :: BLANK = ' '
    CHARACTER*64, PARAMETER :: BAR   = '-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-'


    !!......  LOCAL VARIABLES and their descriptions:

    INTEGER     LDEV, ISTAT
    INTEGER     C, R, K, L, N, V, V1
    INTEGER     JDATE, JTIME, TSTEP
    INTEGER     EDATE, ETIME

    LOGICAL     EFLAG, RDGRID, RDVEC, RDCON, RDOBS, RDOVEC

    !!  input-file description:

    CHARACTER*16    GDNAM1      ! grid name
    INTEGER         NCOLS1      ! number of input-grid columns
    INTEGER         NROWS1      ! number of input-grid rows
    INTEGER         NLAYS1      ! number of input-grid layers
    INTEGER         NTHIK1
    INTEGER         NVARS1      ! number of variables
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
    INTEGER         SDATE1
    INTEGER         STIME1
    INTEGER         TSTEP1
    INTEGER         NRECS1

    CHARACTER*16    VNAME1( MXVARS3 )
    CHARACTER*16    UNITS1( MXVARS3 )
    CHARACTER*80    VDESC1( MXVARS3 )
    INTEGER         VTYPE1( MXVARS3 )

    INTEGER         NZVAR1      ! number of variables for contouring
    CHARACTER*16    ZNAME1( MXVARS3 )
    CHARACTER*80    ZUNIT1( MXVARS3 )
    INTEGER         ZTYPE1( MXVARS3 )

    INTEGER         NCOLS2      ! number of wind-grid columns
    INTEGER         NROWS2      ! number of wind-grid rows
    INTEGER         NLAYS2      ! number of wind-grid layers
    REAL*8          XORIG2      ! X-coordinate origin of dot-grid (map units)
    REAL*8          YORIG2      ! Y-coordinate origin of dot-grid

    !! Run-control variables:

    INTEGER ::  PLOTONLY = ALLPLT
    INTEGER ::  IMGTYPE  = GIFIMG       !  GIFIMG | JPGIMG | PNGIMG
    INTEGER ::  IMGMTHD  = TILED        !  TILED | SMOOTHED
    INTEGER ::  SDATE, STIME, NRECS
    INTEGER ::  COL0,  COL1,  ROW0,  ROW1
    INTEGER ::  NXPIX = 960     !  width  of total image
    INTEGER ::  NYPIX = 720     !  height of total image
    INTEGER ::  NXGRD, NYGRD    !  size of tile-grid within image
    INTEGER ::  XGRD0, YGRD0    !  upper-left corner of tile-grid within image

    LOGICAL ::  DTFLAG, STATFLAG

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

    INTEGER ::  NVRGB = 10
    INTEGER ::  VRGB( 3, MXSCALE )
    DATA VRGB( 1:3,1:10 ) /   0,   0, 144,      &
                              0,  16, 128,      &
                              0,  32, 112,      &
                              0,  48,  96,      &
                              0,  64,  80,      &
                              0,  80,  64,      &
                              0,  96,  48,      &
                              0, 112,  32,      &
                              0, 128,  16,      &
                              0, 144,   0   /

    INTEGER ::  NZRGB = 10
    INTEGER ::  ZRGB( 3, MXSCALE )
    DATA ZRGB( 1:3,1:10 ) /   0,   0, 144,      &
                             16,   0, 128,      &
                             32,   0, 112,      &
                             48,   0,  96,      &
                             64,   0,  80,      &
                             80,   0,  64,      &
                             96,   0,  48,      &
                            112,   0,  32,      &
                            128,   0,  16,      &
                            144,   0,   0   /

    LOGICAL  :: VARPLOT         = .FALSE.
    INTEGER  :: NVARS           = 0
    INTEGER  :: MODE( MXVARS3 ) = LINSCL        !  LINSCL or LOGSCL or EXPSCL
    REAL     :: VMAX( MXVARS3 ) = -9.999E36     !  scale-max for variable
    REAL     :: VMIN( MXVARS3 ) =  9.999E36     !  scale-min for variable
    REAL     :: VFAC( MXVARS3 ) =  1.0          !  scale-factor for variable:  Y_out = Y_in * VFAC + VOFF
    REAL     :: VOFF( MXVARS3 ) =  0.0          !  scale-offset for variable

    INTEGER       :: VTYPES( MXVARS3 ) = 1      !  model-level
    INTEGER       :: VARLVL( MXVARS3 ) = 1      !  model-level
    CHARACTER*16  :: VNAMES( MXVARS3 ) = BLANK
    CHARACTER*16  :: VUNITS( MXVARS3 ) = BLANK
    CHARACTER*16  :: BINFMT( MXVARS3 ) = '%g'
    CHARACTER*256 :: TITLE1( MXVARS3 ) = CMISS3
    CHARACTER*256 :: TITLE2( MXVARS3 ) = CMISS3
    CHARACTER*256 :: TITLE3( MXVARS3 ) = CMISS3

    INTEGER       :: NVECS              = 0
    INTEGER       :: VECDOT             = 0     !  1 if VECFILE is dot-point, 0 if cross-point
    LOGICAL       :: VECPLOT            = .FALSE.
    LOGICAL       :: VECFLAG( MXVARS3 ) = .FALSE.
    REAL          ::  VECSCL( MXVARS3 ) = 25.0
    INTEGER       ::  VECLVL( MXVARS3 ) =  1
    INTEGER       ::  VECINC( MXVARS3 ) =  1
    INTEGER       ::  VECTHK( MXVARS3 ) =  1
    CHARACTER*16  ::   NAMEU( MXVARS3 ) = BLANK
    CHARACTER*16  ::   NAMEV( MXVARS3 ) = BLANK

    INTEGER       :: NCONS              = 0
    LOGICAL       :: CONPLOT            = .FALSE.
    LOGICAL       :: CONFLAG( MXVARS3 ) = .FALSE.
    LOGICAL       :: CONSMTH( MXVARS3 ) = .FALSE.
    REAL          ::  CONMIN( MXVARS3 ) = -BADVAL3
    REAL          ::  CONMAX( MXVARS3 ) =  BADVAL3
    REAL          ::  CONFAC( MXVARS3 ) =  1.0          !  scale-factor for variable
    INTEGER       ::  CONLVL( MXVARS3 ) =  1
    INTEGER       ::  CONTHK( MXVARS3 ) =  2
    INTEGER       ::  ZTYPES( MXVARS3 ) = M3REAL
    CHARACTER*16  ::  ZNAMES( MXVARS3 ) = BLANK
    CHARACTER*16  ::  ZUNITS( MXVARS3 ) = BLANK

    LOGICAL       :: OBSPLOT            = .FALSE.
    LOGICAL       :: OBSFLAG( MXVARS3 ) = .FALSE.
    INTEGER       :: NOBS
    INTEGER       :: OBSSIZE( MXVARS3 ) =  10
    CHARACTER*16  :: OBSNAME( MXVARS3 ) = BLANK

    LOGICAL       :: OVECPLOT            = .FALSE.
    LOGICAL       :: OVECFLAG( MXVARS3 ) = .FALSE.
    INTEGER       ::  NOVEC              =  0
    INTEGER       ::  OVECLEN( MXVARS3 ) =  0
    INTEGER       ::  OVECTHK( MXVARS3 ) =  1
    REAL          ::  OVECSCL( MXVARS3 ) = 25.0
    CHARACTER*16  ::   OUNAME( MXVARS3 ) = BLANK
    CHARACTER*16  ::   OVNAME( MXVARS3 ) = BLANK

    INTEGER       ::  NTVAR3D = 0
    INTEGER       ::  TTYPE3D( MXVARS3 ) = M3REAL
    CHARACTER*16  ::  TNAME3D( MXVARS3 ) = BLANK
    CHARACTER*16  ::  TUNIT3D( MXVARS3 ) = BLANK

    REAL        S, Y, Y0, Y1, UFAC, UOFF, YFAC, AFAC, ZFAC
    REAL        YMAX, YMIN, YBAR, ZMAX, ZMIN, ZBAR, SMAX, SMIN, SBAR, OMAX, OMIN, OBAR
    REAL*8      YSUM, SSUM, ZSUM, OSUM
    INTEGER     CMAX, CMIN, RMAX, RMIN, NCNT, OCNT
    INTEGER     CVMAX, CVMIN, RVMAX, RVMIN
    INTEGER     CZMAX, CZMIN, RZMAX, RZMIN

    INTEGER     VTYPE, ZTYPE

    INTEGER ::  NBINS = 6
    REAL    ::  BINS( MXBIN )

    CHARACTER*4   :: SUFFIX
    CHARACTER*16  :: VNAME, UNITS
    CHARACTER*24  :: DTBUF
    CHARACTER*64  :: BASENAME
    CHARACTER*32  :: MAPNAME1 = 'OUTLSTATES3000'
    CHARACTER*32  :: MAPNAME2 = 'OUTLCOUNTIES'
    CHARACTER*32  :: MAPNAME3 = BLANK
    CHARACTER*256 :: MESG, LINE1, LINE2
    CHARACTER*512 :: EQNAME, EQROOT, MAPDIR, INFILE, VECFILE, CONFILE, OBSFILE, OVECFILE

    REAL, ALLOCATABLE ::  RBUF( :,: )
    REAL, ALLOCATABLE ::  UBUF( :,: )
    REAL, ALLOCATABLE ::  VBUF( :,: )
    REAL, ALLOCATABLE ::  ZBUF( :,: )
    REAL, ALLOCATABLE ::  ALAT( :,: )
    REAL, ALLOCATABLE ::  ALON( :,: )
    REAL, ALLOCATABLE ::  COSA( :,: )
    REAL, ALLOCATABLE ::  SINA( :,: )
    REAL, ALLOCATABLE ::  OBUF( : )
    REAL, ALLOCATABLE ::  OBSX( : )
    REAL, ALLOCATABLE ::  OBSY( : )
    REAL, ALLOCATABLE ::  OSPD( : )
    REAL, ALLOCATABLE ::  OANG( : )
    REAL, ALLOCATABLE ::   OVX( : )
    REAL, ALLOCATABLE ::   OVY( : )


    !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

    LDEV  = INIT3()
    EFLAG = .FALSE.         !  no errors found yet

    WRITE( LDEV, '( 5X, A )' ) BLANK, BAR,                                      &
'Program M3PLOT to create tile-plots of variables from GRIDDED M3IO files',     &
'and save the results as image files, using the "GD" image library, with',      &
'optional wind-vector and observation overlay.',                                &
'',                                                                             &
'Output file formats are GIF, JPEG, and PNG; output file names are of',         &
'the form "${BASENAME}.<VNAME>[.<LLL>].<YYYYDDDHHMMSS.[gif | jpg | png]"',      &
'where ${BASENAME} may be a partial file-name, a directory, or blank,',         &
'VNAME is the variable-name, LLL is the layer number if the input file',        &
'has more than one layer, and YYYYDDDHHMMSS is the Julian date&time.',          &
'If there are no tile/smooth-plot variables, but there are vector-variables,',  &
'VNAME is <U-component-name>_<V-component-name>.',                              &
'',                                                                             &
'For time stepped files, there are two modes of indexing:  date&time based',    &
'and record based.  For a file starting at date&time 2009151 11:30:00',         &
'the starting record has index 2009150113000 in the date&time indexing',        &
'scheme, and index 0000 in the record based scheme.',                           &
'',                                                                             &
'For example, if the variable is "TA2" and the type is GIF, for the',           &
'date&time based indexing, the first imagefile names wil be:',                  &
'as follows:',                                                                  &
'     ${BASENAME} "/foo/bar/qux":  "/foo/bar/qux.TA2.2009150113000.gif"',       &
'     ${BASENAME} "/foo/bar/":     "/foo/bar/TA2.2009150113000.gif"',           &
'     ${BASENAME} blank:           "${cwd}/TA2.2009150113000.gif"',             &
'',                                                                             &
'With record based indexing the first file name would be something like',       &
'     "${BASENAME}/TA2.0000.gif"',                                              &
'',                                                                             &
'If the file is time independent, the index field will be omitted, e.g.',       &
'     "${BASENAME}/LANDUSE.gif"',                                               &
'',                                                                             &
'If the file is multilayered, a 3-digit layer-number field will precede',       &
'the date-time/record-number field, e.g., for layer-31 of variable "TA"',       &
'a file name might be  "/foo/bar/TA.031.2009150113000.gif"',                    &
'',                                                                             &
'If there are vector-overlays, the variable-name field will also include',      &
'the vector-component variable names, separated by underscores, e.g.,',         &
'"/foo/bar/TA_UWIND_VWIND.031.2009150113000.gif"',                              &
'',                                                                             &
'Tiled/smoothed variables and observations may contain MISSING data;',          &
'however, vector-component data must not.',                                     &
'',                                                                             &
'PRECONDITIONS REQUIRED:',                                                      &
'     setenv INFILE    <path name for gridded tile-plot input file>',           &
'     setenv VECFILE   <path name for       vector-input file,   or "NONE">',   &
'     setenv CONFILE   <path name for      contour-input file,   or "NONE">',   &
'     setenv OBSFILE   <path name for observations-input file,   or "NONE">',   &
'     setenv OVECFILE  <path name for   obs-vector-input file,   or "NONE">',   &
'',                                                                             &
'  if ${OVECFILE} not "NONE"',                                                  &
'     setenv  LLFILE   <path name for Lat-Lon (e.g., GRID_CRO_2D) file>',       &
'',                                                                             &
'     setenv CONFIG    <path name for configuration file>',                     &
'     setenv BASENAME  <base name or directory for output images>',             &
'                      If directory, should end with "/"',                      &
'                      LEN( final image-file names ) < 512',                    &
'     setenv MAPDIR    <directory for maps, or "NONE"> [${EDSS_MAPDIR}]',       &
'     setenv IMGDATES  <use date-&-time instead of record-numbers> [Yes]',      &
'     setenv LOGSTATS  <report gridded stats to log?>              [No]',       &
'',                                                                             &
'    ${VECFILE} and ${INFILE} share either dot-point::cross-point grid',        &
'    relationship or the very same grid, and should have a common',             &
'    time step sequence.',                                                      &
'',                                                                             &
'    ${VECFILE} and ${CONFILE} share the very same grid and a common',          &
'    time step sequence.',                                                      &
'',                                                                             &
'    Total number of colors in ${CONFIG} (below) should not exceed 253.',       &
''

    WRITE( LDEV, '( 5X, A )' )                                                  &
'PROGRAM CONTROL:',                                                             &
'${CONFIG} is ASCII , with lines to control the program options, of the form',  &
'',                                                                             &
'    "<NAME>        <value(s)>":',                                              &
'',                                                                             &
'with the folowing fields (where note that COLOR, VCOLOR, and CCOLOR:',         &
'give tables of consecutive color-indices and RGB values.',                     &
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
'                   [default tile palette is a 10-color scale]',                &
'    VCOLOR         <index> <red> <green> <blue> for vector-wind map',          &
'                   where index is consecutive for some range 0 ... N <253 ',   &
'                   and color values are in [0...255]',                         &
'                   [default vector palette is a 10-color scale,',              &
'                   from dark blue to dark green]',                             &
'    CCOLOR         <index> <red> <green> <blue> for contour map',              &
'                   where index is consecutive for some range 0 ... N <253 ',   &
'                   and color values are in [0...255]',                         &
'                   [default contour palette is a 10-color scale,',             &
'                   from dark blue to dark red]',                               &
'    MISSING_COLOR  <red> <green> <blue> [127 127 127]',                        &
'',                                                                             &
'    Map-control variables, for up to 3 maps:',                                 &
'    MAPNAME1       <"states" map-file name in ${MAPDIR} [OUTLHRES]>',          &
'    MAPNAME2       <"county" map-file name in ${MAPDIR} [OUTLCOUNTIES]>',      &
'    MAPNAME3       <"extra"  map-file name in ${MAPDIR} [<blank>]>',           &
'                   Use blank or missing values to suppress these maps.',       &
'',                                                                             &
'    MAPCOLOR1      <red> <green> <blue> [  0   0   0]',                        &
'    MAPCOLOR2      <red> <green> <blue> [ 31  31  31]',                        &
'    MAPCOLOR3      <red> <green> <blue> [ 63  63  63]',                        &
'',                                                                             &
'    MAPWIDTH1      <line-width (pixels) [1]',                                  &
'    MAPWIDTH2      <line-width (pixels) [1]',                                  &
'    MAPWIDTH3      <line-width (pixels) [1]',                                  &
''

    WRITE( LDEV, '( 5X, A )' )                                                  &
'with repeated sections of variable-names and options, per variable plotted:',  &
'',                                                                             &
'    VARIABLE       <vble in ${INFILE}||"ALL"> <[LIN|LOG|EXP]>',                &
'                   e.g., "VARIABLE ALL LIN" for all-vbles "normal" plots,',    &
'                   "VARIABLE TA LIN" for a "normal" TA-plot,',                 &
'                   "VARIABLE TA LOG" for a  log-scaled TA-plot, and',          &
'                   "VARIABLE TA EXP" for an exp-scaled TA-plot.',              &
'                   [uses color scale from COLOR, above]',                      &
'    UNITS          <units-name> [<scale factor> [<scale offset>]]',            &
'                   where default is <units from file>, and scaling uses',      &
'                   the formula',                                               &
'                       Y_display = scale * Y_file +  offset',                  &
'    LEVEL          <model-level> [1]',                                         &
'    SCALE_RANGE    <min> <max>  for the tile color-scale [data-min,data-max]', &
'    FORMAT_BINS    <"C" format for color-scale legend-labels> ["%g"]',         &
'',                                                                             &
'    VECTOR         <X-component name  Y-component name in ${VECFILE}> ',       &
'    VEC_RANGE      <max> for the windspeed color-scale [0.0, vec-max]',        &
'    VEC_LEVEL      <wind-grid model level [1]>',                               &
'    VEC_INCREMENT  <sampling interval for the vector plot (cells)> [1]',       &
'    VEC_THICKNESS  <arrow-shaft thickness (pixels)> [1]',                      &
'                   [uses color scale from VCOLOR, above]',                     &
'',                                                                             &
'    CONTOUR        <variable name in ${CONFILE}> ',                            &
'    CON_UNITS      <units-name> <scale factor> [<units from file> 1]',         &
'    CON_RANGE      <min> <max> for the contour color-scale',                   &
'    CON_LEVEL      <contour-grid model level [1]>',                            &
'    CON_THICKNESS  <contour-line thickness (pixels)> [2]',                     &
'    CON_SMOOTH     <smooth grid to be contoured?> [TRUE]',                     &
'                   [uses color scale from CCOLORi0 - 15, above]',              &
'',                                                                             &
'    OBSERVATION    scalar <observation variable name>',                        &
'    OBS_SIZE       <obs-marker size (pixels)> [10]',                           &
'                   [uses color scale from COLOR, above]',                      &
'',                                                                             &
'    OBS_VECTOR     <speed-vble name> <bearing-vble name>',                     &
'    OVEC_RANGE     <max> for the windspeed color-scale [0.0, vec-max]',        &
'    OVEC_LENGTH    <obs-arrow length (pixels)> [10]',                          &
'    OVEC_THICKNESS <arrow-shaft thickness (pixels)> [1]',                      &
'                   [uses color scale from VCOLOR, above]',                     &
'',                                                                             &
'    TITLE1         <First  legend-header line> [VNAME]',                       &
'    TITLE2         <Second legend-header line> [VDESC]',                       &
'    TITLE3         <Third  legend-header line> [${INFILE}]',                   &
'',                                                                             &
'    Blank lines are ignored (and may be used as section-separators).',         &
'    UNITS option invalid after "VARIABLE ALL..."',                             &
'',                                                                             &
'    Options may appear at most once per tile/smooth-plot variable.',           &
'    One may plot vector-only by using an empty set of VARIABLEs;',             &
'    however, ${INFILE} is still required in order to specify the',             &
'    grid definitions and time step sequence.',                                 &
'',                                                                             &
'    Total number of colors used is at most 253.',                              &
'',                                                                             &
'THE PROGRAM WILL PROMPT YOU for starting date and time, time step, and',       &
'number of time steps to process.',                                             &
'',                                                                             &
'Copyright (C) 2010-2011 Baron Advanced Meteorological Systems, LLC.',          &
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
'$Id: m3plot.f90 280 2025-04-12 15:34:39Z coats $',&
''

    IF ( .NOT. GETYN( 'Continue with program?', .TRUE. ) ) THEN
        MESG = 'Program terminated at user request'
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
    END IF
    CALL M3MESG( BAR )


    !!...............  Get environment variable :

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

    STATFLAG = ENVYN( 'LOGSTATS', 'Report gridded stat to log?', .FALSE., ISTAT )
    IF ( ISTAT .GT. 0 ) THEN
        EFLAG = .TRUE.
        CALL M3MESG( 'Bad environment variable "LOGSTATS"' )
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

    CALL ENVSTR( 'VECFILE', 'Logical name for wind file', 'NONE', VECFILE, ISTAT )
    IF ( ISTAT .GT. 0 ) THEN
        EFLAG = .TRUE.
        CALL M3MESG( 'Bad environment variable "VECFILE"' )
    ELSE IF ( VECFILE .EQ. 'NONE' ) THEN
        VECPLOT = .FALSE.
        CALL M3MESG( 'Vector-plot option turned off' )
    ELSE
        VECPLOT = .TRUE.
    END IF

    CALL ENVSTR( 'CONFILE', 'Logical name for contour-variable file', 'NONE', CONFILE, ISTAT )
    IF ( ISTAT .GT. 0 ) THEN
        EFLAG = .TRUE.
        CALL M3MESG( 'Bad environment variable "CONFILE"' )
    ELSE IF ( CONFILE .EQ. 'NONE' ) THEN
        CONPLOT = .FALSE.
        CALL M3MESG( 'Contour-plot option turned off' )
    ELSE
        CONPLOT = .TRUE.
    END IF

    CALL ENVSTR( 'OBSFILE', 'Logical name for scalar-observation file', 'NONE', OBSFILE, ISTAT )
    IF ( ISTAT .GT. 0 ) THEN
        EFLAG = .TRUE.
        CALL M3MESG( 'Bad environment variable "OBSFILE"' )
    ELSE IF ( OBSFILE .EQ. 'NONE' ) THEN
        OBSPLOT = .FALSE.
        CALL M3MESG( 'Observation-plot option turned off' )
    ELSE
        OBSPLOT = .TRUE.
    END IF

    CALL ENVSTR( 'OVECFILE', 'Logical name for vector-observation file', 'NONE', OVECFILE, ISTAT )
    IF ( ISTAT .GT. 0 ) THEN
        EFLAG = .TRUE.
        CALL M3MESG( 'Bad environment variable "OVECFILE"' )
    ELSE IF ( OVECFILE .EQ. 'NONE' ) THEN
        OVECPLOT = .FALSE.
        CALL M3MESG( 'Observation-vector option turned off' )
    ELSE
        OVECPLOT = .TRUE.
    END IF


    !!...............  Open input files:

     IF ( .NOT.OPEN3( 'INFILE', FSREAD3, PNAME ) ) THEN
        EFLAG = .TRUE.
        MESG  = 'Could not open "INFILE"'
        CALL M3MESG( MESG )
    ELSE IF ( .NOT.DESC3( 'INFILE' ) ) THEN
        EFLAG = .TRUE.
        MESG  = 'Could not get description for "INFILE"'
        CALL M3MESG( MESG )
    ELSE

        CALL NAMEVAL( 'INFILE', INFILE )

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
        XFINL1 = XORIG3D + DBLE( NCOLS3D ) * XCELL3D
        YFINL1 = YORIG3D + DBLE( NROWS3D ) * YCELL3D
        XCELL1 = XCELL3D
        YCELL1 = YCELL3D
        SDATE1 = SDATE3D
        STIME1 = STIME3D
        TSTEP1 = TSTEP3D
        NRECS1 = MXREC3D
        NVARS1 = NVARS3D

        VNAME1( 1:NVARS1 ) = VNAME3D( 1:NVARS1 )
        UNITS1( 1:NVARS1 ) = UNITS3D( 1:NVARS1 )
        VDESC1( 1:NVARS1 ) = VDESC3D( 1:NVARS1 )
        VTYPE1( 1:NVARS1 ) = VTYPE3D( 1:NVARS1 )

        COL0 = 1                        !  default window to plot
        COL1 = NCOLS1
        ROW0 = 1
        ROW1 = NROWS1

        NCOLS2 = NCOLS3D + 1
        NROWS2 = NROWS3D + 1
        XORIG2 = XORIG3D - 0.5D0 * XCELL1
        YORIG2 = YORIG3D - 0.5D0 * YCELL1

        CALL INITXY( GDTYP1, P_ALP1, P_BET1, P_GAM1, XCENT1, YCENT1 )

    END IF              !  if not open3(infile...); else ...


    IF ( .NOT.VECPLOT ) THEN
        CONTINUE
    ELSE IF ( .NOT.OPEN3( 'VECFILE', FSREAD3, PNAME ) ) THEN
        EFLAG = .TRUE.
        MESG  = 'Could not open "VECFILE"'
        CALL M3MESG( MESG )
    ELSE IF ( .NOT.DESC3( 'VECFILE' ) ) THEN
        EFLAG = .TRUE.
        MESG  = 'Could not get description for "VECFILE"'
        CALL M3MESG( MESG )

    ELSE IF ( DBLSAME( XORIG3D, XORIG1 ) .AND. DBLSAME( YORIG3D, YORIG1 ) ) THEN

        !!  cross-point vecfile:

        VECDOT = 0
        NLAYS2 = NLAYS3D
        IF ( TSTEP1 .EQ. 0 ) THEN
            SDATE1 = SDATE3D
            STIME1 = STIME3D
            TSTEP1 = TSTEP3D
            TSTEP  = SEC2TIME( ( MXREC3D - 1 ) * TIME2SEC( TSTEP3D ) )
            EDATE  = SDATE3D
            ETIME  = STIME3D
            CALL NEXTIME( EDATE, ETIME, TSTEP )
        END IF

        IF ( .NOT.FILCHK3( 'VECFILE', GRDDED3, NCOLS1, NROWS1, NLAYS3D, 1 ) ) THEN
            EFLAG = .TRUE.
            MESG  = 'Inconsistent dimensions/type for "VECFILE"'
            CALL M3MESG( MESG )
        ELSE IF ( .NOT.GRDCHK3( 'VECFILE',                               &
                                P_ALP1, P_BET1, P_GAM1, XCENT1, YCENT1,  &
                                XORIG1, YORIG1, XCELL1, YCELL1,          &
                                NLAYS3D, VGTYP3D, VGTOP3D, VGLVS3D ) ) THEN
            EFLAG = .TRUE.
            MESG  = 'Inconsistent coordinates for "VECFILE"'
            CALL M3MESG( MESG )
        END IF

    ELSE IF ( DBLSAME( XORIG3D, XORIG2 ) .AND. DBLSAME( YORIG3D, YORIG2 ) ) THEN

        !!  dot-point vecfile:

        VECDOT = 1
        NLAYS2 = NLAYS3D
        IF ( TSTEP1 .EQ. 0 ) THEN
            SDATE1 = SDATE3D
            STIME1 = STIME3D
            TSTEP1 = TSTEP3D
            TSTEP  = SEC2TIME( ( MXREC3D - 1 ) * TIME2SEC( TSTEP3D ) )
            EDATE  = SDATE3D
            ETIME  = STIME3D
            CALL NEXTIME( EDATE, ETIME, TSTEP )
        END IF

        IF ( .NOT.FILCHK3( 'VECFILE', GRDDED3, NCOLS2, NROWS2, NLAYS3D, 1 ) ) THEN
            EFLAG = .TRUE.
            MESG  = 'Inconsistent dimensions/type for "VECFILE"'
            CALL M3MESG( MESG )
        ELSE IF ( .NOT.GRDCHK3( 'VECFILE',                               &
                                P_ALP1, P_BET1, P_GAM1, XCENT1, YCENT1,  &
                                XORIG2, YORIG2, XCELL1, YCELL1,          &
                                NLAYS3D, VGTYP3D, VGTOP3D, VGLVS3D ) ) THEN
            EFLAG = .TRUE.
            MESG  = 'Inconsistent coordinates for "VECFILE"'
            CALL M3MESG( MESG )
        END IF

    ELSE

        EFLAG = .TRUE.
        MESG  = 'Inconsistent grid-origin for "VECFILE"'
        CALL M3MESG( MESG )

    END IF              !! if not vecplot; or not open3(vecfile...); else ...


    IF ( .NOT.CONPLOT ) THEN
        CONTINUE
    ELSE IF ( .NOT.OPEN3( 'CONFILE', FSREAD3, PNAME ) ) THEN
        EFLAG = .TRUE.
        MESG  = 'Could not open "CONFILE"'
        CALL M3MESG( MESG )
    ELSE IF ( .NOT.DESC3( 'CONFILE' ) ) THEN
        EFLAG = .TRUE.
        MESG  = 'Could not get description for "CONFILE"'
        CALL M3MESG( MESG )
    ELSE  IF ( .NOT.FILCHK3( 'CONFILE', GRDDED3, NCOLS1, NROWS1, NLAYS3D, 1 ) ) THEN
        EFLAG = .TRUE.
        MESG  = 'Inconsistent dimensions/type for "CONFILE"'
        CALL M3MESG( MESG )
    ELSE IF ( .NOT.GRDCHK3( 'CONFILE',                               &
                            P_ALP1, P_BET1, P_GAM1, XCENT1, YCENT1,  &
                            XORIG1, YORIG1, XCELL1, YCELL1,          &
                            NLAYS3D, VGTYP3D, VGTOP3D, VGLVS3D ) ) THEN
        EFLAG = .TRUE.
        MESG  = 'Inconsistent coordinates for "CONFILE"'
        CALL M3MESG( MESG )
    ELSE
        NZVAR1 = NVARS3D
        ZNAME1( 1:NZVAR1 ) = VNAME3D( 1:NZVAR1 )
        ZUNIT1( 1:NZVAR1 ) = UNITS3D( 1:NZVAR1 )
        ZTYPE1( 1:NZVAR1 ) = VTYPE3D( 1:NZVAR1 )
    END IF


    IF ( .NOT.OBSPLOT ) THEN
        CONTINUE
    ELSE IF ( .NOT.OPEN3( 'OBSFILE', FSREAD3, PNAME ) ) THEN
        EFLAG = .TRUE.
        MESG  = 'Could not open "OBSFILE"'
        CALL M3MESG( MESG )
    ELSE IF ( .NOT.DESC3( 'OBSFILE' ) ) THEN
        EFLAG = .TRUE.
        MESG  = 'Could not get description for "OBSFILE"'
        CALL M3MESG( MESG )
    ELSE

        NOBS = NCOLS3D

        IF ( TSTEP1 .EQ. 0 ) THEN
            SDATE1 = SDATE3D
            STIME1 = STIME3D
            TSTEP1 = TSTEP3D
            TSTEP  = SEC2TIME( ( MXREC3D - 1 ) * TIME2SEC( TSTEP3D ) )
            EDATE  = SDATE3D
            ETIME  = STIME3D
            CALL NEXTIME( EDATE, ETIME, TSTEP )
        END IF

        ALLOCATE( OBUF( NOBS ),        &
                  OBSX( NOBS ),        &
                  OBSY( NOBS ), STAT = ISTAT )
        IF ( ISTAT .NE. 0 ) THEN
            EFLAG = .TRUE.
            MESG  = 'Observation-buffer allocation error'
            CALL M3MESG( MESG )
        END IF

    END IF              !!  if not obsplot; else if not open3(obsfile...); else...


    IF ( .NOT.OVECPLOT ) THEN
        CONTINUE
    ELSE IF ( .NOT.OPEN3( 'OVECFILE', FSREAD3, PNAME ) ) THEN
        EFLAG = .TRUE.
        MESG  = 'Could not open "OVECFILE"'
        CALL M3MESG( MESG )
    ELSE IF ( .NOT.DESC3( 'OVECFILE' ) ) THEN
        EFLAG = .TRUE.
        MESG  = 'Could not get description for "OVECFILE"'
        CALL M3MESG( MESG )
    ELSE

        NOVEC = NCOLS3D

        IF ( TSTEP1 .EQ. 0 ) THEN
            SDATE1 = SDATE3D
            STIME1 = STIME3D
            TSTEP1 = TSTEP3D
            TSTEP  = SEC2TIME( ( MXREC3D - 1 ) * TIME2SEC( TSTEP3D ) )
            EDATE  = SDATE3D
            ETIME  = STIME3D
            CALL NEXTIME( EDATE, ETIME, TSTEP )
        END IF

        IF ( .NOT.OPEN3( 'LLFILE', FSREAD3, PNAME ) ) THEN
            EFLAG = .TRUE.
            MESG  = 'Could not open "LLFILE"'
            CALL M3MESG( MESG )
        ELSE IF ( .NOT.DESC3( 'LLFILE' ) ) THEN
            EFLAG = .TRUE.
            MESG  = 'Could not get description for "LLFILE"'
            CALL M3MESG( MESG )
        ELSE IF ( .NOT.FILCHK3( 'LLFILE', GRDDED3, NCOLS1, NROWS1, NLAYS3D, 1 ) ) THEN
                EFLAG = .TRUE.
                MESG  = 'Inconsistent dimensions/type for "LLFILE"'
                CALL M3MESG( MESG )
        ELSE IF ( .NOT.GRDCHK3( 'LLFILE',                                &
                                P_ALP1, P_BET1, P_GAM1, XCENT1, YCENT1,  &
                                XORIG1, YORIG1, XCELL1, YCELL1,          &
                                NLAYS3D, VGTYP3D, VGTOP3D, VGLVS3D ) ) THEN
            EFLAG = .TRUE.
            MESG  = 'Inconsistent coordinates for "LLFILE"'
            CALL M3MESG( MESG )
        END IF

        ALLOCATE(  OSPD( NOVEC ),        &
                   OANG( NOVEC ),        &
                    OVX( NOVEC ),        &
                    OVY( NOVEC ),        &
                  ALAT( NCOLS1,NROWS1 ), &
                  ALON( NCOLS1,NROWS1 ), &
                  COSA( NCOLS1,NROWS1 ), &
                  SINA( NCOLS1,NROWS1 ), STAT = ISTAT )
        IF ( ISTAT .NE. 0 ) THEN
            EFLAG = .TRUE.
            MESG  = 'Observation-vector buffer allocation error'
            CALL M3MESG( MESG )
        ELSE IF ( .NOT.READ3( 'LLFILE', 'LAT', 1,0,0, ALAT ) ) THEN
            EFLAG = .TRUE.
            MESG  = 'Could not readl "LAT" from "LLFILE"'
            CALL M3MESG( MESG )
        ELSE IF ( .NOT.READ3( 'LLFILE', 'LON', 1,0,0, ALON ) ) THEN
            EFLAG = .TRUE.
            MESG  = 'Could not readl "LON" from "LLFILE"'
            CALL M3MESG( MESG )
        ELSE
            CALL LLANGLE( ALAT, ALON, COSA, SINA )
        END IF

    END IF              !!  if not obsplot; else if not open3(obsfile...); else...


    !!...............  Process configuration file:

    CALL RDCONFIG( EFLAG )
    IF ( NXPIX .LT. 280 .OR. NYPIX .LT. 240 ) THEN
        EFLAG = .TRUE.
        CALL M3MESG( 'Image size less than minimum 280x240' )
    END IF


    IF ( EFLAG ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'Fatal input file / environment error(s)', 2 )
    END IF
    CALL M3MESG( BAR )


    !!...............  Allocate grid; initialize graphics; set up maps:

    IF ( VECPLOT .AND. CONPLOT ) THEN
        ALLOCATE( RBUF( NCOLS1,NROWS1 ),        &
                  ZBUF( NCOLS1,NROWS1 ),        &
                  UBUF( NCOLS2,NROWS2 ),        &
                  VBUF( NCOLS2,NROWS2 ), STAT = ISTAT )
    ELSE IF ( VECPLOT ) THEN
        NZRGB = 0
        ALLOCATE( RBUF( NCOLS1,NROWS1 ),        &
                  UBUF( NCOLS2,NROWS2 ),        &
                  VBUF( NCOLS2,NROWS2 ), STAT = ISTAT )
    ELSE IF ( CONPLOT ) THEN
        NVRGB = 0
        ALLOCATE( RBUF( NCOLS1,NROWS1 ),        &
                  ZBUF( NCOLS1,NROWS1 ), STAT = ISTAT )
    ELSE
        NVRGB = 0
        NZRGB = 0
        ALLOCATE( RBUF( NCOLS1,NROWS1 ), STAT = ISTAT )
    END IF

    IF ( ISTAT .NE. 0 ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'Grid-buffer allocation error', 2 )
    END IF


    CALL INITPLOT( PLOTONLY, VECDOT,                                    &
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


    !!...............  Get time step sequence:

    CALL M3MESG( BAR )
    IF ( TSTEP1 .GT. 0 ) THEN
        SDATE = GETNUM( SDATE1, 9999999, SDATE1, 'Enter starting date' )
        STIME = GETNUM( 0,      9999999, STIME1, 'Enter starting time' )
        TSTEP = GETNUM( 0,      9999999, TSTEP1, 'Enter     time step' )
        CALL LASTTIME( SDATE1, STIME1, TSTEP1, NRECS1, EDATE, ETIME )
        N     = CURREC( EDATE, ETIME, SDATE, STIME, TSTEP, JDATE, JTIME )
        NRECS = GETNUM( 1,      9999999, N     , 'Enter  # of records' )
    ELSE
        SDATE = 0
        STIME = 0
        TSTEP = 0
        NRECS = 1
        CALL M3MESG( 'All inputs are time-independent' )
    END IF


    !!...............  Process variable // time step sequence:
    !!...............  Get scale max/min where needed

    IF ( VARPLOT ) THEN

        DO V = 1, NVARS

            VNAME = VNAMES( V )
            UNITS = VUNITS( V )
            VTYPE = VTYPES( V )

            CALL M3MESG( BAR )
            CALL M3MESG( 'Processing "' // TRIM( VNAME ) // '"' )

            IF ( VMIN( V ) .GT. VMAX( V ) ) THEN    !  scale max & min not yet set

                UFAC  = VFAC( V )
                UOFF  = VOFF( V )
                YMAX  = -9.999E36
                YMIN  =  9.999E36
                JDATE = SDATE
                JTIME = STIME
                CALL NEXTIME( JDATE, JTIME, -TSTEP )

                DO N = 1, NRECS

                    CALL NEXTIME( JDATE, JTIME,  TSTEP )

                    IF ( READIT( 'INFILE', VNAME, VTYPE, NCOLS1, NROWS1, VARLVL(V), JDATE, JTIME, RBUF ) ) THEN

                        DO R = ROW0, ROW1
                        DO C = COL0, COL1
                            Y = RBUF( C,R )
                            IF ( Y .LT. AMISS3 )  CYCLE
                            Y = UFAC * Y + UOFF
                            IF ( Y .LT. YMIN   )  YMIN = Y
                            IF ( Y .GT. YMAX   )  YMAX = Y
                        END DO
                        END DO

                    ELSE            !  read3() failed:

                        EFLAG = .TRUE.
                        WRITE( MESG, '( 3A, I9.7, A, I6.6 )' )      &
                            'Error reading "', TRIM( VNAME ),       &
                            '" from "INFILE" for ', JDATE, ':', JTIME
                        CALL M3MESG( MESG )

                    END IF          !  if read3() succeeded, or not

                END DO      !  end max/min loop on time steps N

                IF ( YMAX .LT. YMIN ) THEN
                    MESG = 'No valid data for "' // TRIM( VNAME ) // '" in "INFILE"'
                    CALL M3MESG( MESG )
                END IF

                VMAX( V ) = YMAX
                VMIN( V ) = YMIN

            END IF


            !!........  Compute scaling factors:  rescaled V should have range [0 , NRGB-1 ]

            IF ( VMAX( V ) .LT. VMIN( V ) ) THEN

                CALL M3MESG( 'Bad scale max / min for "' // TRIM( VNAME ) // '" in "INFILE"' )
                Y0   = VMAX( V )
                YFAC = 0.0

            ELSE IF ( MODE( V ) .EQ. LINSCL ) THEN

                Y0   = VMIN( V )
                YFAC = FLOAT( NRGB ) / ( VMAX( V ) - VMIN( V ) )

            ELSE IF ( MODE( V ) .EQ. EXPSCL ) THEN

                Y0   = EXP( VMIN( V ) )
                YFAC = FLOAT( NRGB ) / ( EXP( VMAX( V ) ) - Y0 )

            ELSE IF ( MODE( V ) .EQ. LOGSCL ) THEN

                IF ( VMIN( V ) .LT. 0.0 ) THEN
                    CALL M3MESG( 'Cannot do LOG() of negative values "' // TRIM( VNAME ) // '" in "INFILE"' )
                    CYCLE
                END IF

                Y0   = LOG( VMIN( V ) )
                YFAC = FLOAT( NRGB ) / ( LOG( VMAX( V ) ) - Y0 )

            END IF          !  if mode = lin | log | exp

            IF ( CONFLAG( V ) .AND. CONMIN( V ) .GT. CONMAX( V ) ) THEN    !  scale max & min not yet set

                UFAC  = CONFAC( V )
                YMAX  = -9.999E36
                YMIN  =  9.999E36
                JDATE = SDATE
                JTIME = STIME
                CALL NEXTIME( JDATE, JTIME, -TSTEP )

                DO N = 1, NRECS

                    CALL NEXTIME( JDATE, JTIME,  TSTEP )

                    IF ( READIT( 'CONFILE', ZNAMES(V), ZTYPES(V), NCOLS1, NROWS1, CONLVL(V), JDATE, JTIME, RBUF ) ) THEN

                        DO R = ROW0, ROW1
                        DO C = COL0, COL1
                            Y = RBUF( C,R )
                            IF ( Y .LT. AMISS3 )  CYCLE
                            Y = UFAC * Y
                            IF ( Y .LT. YMIN   )  YMIN = Y
                            IF ( Y .GT. YMAX   )  YMAX = Y
                        END DO
                        END DO

                    ELSE            !  read3() failed:

                        EFLAG = .TRUE.
                        WRITE( MESG, '( 3A, I9.7, A, I6.6 )' )      &
                            'Error reading "', TRIM( VNAME ),       &
                            '" from "CONFILE" for ', JDATE, ':', JTIME
                        CALL M3MESG( MESG )

                    END IF          !  if read3() succeeded, or not

                END DO      !  end max/min loop on time steps N

                IF ( YMAX .LT. YMIN ) THEN
                    MESG = 'No valid data for "' // TRIM( VNAME ) // '" in "INFILE"'
                    CALL M3MESG( MESG )
                END IF

                CONMAX( V ) = YMAX
                CONMIN( V ) = YMIN

            END IF                                  !  if  scale max & min not yet set


            !!........  Construct legend-titles:

            IF ( TITLE1(V) .EQ. CMISS3 )  THEN

                IF ( NLAYS1 .GT. 1 ) THEN
                    WRITE( LINE1, '( A, I3, 1X, A )' ) 'Layer', VARLVL(V), VNAME
                ELSE
                    LINE1 = VNAME
                END IF

                IF ( .NOT.VECFLAG( V ) ) THEN
                    LINE2 = BLANK
                ELSE IF ( NLAYS2 .GT. 1 ) THEN
                    WRITE( LINE2, '( A, I3, 5A )' ) 'Layer', VECLVL(V), ' <', TRIM( NAMEU(V) ), '|', TRIM( NAMEV(V) ), '>'
                ELSE
                    LINE2 = '<' // TRIM( NAMEU(V) ) // ' | ' // TRIM( NAMEV(V) ) // '>'
                END IF

                TITLE1(V) = TRIM( LINE1 ) // BLANK // LINE2

            END IF

            IF ( TITLE3(V) .EQ. CMISS3 )  THEN

                LINE1 = INFILE

                IF ( .NOT.VECFLAG( V ) ) THEN
                    LINE2 = VECFILE
                ELSE
                    LINE2 = BLANK
                END IF

                TITLE3(V) = TRIM( LINE1 ) // BLANK // TRIM( LINE2 )

            END IF


            !!........  Now process the data and make the plots

            JDATE = SDATE
            JTIME = STIME
            CALL NEXTIME( JDATE, JTIME, -TSTEP )

            DO N = 1, NRECS

                CALL NEXTIME( JDATE, JTIME,  TSTEP )
                WRITE ( MESG, '( A, I9.7, A, I6.6 )' ) 'Processing', JDATE, ':', JTIME
                CALL M3MESG( MESG )

                CALL IMGCLR()   !!  Clear this-step image storage

                RDGRID = READIT( 'INFILE', VNAME, VTYPE, NCOLS1, NROWS1, VARLVL(V), JDATE, JTIME, RBUF )


                IF ( VECFLAG( V ) ) THEN
                    RDVEC = (    READ3( 'VECFILE', NAMEU(V), VECLVL(V), JDATE, JTIME, UBUF )     &
                            .AND.READ3( 'VECFILE', NAMEV(V), VECLVL(V), JDATE, JTIME, VBUF ) )
                ELSE
                    RDVEC = .FALSE.
                END IF                  !!  if not vecplot; else if reads failed

                IF ( RDVEC ) THEN       !! if vector reads succeeded, compute stats

                    SMAX = -9.999E36
                    SMIN =  9.999E36
                    SSUM =  0.0D0

                    DO R = ROW0, ROW1
                    DO C = COL0, COL1
                        S = UBUF( C,R )**2 + VBUF( C,R )**2
                        SSUM = SSUM + DBLE( S )
                        IF ( S .LT. SMIN   ) THEN
                            SMIN = S
                            CMIN = C
                            RMIN = R
                        END IF
                        IF ( S .GT. SMAX   )THEN
                            SMAX = S
                            CMAX = C
                            RMAX = R
                        END IF
                    END DO
                    END DO

                    SMAX  = SQRT( SMAX )
                    SMIN  = SQRT( SMIN )
                    SBAR  = SQRT( SSUM / DBLE( ( ROW1 - ROW0 + 1 ) * ( COL1 - COL0 + 1 ) ) )
                    CVMAX = CMAX
                    CVMIN = CMIN
                    RVMAX = RMAX
                    RVMIN = RMIN

                ELSE

                    RDVEC = .FALSE.
                    SMAX  =  BADVAL3
                    SMIN  = -BADVAL3
                    SBAR  =  BADVAL3
                    CVMAX =  IMISS3
                    CVMIN =  IMISS3
                    RVMAX =  IMISS3
                    RVMIN =  IMISS3

                END IF                  !! if vector reads succeeded, compute stats


                IF ( CONFLAG( V ) ) THEN
                    RDCON = READIT( 'CONFILE', ZNAMES(V), ZTYPES(V), NCOLS1, NROWS1, CONLVL(V), JDATE, JTIME, ZBUF )
                ELSE
                    RDCON = .FALSE.
                END IF                  !!  if not vecplot; else if reads failed

                IF ( RDCON ) THEN       !! if vector reads succeeded, compute stats

                    ZMAX = -9.999E36
                    ZMIN =  9.999E36
                    ZSUM =  0.0D0
                    ZFAC = CONFAC( V )

                    DO R = ROW0, ROW1
                    DO C = COL0, COL1
                        S    = ZFAC * ZBUF( C,R )
                        ZSUM = ZSUM + DBLE( S )
                        IF ( S .LT. ZMIN   ) THEN
                            ZMIN = S
                            CMIN = C
                            RMIN = R
                        END IF
                        IF ( S .GT. ZMAX   )THEN
                            ZMAX = S
                            CMAX = C
                            RMAX = R
                        END IF
                    END DO
                    END DO

                    ZBAR  = ZSUM / DBLE( ( ROW1 - ROW0 + 1 ) * ( COL1 - COL0 + 1 ) )
                    CZMAX = CMAX
                    CZMIN = CMIN
                    RZMAX = RMAX
                    RZMIN = RMIN

                ELSE

                    RDCON = .FALSE.
                    ZMAX  = BADVAL3
                    ZMIN  = BADVAL3
                    ZBAR  = BADVAL3
                    CZMAX = IMISS3
                    CZMIN = IMISS3
                    RZMAX = IMISS3
                    RZMIN = IMISS3

                END IF                  !! if vector reads succeeded, compute stats


                IF ( OBSFLAG( V ) ) THEN
                    IF (   READ3( 'OBSFILE',      'LAT', 1, JDATE, JTIME, OBSX )     &
                      .AND.READ3( 'OBSFILE',      'LON', 1, JDATE, JTIME, OBSY )     &
                      .AND.READ3( 'OBSFILE', OBSNAME(V), 1, JDATE, JTIME, OBUF ) ) THEN

                        RDOBS = .TRUE.
                        CALL LLTOXY( NOBS, OBSX, OBSY )

                    END IF

                ELSE
                    RDOBS = .FALSE.
                END IF                  !!  if obs-read succeeds, rescale as appropriate


                IF ( OVECFLAG( V ) ) THEN
                    IF (   READ3( 'OVECFILE',     'LAT', 1, JDATE, JTIME,  OVX )     &
                      .AND.READ3( 'OVECFILE',     'LON', 1, JDATE, JTIME,  OVY )     &
                      .AND.READ3( 'OVECFILE', OUNAME(V), 1, JDATE, JTIME, OSPD )     &
                      .AND.READ3( 'OVECFILE', OVNAME(V), 1, JDATE, JTIME, OANG ) ) THEN

                        RDOVEC = .TRUE.
                        CALL LLTOXY( NOVEC, OVX, OVY )

                    END IF

                ELSE
                    RDOVEC = .FALSE.
                END IF                  !!  if obs-read succeeds, rescale as appropriate


                !!........  Compute grid stats for tiled/smoothed variable:

                YMAX = -9.999E36
                YMIN =  9.999E36
                YSUM =  0.0D0
                NCNT =  0
                UFAC = VFAC( V )
                UOFF = VOFF( V )

                IF ( .NOT.RDGRID ) THEN

                    DO R = ROW0, ROW1
                    DO C = COL0, COL1
                        RBUF( C,R ) = BADVAL3
                    END DO
                    END DO

                ELSE IF ( MODE( V ) .EQ. LINSCL ) THEN

                    DO R = ROW0, ROW1
                    DO C = COL0, COL1
                        Y = RBUF( C,R )
                        IF ( Y .LT. AMISS3 )  CYCLE
                        Y    = UFAC * Y  +  UOFF
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

                ELSE IF ( MODE( V ) .EQ. LOGSCL ) THEN

                    DO R = ROW0, ROW1
                    DO C = COL0, COL1
                        Y = RBUF( C,R )
                        IF ( Y .LT. AMISS3 )  CYCLE
                        Y    = UFAC * Y  +  UOFF
                        YSUM = YSUM + Y
                        NCNT = NCNT + 1
                        IF ( Y .LT. YMIN   ) THEN
                            YMIN = Y
                            CMIN = C
                            RMIN = R
                        END IF
                        IF ( Y .GT. YMAX   )THEN
                            YMAX = Y
                            CMAX = C
                            RMAX = R
                        END IF
                        RBUF( C,R ) = LOG( Y )
                    END DO
                    END DO

                ELSE IF ( MODE( V ) .EQ. EXPSCL ) THEN

                    DO R = ROW0, ROW1
                    DO C = COL0, COL1
                        Y = RBUF( C,R )
                        IF ( Y .LT. AMISS3 )  CYCLE
                        Y    = UFAC * Y  +  UOFF
                        YSUM = YSUM + Y
                        NCNT = NCNT + 1
                        IF ( Y .LT. YMIN   ) THEN
                            YMIN = Y
                            CMIN = C
                            RMIN = R
                        END IF
                        IF ( Y .GT. YMAX   )THEN
                            YMAX = Y
                            CMAX = C
                            RMAX = R
                        END IF
                        RBUF( C,R ) = EXP( Y )
                    END DO
                    END DO

                END IF      !  if mode(v) is lin | log | exp


                !!........  Compute scale factor, scale base, bin-labels:

                IF ( MODE( V ) .EQ. LINSCL ) THEN
                    Y0   = VMIN( V )
                    Y1   = VMAX( V )
                    AFAC = ( Y1 - Y0 ) / FLOAT( NBINS-1 )
                    DO R = 1, NBINS
                        BINS( R ) = VMIN( V ) + AFAC * FLOAT( R-1 )
                    END DO
                    AFAC = FLOAT( NRGB-1 ) / ( VMAX( V ) - VMIN( V ) )
                    IF ( NCNT .GT. 0 ) THEN
                        YBAR = YSUM / DBLE( NCNT )
                    ELSE
                        YBAR = BADVAL3
                    END IF
                ELSE IF ( MODE( V ) .EQ. LOGSCL ) THEN
                    Y0   = LOG( VMIN( V ) )
                    Y1   = LOG( VMAX( V ) )
                    AFAC = ( Y1 - Y0 ) / FLOAT( NBINS-1 )
                    DO R = 1, NBINS
                        BINS( R ) = EXP( Y0 + AFAC * FLOAT( R-1 ) )
                    END DO
                    IF ( NCNT .GT. 0 ) THEN
                        YBAR = EXP( YSUM / DBLE( NCNT ) )
                    ELSE
                        YBAR = BADVAL3
                    END IF
                    AFAC = FLOAT( NRGB-1 ) / ( Y1 - Y0 )
                ELSE IF ( MODE( V ) .EQ. EXPSCL ) THEN
                    Y0   = EXP( VMIN( V ) )
                    Y1   = EXP( VMAX( V ) )
                    AFAC = ( Y1 - Y0 ) / FLOAT( NBINS-1 )
                    DO R = 1, NBINS
                        BINS( R ) = LOG( Y0 + AFAC * FLOAT( R-1 ) )
                    END DO
                    AFAC = FLOAT( NRGB-1 ) / ( Y1 - Y0 )
                    IF ( NCNT .GT. 0 ) THEN
                        YBAR = LOG( YSUM / DBLE( NCNT ) )
                    ELSE
                        YBAR = BADVAL3
                    END IF
                END IF


                !!........  Write the legend and scale bar:

                IF ( TSTEP .NE. 0 ) THEN

                    IF ( STATFLAG ) THEN
                        WRITE( MESG, '( 4A, I9.7, A, I6.6, 2X, 3( A, 1PE14.6, :, 2( A, I5 ) ) )' )      &
                            TRIM( VNAME ), ' (', TRIM( UNITS ), ') @ ', JDATE, ':', JTIME,              &
                             'MAX=',   YMAX, ' @ <C,R>=<', CMAX, ',', RMAX,                             &
                             '> MIN=', YMIN, ' @ <C,R>=<', CMIN, ',', RMIN,                             &
                             '> MEAN=', YBAR
                        CALL M3MESG( MESG )
                    END IF

                    DTBUF = DT2STR( JDATE, JTIME )

                ELSE

                    IF ( STATFLAG ) THEN
                        WRITE( MESG, '( 3A, 3( A, 1PE14.6, :, 2( A, I5 ) ) )' )  &
                            TRIM( VNAME ), ' (', TRIM( UNITS ),                  &
                             ') MAX=', YMAX, ' @ <C,R>=<', CMAX, ',', RMAX,      &
                             '> MIN=', YMIN, ' @ <C,R>=<', CMIN, ',', RMIN,      &
                             '> MEAN=', YBAR
                        CALL M3MESG( MESG )
                    END IF

                    DTBUF = BLANK

                END IF          !!  if tstep nonzero, or not

                CALL  LEGEND( NBINS, BINS,                                      &
                              YMAX,  CMAX,  RMAX, YMIN,  CMIN,  RMIN, YBAR,     &
                              SMAX, CVMAX, RVMAX, SMIN, CVMIN, RVMIN, SBAR,     &
                              VECSCL( V ),                                      &
                              ZMAX, CZMAX, RZMAX, ZMIN, CZMIN, RZMIN, ZBAR,     &
                              CONMIN( V ), CONMAX( V ),                         &
                              TRIM( VNAME ), TRIM( UNITS ),                     &
                              TRIM( ZNAMES( V ) ), TRIM( ZUNITS( V ) ),         &
                              TRIM( DTBUF ),                                    &
                              TRIM( TITLE1(V) ), TRIM( TITLE2(V) ),             &
                              TRIM( TITLE3(V) ), TRIM( BINFMT(V) ) )


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


                !!........  Draw scalar observations plot overlay

                IF ( RDOBS ) THEN

                    !!  rescale obs in the same way as the gridded variable:

                    UFAC = VFAC( V )
                    UOFF = VOFF( V )
                    IF ( MODE( V ) .EQ. LOGSCL ) THEN
                        DO K = 1, NOBS
                            IF ( OBUF( K ) .GT. AMISS3 ) OBUF( K ) = UFAC * OBUF( K )  +  UOFF
                        END DO
                    ELSE IF ( MODE( V ) .EQ. LOGSCL ) THEN
                        DO K = 1, NOBS
                            IF ( OBUF( K ) .GT. AMISS3 ) OBUF( K ) = LOG( UFAC * OBUF( K )  +  UOFF )
                        END DO
                    ELSE IF ( MODE( V ) .EQ. EXPSCL ) THEN
                        DO K = 1, NOBS
                            IF ( OBUF( K ) .GT. AMISS3 ) OBUF( K ) = EXP( UFAC * OBUF( K )  +  UOFF )
                        END DO
                    END IF

                    CALL IMGOBS( NOBS, OBSSIZE( V ), OBSY, OBSX, OBUF, Y0, YFAC )

                END IF          !!  if rdobs


                !!........  Draw vector observations plot overlay

                IF ( RDOVEC ) THEN
                    CALL IMGOVEC( NOVEC, OBSSIZE( V ), OVY, OVX, OSPD, OANG, COSA, SINA, OVECSCL(V), OVECTHK(V) )
                END IF          !!  if rdobs


                !!........  Draw wind-vector plot overlay

                IF ( RDVEC ) THEN
                    CALL IMGVECT( UBUF, VBUF, VECSCL( V ), VECINC( V ), VECTHK( V ) )
                END IF


                !!........  Draw contour plot overlay

                IF ( RDCON ) THEN
                    IF ( CONSMTH(V) ) CALL GRDSMOOTH( NCOLS1, NROWS1, ZBUF )
                    ZFAC = ( CONMAX( V ) - CONMIN( V ) ) / FLOAT( NZRGB )
                    CALL IMGCONT( ZBUF, CONMIN( V ), ZFAC, CONTHK( V ) )
                END IF


                !!........  Construct file-name and write output image:

                IF ( RDVEC ) THEN
                    IF ( NLAYS1 .EQ. 1 ) THEN
                        BASENAME = TRIM( VNAME ) // '_' // TRIM( NAMEU(V) ) // '_' // TRIM( NAMEV(V) )
                    ELSE
                        WRITE( BASENAME, '( 6A, I3.3 )' )       &
                            TRIM( VNAME ), '_', TRIM( NAMEU(V) ), '_', TRIM( NAMEV(V) ), '.', VARLVL( V )
                    END IF
                ELSE
                    IF ( NLAYS1 .EQ. 1 ) THEN
                        BASENAME = TRIM( VNAME )
                    ELSE
                        WRITE( BASENAME, '( 2A, I3.3 )' ) TRIM( VNAME ), '.', VARLVL( V )
                    END IF
                END IF

                IF ( TSTEP .EQ. 0 ) THEN
                    EQNAME = TRIM( EQROOT ) // TRIM( BASENAME ) // SUFFIX
                ELSE IF ( DTFLAG ) THEN
                    WRITE( EQNAME, '( 3A, I7.7,I6.6, A )' ) TRIM( EQROOT ), TRIM( BASENAME ), '.', JDATE, JTIME, SUFFIX
                ELSE IF ( NRECS .LE. 10000 ) THEN
                    WRITE( EQNAME, '( 3A, I4.4, A )' ) TRIM( EQROOT ), TRIM( BASENAME ), '.', N-1, SUFFIX
                ELSE
                    WRITE( EQNAME, '( 3A, I6.6, A )' ) TRIM( EQROOT ), TRIM( BASENAME ), '.', N-1, SUFFIX
                END IF

                CALL IMGWRITE( IMGTYPE, EQNAME, ISTAT )
                IF ( ISTAT .NE. 0 ) THEN
                    EFLAG = .TRUE.
                    WRITE( MESG, '( 3A, I9.7, A, I6.6 )' )      &
                        'Error writing image-file for variable "', TRIM( VNAME ), ' date%time', JDATE, ':', JTIME
                    CALL M3MESG( MESG )
                END IF

            END DO          !!  end processing loop on time steps N

        END DO              !!  end loop on requested variables V


    ELSE IF ( VECPLOT ) THEN    !!  wind vectors only


        !!........  Construct the title-lines:

        DO V = 1, NVECS

            VNAME = '<' // TRIM( NAMEU(V) ) // '| ' // TRIM( NAMEV(V) ) // '>'

            IF ( TITLE1(V) .EQ. CMISS3 )  THEN
                IF ( NLAYS2 .GT. 1 ) THEN
                    WRITE( TITLE1(V), '( A, I3, 1X, A )' ) 'Layer', VECLVL(V), VNAME
                ELSE
                    TITLE1(V) = VNAME
                END IF
            END IF

            IF ( TITLE3(V) .EQ. CMISS3 )  THEN
                TITLE3(V) = VECFILE
            END IF

        END DO


        !!........  Now process the data and make the plots

        YMAX = BADVAL3          !! "missing" for tile-plot stats
        YMIN = BADVAL3
        YBAR = BADVAL3
        CMAX = IMISS3
        RMAX = IMISS3
        CMIN = IMISS3
        RMIN = IMISS3

        JDATE = SDATE
        JTIME = STIME
        CALL NEXTIME( JDATE, JTIME, -TSTEP )

        DO N = 1, NRECS

            CALL NEXTIME( JDATE, JTIME,  TSTEP )
            WRITE ( MESG, '( A, I9.7, A, I6.6 )' ) 'Processing', JDATE, ':', JTIME
            CALL M3MESG( MESG )

            DO V = 1, NVECS

                CALL IMGCLR()   !!  Clear this-step image storage

                IF ( .NOT. READ3( 'VECFILE', NAMEU(V), VECLVL(V), JDATE, JTIME, UBUF ) ) CYCLE
                IF ( .NOT. READ3( 'VECFILE', NAMEV(V), VECLVL(V), JDATE, JTIME, VBUF ) ) CYCLE


                !!........  Compute grid stats:

                YMAX =  0.0
                YMIN =  9.999E36
                YSUM =  0.0D0

                DO R = ROW0, ROW1
                DO C = COL0, COL1
                    S = UBUF( C,R )**2 + VBUF( C,R )**2
                    SSUM = SSUM + DBLE( S )
                    IF ( S .LT. SMIN   ) THEN
                        SMIN  = S
                        CVMIN = C
                        RVMIN = R
                    END IF
                    IF ( S .GT. SMAX   )THEN
                        SMAX  = S
                        CVMAX = C
                        RVMAX = R
                    END IF
                END DO
                END DO

                SMAX  = SQRT( SMAX )
                SMIN  = SQRT( SMIN )
                SBAR  = SQRT( SSUM / DBLE( ( ROW1 - ROW0 + 1 ) * ( COL1 - COL0 + 1 ) ) )


                IF ( CONFLAG( V ) ) THEN
                    RDCON = READ3( 'CONFILE', NAMEU(V), VECLVL(V), JDATE, JTIME, ZBUF )
                ELSE
                    RDCON = .FALSE.
                END IF                  !!  if not vecplot; else if reads failed

                IF ( RDCON ) THEN       !! if vector reads succeeded, compute stats

                    ZMAX = -9.999E36
                    ZMIN =  9.999E36
                    ZSUM =  0.0D0
                    ZFAC = CONFAC( V )

                    DO R = ROW0, ROW1
                    DO C = COL0, COL1
                        S = ZFAC * ZBUF( C,R )
                        ZSUM = ZSUM + DBLE( S )
                        IF ( S .LT. ZMIN   ) THEN
                            SMIN = S
                            CMIN = C
                            RMIN = R
                        END IF
                        IF ( S .GT. ZMAX   )THEN
                            SMAX = S
                            CMAX = C
                            RMAX = R
                        END IF
                    END DO
                    END DO

                    ZBAR  = ZSUM / DBLE( ( ROW1 - ROW0 + 1 ) * ( COL1 - COL0 + 1 ) )
                    CZMAX = CMAX
                    CZMIN = CMIN
                    RZMAX = RMAX
                    RZMIN = RMIN

                ELSE

                    RDCON = .FALSE.
                    ZMAX  = BADVAL3
                    ZMIN  = BADVAL3
                    ZBAR  = BADVAL3
                    CZMAX = IMISS3
                    CZMIN = IMISS3
                    RZMAX = IMISS3
                    RZMIN = IMISS3

                END IF                  !! if vector reads succeeded, compute stats


                !!........  Write the legend and scale bar:

                IF ( TSTEP .NE. 0 ) THEN
                    DTBUF = DT2STR( JDATE, JTIME )
                ELSE
                    DTBUF = BLANK
                END IF

                CALL  LEGEND( NBINS, BINS,                                      &
                              YMAX,  CMAX,  RMAX, YMIN,  CMIN,  RMIN, YBAR,     &
                              SMAX, CVMAX, RVMAX, SMIN, CVMIN, RVMIN, SBAR,     &
                              VECSCL( V ),                                      &
                              ZMAX, CZMAX, RZMAX, ZMIN, CZMIN, RZMIN, ZBAR,     &
                              CONMIN( V ), CONMAX( V ),                         &
                              TRIM( VNAME ), TRIM( UNITS ),                     &
                              TRIM( ZNAMES( V ) ), TRIM( ZUNITS( V ) ),         &
                              TRIM( DTBUF ),                                    &
                              TRIM( TITLE1(V) ), TRIM( TITLE2(V) ),             &
                              TRIM( TITLE3(V) ), TRIM( BINFMT(V) ) )


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


                !!........  Draw wind-vector plot

                CALL IMGVECT( UBUF, VBUF, VECSCL( V ), VECINC( V ), VECTHK( V ) )


                !!........  Draw contour plot overlay

                IF ( RDCON ) THEN
                    IF ( CONSMTH(V) ) CALL GRDSMOOTH( NCOLS1, NROWS1, ZBUF )
                    ZFAC = ( CONMAX( V ) - CONMIN( V ) ) / FLOAT( NZRGB )
                    CALL IMGCONT( ZBUF, CONMIN( V ), ZFAC, CONTHK( V ) )
                END IF


                !!........  Construct file-name and write output image:

                IF ( NLAYS2 .EQ. 1 ) THEN
                    BASENAME = TRIM( NAMEU(V) ) // '_' // TRIM( NAMEV(V) )
                ELSE
                    WRITE( BASENAME, '( 4A, I3.3 )' ) TRIM( NAMEU(V) ), '_', TRIM( NAMEV(V) ), '.', VECLVL( V )
                END IF

                IF ( TSTEP .EQ. 0 ) THEN
                    EQNAME = TRIM( EQROOT ) // TRIM( BASENAME ) // SUFFIX
                ELSE IF ( DTFLAG ) THEN
                    WRITE( EQNAME, '( 3A, I7.7,I6.6, A )' ) TRIM( EQROOT ), TRIM( BASENAME ), '.', JDATE, JTIME, SUFFIX
                ELSE IF ( NRECS .LE. 10000 ) THEN
                    WRITE( EQNAME, '( 3A, I4.4, A )' ) TRIM( EQROOT ), TRIM( BASENAME ), '.', N-1, SUFFIX
                ELSE
                    WRITE( EQNAME, '( 3A, I6.6, A )' ) TRIM( EQROOT ), TRIM( BASENAME ), '.', N-1, SUFFIX
                END IF

                !!........  Save image to a file:

                CALL IMGWRITE( IMGTYPE, EQNAME, ISTAT )
                IF ( ISTAT .NE. 0 ) THEN
                    EFLAG = .TRUE.
                    WRITE( MESG, '( 3A, I9.7, A, I6.6 )' )      &
                        'Error writing image-file for variable "', TRIM( VNAME ), ' date%time', JDATE, ':', JTIME
                    CALL M3MESG( MESG )
                END IF

            END DO      !!  end loop on requested variables V

        END DO          !!  end processing loop on time steps N


    END IF                              !!  if varplot; else if vecplot

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

        INTEGER IDEV, ISTAT, I, K, L, M, N, V
        INTEGER IRGB, IVRGB, IZRGB
        REAL    XX, YY
        LOGICAL AFLAG, BFLAG, CFLAG, SFLAG

        CHARACTER*256   MESG
        CHARACTER*512   LINE
        CHARACTER*32    FIELD( 6 )
        CHARACTER*32    VNAME, VMODE

        !!-----------  body of subroutine:  -----------------------

        AFLAG = .FALSE.
        BFLAG = .FALSE.
        CFLAG = .FALSE.

        IDEV  = GETEFILE( 'CONFIG', .TRUE., .TRUE., 'M3PLOT/RDCONFIG' )

        IF ( IDEV .LT. 0 ) THEN
            EFLAG = .TRUE.
            CALL M3MESG( 'Could not open "CONFIG"' )
            RETURN
        END IF

        IRGB  = 0
        IVRGB = 0
        IZRGB = 0
        NVARS = 0
        NVECS = 0
        V     = 0

        DO L = 1, 9999999

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
                            IRGB = MAX( I, IRGB )   !  accumulate colors
                        ELSE
                            CFLAG = .TRUE.              !  starting a color-scale:  use current I
                            NRGB = I
                        END IF
                        RGB( 1,I ) = STR2INT( FIELD( 3 ) )
                        RGB( 2,I ) = STR2INT( FIELD( 4 ) )
                        RGB( 3,I ) = STR2INT( FIELD( 5 ) )
                    END IF
                END IF

            ELSE IF ( FIELD( 1 ) .EQ. 'VCOLOR' ) THEN   !!  vwector-color

                IF ( N .NE. 5 ) THEN
                    BFLAG = .TRUE.
                    WRITE( MESG, '( A, I7 )' ) 'Bad number of fields for "VCOLOR" in "CONFIG" at line', L
                    CALL M3MESG( MESG )
                ELSE IF ( VECPLOT ) THEN
                    I = STR2INT( FIELD( 2 ) )
                    IF ( I .GT. MXSCALE .OR. I .LT. 1 ) THEN
                        BFLAG = .TRUE.
                        WRITE( MESG, '( A, I7 )' ) 'Bad index for "VCOLOR" in "CONFIG" at line', L
                        CALL M3MESG( MESG )
                    ELSE
                        IVRGB = MAX( I, IVRGB )   !  accumulate colors
                        VRGB( 1,I ) = STR2INT( FIELD( 3 ) )
                        VRGB( 2,I ) = STR2INT( FIELD( 4 ) )
                        VRGB( 3,I ) = STR2INT( FIELD( 5 ) )
                    END IF
                END IF

            ELSE IF ( FIELD( 1 ) .EQ. 'CCOLOR' ) THEN   !!  vwector-color

                IF ( N .NE. 5 ) THEN
                    BFLAG = .TRUE.
                    WRITE( MESG, '( A, I7 )' ) 'Bad number of fields for "CCOLOR" in "CONFIG" at line', L
                    CALL M3MESG( MESG )
                ELSE IF ( CONPLOT ) THEN
                    I = STR2INT( FIELD( 2 ) )
                    IF ( I .GT. MXSCALE .OR. I .LT. 1 ) THEN
                        BFLAG = .TRUE.
                        WRITE( MESG, '( A, I7 )' ) 'Bad index for "VCOLOR" in "CONFIG" at line', L
                        CALL M3MESG( MESG )
                    ELSE
                        IZRGB = MAX( I, IZRGB )   !  accumulate colors
                        ZRGB( 1,I ) = STR2INT( FIELD( 3 ) )
                        ZRGB( 2,I ) = STR2INT( FIELD( 4 ) )
                        ZRGB( 3,I ) = STR2INT( FIELD( 5 ) )
                    END IF
                END IF

            ELSE IF ( FIELD( 1 ) .EQ. 'LEGEND_BINS' ) THEN

                IF ( N .NE. 2 ) THEN
                    BFLAG = .TRUE.
                    WRITE( MESG, '( A, I7 )' ) 'Bad number of fields for "LEGEND_BINS" in "CONFIG" at line', L
                    CALL M3MESG( MESG )
                ELSE
                    NBINS = STR2INT( FIELD( 2 ) )
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

            ELSE IF ( FIELD( 1 ) .EQ. 'VARIABLE' ) THEN         !  VARIABLE <VNAME> <mode: LIN | LOG | EXP >

                VARPLOT = .TRUE.

                IF ( N .GT. 3  .OR. N .LT. 2 ) THEN
                    BFLAG = .TRUE.
                    WRITE( MESG, '( A, I7 )' ) 'Bad number of fields for "VARIABLE" in "CONFIG" at line', L
                    CALL M3MESG( MESG )
                ELSE IF ( AFLAG ) THEN
                    BFLAG = .TRUE.
                    WRITE( MESG, '( A, I7 )' ) '"ALL VARIABLES" already set in "CONFIG" at line', L
                    CALL M3MESG( MESG )
                ELSE

                    VNAME = FIELD( 2 )
                    CALL UPCASE( VNAME )

                    IF ( N .EQ. 3 ) THEN
                        VMODE = FIELD( 3 )
                        CALL UPCASE( VMODE )
                        M = INDEX1( VMODE, 3, MODES )
                        IF ( M .LE. 0 ) THEN
                            BFLAG = .TRUE.
                            WRITE( MESG, '( 3 A, I7 )' ) 'Bad scale-mode for "VARIABLE" in "CONFIG" at line', L
                            CALL M3MESG( MESG )
                            M = LINSCL
                        END IF
                    ELSE
                        M = LINSCL
                    END IF

                    IF ( VNAME .EQ. 'ALL' ) THEN               ! ""all variables"

                        DO V = 1, NVARS1
                            VTYPES( V ) = VTYPE1( V )
                            VNAMES( V ) = VNAME1( V )
                            VUNITS( V ) = UNITS1( V )
                            TITLE2( V ) = VDESC1( V )
                            VTYPES( V ) = VTYPE1( V )
                            MODE  ( V ) = M
                        END DO
                        NVARS = NVARS1
                        AFLAG = .TRUE.

                    ELSE

                        K = INDEX1( FIELD(2), NVARS1, VNAME1 )
                        IF ( K .LE. 0 ) THEN
                            BFLAG = .TRUE.
                            WRITE( MESG, '( 3 A, I7 )' )                &
                                 'Variable "', TRIM( FIELD(2) ),        &
                                 '" not in "INFILE" for "CONFIG" at line', L
                            CALL M3MESG( MESG )
                        ELSE
                            V     = V + 1
                            NVARS = V
                            VNAMES( V ) = FIELD(2)
                            VUNITS( V ) = UNITS1( K )
                            TITLE2( V ) = VDESC1( K )
                            VTYPES( V ) = VTYPE1( K )
                            MODE  ( V ) = M
                        END IF

                    END IF      !  if "all variables" or not

                END IF          !  if n > 3 for "variable"; else...

            ELSE IF ( FIELD( 1 ) .EQ. 'UNITS' ) THEN

                IF ( AFLAG ) THEN               !!  "all variables" -- invalid for "units"
                    BFLAG = .TRUE.
                    WRITE( MESG, '( A, I7 )' ) '"ALL VARIABLES" set for "UNITS" in "CONFIG" at line', L
                    CALL M3MESG( MESG )
                ELSE IF ( V .EQ. 0 ) THEN
                    BFLAG = .TRUE.
                    WRITE( MESG, '( A, I7 )' ) '"VARIABLE" not yet set for "UNITSs" in "CONFIG" at line', L
                    CALL M3MESG( MESG )
                ELSE IF ( N .EQ. 2 ) THEN       !!  just change name of units
                    VUNITS( V ) = FIELD( 2 )
                    VFAC( V )   = 1.0
                    VOFF( V )   = 0.0
                ELSE IF ( N .EQ. 3 ) THEN       !!  name and scale factor
                    VUNITS( V ) = FIELD( 2 )
                    VFAC( V )   = STR2REAL( FIELD( 3 ) )
                    VOFF( V )   = 0.0
                ELSE IF ( N .EQ. 4 ) THEN       !!  name, scale factor, offset
                    VUNITS( V ) = FIELD( 2 )
                    VFAC( V )   = STR2REAL( FIELD( 3 ) )
                    VOFF( V )   = STR2REAL( FIELD( 4 ) )
                ELSE
                    BFLAG = .TRUE.
                    WRITE( MESG, '( A, I7 )' ) 'Bad number of fields for "UNITS" in "CONFIG" at line', L
                    CALL M3MESG( MESG )
                END IF

            ELSE IF ( FIELD( 1 ) .EQ. 'LEVEL' ) THEN

                IF ( N .NE. 2 ) THEN
                    BFLAG = .TRUE.
                    WRITE( MESG, '( A, I7 )' ) 'Bad number of fields for "LEVEL" in "CONFIG" at line', L
                    CALL M3MESG( MESG )
                ELSE IF ( V .EQ. 0 ) THEN
                    BFLAG = .TRUE.
                    WRITE( MESG, '( A, I7 )' ) '"VARIABLE" not yet set for "LEVEL" in "CONFIG" at line', L
                    CALL M3MESG( MESG )
                ELSE
                    M = STR2INT( FIELD( 2 ) )
                    IF ( M .LT. 1 .OR. M .GT. NLAYS1 ) THEN
                        BFLAG = .TRUE.
                        WRITE( MESG, '( A, I7 )' ) 'Out-of-range "LEVEL" in "CONFIG" at line', L
                        CALL M3MESG( MESG )
                    ELSE IF ( AFLAG ) THEN               ! ""all variables"
                        VARLVL( : ) = M
                    ELSE
                        VARLVL( V ) = M
                    END IF
                END IF

            ELSE IF ( FIELD( 1 ) .EQ. 'SCALE_RANGE' ) THEN

                IF ( N .NE. 3 ) THEN
                    BFLAG = .TRUE.
                    WRITE( MESG, '( A, I7 )' ) 'Bad number of fields for "SCALE_RANGE" in "CONFIG" at line', L
                    CALL M3MESG( MESG )
                ELSE IF ( V .EQ. 0 ) THEN
                    BFLAG = .TRUE.
                    WRITE( MESG, '( A, I7 )' ) '"VARIABLE" not yet set for "SCALE_RANGE" in "CONFIG" at line', L
                    CALL M3MESG( MESG )
                ELSE
                    XX = STR2REAL( FIELD( 2 ) )
                    YY = STR2REAL( FIELD( 3 ) )
                    IF ( AFLAG ) THEN               ! ""all variables"
                        VMIN( : ) = XX
                        VMAX( : ) = YY
                    ELSE
                        VMIN( V ) = XX
                        VMAX( V ) = YY
                    END IF
                END IF

            ELSE IF ( FIELD( 1 ) .EQ. 'FORMAT_BINS' ) THEN

                IF ( N .NE. 2 ) THEN
                    BFLAG = .TRUE.
                    WRITE( MESG, '( A, I7 )' ) 'Bad number of fields for "FORMAT_BINS" in "CONFIG" at line', L
                    CALL M3MESG( MESG )
                ELSE IF ( AFLAG ) THEN               ! ""all variables"
                    BINFMT( : ) = FIELD( 2 )
                ELSE IF ( V .EQ. 0 ) THEN            ! ""all variables"
                    BINFMT( : ) = FIELD( 2 )
                ELSE
                    BINFMT( V ) = FIELD( 2 )
                END IF

            ELSE IF ( FIELD( 1 ) .EQ. 'OBSERVATION' ) THEN         !  VECTOR <U-variable> <V-variable>

                IF ( .NOT.OBSPLOT  ) THEN
                    CONTINUE
                ELSE IF ( N .NE. 2  ) THEN
                    BFLAG = .TRUE.
                    WRITE( MESG, '( A, I7 )' ) 'Bad number of fields for "OBSERVATION" in "CONFIG" at line', L
                    CALL M3MESG( MESG )
                ELSE IF ( AFLAG ) THEN
                    OBSNAME( : ) = FIELD( 2 )
                    OBSFLAG( : ) = .TRUE.
                ELSE IF ( V .GT. 0 ) THEN
                    OBSNAME( V ) = FIELD( 2 )
                    OBSFLAG( V ) = .TRUE.
                ELSE                            !  if ( v .eq. 0 ) then
                    BFLAG = .TRUE.
                    WRITE( MESG, '( A, I7 )' ) 'No variable for "OBSERVATION" in "CONFIG" at line', L
                    CALL M3MESG( MESG )
                END IF

            ELSE IF ( FIELD( 1 ) .EQ. 'OBS_SIZE' ) THEN

                IF ( .NOT.VECPLOT  ) THEN
                    CONTINUE
                ELSE IF ( N .NE. 2 ) THEN
                    BFLAG = .TRUE.
                    WRITE( MESG, '( A, I7 )' ) 'Bad number of fields for "OBS_SIZE" in "CONFIG" at line', L
                    CALL M3MESG( MESG )
                ELSE IF ( V .EQ. 0 ) THEN
                    BFLAG = .TRUE.
                    WRITE( MESG, '( A, I7 )' ) '"VARIABLE" not yet set for "OBS_SIZE" in "CONFIG" at line', L
                    CALL M3MESG( MESG )
                ELSE IF ( AFLAG ) THEN
                    OBSSIZE( : ) = STR2INT( FIELD( 2 ) )
                ELSE
                    OBSSIZE( V ) = STR2INT( FIELD( 2 ) )
                END IF

            ELSE IF ( FIELD( 1 ) .EQ. 'VECTOR' ) THEN         !  VECTOR <U-variable> <V-variable>

                IF ( .NOT.VECPLOT  ) THEN
                    CONTINUE
                ELSE IF ( N .NE. 3  ) THEN
                    BFLAG = .TRUE.
                    WRITE( MESG, '( A, I7 )' ) 'Bad number of fields for "VECTOR" in "CONFIG" at line', L
                    CALL M3MESG( MESG )
                ELSE IF ( V .EQ. 0 ) THEN
                    NVECS = NVECS + 1
                    NAMEU( NVECS )   = FIELD( 2 )
                    NAMEV( NVECS )   = FIELD( 3 )
                    VECFLAG( NVECS ) = .TRUE.
                ELSE IF ( VECFLAG( V ) ) THEN
                    BFLAG = .TRUE.
                    WRITE( MESG, '( A, I7 )' ) 'Repeated "VECTOR" in "CONFIG" at line', L
                    CALL M3MESG( MESG )
                ELSE IF ( AFLAG ) THEN
                    NAMEU( : )   = FIELD( 2 )
                    NAMEV( : )   = FIELD( 3 )
                    VECFLAG( : ) = .TRUE.
                ELSE
                    NAMEU( V )   = FIELD( 2 )
                    NAMEV( V )   = FIELD( 3 )
                    VECFLAG( V ) = .TRUE.
                END IF

            ELSE IF ( FIELD( 1 ) .EQ. 'VEC_RANGE' ) THEN

                IF ( .NOT.VECPLOT  ) THEN
                    CONTINUE
                ELSE IF ( N .NE. 2 ) THEN
                    BFLAG = .TRUE.
                    WRITE( MESG, '( A, I7 )' ) 'Bad number of fields for "VEC_RANGE" in "CONFIG" at line', L
                    CALL M3MESG( MESG )
                ELSE IF ( V .EQ. 0 ) THEN
                    BFLAG = .TRUE.
                    WRITE( MESG, '( A, I7 )' ) '"VARIABLE" not yet set for "VEC_RANGE" in "CONFIG" at line', L
                    CALL M3MESG( MESG )
                ELSE
                    VECSCL( V ) = STR2REAL( FIELD( 2 ) )
                END IF

            ELSE IF ( FIELD( 1 ) .EQ. 'VEC_LEVEL' ) THEN

                IF ( .NOT.VECPLOT  ) THEN
                    CONTINUE
                ELSE IF ( N .NE. 2 ) THEN
                    BFLAG = .TRUE.
                    WRITE( MESG, '( A, I7 )' ) 'Bad number of fields for "VEC_LEVEL" in "CONFIG" at line', L
                    CALL M3MESG( MESG )
                ELSE IF ( V .EQ. 0 .AND. NVECS .EQ. 0 ) THEN
                    BFLAG = .TRUE.
                    WRITE( MESG, '( A, I7 )' ) '"VARIABLE" not yet set for "VEC_LEVEL" in "CONFIG" at line', L
                    CALL M3MESG( MESG )
                ELSE IF ( NVECS .GT. 0 ) THEN
                    VECLVL( NVECS ) = STR2INT( FIELD( 2 ) )
                ELSE IF ( AFLAG ) THEN
                    VECLVL( : ) = STR2INT( FIELD( 2 ) )
                ELSE
                    VECLVL( V ) = STR2INT( FIELD( 2 ) )
                END IF

             ELSE IF ( FIELD( 1 ) .EQ. 'VEC_INCREMENT' ) THEN

                IF ( .NOT.VECPLOT  ) THEN
                    CONTINUE
                ELSE IF ( N .NE. 2 ) THEN
                    BFLAG = .TRUE.
                    WRITE( MESG, '( A, I7 )' ) 'Bad number of fields for "VEC_INCREMENT" in "CONFIG" at line', L
                    CALL M3MESG( MESG )
                ELSE IF ( V .EQ. 0 .AND. NVECS .EQ. 0 ) THEN
                    BFLAG = .TRUE.
                    WRITE( MESG, '( A, I7 )' ) '"VARIABLE" not yet set for "VEC_INCREMENT" in "CONFIG" at line', L
                    CALL M3MESG( MESG )
                ELSE IF ( NVECS .GT. 0 ) THEN
                    VECINC( NVECS ) = STR2INT( FIELD( 2 ) )
                ELSE IF ( AFLAG ) THEN
                    VECINC( : ) = STR2INT( FIELD( 2 ) )
                ELSE
                    VECINC( V ) = STR2INT( FIELD( 2 ) )
                END IF

             ELSE IF ( FIELD( 1 ) .EQ. 'VEC_THICKNESS' ) THEN

                IF ( .NOT.VECPLOT  ) THEN
                    CONTINUE
                ELSE IF ( N .NE. 2 ) THEN
                    BFLAG = .TRUE.
                    WRITE( MESG, '( A, I7 )' ) 'Bad number of fields for "VEC_THICKNESS" in "CONFIG" at line', L
                    CALL M3MESG( MESG )
                ELSE IF ( V .EQ. 0 .AND. NVECS .EQ. 0 ) THEN
                    BFLAG = .TRUE.
                    WRITE( MESG, '( A, I7 )' ) '"VARIABLE" not yet set for "VEC_THICKNESS" in "CONFIG" at line', L
                    CALL M3MESG( MESG )
                ELSE IF ( NVECS .GT. 0 ) THEN
                    VECTHK( NVECS ) = STR2INT( FIELD( 2 ) )
                ELSE IF ( AFLAG ) THEN
                    VECTHK( : ) = STR2INT( FIELD( 2 ) )
                ELSE
                    VECTHK( V ) = STR2INT( FIELD( 2 ) )
                END IF


            ELSE IF ( FIELD( 1 ) .EQ. 'CONTOUR' ) THEN         !  CONTOUR <Z-variable>

                IF ( .NOT.CONPLOT  ) THEN
                    CONTINUE
                ELSE IF ( N .NE. 2  ) THEN
                    BFLAG = .TRUE.
                    WRITE( MESG, '( A, I7 )' ) 'Bad number of fields for "CONTOUR" in "CONFIG" at line', L
                    CALL M3MESG( MESG )
                ELSE IF ( V .EQ. 0 ) THEN
                    BFLAG = .TRUE.
                    WRITE( MESG, '( A, I7 )' ) '"VARIABLE" not yet set for "CONTOUR" in "CONFIG" at line', L
                    CALL M3MESG( MESG )
                ELSE IF ( CONFLAG( V ) ) THEN
                    BFLAG = .TRUE.
                    WRITE( MESG, '( A, I7 )' ) 'Repeated "CONTOUR" in "CONFIG" at line', L
                    CALL M3MESG( MESG )
                ELSE IF ( AFLAG ) THEN
                    ZNAMES ( : ) = FIELD( 2 )
                    CONFLAG( : ) = .TRUE.
                ELSE
                    ZNAMES ( V ) = FIELD( 2 )
                    CONFLAG( V ) = .TRUE.
                END IF


            ELSE IF ( FIELD( 1 ) .EQ. 'CON_UNITS' ) THEN

                IF ( N .NE. 3 ) THEN
                    BFLAG = .TRUE.
                    WRITE( MESG, '( A, I7 )' ) 'Bad number of fields for "UNITS" in "CONFIG" at line', L
                    CALL M3MESG( MESG )
                ELSE IF ( AFLAG ) THEN              !!  "all variables" -- invalid for "units"
                    BFLAG = .TRUE.
                    WRITE( MESG, '( A, I7 )' ) '"ALL VARIABLES" set for "UNITS" in "CONFIG" at line', L
                    CALL M3MESG( MESG )
                ELSE
                    ZUNITS( V ) = FIELD( 2 )
                    CONFAC( V ) = STR2REAL( FIELD( 3 ) )
                END IF
            ELSE IF ( FIELD( 1 ) .EQ. 'CON_RANGE' ) THEN

                IF ( .NOT.CONPLOT  ) THEN
                    CONTINUE
                ELSE IF ( N .NE. 3 ) THEN
                    BFLAG = .TRUE.
                    WRITE( MESG, '( A, I7 )' ) 'Bad number of fields for "CON_RANGE" in "CONFIG" at line', L
                    CALL M3MESG( MESG )
                ELSE IF ( V .EQ. 0 ) THEN
                    BFLAG = .TRUE.
                    WRITE( MESG, '( A, I7 )' ) '"VARIABLE" not yet set for "CON_RANGE" in "CONFIG" at line', L
                    CALL M3MESG( MESG )
                ELSE
                    CONMIN( V ) = STR2REAL( FIELD( 2 ) )
                    CONMAX( V ) = STR2REAL( FIELD( 3 ) )
                END IF

            ELSE IF ( FIELD( 1 ) .EQ. 'CON_LEVEL' ) THEN

                IF ( .NOT.CONPLOT  ) THEN
                    CONTINUE
                ELSE IF ( N .NE. 2 ) THEN
                    BFLAG = .TRUE.
                    WRITE( MESG, '( A, I7 )' ) 'Bad number of fields for "CON_LEVEL" in "CONFIG" at line', L
                    CALL M3MESG( MESG )
                ELSE IF ( V .EQ. 0 .AND. NCONS .EQ. 0 ) THEN
                    BFLAG = .TRUE.
                    WRITE( MESG, '( A, I7 )' ) '"VARIABLE" not yet set for "CON_LEVEL" in "CONFIG" at line', L
                    CALL M3MESG( MESG )
                ELSE IF ( NCONS .GT. 0 ) THEN
                    CONLVL( NCONS ) = STR2INT( FIELD( 2 ) )
                ELSE IF ( AFLAG ) THEN
                    CONLVL( : ) = STR2INT( FIELD( 2 ) )
                ELSE
                    CONLVL( V ) = STR2INT( FIELD( 2 ) )
                END IF

             ELSE IF ( FIELD( 1 ) .EQ. 'CON_THICKNESS' ) THEN

                IF ( .NOT.CONPLOT  ) THEN
                    CONTINUE
                ELSE IF ( N .NE. 2 ) THEN
                    BFLAG = .TRUE.
                    WRITE( MESG, '( A, I7 )' ) 'Bad number of fields for "CON_THICKNESS" in "CONFIG" at line', L
                    CALL M3MESG( MESG )
                ELSE IF ( V .EQ. 0 .AND. NCONS .EQ. 0 ) THEN
                    BFLAG = .TRUE.
                    WRITE( MESG, '( A, I7 )' ) '"VARIABLE" not yet set for "CON_THICKNESS" in "CONFIG" at line', L
                    CALL M3MESG( MESG )
                ELSE IF ( NCONS .GT. 0 ) THEN
                    CONTHK( NCONS ) = STR2INT( FIELD( 2 ) )
                ELSE IF ( AFLAG ) THEN
                    CONTHK( : ) = STR2INT( FIELD( 2 ) )
                ELSE
                    CONTHK( V ) = STR2INT( FIELD( 2 ) )
                END IF

             ELSE IF ( FIELD( 1 ) .EQ. 'CON_SMOOTH' ) THEN

                CALL UPCASE( FIELD( 2 ) )
                SFLAG = ( FIELD(2)(1:1) .EQ. 'T' .OR. FIELD(2)(1:1) .EQ. 'Y' )
                IF ( .NOT.CONPLOT  ) THEN
                    CONTINUE
                ELSE IF ( N .NE. 2 ) THEN
                    BFLAG = .TRUE.
                    WRITE( MESG, '( A, I7 )' ) 'Bad number of fields for "CON_SMOOTH" in "CONFIG" at line', L
                    CALL M3MESG( MESG )
                ELSE IF ( V .EQ. 0 .AND. NCONS .EQ. 0 ) THEN
                    BFLAG = .TRUE.
                    WRITE( MESG, '( A, I7 )' ) '"VARIABLE" not yet set for "CON_SMOOTH" in "CONFIG" at line', L
                    CALL M3MESG( MESG )
                ELSE IF ( NCONS .GT. 0 ) THEN
                    CONSMTH( NCONS ) = SFLAG
                ELSE IF ( AFLAG ) THEN
                    CONSMTH( : ) = SFLAG
                ELSE
                    CONSMTH( V ) = SFLAG
                END IF

           ELSE IF ( FIELD( 1 ) .EQ. 'TITLE1' ) THEN

                IF ( V .EQ. 0 ) THEN
                    BFLAG = .TRUE.
                    WRITE( MESG, '( A, I7 )' ) '"VARIABLE" not yet set for "TITLE1" in "CONFIG" at line', L
                    CALL M3MESG( MESG )
                ELSE
                    M = INDEX( 'TITLE1', LINE ) + 7
                    N = M + LBLANK( LINE( M: ) )
                    IF ( AFLAG ) THEN               ! ""all variables"
                       TITLE1( : ) = LINE( N: )
                    ELSE
                        TITLE1( V ) = LINE( N: )
                    END IF
                END IF

            ELSE IF ( FIELD( 1 ) .EQ. 'TITLE2' ) THEN

                IF ( V .EQ. 0 ) THEN
                    BFLAG = .TRUE.
                    WRITE( MESG, '( A, I7 )' ) '"VARIABLE" not yet set for "TITLE2" in "CONFIG" at line', L
                    CALL M3MESG( MESG )
                ELSE
                    M = INDEX( 'TITLE2', LINE ) + 7
                    N = M + LBLANK( LINE( M: ) )
                    IF ( AFLAG ) THEN               ! ""all variables"
                       TITLE2( : ) = LINE( N: )
                    ELSE
                        TITLE2( V ) = LINE( N: )
                    END IF
                END IF

            ELSE IF ( FIELD( 1 ) .EQ. 'TITLE3' ) THEN

                IF ( V .EQ. 0 ) THEN
                    BFLAG = .TRUE.
                    WRITE( MESG, '( A, I7 )' ) '"VARIABLE" not yet set for "TITLE3" in "CONFIG" at line', L
                    CALL M3MESG( MESG )
                ELSE
                    M = INDEX( 'TITLE3', LINE ) + 7
                    N = M + LBLANK( LINE( M: ) )
                    IF ( AFLAG ) THEN               ! ""all variables"
                       TITLE3( : ) = LINE( N: )
                    ELSE
                        TITLE3( V ) = LINE( N: )
                    END IF
                END IF

            ELSE IF ( FIELD( 1 ) .EQ. 'OBS_VECTOR' ) THEN         !  VECTOR <U-variable> <V-variable>

                IF ( .NOT.OVECPLOT  ) THEN
                    CONTINUE
                ELSE IF ( N .NE. 3  ) THEN
                    BFLAG = .TRUE.
                    WRITE( MESG, '( A, I7 )' ) 'Bad number of fields for "OBS_VECTOR" in "CONFIG" at line', L
                    CALL M3MESG( MESG )
                ELSE IF ( V .EQ. 0 ) THEN
                    NOVEC = NOVEC + 1
                    OUNAME( NOVEC )   = FIELD( 2 )
                    OVNAME( NOVEC )   = FIELD( 3 )
                    VECFLAG( NOVEC ) = .TRUE.
                ELSE IF ( VECFLAG( V ) ) THEN
                    BFLAG = .TRUE.
                    WRITE( MESG, '( A, I7 )' ) 'Repeated "OBS_VECTOR" in "CONFIG" at line', L
                    CALL M3MESG( MESG )
                ELSE IF ( AFLAG ) THEN
                    OUNAME( : )   = FIELD( 2 )
                    OVNAME( : )   = FIELD( 3 )
                    OVECFLAG( : ) = .TRUE.
                ELSE
                    OUNAME( V )   = FIELD( 2 )
                    OVNAME( V )   = FIELD( 3 )
                    OVECFLAG( V ) = .TRUE.
                END IF

            ELSE IF ( FIELD( 1 ) .EQ. 'OVEC_RANGE' ) THEN

                IF ( .NOT.OVECPLOT  ) THEN
                    CONTINUE
                ELSE IF ( N .NE. 2 ) THEN
                    BFLAG = .TRUE.
                    WRITE( MESG, '( A, I7 )' ) 'Bad number of fields for "OVEC_RANGE" in "CONFIG" at line', L
                    CALL M3MESG( MESG )
                ELSE IF ( V .EQ. 0 ) THEN
                    BFLAG = .TRUE.
                    WRITE( MESG, '( A, I7 )' ) '"VARIABLE" not yet set for "OVEC_RANGE" in "CONFIG" at line', L
                    CALL M3MESG( MESG )
                ELSE
                    OVECSCL( V ) = STR2REAL( FIELD( 2 ) )
                END IF

             ELSE IF ( FIELD( 1 ) .EQ. 'OVEC_THICKNESS' ) THEN

                IF ( .NOT.VECPLOT  ) THEN
                    CONTINUE
                ELSE IF ( N .NE. 2 ) THEN
                    BFLAG = .TRUE.
                    WRITE( MESG, '( A, I7 )' ) 'Bad number of fields for "OVEC_THICKNESS" in "CONFIG" at line', L
                    CALL M3MESG( MESG )
                ELSE IF ( V .EQ. 0 .AND. NVECS .EQ. 0 ) THEN
                    BFLAG = .TRUE.
                    WRITE( MESG, '( A, I7 )' ) '"VARIABLE" not yet set for "OVEC_THICKNESS" in "CONFIG" at line', L
                    CALL M3MESG( MESG )
                ELSE IF ( NVECS .GT. 0 ) THEN
                    OVECTHK( NVECS ) = STR2INT( FIELD( 2 ) )
                ELSE IF ( AFLAG ) THEN
                    OVECTHK( : ) = STR2INT( FIELD( 2 ) )
                ELSE
                    OVECTHK( V ) = STR2INT( FIELD( 2 ) )
                END IF

            ELSE

                WRITE( MESG, '( 3 A, I9 )' ) 'Unrecognized field "', FIELD(1), '" in "CONFIG" at line', L
                CALL M3MESG( MESG )
                BFLAG = .TRUE.
                CYCLE

            END IF              !  if field(1) is...

        END DO

99      CONTINUE        !  EOF-exit from loop


        !!........  Fix up UNITS, ZUNITS, OVECLEN:

        DO V = 1, NVARS

            IF ( VUNITS( V ) .NE. BLANK ) CYCLE         !  already set

            M = INDEX1( VNAMES( V ), NVARS1, VNAME1 )
            IF ( M .LE. 0 ) THEN
                BFLAG = .TRUE.
                MESG  = 'Requested variable "' // TRIM( VNAMES( V ) ) // '" not in "INFILE"'
                CALL M3MESG( MESG )
            ELSE
                VUNITS( V ) = UNITS1( M )
            END IF

        END DO

        DO V = 1, NVARS

            IF ( ZNAMES( V ) .EQ. BLANK ) CYCLE

            M = INDEX1( ZNAMES( V ), NZVAR1, ZNAME1 )
            IF ( M .LE. 0 ) THEN
                BFLAG = .TRUE.
                MESG  = 'Requested variable "' // TRIM( ZNAMES( V ) ) // '" not in CONFILE'
                CALL M3MESG( MESG )
            ELSE
                ZUNITS( V ) = ZUNIT1( M )
                ZTYPES( V ) = ZTYPE1( M )
            END IF

        END DO

        M = 4 * ( NXGRD + NYGRD ) / ( COL1 + ROW1 - COL0 - ROW0 )
        DO V = 1, NVARS

            IF ( OVECLEN( V ) .GT. 0 ) CYCLE         !  already set
            OVECLEN( V ) = M

        END DO

        IF ( BFLAG ) THEN
            EFLAG = .TRUE.
            CALL M3MESG( 'Error(s) reading/processing file "CONFIG"' )
        END IF

        IF ( IRGB  .GT. 0 )  NRGB  = IRGB
        IF ( IVRGB .GT. 0 )  NVRGB = IVRGB
        IF ( IZRGB .GT. 0 )  NZRGB = IZRGB

        IF ( VECPLOT .AND. .NOT.VARPLOT ) THEN  !!  override color-palette for LEGEND()
            NRGB = NVRGB
            RGB  = VRGB
        END IF

        CLOSE( IDEV )

        RETURN

    END SUBROUTINE RDCONFIG


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    SUBROUTINE GRDSMOOTH( NCOLS, NROWS, GRID )

        INTEGER, INTENT(IN   ) :: NCOLS, NROWS
        REAL   , INTENT(INOUT) :: GRID( NCOLS,NROWS )

        REAL, PARAMETER :: A1 = 12.0 / 24.0
        REAL, PARAMETER :: A2 =  2.0 / 24.0
        REAL, PARAMETER :: A3 =  1.0 / 24.0

        REAL, PARAMETER :: B1 = 12.0 / 20.0
        REAL, PARAMETER :: B2 =  2.0 / 20.0
        REAL, PARAMETER :: B3 =  1.0 / 20.0

        REAL, PARAMETER :: C1 = 12.0 / 17.0
        REAL, PARAMETER :: C2 =  2.0 / 17.0
        REAL, PARAMETER :: C3 =  1.0 / 17.0

        INTEGER C, R
        REAL    WORK( NCOLS,NROWS )

        R = 1
        C = 1
        WORK( C,R ) = C1 * GRID( C,R ) + C2 * ( GRID( C+1,R ) + GRID( C,R+1 ) ) + C3 * GRID( C+1,R+1 )
        DO C = 2, NCOLS-1
            WORK( C,R ) = B1 *   GRID( C,R ) +                                          &
                          B2 * ( GRID( C-1,R ) + GRID( C,R+1 ) + GRID( C+1,R ) ) +      &
                          B3 * ( GRID( C-1,R+1 ) + GRID( C+1,R+1 ) )
        END DO
        C = NCOLS
        WORK( C,R ) = C1 * GRID( C,R ) + C2 * ( GRID( C-1,R ) + GRID( C,R+1 ) ) + C3 * GRID( C-1,R+1 )

        DO R = 2, NROWS-1
            C = 1
            WORK( C,R ) = B1 *   GRID( C,R ) +                                          &
                          B2 * ( GRID( C,R-1 ) + GRID( C,R+1 ) + GRID( C+1,R ) ) +      &
                          B3 * ( GRID( C+1,R-1 ) + GRID( C+1,R+1 ) )
            DO C = 2, NCOLS-1
                WORK( C,R ) = A1 *   GRID( C,R ) +                                                              &
                              A2 * ( GRID( C-1,R ) +  GRID( C,R-1 ) + GRID( C,R+1 ) + GRID( C+1,R ) ) +         &
                              A3 * ( GRID( C-1,R-1 ) + GRID( C+1,R-1 )+  GRID( C+1,R-1 ) + GRID( C+1,R+1 ) )
            END DO
            C = NCOLS
            WORK( C,R ) = B1 *   GRID( C,R ) +                                          &
                          B2 * ( GRID( C,R-1 ) + GRID( C,R+1 ) + GRID( C-1,R ) ) +      &
                          B3 * ( GRID( C-1,R-1 ) + GRID( C-1,R+1 ) )
        END DO

        R = NROWS
        C = 1
        WORK( C,R ) = C1 * GRID( C,R ) + C2 * ( GRID( C+1,R ) + GRID( C,R-1 ) ) + C3 * GRID( C+1,R-1 )
        DO C = 2, NCOLS-1
            WORK( C,R ) = B1 *   GRID( C,R ) +                                          &
                          B2 * ( GRID( C-1,R ) + GRID( C,R-1 ) + GRID( C+1,R ) ) +      &
                          B3 * ( GRID( C-1,R-1 ) + GRID( C+1,R-1 ) )
        END DO
        C = NCOLS
        WORK( C,R ) = C1 * GRID( C,R ) + C2 * ( GRID( C-1,R ) + GRID( C,R-1 ) ) + C3 * GRID( C-1,R-1 )


        R = 1
        C = 1
        GRID( C,R ) = C1 * WORK( C,R ) + C2 * ( WORK( C+1,R ) + WORK( C,R+1 ) ) + C3 * WORK( C+1,R+1 )
        DO C = 2, NCOLS-1
            GRID( C,R ) = B1 *   WORK( C,R ) +                                          &
                          B2 * ( WORK( C-1,R ) + WORK( C,R+1 ) + WORK( C+1,R ) ) +      &
                          B3 * ( WORK( C-1,R+1 ) + WORK( C+1,R+1 ) )
        END DO
        C = NCOLS
        GRID( C,R ) = C1 * WORK( C,R ) + C2 * ( WORK( C-1,R ) + WORK( C,R+1 ) ) + C3 * WORK( C-1,R+1 )

        DO R = 2, NROWS-1
            C = 1
            GRID( C,R ) = B1 *   WORK( C,R ) +                                          &
                          B2 * ( WORK( C,R-1 ) + WORK( C,R+1 ) + WORK( C+1,R ) ) +      &
                          B3 * ( WORK( C+1,R-1 ) + WORK( C+1,R+1 ) )
            DO C = 2, NCOLS-1
                GRID( C,R ) = A1 *   WORK( C,R ) +                                                              &
                              A2 * ( WORK( C-1,R ) +  WORK( C,R-1 ) + WORK( C,R+1 ) + WORK( C+1,R ) ) +         &
                              A3 * ( WORK( C-1,R-1 ) + WORK( C+1,R-1 )+  WORK( C+1,R-1 ) + WORK( C+1,R+1 ) )
            END DO
            C = NCOLS
            GRID( C,R ) = B1 *   WORK( C,R ) +                                          &
                          B2 * ( WORK( C,R-1 ) + WORK( C,R+1 ) + WORK( C-1,R ) ) +      &
                          B3 * ( WORK( C-1,R-1 ) + WORK( C-1,R+1 ) )
        END DO

        R = NROWS
        C = 1
        GRID( C,R ) = C1 * WORK( C,R ) + C2 * ( WORK( C+1,R ) + WORK( C,R-1 ) ) + C3 * WORK( C+1,R-1 )
        DO C = 2, NCOLS-1
            GRID( C,R ) = B1 *   WORK( C,R ) +                                          &
                          B2 * ( WORK( C-1,R ) + WORK( C,R-1 ) + WORK( C+1,R ) ) +      &
                          B3 * ( WORK( C-1,R-1 ) + WORK( C+1,R-1 ) )
        END DO
        C = NCOLS
        GRID( C,R ) = C1 * WORK( C,R ) + C2 * ( WORK( C-1,R ) + WORK( C,R-1 ) ) + C3 * WORK( C-1,R-1 )

        RETURN

    END SUBROUTINE GRDSMOOTH


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
    !!    Sin, cos of angle from X-axis to East (equiv. Y-axis to North)
    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

    SUBROUTINE LLANGLE( ALAT, ALON, SINA, COSA )

        REAL, INTENT(IN   ) :: ALAT( NCOLS1,NROWS1 )
        REAL, INTENT(IN   ) :: ALON( NCOLS1,NROWS1 )
        REAL, INTENT(  OUT) :: SINA( NCOLS1,NROWS1 )
        REAL, INTENT(  OUT) :: COSA( NCOLS1,NROWS1 )

        INTEGER C, R
        REAL    DU, DV, CA, SA, HY, C1, S1, C2, S2

        DO R = 2, NROWS1-1              !!  interior
        DO C = 2, NCOLS1-1
            DU = ALON( C+1,R ) - ALON( C-1,R )
            DV = ALAT( C+1,R ) - ALAT( C-1,R )
            HY = 1.0 / SQRT( ( DU**2 + DV**2 ) )
            C1 = DV * HY
            S1 = DU * HY
            DV = ALON( C,R+1 ) - ALON( C,R-1 )
            DU = ALAT( C,R+1 ) - ALAT( C,R-1 )
            HY = 1.0 / SQRT( ( DU**2 + DV**2 ) )
            C2 = -DV * HY
            S2 =  DU * HY
            COSA( C,R ) = 0.5 * ( C1 + C2 )
            SINA( C,R ) = 0.5 * ( S1 + S2 )
        END DO
        END DO

        C = 1                           !!  bottom row
        DO R = 2, NROWS1-1
            DV = ALON( C,R+1 ) - ALON( C,R-1 )
            DU = ALAT( C,R+1 ) - ALAT( C,R-1 )
            HY = 1.0 / SQRT( ( DU**2 + DV**2 ) )
            C2 = -DV * HY
            S2 =  DU * HY
            COSA( C,R ) = 0.5 * ( C1 + C2 )
            SINA( C,R ) = 0.5 * ( S1 + S2 )
        END DO

        C = NCOLS1                      !!  top row
        DO R = 2, NROWS1-1
            DV = ALON( C,R+1 ) - ALON( C,R-1 )
            DU = ALAT( C,R+1 ) - ALAT( C,R-1 )
            HY = 1.0 / SQRT( ( DU**2 + DV**2 ) )
            C2 = -DV * HY
            S2 =  DU * HY
            COSA( C,R ) = 0.5 * ( C1 + C2 )
            SINA( C,R ) = 0.5 * ( S1 + S2 )
        END DO

        R = 1                           !!  left column
        DO C = 2, NCOLS1-1
            DU = ALON( C+1,R ) - ALON( C-1,R )
            DV = ALAT( C+1,R ) - ALAT( C-1,R )
            HY = 1.0 / SQRT( ( DU**2 + DV**2 ) )
            COSA( C,R ) = DV * HY
            SINA( C,R ) = DU * HY
        END DO

        R = NROWS1                      !!  right column
        DO C = 2, NCOLS1-1
            DU = ALON( C+1,R ) - ALON( C-1,R )
            DV = ALAT( C+1,R ) - ALAT( C-1,R )
            HY = 1.0 / SQRT( ( DU**2 + DV**2 ) )
            COSA( C,R ) = DV * HY
            SINA( C,R ) = DU * HY
        END DO

        C = 1                           !!  SW corner
        R = 1
        DU = ALON( C+1,R ) - ALON( C,R )
        DV = ALAT( C+1,R ) - ALAT( C,R )
        HY = 1.0 / SQRT( ( DU**2 + DV**2 ) )
        C1 = DV * HY
        S1 = DU * HY
        DV = ALON( C,R+1 ) - ALON( C,R )
        DU = ALAT( C,R+1 ) - ALAT( C,R )
        HY = 1.0 / SQRT( ( DU**2 + DV**2 ) )
        C2 = -DV * HY
        S2 =  DU * HY
        COSA( C,R ) = 0.5 * ( C1 + C2 )
        SINA( C,R ) = 0.5 * ( S1 + S2 )

        C = NCOLS1                      !!  SE corner
        R = 1
        DU = ALON( C,R ) - ALON( C-1,R )
        DV = ALAT( C,R ) - ALAT( C-1,R )
        HY = 1.0 / SQRT( ( DU**2 + DV**2 ) )
        C1 = DV * HY
        S1 = DU * HY
        DV = ALON( C,R+1 ) - ALON( C,R )
        DU = ALAT( C,R+1 ) - ALAT( C,R )
        HY = 1.0 / SQRT( ( DU**2 + DV**2 ) )
        C2 = -DV * HY
        S2 =  DU * HY
        COSA( C,R ) = 0.5 * ( C1 + C2 )
        SINA( C,R ) = 0.5 * ( S1 + S2 )

        C = 1                           !! NW corner
        R = NROWS1
        DU = ALON( C+1,R ) - ALON( C,R )
        DV = ALAT( C+1,R ) - ALAT( C,R )
        HY = 1.0 / SQRT( ( DU**2 + DV**2 ) )
        C1 = DV * HY
        S1 = DU * HY
        DV = ALON( C,R ) - ALON( C,R-1 )
        DU = ALAT( C,R ) - ALAT( C,R-1 )
        HY = 1.0 / SQRT( ( DU**2 + DV**2 ) )
        C2 = -DV * HY
        S2 =  DU * HY
        COSA( C,R ) = 0.5 * ( C1 + C2 )
        SINA( C,R ) = 0.5 * ( S1 + S2 )

        C = NCOLS1                      !! NE corner
        R = NROWS1
        DU = ALON( C,R ) - ALON( C-1,R )
        DV = ALAT( C,R ) - ALAT( C-1,R )
        HY = 1.0 / SQRT( ( DU**2 + DV**2 ) )
        C1 = DV * HY
        S1 = DU * HY
        DV = ALON( C,R ) - ALON( C,R-1 )
        DU = ALAT( C,R ) - ALAT( C,R-1 )
        HY = 1.0 / SQRT( ( DU**2 + DV**2 ) )
        C2 = -DV * HY
        S2 =  DU * HY
        COSA( C,R ) = 0.5 * ( C1 + C2 )
        SINA( C,R ) = 0.5 * ( S1 + S2 )

        RETURN

    END SUBROUTINE LLANGLE


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
    !!    in-place transform: REAL Lat-Lon to X-Y
    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

    SUBROUTINE LLTOXY( N,  X, Y )

        USE MODGCTP

        INTEGER, INTENT( IN    ) :: N
        REAL,    INTENT( INOUT ) :: X( N ) , Y( N )

        REAL*8  XLOC( N ), YLOC( N )
        REAL*8  DLAT( N ), DLON( N )

        !!....................  body  .............................

        IF ( GDTYP1 .EQ. LATGRD3 )  RETURN

        DLAT = DBLE( Y )
        DLON = DBLE( X )

        CALL XY2XY( LATGRD3, 0.0D0,  0.0D0,  0.0D0,  0.0D0, 0.0D0,      &
                    GDTYP1,  P_ALP1, P_BET1, P_GAM1, XCENT1, YCENT1,    &
                    N, DLON, DLAT, XLOC, YLOC )

        X = REAL( XLOC )
        Y = REAL( YLOC )

        RETURN

    END SUBROUTINE LLTOXY


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
    !!    READ3 and convert to REAL
    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

    LOGICAL FUNCTION READIT( FNAME, VNAME, VTYPE, NCOLS, NROWS, LAYER, JDATE, JTIME, RBUF )

        CHARACTER*(*), INTENT(IN   ) :: FNAME, VNAME
        INTEGER      , INTENT(IN   ) :: VTYPE, NCOLS, NROWS, LAYER, JDATE, JTIME
        REAL         , INTENT(  OUT) :: RBUF( NCOLS, NROWS )

        INTEGER     C, R
        INTEGER     IBUF( NCOLS, NROWS )
        REAL*8      DBUF( NCOLS, NROWS )

        IF ( VTYPE .EQ. M3REAL ) THEN
            READIT = READ3( FNAME, VNAME, LAYER, JDATE, JTIME, RBUF )
        ELSE IF ( VTYPE .EQ. M3DBLE ) THEN
            READIT = READ3( FNAME, VNAME, LAYER, JDATE, JTIME, DBUF )
            RBUF   = SNGL( DBUF )
        ELSE IF ( VTYPE .EQ. M3INT  ) THEN
            READIT = READ3( FNAME, VNAME, LAYER, JDATE, JTIME, IBUF )
            RBUF   = FLOAT( IBUF )
        ELSE
            READIT = .FALSE.
        END IF

        RETURN

    END FUNCTION READIT


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


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION DBLSAME( P, Q )
        REAL*8, INTENT( IN ) :: P, Q
        DBLSAME = ( (P - Q)**2  .LT.  1.0D-9*( P*P + Q*Q + 1.0D-5 ) )
        RETURN
    END FUNCTION DBLSAME


END PROGRAM M3PLOT
