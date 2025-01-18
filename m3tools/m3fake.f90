
PROGRAM M3FAKE

    !!***********************************************************************
    !! Version "$ $Id: m3fake.f90 1 2017-06-10 18:05:20Z coats $"
    !! EDSS/Models-3 M3TOOLS.
    !! Copyright (C) 1992-2002 MCNC, 
    !! (C) 1995-2002,2005-2013 Carlie J. Coats, Jr.,
    !! (C) 2002-2010 Baron Advanced Meteorological Systems. LLC., and 
    !! (C) 2015-2016 UNC Institute for the Environment.
    !! Distributed under the GNU GENERAL PUBLIC LICENSE version 2
    !! See file "GPL.txt" for conditions of use.
    !!.........................................................................
    !!  subroutine body starts at line  134
    !!
    !!  FUNCTION:
    !!       Generate new EDSS/Models-3 I/O API file with the user-sepcified
    !!       structure and "fake" data.
    !!
    !!  PRECONDITIONS REQUIRED:
    !!       none
    !!
    !!  SUBROUTINES AND FUNCTIONS CALLED:
    !!       FAKESTEP
    !!
    !!  REVISION  HISTORY:
    !!      prototype 8/1995 by CJC
    !!      Modified  8/1997 by CJC:  additional file types supported
    !!      Modified  1/2000 by CJC:  additional coordinate types supported
    !!      Version  11/2001 by CJC for I/O API Version 2.1
    !!      Version   1/2002 by CJC for I/O API Version 2.2:  bug-fix in
    !!           "timestep" formula; enhanced opening-screen text.
    !!      Version   6/2008 by CJC:  Albers map-projection support
    !!      Version 02/2010 by CJC for I/O API v3.1:  Fortran-90 only;
    !!          USE M3UTILIO, and related changes.
    !!      Version  02/2015 by CJC for I/O API v3.2:  F90 free-format source
    !!      Support for M3INT8 variables.
    !!      Version  03/2016 by CJC:  improved GRID NAME prompt
    !!***********************************************************************

    USE M3UTILIO

    IMPLICIT NONE

    !!...........   EXTERNAL FUNCTIONS and their descriptions:

     INTEGER, EXTERNAL :: IARGC

    !!...........   PARAMETERS and their descriptions:

    DOUBLE PRECISION, PARAMETER :: SIXTH = 1.0D0 / 6.0D0

    INTEGER,      PARAMETER :: ATYPES( 6 ) = (/ 1, 2, 3, 4, 5, 6 /)
    CHARACTER*60, PARAMETER ::  AMENU( 6 ) =            &
        (/  'Column   number (1...NCOLS)        ',      &
            'Row      number (1...NROWS)        ',      &
            'Layer    number (1...NLAYS)        ',      &
            'Timestep number (1...NSTEPS)       ',      &
            'User-specified fill value          ',      &
            'Input from user-specified file     '   /)

    INTEGER, PARAMETER :: FTYPES( 7 ) =  (/  CUSTOM3, GRDDED3, BNDARY3, IDDATA3, PROFIL3, GRNEST3, SMATRX3   /)

    CHARACTER*60, PARAMETER :: FMENU( 7 ) =         &
        (/  'File type CUSTOM           ',          &
            'File type GRIDDED          ',          &
            'File type BOUNDARY         ',          &
            'File type IDDATA           ',          &
            'File type PROFILE          ',          &
            'File type GRID-NEST        ',          &
            'File type SPARSE MATRIX    '    /)

    INTEGER, PARAMETER :: VTYPES( 4 ) = (/ M3REAL, M3DBLE, M3INT, M3INT8 /)

    CHARACTER*60, PARAMETER :: VMENU( 4 ) =                 &
        (/  'Primitive data type REAL               ',      &
            'Primitive data type DOUBLE PRECISION   ',      &
            'Primitive data type INTEGER            ',      &
            'Primitive data type INTEGER8           '    /)

    INTEGER, PARAMETER :: GTYPES( 10 ) =                    &
        (/  LATGRD3, LAMGRD3, MERGRD3, STEGRD3, UTMGRD3,    &
            POLGRD3, EQMGRD3, TRMGRD3, ALBGRD3, IMISS3 /)

    CHARACTER*60, PARAMETER :: GMENU( 10 ) =                            &
        (/  'Map projection type LAT-LON                        ',      &
            'Map projection type LAMBERT CONFORMAL CONIC        ',      &
            'Map projection type MERCATOR( general)             ',      &
            'Map projection type (tangent) STEREOGRAPHIC        ',      &
            'Map projection type UTM                            ',      &
            'Map projection type (secant) POLAR STEREOGRAPHIC   ',      &
            'Map projection type EQUATORIAL MERCATOR            ',      &
            'Map projection type TRANSVERSE MERCATOR            ',      &
            'Map projection type ALBERS EQUAL-AREA CONIC        ',      &
            'Map projection type MISSING or NOT APPLICABLE      '    /)

    INTEGER, PARAMETER :: VGTYPES( 7 ) =                                &
        (/  VGPRES3, VGZVAL3, VGHVAL3, VGSGPH3, VGSGPN3, VGSIGZ3, IMISS3 /)

    CHARACTER*60, PARAMETER :: VGMENU( 7 ) =                            &
        (/  'Vertical coordinate type PRESSURE (mb)             ',      &
            'Vertical coordinate type Z (height a.g.l., m)      ',      &
            'Vertical coordinate type H (height a.s.l., m)      ',      &
            'Vertical coordinate type HYDROSTATIC SIGMA-P       ',      &
            'Vertical coordinate type NONHYDROSTATIC SIGMA-P    ',      &
            'Vertical coordinate type SIGMA-Z                   ',      &
            'Vertical coordinate type MISSING or NOT APPLICABLE '  /)

    CHARACTER*16, PARAMETER :: BLANK = ' '
    CHARACTER*16, PARAMETER :: PNAME = 'M3FAKE'
    CHARACTER*72, PARAMETER :: BAR   = &
    '-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-'


    !!...........   LOCAL VARIABLES and their descriptions:

    INTEGER         IDUM        !  value from INIT3()
    INTEGER         OPTYPES( MXVARS3 )
    REAL            FILLVAL( MXVARS3 )
    REAL*8          XSCR, YSCR
    REAL            VSCR
    INTEGER         V, L, T
    INTEGER         NSTEPS, JDATE, JTIME, TSTEP
    INTEGER         IDEV
    INTEGER         ARGCNT
    CHARACTER*16    FNAME, CNAME
    CHARACTER*256   MESG
    CHARACTER*512   ENVBUF  !  value from command line arguments


    !!***********************************************************************
    !!   begin body of program M3FAKE

    IDUM = INIT3()

    WRITE( *,'( 5X, A )' ) BLANK, BAR, BLANK,                               &
'Program "M3FAKE" to create "dummied-up" M3IO files according to the',      &
'user-specified file description, user-selected formulas, and optionally',  &
'ASCII input data files.',                                                  &
'',                                                                         &
'USAGE:  "m3fake [<lname>]" and follow the prompts.',                       &
'',                                                                         &
'THE PROGRAM WILL PROMPT YOU for the logical name of the',                  &
'output file (unless you put it on the command line),',                     &
'the specifications for the output file, and (if appropriate)',             &
'the logical names of the ASCII input data files.',                         &
'',                                                                         &
'PRECONDITIONS REQUIRED:',                                                  &
'     setenv  GRIDDESC        <path-name>',                                 &
'     setenv  <output lname>  <path-name>',                                 &
'     setenv  <input  lname>  <path-name> for each input file',             &
'',                                                                         &
'     The input data files, if any,  must be a list-directed',              &
'     (e.g., comma-delimited) ASCII file containing all of the',            &
'     desired input data for a single variable each, to be',                &
'     copied into the output file.  This option is available',              &
'     for REAL variables only.  The data should be ordered /',              &
'     formatted as follows:',                                               &
'',                                                                         &
'         The fastest subscript should be grid-column',                     &
'         There should be a line-break after each column',                  &
'         The   next  subscript should be grid-row',                        &
'         The   next  subscript should be grid-layer if present',           &
'         The slowest subscript should be time-step if present',            &
'',                                                                         &
'See URL',                                                                  &
'',                                                                         &
'   https://www.cmascenter.org/ioapi/documentation/3.1/html/AA.html#tools', &
'',                                                                         &
'Program copyright (C) 1992-2002 MCNC, (C) 1995-2013 Carlie J. Coats, Jr.', &
'(C) 2002-2010 Baron Advanced Meteorological Systems, LLC., and',           &
'(C) 2015 UNC Institute for the Environment.',                              &
'Released under Version 2 of the GNU General Public License. See',          &
'enclosed GPL.txt, or URL',                                                 &
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
'$Id: m3fake.f90 1 2017-06-10 18:05:20Z coats $',&
' '

    IF ( .NOT.GETYN( 'Continue with program?', .TRUE. ) ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'Program aborted', 2 )
    END IF

    CALL M3MESG( BAR )
    CALL M3MESG( 'First, let us define the file type and get the dimensions.' )
    CALL M3MESG( BLANK )
    FTYPE3D = FTYPES( GETMENU( 3, 2,  'Enter file type', FMENU ) )


    !!...............   Get file, grid description:

    CALL GETSTR( 'Enter the grid name (or RET to enter grid parms) >>', BLANK,  GDNAM3D )

    IF ( DSCGRID( GDNAM3D, CNAME, GDTYP3D,              &      !  "standard" grid name
                  P_ALP3D, P_BET3D, P_GAM3D,            &
                  XCENT3D, YCENT3D, XORIG3D, YORIG3D,   &
                  XCELL3D, YCELL3D,                     &
                  NCOLS3D, NROWS3D, NTHIK3D ) ) THEN

        IF( FTYPE3D .EQ. CUSTOM3 ) THEN

            NCOLS3D = GETNUM( 1, 1999999999, 100, 'Enter BLOBSIZE' )

        ELSE IF( FTYPE3D .EQ. IDDATA3 ) THEN

            NCOLS3D = IMISS3
            NROWS3D = GETNUM( 1, 1999999999, 100, 'Enter NUMBER OF SITES' )

        ELSE IF( FTYPE3D .EQ. PROFIL3 ) THEN

            NCOLS3D = GETNUM( 1, 1999999999, 100, 'Enter MAX LEVEL-COUNT' )
            NROWS3D = GETNUM( 1, 1999999999, 100, 'Enter NUMBER OF SITES' )

        ELSE IF( FTYPE3D .EQ. GRNEST3 ) THEN

            NCOLS3D = GETNUM( 1, 1999999999, 10, 'Enter MAX NEST-COUNT' )
            NROWS3D = GETNUM( 1, 1999999999, 1000, 'Enter NUMBER OF CELLS PER NEST' )

        ELSE IF( FTYPE3D .EQ. SMATRX3 ) THEN

            NCOLS3D = GETNUM( 1, 1999999999, 10000, 'Enter MAX NUMBER OF COEFFS' )
            NROWS3D = GETNUM( 1, 1999999999, 1000, 'Enter number of matrix rows' )

        END IF      !  through ftype3d's (where dscgrid() worked)

    ELSE            !  else dscgrid() failed

        IF( FTYPE3D .EQ. CUSTOM3 ) THEN

            NCOLS3D = GETNUM( 1, 1999999999, 100, 'Enter BLOBSIZE' )
            NROWS3D = IMISS3
            NTHIK3D = IMISS3

        ELSE IF( FTYPE3D .EQ. GRDDED3 ) THEN

            NCOLS3D = GETNUM( 1, 1999999999, 100, 'Enter number of GRID COLUMNS' )
            NROWS3D = GETNUM( 1, 1999999999, 100, 'Enter number of GRID ROWS' )
            NTHIK3D = IMISS3

        ELSE IF( FTYPE3D .EQ. BNDARY3 ) THEN

            NCOLS3D = GETNUM( 1, 1999999999, 100, 'Enter number of GRID COLUMNS' )
            NROWS3D = GETNUM( 1, 1999999999, 100, 'Enter number of GRID ROWS' )
            NTHIK3D = MIN( NCOLS3D / 2, NROWS3D / 2 ) - 1
            NTHIK3D = GETNUM( -NTHIK3D, NTHIK3D, 1, 'Enter PERIMETER THICKNESS (cells)' )

        ELSE IF( FTYPE3D .EQ. IDDATA3 ) THEN

            NCOLS3D = IMISS3
            NROWS3D = GETNUM( 1, 1999999999, 100, 'Enter NUMBER OF SITES' )
            NTHIK3D = IMISS3

        ELSE IF( FTYPE3D .EQ. PROFIL3 ) THEN

            NCOLS3D = GETNUM( 1, 1999999999, 100, 'Enter MAX LEVEL-COUNT' )
            NROWS3D = GETNUM( 1, 1999999999, 100, 'Enter NUMBER OF SITES' )
            NTHIK3D = IMISS3

        ELSE IF( FTYPE3D .EQ. GRNEST3 ) THEN

            NCOLS3D = GETNUM( 1, 1999999999, 10, 'Enter MAX NEST-COUNT' )
            NROWS3D = GETNUM( 1, 1999999999, 1000, 'Enter NUMBER OF CELLS PER NEST' )
            NTHIK3D = IMISS3

        ELSE IF( FTYPE3D .EQ. SMATRX3 ) THEN

            NCOLS3D = GETNUM( 1, 1999999999, 10000, 'Enter MAX NUMBER OF COEFFS' )
            NROWS3D = GETNUM( 1, 1999999999, 1000, 'Enter number of matrix rows' )
            NTHIK3D = IMISS3

        END IF      !  through all the ftype3d's

        CALL M3MESG( BLANK )
        CALL M3MESG(  'Next, let us construct the horizontal coordinate system' )
        CALL M3MESG( '("map projection") for the file' )
        CALL M3MESG( BLANK )

        GDTYP3D = GTYPES( GETMENU( 10, 2, 'Enter coordinate system type', GMENU ) )

        IF ( GDTYP3D .EQ. LATGRD3 ) THEN

            XCELL3D = GETDBLE( 0.0D0, 1.0D36, 0.25D0, 'Enter cell size DX (deg.E)' )
            YCELL3D = GETDBLE( 0.0D0, 1.0D36, SIXTH,  'Enter cell size DY (deg.N)' )
            XSCR    = -90.0D0
            YSCR    =  27.0D0

        ELSE IF ( GDTYP3D .EQ. LAMGRD3 ) THEN

            P_ALP3D = GETDBLE( -90.0D0,   90.0D0,  30.0D0, 'Enter first  latitude for cone' )
            P_BET3D = GETDBLE( P_ALP3D,   90.0D0,  60.0D0, 'Enter second latitude for cone' )
            P_GAM3D = GETDBLE( -180.0D0, 180.0D0, -90.0D0, 'Enter central longitude for cone' )
            XCENT3D = GETDBLE( -180.0D0, 180.0D0, P_GAM3D, 'Enter longitude for X-Y origin' )
            YSCR    = 0.5D0 * ( P_ALP3D + P_BET3D )
            YCENT3D = GETDBLE( P_ALP3D,  P_BET3D,    YSCR, 'Enter latitude for X-Y origin' )
            XCELL3D = GETDBLE( 0.0D0, 1.0D36, 5.0D3,  'Enter cell size DX (m)' )
            YCELL3D = GETDBLE( 0.0D0, 1.0D36, 5.0D3,  'Enter cell size DY (m)' )
            XSCR    = XCELL3D * DBLE( -NCOLS3D / 2 )
            YSCR    = YCELL3D * DBLE( -NROWS3D / 2 )

        ELSE IF ( GDTYP3D .EQ. MERGRD3 ) THEN

            P_ALP3D = GETDBLE(  -90.0D0,  90.0D0,    30.0D0, 'Enter latitude  for cylinder origin' )
            P_BET3D = GETDBLE( -180.0D0, 180.0D0,   -90.0D0, 'Enter longitude for cylinder origin' )
            P_GAM3D = GETDBLE( -180.0D0, 180.0D0,   -90.0D0, 'Enter angle between axis and N pole' )
            XCENT3D = GETDBLE(    0.0D0,  90.0D0,    30.0D0, 'Enter latitude for X-Y origin' )
            YCENT3D = GETDBLE( -180.0D0, 180.0D0,   -90.0D0, 'Enter longitude for X-Y origin' )
            XCELL3D = GETDBLE(    0.0D0,  1.0D36,     5.0D3, 'Enter cell size DX (m)' )
            YCELL3D = GETDBLE(    0.0D0,  1.0D36,     5.0D3, 'Enter cell size DY (m)' )
            XSCR    = XCELL3D * DBLE( -NCOLS3D / 2 )
            YSCR    = YCELL3D * DBLE( -NROWS3D / 2 )

      ELSE IF ( GDTYP3D .EQ. STEGRD3 ) THEN

            P_ALP3D = GETDBLE(  -90.0D0,  90.0D0,    60.0D0, 'Enter latitude for point of tangency' )
            P_BET3D = GETDBLE( -180.0D0, 180.0D0,   -90.0D0, 'Enter longitude for point of tangency')
            P_GAM3D = GETDBLE( -180.0D0, 180.0D0,     0.0D0, 'Enter angle between Y and true N at origin')
            XCENT3D = GETDBLE(    0.0D0,  90.0D0,   P_ALP3D, 'Enter latitude for X-Y origin' )
            YCENT3D = GETDBLE( -180.0D0, 180.0D0,   P_BET3D, 'Enter longitude for X-Y origin' )
            XCELL3D = GETDBLE(    0.0D0,  1.0D36,     5.0D3, 'Enter cell size DX (m)' )
            YCELL3D = GETDBLE(    0.0D0,  1.0D36,     5.0D3, 'Enter cell size DY (m)' )
            XSCR    = XCELL3D * DBLE( -NCOLS3D / 2 )
            YSCR    = YCELL3D * DBLE( -NROWS3D / 2 )

      ELSE IF ( GDTYP3D .EQ. UTMGRD3 ) THEN

            P_ALP3D = DBLE( GETNUM( 1, 36, 17, 'Enter UTM zone' ) )
            P_BET3D = DBLE( BADVAL3 )
            P_GAM3D = DBLE( BADVAL3 )
            XSCR    = DBLE( BADVAL3 )
            YSCR    = -XSCR
            XCENT3D = GETDBLE( XSCR,  YSCR,   0.0D0, 'Enter UTM X offset for origin (m)' )
            YCENT3D = GETDBLE( XSCR,  YSCR,   0.0D0, 'Enter UTM Y offset for origin (m)' )
            XCELL3D = GETDBLE( 0.0D0, 1.0D36, 5.0D3, 'Enter cell size DX (m)' )
            YCELL3D = GETDBLE( 0.0D0, 1.0D36, 5.0D3, 'Enter cell size DY (m)' )
            XSCR    =  350.0D3
            YSCR    = 3785.0D3

      ELSE IF ( GDTYP3D .EQ. POLGRD3 ) THEN

            WRITE( *,'( 5X, A )' ) ' ',                                 &
               'NORTH POLAR projections project from the SOUTH POLE',   &
               'and contain the NORTH POLE in the projected plane'
            IF ( GETYN( 'Is this projection NORTH POLAR', .TRUE. ) ) THEN
                P_ALP3D = 1.0D0
                YSCR    = 90.0D0
            ELSE
                P_ALP3D = -1.0D0
                YSCR    = -90.0D0
            END IF
            P_BET3D = GETDBLE(  -90.0D0,  90.0D0, 60.0D0,  'Enter SECANT LATITUDE')
            P_GAM3D = GETDBLE( -180.0D0, 180.0D0,  0.0D0,  'Enter CENTRAL LONGITUDE')
            XCENT3D = GETDBLE( -180.0D0, 180.0D0, P_GAM3D, 'Enter longitude for X-Y origin' )
            YCENT3D = GETDBLE(  -90.0D0,  90.0D0,    YSCR, 'Enter latitude for X-Y origin' )
            XCELL3D = GETDBLE(    0.0D0,  1.0D36,   5.0D3, 'Enter cell size DX (m)' )
            YCELL3D = GETDBLE(    0.0D0,  1.0D36,   5.0D3, 'Enter cell size DY (m)' )
            XSCR    = XCELL3D * DBLE( -NCOLS3D / 2 )
            YSCR    = YCELL3D * DBLE( -NROWS3D / 2 )

      ELSE IF ( GDTYP3D .EQ. EQMGRD3 ) THEN

            P_ALP3D = GETDBLE( -180.0D0, 180.0D0,   0.0D0, 'Enter latitude of true scale' )
            P_BET3D = 0.0D0
            P_GAM3D = GETDBLE(  -90.0D0,  90.0D0,   0.0D0, 'Enter longitude of the central meridian' )
            XCENT3D = GETDBLE( -180.0D0, 180.0D0, P_GAM3D, 'Enter longitude for X-Y origin' )
            YCENT3D = GETDBLE(  -90.0D0,  90.0D0, P_ALP3D, 'Enter latitude for X-Y origin' )
            XCELL3D = GETDBLE(    0.0D0,  1.0D36,   5.0D3, 'Enter cell size DX (m)' )
            YCELL3D = GETDBLE(    0.0D0,  1.0D36,   5.0D3, 'Enter cell size DY (m)' )
            XSCR    = XCELL3D * DBLE( -NCOLS3D / 2 )
            YSCR    = YCELL3D * DBLE( -NROWS3D / 2 )

      ELSE IF ( GDTYP3D .EQ. TRMGRD3 ) THEN

            P_ALP3D = GETDBLE( -180.0D0,  180.0D0, 0.0D0,   'Enter latitude of the origin' )
            P_BET3D = GETDBLE(    0.0D0,  9.99D36, 1.0D0,   'Enter scale factor at the central meridian' )
            P_GAM3D = GETDBLE(  -90.0D0,   90.0D0, 0.0D0,   'Enter longitude of the central meridian' )
            XCENT3D = GETDBLE( -180.0D0,  180.0D0, P_GAM3D, 'Enter longitude for X-Y origin' )
            YSCR    = 0.5D0 * ( P_ALP3D + P_BET3D )
            YCENT3D = GETDBLE( P_ALP3D, P_BET3D, YSCR,      'Enter latitude for X-Y origin' )
            XCELL3D = GETDBLE(   0.0D0,  1.0D36, 5.0D3,    'Enter cell size DX (m)' )
            YCELL3D = GETDBLE(   0.0D0,  1.0D36, 5.0D3,    'Enter cell size DY (m)' )
            XSCR    = XCELL3D * DBLE( -NCOLS3D / 2 )
            YSCR    = YCELL3D * DBLE( -NROWS3D / 2 )

        ELSE IF ( GDTYP3D .EQ. ALBGRD3 ) THEN

            P_ALP3D = GETDBLE( -90.0D0,   90.0D0,  30.0D0, 'Enter first  latitude for cone' )
            P_BET3D = GETDBLE( P_ALP3D,   90.0D0,  60.0D0, 'Enter second latitude for cone' )
            P_GAM3D = GETDBLE( -180.0D0, 180.0D0, -90.0D0, 'Enter central longitude for cone' )
            XCENT3D = GETDBLE( -180.0D0, 180.0D0, P_GAM3D, 'Enter longitude for X-Y origin' )
            YSCR    = 0.5D0 * ( P_ALP3D + P_BET3D )
            YCENT3D = GETDBLE( P_ALP3D,  P_BET3D,    YSCR, 'Enter latitude for X-Y origin' )
            XCELL3D = GETDBLE( 0.0D0,     1.0D36,   5.0D3, 'Enter cell size DX (m)' )
            YCELL3D = GETDBLE( 0.0D0,     1.0D36,   5.0D3, 'Enter cell size DY (m)' )
            XSCR    = XCELL3D * DBLE( -NCOLS3D / 2 )
            YSCR    = YCELL3D * DBLE( -NROWS3D / 2 )

      ELSE

            CALL M3MESG( 'Grid/coordinate type:  "UNKNOWN" or "MISSING"' )
            P_ALP3D = GETDBLE( DBLE( BADVAL3 ), -DBLE( BADVAL3 ), 0.0D0, 'Enter projection parameter ALPHA' )
            P_BET3D = GETDBLE( DBLE( BADVAL3 ), -DBLE( BADVAL3 ), 0.0D0, 'Enter projection parameter BETA ' )
            P_GAM3D = GETDBLE( DBLE( BADVAL3 ), -DBLE( BADVAL3 ), 0.0D0, 'Enter projection parameter GAMMA' )
            XCENT3D = GETDBLE( DBLE( BADVAL3 ), -DBLE( BADVAL3 ), 0.0D0, 'Enter X-Y origin' )
            YCENT3D = GETDBLE( DBLE( BADVAL3 ), -DBLE( BADVAL3 ), 0.0D0, 'Enter X-Y origin' )
            XCELL3D = GETDBLE( DBLE( BADVAL3 ), -DBLE( BADVAL3 ), 5.0D3, 'Enter cell size DX (m)' )
            YCELL3D = GETDBLE( DBLE( BADVAL3 ), -DBLE( BADVAL3 ), 5.0D3, 'Enter cell size DY (m)' )
            XSCR    = 0.0D0
            YSCR    = 0.0D0

        END IF      !  through grdtyp3d's (where dscgrid failed)

        WRITE( *,'( 5X, A )' ) '',                                      &
            'Grid corners are given in terms of map-projection units',  &
            '(meters, for everything except LL) from the Cartesian',    &
            'origin, and specify the lower-left corner of cell (1,1).'
             XORIG3D = GETDBLE( -1.0D36, 1.0D36, XSCR, 'Enter starting grid corner X' )
        YORIG3D = GETDBLE( -1.0D36, 1.0D36, YSCR, 'Enter starting grid corner Y' )

    END IF            !  if dscgrid() worked, or not


    !!...............   Get vertical structure description:

    WRITE( *,'( 5X, A )' ) ' ',                                         &
        'Next, let us construct the vertical coordinate system ',       &
        'for the file.  You will need to enter the number of layers,',  &
        'the vertical-coordinate type, and the layer-values for',       &
        'layers L = 0, ..., <number of layers>.', ' '

    NLAYS3D = GETNUM( 1, MXLAYS3, 30, 'Enter number of layers' )

    IF ( NLAYS3D .GT. 1 ) THEN

        VGTYP3D = VGTYPES( GETMENU( 7, 4, 'Enter vertical coordinate type', VGMENU ) )

        IF ( ( VGTYP3D .EQ. VGSGPH3 ) .OR.      &
             ( VGTYP3D .EQ. VGSGPN3 ) .OR.      &
             ( VGTYP3D .EQ. VGSIGZ3 )    ) THEN
            VGTOP3D = GETREAL( 0.0, 9.99E36, 100.0, 'Enter MODEL TOP for sigma coordinate' )
        ELSE
            VGTOP3D = BADVAL3
        END IF
        DO  L = 1, NLAYS3D+1
            WRITE( MESG, '( A, I4, :, A )' ) 'Enter value for layer', L-1
            VSCR = FLOAT( L-1 ) / FLOAT( NLAYS3D )
            VGLVS3D( L ) = GETREAL( 0.0, 9.99E36, VSCR, MESG )
        END DO

    ELSE

        VGTYP3D = IMISS3
        VGTOP3D = BADVAL3
        VGLVS3D( : ) = BADVAL3

    END IF      !  if nlays3d > 1, or not


    !!...............   Get timestep structure:

    WRITE( *,'( 5X, A )' ) ' ',                                         &
        'Next, let us construct the time step structure for the file',  &
        '    TSTEP > 0:  "timestepped" file;',                          &
        '    TSTEP = 0   "time-independent" file;',                     &
        '    TSTEP > 0   "circular-buffer" file.', ' '

    TSTEP3D = GETNUM( -999999999, 999999999, 10000, 'Enter TSTEP (hhmmss)' )
    IF ( TSTEP3D .EQ. 0 ) THEN
        SDATE3D = 0
        STIME3D = 0
        NSTEPS  = 1
    ELSE
        CALL GETDTTIME( JDATE, JTIME )      !!  "today::now"
        SDATE3D = GETNUM( -1000,  9999366, JDATE, 'Enter STARTING DATE (yyyyddd)' )
        STIME3D = GETNUM(     0,   235959,     0, 'Enter STARTING TIME (hhmmss)' )
        NSTEPS  = GETNUM(     0,999999999,    24, 'Enter number of time steps' )
    END IF

    JDATE = SDATE3D
    JTIME = STIME3D
    TSTEP = TSTEP3D


    !!...............   Get variables and their descriptions:

    NVARS3D = GETNUM( 0, MXVARS3, 1, 'Enter number of variables' )

    WRITE( *,'( 5X, A )' ) ' ', 'Now enter the variable descriptions', ' '

    DO  V = 1, NVARS3D

        WRITE( MESG,94000 ) 'Enter NAME for variable', V, ' > '
        WRITE( *,95000 ) MESG ( 1 : 1+LEN_TRIM( MESG ) )
        READ(  *,'( A )' ) VNAME3D( V )

        WRITE( MESG,94000 ) 'Enter UNITS for variable', V, ' > '
        WRITE( *,95000 ) MESG ( 1 : 1+LEN_TRIM( MESG ) )
        READ(  *,'( A )' ) UNITS3D( V )

        WRITE( MESG,94000 ) 'Enter DESCRIPTION for variable', V, ' > '
        WRITE( *,95000 ) MESG ( 1 : 1+LEN_TRIM( MESG ) )
        READ(  *,'( A )' ) VDESC3D( V )

        WRITE( MESG,94000 ) 'Enter TYPE for variable', V
        VTYPE3D( V ) = VTYPES( GETMENU( 3, 1, MESG, VMENU ) )

        WRITE( MESG,94000 ) 'Enter FILLER FORMULA for variable', V
        OPTYPES( V ) = ATYPES( GETMENU( 6, 1, MESG, AMENU ) )
        IF ( OPTYPES( V ) .EQ. 5 ) THEN
            FILLVAL( V ) = GETREAL( BADVAL3, -BADVAL3, 0.0, 'Enter fill value' )
        ELSE IF ( OPTYPES( V ) .EQ. 6 ) THEN

            WRITE( *,'( 5X, A )' ) ' ',                                 &
                'The input data file must be a list-directed (e.g.,',   &
                'comma-delimited) ASCII file containing all of the',    &
                'desired input data to be copied into the output',      &
                'file, ordered and formatted as follows:',              &
                ' ',                                                    &
                '    The fastest subscript should be grid-column',      &
                '    There should be a line-break after each column',   &
                '    The   next  subscript should be grid-row',         &
                '    The   next  subscript should be grid-layer',       &
                '    The slowest subscript should be time-step', ' '
            IDEV = PROMPTFFILE('Enter logical name for input file', .TRUE., .TRUE., VNAME3D( V ), PNAME )
            OPTYPES( V ) = -IDEV

        END IF

    END DO

    WRITE( *,'( 5X, A )' )  ' ', 'Now enter the file description (end with a BLANK LINE)', ' '

    DO  L = 1, MXDESC3
        WRITE( *,95000 ) ' > '
        READ ( *,'( A )', END=45 ) FDESC3D( L )
        IF ( FDESC3D( L ) .EQ. BLANK )  EXIT
    END DO

45  CONTINUE
    CALL M3MESG( BLANK )

    !!...........   Open output file (prompting as necessary)

    ARGCNT = IARGC()

55  CONTINUE

        IF ( ARGCNT .EQ. 0 ) THEN   !  prompt for file name

            FNAME = PROMPTMFILE( 'Enter logical name for output file', FSUNKN3, 'OUTFILE', PNAME )

        ELSE IF ( ARGCNT .EQ. 1 ) THEN  !  file name from command line

            CALL GETARG( 1, ENVBUF )
            FNAME = ENVBUF( 1:16 )
            IF ( .NOT. OPEN3( FNAME, FSUNKN3, PNAME ) ) THEN

                MESG = 'Could not open "' // FNAME // '"'
                CALL M3WARN( PNAME, 0, 0, MESG )
                IF ( GETYN( 'Prompt for logical file name?', .TRUE. ) ) THEN
                    ARGCNT = 0
                    GO TO  55
                END IF

            END IF      !  if open3() failed

        ELSE

            CALL M3WARN( PNAME, 0, 0, 'Usage:  "m3fake [file]"' )
            IF ( GETYN( 'Prompt for logical file name?', .TRUE. ) ) THEN
                ARGCNT = 0
                GO TO  55
            END IF

        END IF      !  if argcnt 0, 1, or "wrong"

    DO  T = 1, NSTEPS

        CALL FAKESTEP( FNAME, JDATE, JTIME, OPTYPES, FILLVAL )
        CALL NEXTIME ( JDATE, JTIME, TSTEP )

    END DO


    CALL M3EXIT( PNAME, 0, 0, 'Program completed successfully', 0 )


    !!...........   Format Statements ..................................

94000   FORMAT( A, I4, :, A )
95000   FORMAT ( /5X , A , $ )          !  generic prompt format.


END PROGRAM M3FAKE

