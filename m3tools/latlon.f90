
PROGRAM LATLON

    !!***********************************************************************
    !! Version "$Id: latlon.F 1703 2014-12-17 21:39:36Z coats@bdsl$"
    !! EDSS/Models-3 M3TOOLS.
    !! Copyright (C) 1992-2002 MCNC, (C) 1997-2017 Carlie J. Coats, Jr.,
    !! (C) 2002-2012 Baron Advanced Meteorological Systems. LLC., and
    !! (C) 2014-2015 UNC Institute for the Environment.
    !! Distributed under the GNU GENERAL PUBLIC LICENSE version 2
    !! See file "GPL.txt" for conditions of use.
    !!.........................................................................
    !!  program body      starts at line   77
    !!  subroutine MAKEGRD starts at line 343
    !!  subroutine MAKEBDY starts at line 405
    !!
    !!  DESCRIPTION:
    !!       Builds 1-layer time-independent gridded file with LAT and LON
    !!
    !!  PRECONDITIONS REQUIRED:
    !!       "setenv"s for output files, GRIDDESC file
    !!       "f77 latlon.F -o latlon -L/home/xcc/SunOS5 -lemstuff -lm3io -lnetcdf"
    !!	from a directory containing PARMS3.EXT, FDESC3.EXT, IODECL3.EXT
    !!
    !!  SUBROUTINES AND FUNCTIONS CALLED:
    !!       I/O API and utility routines; Lambert conversion routines from
    !!	libemstuff
    !!
    !!  REVISION  HISTORY:
    !!      prototype 7/96 by CJC
    !!      Modified  9/99 by CJC for enhanced portability
    !!      Modified  9/99 by CJC:  more internal documentation about
    !!                             I/O API grid concepts.
    !!      Version 11/2001 by CJC for I/O API Version 2.1
    !!      Version 11/2005 by CJC:  eliminate unused vbles
    !!      Version 11/2007 by CJC:  splash-screen/contact-info update
    !!       Version 06/2008 by CJC:  Changes for Albers conformal conic
    !!
    !!      Version 02/2010 by CJC for I/O API v3.1:  Fortran-90 only;
    !!      USE M3UTILIO, and related changes.
    !!      Version 12/2013 by CJC:  PARAMETER  CMENU(:), CTYPE(:)
    !!      Version 02/2015 by CJC for I/O API-v3.2:  USE MODGCTP;
    !!      Fortran-90 "free" source format; use generics for "GET*()"
    !!      Version 12/2017 by CJC;  add double-precision vbles LATD, LOND;
    !!      add/fix EQMGRD3, TRMGRD3, MERGRD3, POLGRD3, STEGRD3, LEQGRD3 support.
    !!***********************************************************************

    USE M3UTILIO
    USE MODGCTP

    IMPLICIT NONE

    !!...........   PARAMETERS and their descriptions:

    CHARACTER*16, PARAMETER :: NONE  = 'NONE'
    CHARACTER*16, PARAMETER :: PNAME = 'LATLON'
    CHARACTER*20, PARAMETER :: CMENU( 10 ) = &
        (/  'lat-lon (ISO 6709)   ',        &   !  coordinate types menu item  1
            'Lambert Conformal    ',        &   !  coordinate types menu item  2
            'Lambert Equal Area   ',        &   !  coordinate types menu item  3
            'Equatorial Mercator  ',        &   !  coordinate types menu item  4
            'Transverse Mercator  ',        &   !  coordinate types menu item  5
            'General    Mercator  ',        &   !  coordinate types menu item  6
            'Polar  Stereographic ',        &   !  coordinate types menu item  7
            'General Stereographic',        &   !  coordinate types menu item  8
            'UTM                  ',        &   !  coordinate types menu item  9
            'Albers Equal-Area    '  /)         !  coordinate types menu item 10

    INTEGER, PARAMETER :: CTYPE( 10 ) =  &
        (/ LATGRD3, LAMGRD3, LEQGRD3, EQMGRD3, TRMGRD3, MERGRD3, POLGRD3, STEGRD3, UTMGRD3, ALBGRD3 /)

    !!...........   LOCAL VARIABLES and their descriptions:

    INTEGER         L
    INTEGER         LOGDEV
    CHARACTER*16    ANAME, BNAME, GNAME
    CHARACTER*160   MESG


    !!***********************************************************************
    !!.......   First:  Initialize the I/O API:

    LOGDEV = INIT3()	!  initialization returns unit # for log

    WRITE( *, '( 5X, A )' ) ' ',                                            &
'Program LATLON to construct matching TIME-INDEPENDENT 1-LAYER  GRIDDED',   &
'and BOUNDARY TIME-INDEPENDENT I/O API files containing the latitude and',  &
'longitude at cell centers, for a user specified coordinate system and',    &
'grid. You may turn off either file (GRIDDED or BOUNDARY) by responding',   &
'"NONE" to the prompt for its name.',                                       &
'',                                                                         &
'Specifications for this grid may either come from a GRIDDESC file',        &
'(if it has a named grid), or may be entered interactively.',               &
'',                                                                         &
'NOTE:  Currently, only Lat-lon, Lambert, UTM, Equatorial or Transverse',   &
'Mercator, Polar Stereographic and Albers Equal-Area coordinate systems,',  &
'and boundaries with  NTHIK > 0  are supported interactively.',             &
'',                                                                         &
'You will be prompted for the logical name of the output files.',           &
'You will need to have set up the environment for this program ',           &
'by appropriate commands ',                                                 &
'',                                                                         &
'    setenv  <FILENAME> <PHYSICAL PATH NAME>"',                             &
'',                                                                         &
'for the output files and (if you use it)  the GRIDDESC file.',             &
'',                                                                         &
'See URL',                                                                  &
'https://www.cmascenter.org/ioapi/documentation/3.1/html#tools',            &
'',                                                                         &
'Program copyright (C) 1992-2002 MCNC, (C) 1995-2017 Carlie J. Coats, Jr.', &
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
'$Id: latlon.f90 107 2018-07-26 14:05:39Z coats $',&
''

    IF ( .NOT. GETVAL( 'Continue with program?', .TRUE. ) ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'Program ended at user request', 0 )
    END IF

    MESG = 'Enter logical name for GRIDDED output file, or "NONE"'
    CALL GETSTR( MESG, 'GRDFILE', GNAME )

    MESG = 'Enter logical name for BOUNDARY output file, or "NONE"'
    CALL GETSTR( MESG, 'BDYFILE', BNAME )

    IF ( GNAME .EQ. NONE  .AND.  BNAME .EQ. NONE ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'No output files requested', 2 )
    END IF

    !!  According to EDSS/Models-3 I/O API coordinate and grid
    !!  conventions, the basic idea is:
    !!
    !!  One defines a horizontal Cartesian coordinate system
    !!  ("map projection") as giving (X,Y) coordinates (generally
    !!  in MKS units, i.e. Meters) relative to some known origin
    !!  (XCENT,YCENT), in terms of some known defining angles
    !!  (P_ALP, P_BET, P_GAM).
    !!
    !!  Having defined a map projection, one than then define
    !!  horizontal grids within it by specifying:
    !!
    !!      The (1,1)-corner (XORIG, YORIG) in terms of the
    !!      Cartesian coordinates of the map projection
    !!      (note that for the specification of this corner,
    !!      we take a "grid-cells are volumes" approach rather
    !!      than a Poincare-dual "grid is array of node-points"
    !!      point of view);
    !!
    !!      The cellsize (XCELL, YCELL) in both coordinate
    !!      directions;
    !!
    !!      The dimensionality (NCOLS,NROWS).
    !!
    !!      (Optionally) the thickness NTHIK in cells of a
    !!      boundary data-structure for the grid.
    !!
    !!  Note that frequently, we expect to have multiple grids
    !!  with the same map projection.
    !!
    !!  Having defined a grid in terms of a (MKS-unit) Cartesian
    !!  coordinate system mapped from the surface of the Earth,
    !!  one can then transform grid-related problems into problems
    !!  stated in (non-metric) grid-normal coordinates defined
    !!  relative to the (1,1) corner of the grid, as given by the
    !!  formulas
    !!
    !!      REAL C,R
    !!      ...
    !!      C = (X - XORIG) / XCELL
    !!      R = (Y - YORIG) / YCELL
    !!
    !!  for which the (I,J) cell is {(C,R): I-1 <= C <I, J-1 <= R <J }

    IF ( GETVAL( 'Specify grid by name from GRIDDESC file?', .TRUE. ) ) THEN

        CALL GETSTR( 'Enter grid name', 'SMRAQ54_50X48', GDNAM3D )
        IF ( .NOT. DSCGRID( GDNAM3D, ANAME  , GDTYP3D,          &
                            P_ALP3D, P_BET3D, P_GAM3D,          &
                            XCENT3D, YCENT3D,                   &
                            XORIG3D, YORIG3D, XCELL3D, YCELL3D, &
                            NCOLS3D, NROWS3D, NTHIK3D ) ) THEN

            MESG = 'Grid "' // TRIM( GDNAM3D ) // '" not found in GRIDDESC file'
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )

        END IF          !  if DSCGRID failed

    ELSE        !  enter grid specs interactively

        CALL GETSTR( 'Enter grid name', 'SMRAQ54_48X50', GDNAM3D )
        GDTYP3D = CTYPE( GETVAL( 10, 2, 'Enter number for horizontal coordinate system type', CMENU ) )

        IF ( GDTYP3D .EQ. LATGRD3 ) THEN !  lat-lon:  no P_ALP, ...

            P_ALP3D = 0.0D0
            P_BET3D = 0.0D0
            P_GAM3D = 0.0D0
            XCENT3D = 0.0D0
            YCENT3D = 0.0D0

        ELSE IF ( GDTYP3D .EQ. LAMGRD3  .OR.    &
                  GDTYP3D .EQ. ALBGRD3  ) THEN !  Lambert or Albers conic projection

            P_ALP3D = GETVAL(  -90.0D0, 90.0D0,   30.0D0, 'Enter secant angle     P_ALP' )
            P_BET3D = GETVAL(  P_ALP3D, 90.0D0,   60.0D0, 'Enter secant angle     P_BET' )
            P_GAM3D = GETVAL( -180.0D0, 180.0D0, -90.0D0, 'Enter central meridian P_GAM' )
            XCENT3D = GETVAL( -180.0D0, 180.0D0, P_GAM3D, 'Enter X coord origin   XCENT' )
            YCENT3D = GETVAL(  -90.0D0,  90.0D0,  40.0D0, 'Enter Y coord origin   YCENT' )

        ELSE IF ( GDTYP3D .EQ. UTMGRD3 ) THEN !  UTM projection

            P_ALP3D = DBLE( GETVAL( 1, 60, 17, 'Enter UTM zone' ) )
            P_BET3D = 0.0D0
            P_GAM3D = 0.0D0
            XCENT3D = GETVAL( -999999999.0D0, 999999999.0D0, 0.0D0, 'Enter UTM offset XCENT' )
            YCENT3D = GETVAL( -999999999.0D0, 999999999.0D0, 0.0D0, 'Enter UTM offset YCENT' )

        ELSE IF ( GDTYP3D .EQ. TRMGRD3 ) THEN !  Transverse Mercator projection

            P_ALP3D = GETVAL(  -90.0D0,  90.0D0,  30.0D0, 'Enter latitude of origin     P_ALP' )
            P_BET3D = GETVAL(    0.0D0,   1.0D0,   1.0D0, 'Enter scale factor at center P_BET' )
            P_GAM3D = GETVAL( -180.0D0, 180.0D0, -90.0D0, 'Enter central meridian       P_GAM' )
            XCENT3D = GETVAL( -180.0D0, 180.0D0, P_GAM3D, 'Enter X coord origin lon     XCENT' )
            YCENT3D = GETVAL(  -90.0D0,  90.0D0,  40.0D0, 'Enter Y coord origin lat     YCENT' )

        ELSE IF ( GDTYP3D .EQ. EQMGRD3 ) THEN !  Equatorial Mercator projection

            P_ALP3D = GETVAL(  -90.0D0,  90.0D0,  30.0D0, 'Enter latitude of true scale P_ALP' )
            P_BET3D = 0.0D0
            P_GAM3D = GETVAL( -180.0D0, 180.0D0, -90.0D0, 'Enter central meridian       P_GAM' )
            XCENT3D = GETVAL( -180.0D0, 180.0D0, P_GAM3D, 'Enter X coord origin lon     XCENT' )
            YCENT3D = GETVAL(  -90.0D0,  90.0D0,  40.0D0, 'Enter Y coord origin lat     YCENT' )

        ELSE IF ( GDTYP3D .EQ. MERGRD3 ) THEN !  General Mercator projection

            P_ALP3D = GETVAL(  -90.0D0,  90.0D0,  30.0D0, 'Enter origin latitude     P_ALP' )
            P_BET3D = GETVAL(  -90.0D0,  90.0D0,  30.0D0, 'Enter origin longitude    P_BET' )
            P_GAM3D = GETVAL( -180.0D0, 180.0D0, -90.0D0, 'Enter bearing from North  P_GAM' )
            XCENT3D = GETVAL( -180.0D0, 180.0D0, P_BET3D, 'Enter X coord origin lon  XCENT' )
            YCENT3D = GETVAL(  -90.0D0,  90.0D0,  40.0D0, 'Enter Y coord origin lat  YCENT' )

        ELSE IF ( GDTYP3D .EQ. POLGRD3 ) THEN !  Polar Stereographic projection

            P_ALP3D = DBLE( 2*GETVAL( 0, 1, 1, 'Enter 1 for Northern Hemisphere, 0 for Southern' ) - 1 )
            P_BET3D = GETVAL(  -90.0D0,  90.0D0,  30.0D0, 'Enter latitude of true scale P_BET' )
            P_GAM3D = GETVAL( -180.0D0, 180.0D0, -90.0D0, 'Enter central meridian       P_GAM' )
            XCENT3D = GETVAL( -180.0D0, 180.0D0, P_BET3D, 'Enter X coord origin lon     XCENT' )
            YCENT3D = GETVAL(  -90.0D0,  90.0D0,  40.0D0, 'Enter Y coord origin lat     YCENT' )

        ELSE IF ( GDTYP3D .EQ. STEGRD3 ) THEN !  General Stereographic projection

            P_ALP3D = GETVAL(  -90.0D0,  90.0D0,  30.0D0, 'Enter tangent latitude    P_ALP' )
            P_BET3D = GETVAL(  -90.0D0,  90.0D0,  30.0D0, 'Enter tangent longitude   P_BET' )
            P_GAM3D = GETVAL( -180.0D0, 180.0D0, -90.0D0, 'Enter bearing from North  P_GAM' )
            XCENT3D = GETVAL( -180.0D0, 180.0D0, P_BET3D, 'Enter X coord origin lon  XCENT' )
            YCENT3D = GETVAL(  -90.0D0,  90.0D0,  40.0D0, 'Enter Y coord origin lat  YCENT' )

        ELSE IF ( GDTYP3D .EQ. LEQGRD3 ) THEN !  Lambert Azimuthal Equal Area projection

            P_ALP3D = GETVAL(  -90.0D0,  90.0D0,  30.0D0, 'Enter latitude of origin  P_ALP' )
            P_BET3D = 0.0D0
            P_GAM3D = GETVAL( -180.0D0, 180.0D0, -90.0D0, 'Enter central meridian    P_GAM' )
            XCENT3D = GETVAL( -180.0D0, 180.0D0, P_GAM3D, 'Enter X coord origin lon  XCENT' )
            YCENT3D = GETVAL(  -90.0D0,  90.0D0,  40.0D0, 'Enter Y coord origin lat  YCENT' )

        ELSE

            CALL M3EXIT( PNAME, 0, 0, 'Only Lat-Lon, Lambert, UTM, and Albers supported', 2 )

        END IF  !  if descriptive angles relevant for this type

        NCOLS3D = GETVAL( 1, 999999999, 48, 'Enter number NCOLS of grid columns' )
        NROWS3D = GETVAL( 1, 999999999, 50, 'Enter number NROWS of grid rows' )
        NTHIK3D = GETVAL( 1, 999999999,  1, 'Enter bdy thickness NTHIK (cells)' )

        XCELL3D = GETVAL(  0.0D0,  9.0D36, 54000.0D0, 'Enter X cell size XCELL (meters)' )
        YCELL3D = GETVAL(  0.0D0,  9.0D36, XCELL3D,   'Enter Y cell size YCELL (meters)' )
        XORIG3D = GETVAL( -9.0D36, 9.0D36, 0.5D0*XCELL3D*( DBLE( NCOLS3D ) - 0.5D0 ),    &
                     'Enter SW corner X coord for (1,1)-cell' )
        YORIG3D = GETVAL( -9.0D36, 9.0D36, 0.5D0*YCELL3D*( DBLE( NROWS3D ) - 0.5D0 ),    &
                     'Enter SW corner Y coord for (1,1)-cell' )

    END IF      !  if specify horizontal grid by name, or interactively


    !!.......   Now enter vertical coordinate structure:

    NLAYS3D = 1
    VGTYP3D = VGSGPH3       ! hydrostatic sigma-P from PARMS3.EXT
    VGTOP3D = 100.0         ! model top (mb)
    VGLVS3D( 1 ) = 1.0
    VGLVS3D( 2 ) = 0.0


    !!.......   Time step structure: zeros for time-independent file

    SDATE3D = 0
    STIME3D = 0
    TSTEP3D = 0

    !!.......   Variables and their descriptions; file description

    NVARS3D = 4

    VNAME3D( 1 ) = 'LAT'
    UNITS3D( 1 ) = 'degrees lat'
    VDESC3D( 1 ) = 'ISO-Standard 6709 cell-centers latitudes as REAL'
    VTYPE3D( 1 ) = M3REAL

    VNAME3D( 2 ) = 'LON'
    UNITS3D( 2 ) = 'degrees lon'
    VDESC3D( 2 ) = 'ISO-Standard 6709 cell-centers longitudes as REAL'
    VTYPE3D( 2 ) = M3REAL

    VNAME3D( 3 ) = 'LATD'
    UNITS3D( 3 ) = 'degrees lat'
    VDESC3D( 3 ) = 'ISO-Standard 6709 cell-centers latitudes as REAL*8'
    VTYPE3D( 3 ) = M3DBLE

    VNAME3D( 4 ) = 'LOND'
    UNITS3D( 4 ) = 'degrees lon'
    VDESC3D( 4 ) = 'ISO-Standard 6709 cell-centers longitudes as REAL*8'
    VTYPE3D( 4 ) = M3DBLE

    FTYPE3D = GRDDED3		!  set file data type
    FDESC3D = ' '
    FDESC3D( 1 ) = '1-layer gridded file:  LATs and LONs'
    FDESC3D( 2 ) = 'Generated by sample program LATLON'


    !!.......   Where file names GNAME, BNAME are not "NONE":
    !!.......   Open files as "unknown" -- if they do not exist, create them;
    !!.......   else check header against description supplied in FDESC3.EXT;
    !!.......   open for output in any case.
    !!.......   Use subroutines MAKEGRD, MAKEBDY to allocate arrays for variables
    !!.......   LAT and LON, compute them, and write them to files GNAME and BNAME.


    IF ( GNAME .NE. NONE ) THEN

        IF ( .NOT. OPEN3( GNAME, FSUNKN3, PNAME ) ) THEN
            MESG = 'Could not open file "' // TRIM( GNAME ) // '" for output'
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
        END IF

        CALL MAKEGRD( GNAME )	!  see below, in this file.

    END IF				!  if gname not "none"

    IF ( BNAME .NE. NONE ) THEN	!  reuses file description

        FTYPE3D = BNDARY3		!  reset file data type, description
        FDESC3D( 1 ) = '1-layer boundary file:  LATs and LONs'

        IF ( .NOT. OPEN3( BNAME, FSUNKN3, PNAME ) ) THEN
            MESG = 'Could not open file "' // TRIM( BNAME ) // '" for output'
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
        END IF

        CALL MAKEBDY( BNAME )	!  see below, in this file.

    END IF				!  if bname not "none"


    !!.......   Clean up and exit (M3EXIT calls SHUT3() automatically)

    CALL M3EXIT( PNAME, 0, 0, 'Successful completion of program LATLON', 0 )


CONTAINS  !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-



    !!*************  subroutine MAKEGRD starts here  ***********************

    SUBROUTINE  MAKEGRD( GNAME )

        !!...........   ARGUMENTS and their descriptions:

        CHARACTER*16, INTENT( IN ) :: GNAME   !  name of output file


        !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

        REAL    LATS( NCOLS3D, NROWS3D )
        REAL    LONS( NCOLS3D, NROWS3D )
        REAL*8  LATD( NCOLS3D, NROWS3D )
        REAL*8  LOND( NCOLS3D, NROWS3D )

        INTEGER         R, C            !  row, column counters
        INTEGER         ZONE            !  UTM zone
        REAL            X0, Y0, X, Y    !  scratch variables
        CHARACTER*80    MESG


        !!***********************************************************************
        !!   begin body of subroutine  MAKEGRD

        CALL GRID2XY( LATGRD3, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0,           &
                   GDTYP3D, P_ALP3D, P_BET3D, P_GAM3D, XCENT3D, YCENT3D,    &
                   NCOLS3D, NROWS3D, XORIG3D, YORIG3D, XCELL3D, YCELL3D,    &
                   LOND, LATD )

        DO  R = 1, NROWS3D
        DO  C = 1, NCOLS3D
            LONS( C, R ) = SNGL( LOND( C,R ) )
            LATS( C, R ) = SNGL( LATD( C,R ) )
        END DO
        END DO


        !!.......   Write out results to file GNAME, then return:

        IF ( .NOT. WRITE3( GNAME, 'LAT', 0, 0, LATS ) ) THEN
            MESG = 'Error writing "LAT" to file "' // TRIM( GNAME ) // '"'
            CALL M3EXIT( 'LATLON/MAKEGRD', 0, 0, MESG, 2 )
        END IF

        IF ( .NOT. WRITE3( GNAME, 'LON', 0, 0, LONS ) ) THEN
            MESG = 'Error writing "LON" to file "' // TRIM( GNAME ) // '"'
            CALL M3EXIT( 'LATLON/MAKEGRD', 0, 0, MESG, 2 )
        END IF

        IF ( .NOT. WRITE3( GNAME, 'LATD', 0, 0, LATD ) ) THEN
            MESG = 'Error writing "LATD" to file "' // TRIM( GNAME ) // '"'
            CALL M3EXIT( 'LATLON/MAKEGRD', 0, 0, MESG, 2 )
        END IF

        IF ( .NOT. WRITE3( GNAME, 'LOND', 0, 0, LOND ) ) THEN
            MESG = 'Error writing "LOND" to file "' // TRIM( GNAME ) // '"'
            CALL M3EXIT( 'LATLON/MAKEGRD', 0, 0, MESG, 2 )
        END IF

        RETURN

        END SUBROUTINE  MAKEGRD



    !!*************  subroutine MAKEBDY starts here  ***********************
    !!
    !!  This also serves as an example to show how to traverse the
    !!  standard storage order for I/O API BNDARY3 data structures.
    !!***********************************************************************

    SUBROUTINE  MAKEBDY( BNAME )

        IMPLICIT NONE


        !!...........   ARGUMENTS and their descriptions:

        CHARACTER*16, INTENT( IN ) :: BNAME   !  name of output file


        !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

        REAL    LATB( 2*NTHIK3D*( NCOLS3D + NROWS3D + 2*NTHIK3D ) )
        REAL    LONB( 2*NTHIK3D*( NCOLS3D + NROWS3D + 2*NTHIK3D ) )

        REAL*8  XBDY( 2*NTHIK3D*( NCOLS3D + NROWS3D + 2*NTHIK3D ) )
        REAL*8  YBDY( 2*NTHIK3D*( NCOLS3D + NROWS3D + 2*NTHIK3D ) )
        REAL*8  LATD( 2*NTHIK3D*( NCOLS3D + NROWS3D + 2*NTHIK3D ) )
        REAL*8  LOND( 2*NTHIK3D*( NCOLS3D + NROWS3D + 2*NTHIK3D ) )

        INTEGER         R, C, K         !  row, column, bdy-cell counters
        INTEGER         NPERM
        REAL            X0, Y0, X, Y    !  scratch variables
        CHARACTER*80    MESG


        !!***********************************************************************

        X0 = XORIG3D - 0.5D0 * XCELL3D  !  to get to cell-centers
        Y0 = YORIG3D - 0.5D0 * YCELL3D  !             "     "
        K  = 0

        DO  R = 1 - NTHIK3D, 0		!  south boundary component
        DO  C = 1 , NCOLS3D + NTHIK3D
            K = K + 1
            XBDY( K ) = X0 + DBLE( C ) * XCELL3D
            YBDY( K ) = Y0 + DBLE( R ) * YCELL3D
        END DO
        END DO

        DO  R = 1, NROWS3D + NTHIK3D 	!  east boundary component
        DO  C = NCOLS3D + 1, NCOLS3D + NTHIK3D
            K = K + 1
            XBDY( K ) = X0 + DBLE( C ) * XCELL3D
            XBDY( K ) = Y0 + DBLE( R ) * YCELL3D
        END DO
        END DO

        DO  R = NROWS3D + 1, NROWS3D + NTHIK3D	!  north bdy component
        DO  C = 1 - NTHIK3D, NCOLS3D
            K = K + 1
            XBDY( K ) = X0 + DBLE( C ) * XCELL3D
            XBDY( K ) = Y0 + DBLE( R ) * YCELL3D
        END DO
        END DO

        DO  R = 1 - NTHIK3D, NROWS3D
        DO  C = 1 - NTHIK3D, 0                  !  west bdy component
            K = K + 1
            XBDY( K ) = X0 + DBLE( C ) * XCELL3D
            XBDY( K ) = Y0 + DBLE( R ) * YCELL3D
        END DO
        END DO

        NPERM = 2 * NTHIK3D * ( NCOLS3D + NROWS3D + 2 * NTHIK3D )
        CALL XY2XY( GDTYP3D,P_ALP3D,P_BET3D,P_GAM3D,XCENT3D,YCENT3D,        &
                    LATGRD3,0.0D0,  0.0D0,  0.0D00, 0.0D00, 0.0D00,         &
                    NPERM, XBDY, YBDY, LOND, LATD )

        DO C = 1, 2 * NTHIK3D * ( NCOLS3D + NROWS3D + 2 * NTHIK3D )
            LONB( C ) = SNGL( LOND( C ) )
            LATB( C ) = SNGL( LATD( C ) )
        END DO


        !!.......   Write out results to file BNAME, then return:

        IF ( .NOT. WRITE3( BNAME, 'LAT', 0, 0, LATB ) ) THEN
            MESG = 'Error writing "LAT" to file "' // TRIM( BNAME ) // '"'
            CALL M3EXIT( 'LATLON/MAKEBDY', 0, 0, MESG, 2 )
        END IF

        IF ( .NOT. WRITE3( BNAME, 'LON', 0, 0, LONB ) ) THEN
            MESG = 'Error writing "LON" to file "' // TRIM( BNAME ) // '"'
            CALL M3EXIT( 'LATLON/MAKEBDY', 0, 0, MESG, 2 )
        END IF

        IF ( .NOT. WRITE3( BNAME, 'LATD', 0, 0, LATD ) ) THEN
            MESG = 'Error writing "LATD" to file "' // TRIM( BNAME ) // '"'
            CALL M3EXIT( 'LATLON/MAKEBDY', 0, 0, MESG, 2 )
        END IF

        IF ( .NOT. WRITE3( BNAME, 'LOND', 0, 0, LOND ) ) THEN
            MESG = 'Error writing "LOND" to file "' // TRIM( BNAME ) // '"'
            CALL M3EXIT( 'LATLON/MAKEBDY', 0, 0, MESG, 2 )
        END IF

        RETURN

    END SUBROUTINE  MAKEBDY


END PROGRAM LATLON
