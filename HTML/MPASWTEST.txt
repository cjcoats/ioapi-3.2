
PROGRAM MPASWTEST

    !!***********************************************************************
    !!  Copyright (c) 2017 Carlie J. Coats, Jr.
    !!  Distributed under the GNU GENERAL PUBLIC LICENSE version 2
    !!  See file "GPL.txt" for conditions of use.
    !!.......................................................................
    !!
    !!  DESCRIPTION:
    !!      Compute SMOKE style grid-allocation weights for an aircraft-flight
    !!      segment.
    !!
    !!  REVISION  HISTORY:
    !!      Prototype  05/2017 by Carlie J. Coats, Jr., UNC IE
    !!
    !!.......................................................................

    USE MODMPASFIO
    USE MODNCFIO
    USE M3UTILIO

    IMPLICIT NONE

    !!...........   PARAMETERS and their descriptions:

    INTEGER     , PARAMETER :: NDIMS = 2            !!  for OUTFILE
    INTEGER     , PARAMETER :: NVARS = 2
    CHARACTER*1,  PARAMETER :: BLANK = ' '
    CHARACTER*80, PARAMETER :: BAR   = '-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-'
    CHARACTER*16, PARAMETER :: PNAME = 'MPASWTEST'

    !!...........   LOCAL VARIABLES and their descriptions:

    CHARACTER*1024   MESG

    INTEGER     ISTAT
    INTEGER     MPSEGS, MXSEGS, MPVARS, MPRECS
    INTEGER     I, K, L, V
    INTEGER     NLAYSP1, NCELLS

    REAL        ALAT, ALON, AHGT, ZLAT, ZLON, ZHGT, DIST

    CHARACTER*32    VNAMES( MXVARS3 )         !!  data structures for DESCNCVAR()
    INTEGER         VTYPES( MXVARS3 )
    INTEGER         VNDIMS( MXVARS3 )
    INTEGER          VDIMS( 7,MXVARS3 )
    CHARACTER*32     DNAME( 7,MXVARS3 )

    INTEGER, ALLOCATABLE ::   CELLS( : )    !!  (NCELLS) data structures for ARC2MPAS()
    REAL   , ALLOCATABLE ::   ZGRID( :,: )  !!  i
    REAL   , ALLOCATABLE ::   WGHTS( :,: )
    REAL   , ALLOCATABLE ::   ZBOTS( : )
    REAL   , ALLOCATABLE ::   ZTOPS( : )
    REAL   , ALLOCATABLE ::   WSUMS( : )
    REAL   , ALLOCATABLE :: WGRID2D( : )
    REAL   , ALLOCATABLE :: WGRID3D( :,: )

    CHARACTER*64            :: DIMNAMES( NDIMS )
    INTEGER                 :: DIMSIZES( NDIMS )

    CHARACTER*16            :: MPVNAMES( NVARS )
    CHARACTER*16            :: MPVUNITS( NVARS )
    INTEGER                 :: MPVTYPES( NVARS )
    INTEGER                 :: MPVNDIMS( NVARS )
    CHARACTER*16            :: MPVDNAME( NDIMS,NVARS )


    !!***********************************************************************
    !!.......   First:  Initialize the I/O API:

    L = INIT3()	!  initialization returns unit # for log

    WRITE( *, '( 5X, A )' ) BAR,                                            &
'Program MPASWTEST to read an MPAS file header and vertical grid',          &
'variable "zgrid", take input points <ALAT,ALON,AHGT> and ',                &
'<ZLAT,ZLON,ZHGT>, and construct the 3-D set of weights for weighting',     &
'emissions along the indicated segment to the MPAS grid.',                  &
'',                                                                         &
'PRECONDITIONS REQUIRED:',                                                  &
'    setenv  INFILE    <path name for  input MPAS-gridded file>',           &
'    setenv  OUTFILE   <path name for output MPAS-gridded file>',           &
'',                                                                         &
'    AHGT, ZHGT are in meters above mean sea level.',                       &
'    LAT and LON values are in degrees.',                                   &
'',                                                                         &
'Program copyright (C) 2017 Carlie J. Coats, Jr..',                         &
'Released under Version 2 of the GNU General Public License.',              &
'See enclosed GPL.txt, or URL',                                             &
''  ,                                                                       &
'    https://www.gnu.org/licenses/old-licenses/gpl-2.0.html',               &
'',                                                                         &
'',                                                                         &
'Now enter endpoints <ALAT,ALON,AHGT>, <ZLAT,ZLON,ZHGT>:',                  &
''

    ALAT = GETREAL(  -90.0,  90.0,  38.90, 'Enter LATITUDE  for first point' )  !!  default:  Washington, DC
    ALON = GETREAL( -180.0, 180.0, -77.03, 'Enter LONGITUDE for first point' )
    AHGT = GETREAL(  0.0, 30000.0, 1000.0, 'Enter ELEVATION for first point' )
    CALL M3MESG( BLANK )
    ZLAT = GETREAL(  -90.0,  90.0,  38.30, 'Enter LATITUDE  for second point' ) !!  default:  FGredricksburg, VA
    ZLON = GETREAL( -180.0, 180.0, -77.50, 'Enter LONGITUDE for second point' )
    ZHGT = GETREAL(  0.0, 30000.0, 2000.0, 'Enter ELEVATION for second point' )

    IF ( .NOT.INITMPGRID( 'INFILE' ) ) THEN

        CALL M3EXIT( PNAME, 0,0, 'Could not read header for "INFILE"', 2 )

    ELSE IF ( .NOT.DESCMPAS( 'INFILE', MPRECS, MPVARS, VNAMES,        &
                             VTYPES, VNDIMS, VDIMS, DNAME ) ) THEN

        CALL M3EXIT( PNAME, 0,0, 'Could not read header for "INFILE"', 2 )

    ELSE

        I = FINDCELL( ALAT, ALON )
        K = FINDCELL( ZLAT, ZLON )
        WRITE( MESG, '( 2(A, 1PE16.9), A, I10 )' ) '<', ALAT, ',', ALON, '> is in cell', I
        CALL M3MESG( MESG )
        WRITE( MESG, '( 2(A, 1PE16.9), A, I10 )' ) '<', ZLAT, ',', ZLON, '> is in cell', K
        CALL M3MESG( MESG )

        I = FINDVRTX( ALAT, ALON )
        K = FINDVRTX( ZLAT, ZLON )
        WRITE( MESG, '( 2(A, 1PE16.9), A, I10 )' ) '<', ALAT, ',', ALON, '>: closest vertex', I
        CALL M3MESG( MESG )
        WRITE( MESG, '( 2(A, 1PE16.9), A, I10 )' ) '<', ZLAT, ',', ZLON, '>: closest vertex', K
        CALL M3MESG( MESG )
        
        DIST = SPHEREDIST( ALAT, ALON, ZLAT, ZLON )
        WRITE( MESG, '( A, 1PE13.6, 1X, A )' ) 'Horizontal distance between points:', DIST, 'meters'
        CALL M3MESG( MESG )

    END IF
    

    !!...............   Allocate arrays:

    MXSEGS  = CEILING( SQRT( 2.0 * FLOAT( MPCELLS ) + 3.0 ) )     !!  estimate
    NLAYSP1 = MPVLVLS+1
    NCELLS  = MPCELLS

    ALLOCATE(   CELLS( MPCELLS ),           &
        ZGRID( NLAYSP1,MPCELLS ),           &
        WGHTS( MPVLVLS,MXSEGS  ),           &
                ZBOTS( MXSEGS  ),           &
                ZTOPS( MXSEGS  ),           &
                WSUMS( MXSEGS  ),           &
        WGRID2D(       MPCELLS ),           &
      WGRID3D( MPVLVLS,MPCELLS ), STAT = ISTAT )

    IF ( ISTAT .NE. 0 ) THEN
        WRITE( MESG, '( A, I10 )' ) 'Memory allocation failed:  STAT=', ISTAT
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
    END IF


    !!...............   Read ZGRID

    IF ( .NOT.READMPAS( 'INFILE', 'zgrid', NLAYSP1, NCELLS, ZGRID ) ) THEN
        CALL M3EXIT( PNAME, 0,0, 'Could not read "zgrid" from "INFILE"', 2 )
    END IF


    !!...............   Compute cells and weights:  3-D-verbose version of ARC2MPAS()

    IF ( .NOT.ARC2MPAS( ALAT, ALON, AHGT, ZLAT, ZLON, ZHGT,     &
                        MPVLVLS, MXSEGS, ZGRID,                 &
                        MPSEGS, CELLS, WGHTS, ZBOTS, ZTOPS, WSUMS ) ) THEN
        CALL M3EXIT( PNAME, 0,0, 'ARC2MPAS() failure', 2 )
    END IF


    !!...............   Copy  results to full grids:

    WGRID2D = 0.0
    WGRID3D = 0.0
    DO I = 1, MPSEGS

        K = CELLS( I )

        WGRID2D( K ) = WSUMS( I )

        DO L = 1, MPVLVLS
            WGRID3D( L,K ) = WGHTS( L,I )
        END DO

    END DO


    !!...............   Create and write output file (note NDIMS=2, NVARS=2)

    DIMNAMES( 1 )  = 'nCells'
    DIMSIZES( 1 )  = MPCELLS
    DIMNAMES( 2 )  = 'nVertLevels'
    DIMSIZES( 2 )  = MPVLVLS

    MPVDNAME        = BLANK
    MPVNAMES( 1 )   = 'WEIGHT2D'
    MPVUNITS( 1 )   = 'none'
    MPVTYPES( 1 )   = M3REAL
    MPVNDIMS( 1 )   = 1
    MPVDNAME( 1,1 ) = 'nCells'

    MPVNAMES( 2 )   = 'WEIGHT3D'
    MPVUNITS( 2 )   = 'none'
    MPVTYPES( 2 )   = M3REAL
    MPVNDIMS( 2 )   = 2
    MPVDNAME( 2,1 ) = 'nVertLevels'
    MPVDNAME( 2,2 ) = 'nCells'

    IF ( .NOT.CREATEMPAS( 'OUTFILE', NVARS, MPVNAMES, MPVTYPES, MPVNDIMS, MPVDNAME, MPVUNITS ) ) THEN
        CALL M3EXIT( PNAME, 0,0, 'OPENMPAS( OUTFILE, ...) failure', 2 )
    END IF

    IF ( .NOT.WRITEMPAS( 'OUTFILE', 'WEIGHT2D', MPCELLS, WGRID2D ) ) THEN
        CALL M3EXIT( PNAME, 0,0, 'WRITEMPAS( OUTFILE, WGRID2D, ...) failure', 2 )
    END IF

    IF ( .NOT.WRITEMPAS( 'OUTFILE', 'WEIGHT3D', MPVLVLS, MPCELLS, WGRID3D ) ) THEN
        CALL M3EXIT( PNAME, 0,0, 'WRITEMPAS( OUTFILE, WGRID3D, ...) failure', 2 )
    END IF

    !!.......   Clean up and exit

    CALL SHUTMPGRID()
    CALL M3EXIT( PNAME, 0, 0, 'Successful completion of program', 0 )


END PROGRAM MPASWTEST

