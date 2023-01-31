
PROGRAM WRFWNDW

    !!***************************************************************
    !! Version "$Id: wrfwndw.f90 236 2023-01-14 19:40:25Z coats $"
    !! EDSS/Models-3 M3TOOLS.
    !! (C) 2023 UNC Institute for the Environment.
    !! Distributed under the GNU GENERAL PUBLIC LICENSE version 2
    !! See file "GPL.txt" for conditions of use.
    !!..............................................................
    !!  Program body starts at line  93
    !!
    !!  DESCRIPTION:
    !!      Window a WRFOUT-format netCDF file to a specified XY subgrid.
    !!
    !!  PRECONDITIONS:
    !!      see below
    !!
    !!  REVISION  HISTORY:
    !!      Prototype  1/2023 by Carlie J. Coats, Jr., UNC IE
    !!***************************************************************

    USE M3UTILIO
    USE MODWRFIO
    USE MODNCFIO
    USE MODGCTP

    IMPLICIT NONE

    !!......  PARAMETERS and their descriptions:

    CHARACTER*16, PARAMETER ::  TYPES( 0:7 ) =  &
                    (/  'invalid        ',      &
                        'byte           ',      &
                        'character      ',      &
                        'int16          ',      &
                        'integer        ',      &
                        'real           ',      &
                        'double         ',      &
                        'invalid        '       /)
    CHARACTER*16, PARAMETER :: PNAME = 'WRFWNDW'
    CHARACTER*16, PARAMETER :: COLON = ':'
    CHARACTER*16, PARAMETER :: COMMA = ','
    CHARACTER*16, PARAMETER :: BLANK = ' '
    CHARACTER*64, PARAMETER :: BAR   = '-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-'


    !!......  LOCAL VARIABLES and their descriptions:

    INTEGER         LDEV, IDEV, ISTAT, IERR
    INTEGER         JDATE, JTIME, JSTEP, IREC, IVAR
    LOGICAL ::      EFLAG = .FALSE.
    
    CHARACTER*256   MESG

    !!......  Data structures for output cross-point (unstaggered) grid

    CHARACTER*16    CNAME
    CHARACTER*16    GNAMEX
    INTEGER         NCOLSX      ! number of grid columns
    INTEGER         NROWSX      ! number of grid rows
    INTEGER         NTHIKX      ! bdy thickness
    INTEGER         GDTYPX      ! grid type:  X=LAT-LON, 2=UTM, ...
    INTEGER         FTYPEX      ! file type:  gridded, custom, boundary...
    REAL*8          P_ALPX      ! first, second, third map
    REAL*8          P_BETX      ! projection descriptive
    REAL*8          P_GAMX      ! parameters.
    REAL*8          XCENTX      ! lon for coord-system X=0
    REAL*8          YCENTX      ! lat for coord-system Y=0
    REAL*8          XORIGX      ! X-coordinate origin of grid (map units)
    REAL*8          YORIGX      ! Y-coordinate origin of grid
    REAL*8          XFINLX      ! X-coordinate UR corner of grid (map units)
    REAL*8          YFINLX      ! Y-coordinate UR corner of grid
    REAL*8          XCELLX      ! X-coordinate cell dimension
    REAL*8          YCELLX      ! Y-coordinate cell dimension


    !!......  Data structures for newly-created WRFOUT file

    INTEGER         CDFIDWO
    INTEGER         NDIMSWO
    INTEGER         NRECSWO
    INTEGER         WEDIDX, WEDIDS, SNDIDX, SNDIDS, TIMDID
    INTEGER         DSIZEWO( MXWRFDIMS )
    CHARACTER*32    DNAMEWO( MXWRFDIMS )
    INTEGER         VDIMSWO( MXWRFDIMS, MXWRFVARS )
    CHARACTER*32 :: MORDRWO( MXWRFVARS ) = BLANK


    !!--------------------------------------------------------------
    !!   begin body of program WRFWNDW

    LDEV  = INIT3()

    WRITE( LDEV, '( 5X, A )' )                                                  &
'Program WRFWNDW to get a specified cross-point window description from',       &
'GRIDDESC, then to read and window a set of variables from WRF netCDF',         &
'Program WRFWNDW to read and window a set of variables from a WRFOUT',          &
'netCDF file, and write it out to a new WRFOUT-format netCDF file.',            &
'',                                                                             &
'PRECONDITIONS REQUIRED:',                                                      &
'',                                                                             &
'   setenv GRIDDESC   <path name>',                                             &
'   setenv OUTGRID    <GRIDDESC-name for the un-staggered output grid>',        &
'   setenv WRFIFILE   <path name for  input WRF-netCDF format WRFOUT file>',    &
'   setenv WRFOFILE   <path name for output windowed ...',                      &
'',                                                                             &
'   The output grid must have the same map projection, cell-size, and',         &
'   grid-alignment as the unstaggered grid from the WRF input file.',           &
'',                                                                             &
'Program copyright (C) 2023 UNC Institute for the Environment.',                &
'Released under Version 2 of the GNU General Public License.',                  &
'See enclosed GPL.txt, or URL',                                                 &
''  ,                                                                           &
'    https://www.gnu.org/licenses/old-licenses/gpl-2.0.html',                   &
''  ,                                                                           &
'Comments and questions are welcome and can be sent to'  ,                      &
'',                                                                             &
'    Carlie J. Coats, Jr.    carlie@jyarborough.com',                           &
'or',                                                                           &
'    UNC Institute for the Environment',                                        &
'    100 Europa Dr., Suite 490 Rm 405',                                         &
'    Campus Box 1105',                                                          &
'    Chapel Hill, NC 27599-1105',                                               &
'',                                                                             &
'Program version: ',                                                            &
'$Id: wrfwndw.f90 236 2023-01-14 19:40:25Z coats $',&
' '

    IF ( .NOT.GETYN( 'Continue with program?', .TRUE. ) ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'Program aborted', 2 )
    END IF


    !!...............  Get output grid description
    !!...............  Open and get descriptions for input "points", "infile":

    CALL ENVSTR( 'OUTGRID', 'GRIDDESC name for output un-staggered grid', 'WRF_FOO_CRO', GNAMEX, ISTAT )
    IF ( ISTAT .GT. 0 ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'Bad environment variable "OUTGRID"', 2 )
    END IF

    IF ( .NOT.DSCGRID( GNAMEX, CNAME,                               &
                  GDTYPX, P_ALPX, P_BETX, P_GAMX,  XCENTX, YCENTX,  &
                  XORIGX, YORIGX, XCELLX, YCELLX, NCOLSX, NROWSX, NTHIKX ) ) THEN

        MESG = 'Could not get grid description for "'// TRIM( GNAMEX ) //'" in GRIDDESC file'
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )

    ELSE

        CALL M3MESG( BAR )
        CALL M3MESG( 'Output-window cross-point (un-staggered) grid parameters' )
        WRITE( MESG, '( A, I10 )' ) 'NCOLS=', NCOLSX
        CALL M3MESG( MESG )
        WRITE( MESG, '( A, I10 )' ) 'NROWS=', NROWSX
        CALL M3MESG( MESG )
        WRITE( MESG, '( A, I10 )' ) 'GDTYP=', GDTYPX
        CALL M3MESG( MESG )
        WRITE( MESG, '( A, 1PD24.16 )' ) 'P_ALP', P_ALPX
        CALL M3MESG( MESG )
        WRITE( MESG, '( A, 1PD24.16 )' ) 'P_BET', P_BETX
        CALL M3MESG( MESG )
        WRITE( MESG, '( A, 1PD24.16 )' ) 'P_GAM', P_GAMX
        CALL M3MESG( MESG )
        WRITE( MESG, '( A, 1PD24.16 )' ) 'XCENT', XCENTX
        CALL M3MESG( MESG )
        WRITE( MESG, '( A, 1PD24.16 )' ) 'YCENT', YCENTX
        CALL M3MESG( MESG )
        WRITE( MESG, '( A, 1PD24.16 )' ) 'XORIG', XORIGX
        CALL M3MESG( MESG )
        WRITE( MESG, '( A, 1PD24.16 )' ) 'YORIG', YORIGX
        CALL M3MESG( MESG )
        WRITE( MESG, '( A, 1PD24.16 )' ) 'XCELL', XCELLX
        CALL M3MESG( MESG )
        WRITE( MESG, '( A, 1PD24.16 )' ) 'YCELL', YCELLX
        CALL M3MESG( MESG )
        CALL M3MESG( BAR )

    END IF


    !!...............  Open WRF input file and read its description into
    !!...............  MODWRFIO variables:

    IF ( .NOT.OPENWRF( 'WRFIFILE', GNAMEX, FSREAD3 ) ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'Error(s) opening/processing input WRF file  "WRFIFILE"', 2 )
    ELSE IF ( .NOT.SAMEPROJ( GDTYP1, P_ALP1, P_BET1, P_GAM1, XCENT1, YCENT1, XCELL1, YCELL1, XORIG1, YORIG1 ) ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'Inconsistent grid/map parameters for "WRFIFILE", "OUTGRID"', 2 )
    ELSE
        JDATE  = SDATEW
        JTIME  = STIMEW
        JSTEP  = TSTEPW
    END IF


    !!...............  Open/create WRF output file:

    IF ( .NOT.CRTWRFOUT( 'WRFOFILE', GNAMEX, FSNEW3 ) ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'Error(s) creating/opening WRF output file  "WRFOFILE"', 2 )
    END IF


    !!............... Process time step sequence
    !!............... NOTES (1) Every variable is subscripted at least by timestep-number IREC;
    !!............... (2) at most 4 dimensions.
    !!............... (3) "Times" is the only non-REAL | INT | DBLE variable

    DO IREC = 1, NRECSWO

        DO IVAR = 1, NVARSW

            IF ( VNAMEW( IVAR ) .EQ. 'Times' ) THEN

                IF ( .NOT.COPYTIME( IVAR, IREC ) ) THEN
                    EFLAG = .TRUE.
                END IF

            ELSE IF ( MORDRWO( IVAR ) .EQ. 'XY' ) THEN      !!  must be 3D, subscripted X-Y-T

                IF ( VTYPEW( IVAR ) .EQ. M3INT ) THEN

                    IF ( .NOT.WNDWXYI( IVAR, IREC ) ) THEN
                        EFLAG = .TRUE.
                    END IF

                ELSE IF ( VTYPEW( IVAR ) .EQ. M3REAL ) THEN

                    IF ( .NOT.WNDWXYR( IVAR, IREC ) ) THEN
                        EFLAG = .TRUE.
                    END IF

                ELSE IF ( VTYPEW( IVAR ) .EQ. M3DBLE ) THEN

                    IF ( .NOT.WNDWXYD( IVAR, IREC ) ) THEN
                        EFLAG = .TRUE.
                    END IF

                ELSE

                    WRITE( MESG, '(A, I9 )' ) 'Unsupported type', VTYPEW(IVAR), ' for ', VNAMEW( IVAR )
                    EFLAG = .TRUE.

                END IF

            ELSE IF ( MORDRWO( IVAR ) .EQ. 'XYZ' ) THEN      !!  must be 4D, subscripted X-Y-Z-T

                IF ( VTYPEW( IVAR ) .EQ. M3INT ) THEN

                    IF ( .NOT.WNDWXYZI( IVAR, IREC ) ) THEN
                        EFLAG = .TRUE.
                    END IF

                ELSE IF ( VTYPEW( IVAR ) .EQ. M3REAL ) THEN

                    IF ( .NOT.WNDWXYZR( IVAR, IREC ) ) THEN
                        EFLAG = .TRUE.
                    END IF

                ELSE IF ( VTYPEW( IVAR ) .EQ. M3DBLE ) THEN

                    IF ( .NOT.WNDWXYZD( IVAR, IREC ) ) THEN
                        EFLAG = .TRUE.
                    END IF

                ELSE

                    WRITE( MESG, '(A, I9 )' ) 'Unsupported type', VTYPEW(IVAR), ' for ', VNAMEW( IVAR )
                    EFLAG = .TRUE.

                END IF

            ELSE

                IF ( VTYPEW( IVAR ) .EQ. M3INT ) THEN

                    IF ( DIMCNT( IVAR ) .EQ. 1 ) THEN
                        IF ( .NOT.COPYVAR1I( IVAR, IREC ) ) THEN
                            EFLAG = .TRUE.
                        END IF
                    ELSE IF ( DIMCNT( IVAR ) .EQ. 2 ) THEN
                        IF ( .NOT.COPYVAR2I( IVAR, IREC ) ) THEN
                            EFLAG = .TRUE.
                        END IF
                    ELSE IF ( DIMCNT( IVAR ) .EQ. 3 ) THEN
                        IF ( .NOT.COPYVAR3I( IVAR, IREC ) ) THEN
                            EFLAG = .TRUE.
                        END IF
                    ELSE IF ( DIMCNT( IVAR ) .EQ. 4 ) THEN
                        IF ( .NOT.COPYVAR4I( IVAR, IREC ) ) THEN
                            EFLAG = .TRUE.
                        END IF
                    END IF

                ELSE IF ( VTYPEW( IVAR ) .EQ. M3REAL ) THEN

                    IF ( DIMCNT( IVAR ) .EQ. 1 ) THEN
                        IF ( .NOT.COPYVAR1R( IVAR, IREC ) ) THEN
                            EFLAG = .TRUE.
                        END IF
                    ELSE IF ( DIMCNT( IVAR ) .EQ. 2 ) THEN
                        IF ( .NOT.COPYVAR2R( IVAR, IREC ) ) THEN
                            EFLAG = .TRUE.
                        END IF
                    ELSE IF ( DIMCNT( IVAR ) .EQ. 3 ) THEN
                        IF ( .NOT.COPYVAR3R( IVAR, IREC ) ) THEN
                            EFLAG = .TRUE.
                        END IF
                    ELSE IF ( DIMCNT( IVAR ) .EQ. 4 ) THEN
                        IF ( .NOT.COPYVAR4R( IVAR, IREC ) ) THEN
                            EFLAG = .TRUE.
                        END IF
                    END IF

                ELSE IF ( VTYPEW( IVAR ) .EQ. M3DBLE ) THEN

                    IF ( DIMCNT( IVAR ) .EQ. 1 ) THEN
                        IF ( .NOT.COPYVAR1D( IVAR, IREC ) ) THEN
                            EFLAG = .TRUE.
                        END IF
                    ELSE IF ( DIMCNT( IVAR ) .EQ. 2 ) THEN
                        IF ( .NOT.COPYVAR2D( IVAR, IREC ) ) THEN
                            EFLAG = .TRUE.
                        END IF
                    ELSE IF ( DIMCNT( IVAR ) .EQ. 3 ) THEN
                        IF ( .NOT.COPYVAR3D( IVAR, IREC ) ) THEN
                            EFLAG = .TRUE.
                        END IF
                    ELSE IF ( DIMCNT( IVAR ) .EQ. 4 ) THEN
                        IF ( .NOT.COPYVAR4D( IVAR, IREC ) ) THEN
                            EFLAG = .TRUE.
                        END IF
                    END IF

                ELSE

                    WRITE( MESG, '(A, I9 )' ) 'Unsupported type', VTYPEW(IVAR), ' for ', VNAMEW( IVAR )
                    EFLAG = .TRUE.

                END IF

            END IF      !!  if memory-order "XY, or "XYZ, or...

        END DO

        CALL NEXTIME( JDATE, JTIME, JSTEP )

    END DO


    !!...............  Completion;

    IERR =  NF_CLOSE( CDFIDW )
    IF ( IERR .NE. 0 ) THEN
        MESG  = NF_STRERROR( IERR )
        CALL M3MESG( MESG )
        WRITE( MESG, '( A, I10 )' ) 'NF_CLOSE( "WRFIFILE" ) failure:  IERR=', IERR
        CALL M3MESG( MESG )
        EFLAG = .TRUE.
    END IF

    IERR =  NF_CLOSE( CDFIDWO )
    IF ( IERR .NE. 0 ) THEN
        MESG  = NF_STRERROR( IERR )
        CALL M3MESG( MESG )
        WRITE( MESG, '( A, I10 )' ) 'NF_CLOSE( "WRFOFILE" ) failure:  IERR=', IERR
        CALL M3MESG( MESG )
        EFLAG = .TRUE.
    END IF

    IF ( EFLAG ) THEN
        MESG  = 'Failure in program'
        ISTAT = 2
    ELSE
        MESG  = 'Success in program'
        ISTAT = 0
    END IF

    CALL M3EXIT( PNAME, 0, 0, MESG, ISTAT )


CONTAINS    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
    !!  Check for map-p;rojection, grid-alignment consistency
    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

    LOGICAL FUNCTION SAMEPROJ( GDTYP1, P_ALP1, P_BET1, P_GAM1, XCENT1, YCENT1, XCELL1, YCELL1, XORIG1, YORIG1 )

        INTEGER, INTENT( IN ) :: GDTYP1
        REAL*8 , INTENT( IN ) :: P_ALP1, P_BET1, P_GAM1, XCENT1, YCENT1, XCELL1, YCELL1, XORIG1, YORIG1

        LOGICAL EFLAG
        REAL*8  T

        EFLAG = .FALSE.

        IF ( GDTYP1 .NE. GDTYP1 ) THEN
            EFLAG = .TRUE.
            CALL M3MESG( 'Inconsistent map-projection type' )
        END IF

        IF ( DBLERR( P_ALP1, P_ALPX ) ) THEN
            EFLAG = .TRUE.
            CALL M3MESG( 'Inconsistent map-projection parameter P_ALP' )
        END IF

        IF ( DBLERR( P_BET1, P_BETX ) ) THEN
            EFLAG = .TRUE.
            CALL M3MESG( 'Inconsistent map-projection parameter P_BET' )
        END IF

        IF ( DBLERR( P_GAM1, P_GAMX ) ) THEN
            EFLAG = .TRUE.
            CALL M3MESG( 'Inconsistent map-projection parameter P_GAM' )
        END IF

        IF ( DBLERR( XCENT1, XCENTX ) ) THEN
            EFLAG = .TRUE.
            CALL M3MESG( 'Inconsistent map-projection parameter XCENT' )
        END IF

        IF ( DBLERR( YCENT1, YCENTX ) ) THEN
            EFLAG = .TRUE.
            CALL M3MESG( 'Inconsistent map-projection parameter YCENT' )
        END IF

        IF ( DBLERR( XCELL1, XCELLX ) ) THEN
            EFLAG = .TRUE.
            CALL M3MESG( 'Inconsistent map-projection parameter XCELL' )
        END IF

        IF ( DBLERR( YCELL1, YCELLX ) ) THEN
            EFLAG = .TRUE.
            CALL M3MESG( 'Inconsistent map-projection parameter YCELL' )
        END IF

        T = ( XORIGX - XORIG1 ) / XCELL1
        IF ( DBLERR( T, DBLE( INT( T ) ) ) ) THEN
            EFLAG = .TRUE.
            CALL M3MESG( 'Inconsistent grid-alignment in X' )
        END IF

        T = ( YORIGX - YORIG1 ) / YCELL1
        IF ( DBLERR( T, DBLE( INT( T ) ) ) ) THEN
            EFLAG = .TRUE.
            CALL M3MESG( 'Inconsistent grid-alignment in Y' )
        END IF

        SAMEPROJ = ( .NOT.EFLAG )

        RETURN

    END  FUNCTION SAMEPROJ


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
    !!  Open new or unknown WRF-output file with logical name FNAME for read/write
    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

    LOGICAL FUNCTION CRTWRFOUT( FNAME, GNAME, FSTATUS )

        !!-----------   Arguments:

        CHARACTER(LEN=*), INTENT(IN   ) :: FNAME        !!  logical file name
        CHARACTER(LEN=*), INTENT(IN   ) :: GNAME        !!  GRIDDESC name for output grid
        INTEGER         , INTENT(IN   ) :: FSTATUS      !!  FSUNKN3, FSCREA3, FSNEW3

        REAL*8      , PARAMETER :: ARW_SPHERE = 20.0d0
        CHARACTER*24, PARAMETER :: PNAME      = 'CRTWRFOUT'

        !!-----------   Local Variables:

        INTEGER         I, J, L, M, N, V
        INTEGER         IERR, FID, VID, FSTAT
        INTEGER         NDIMS, NVARSW, NATTS, UDIM, VDIMS, VTYPE
        INTEGER         DIMID( MXWRFDIMS )
        LOGICAL         EFLAG
        REAL*8          CENTX, CENTY, CENTLAT, CENTLON
        REAL            RLAT, RLON
        CHARACTER*32    ANAME, VNAME
        CHARACTER*256   MESG
        CHARACTER*512   EQNAME

        !!-----------   Body:

        IF ( FSTATUS .EQ. FSCREA3 ) THEN
            FSTAT = NF_WRITE + NF_NOCLOBBER
        ELSE IF ( FSTATUS .EQ. FSUNKN3 ) THEN
            FSTAT = NF_WRITE
        ELSE IF ( FSTATUS .EQ. FSNEW3 ) THEN
            FSTAT = NF_WRITE + NF_NOCLOBBER
        ELSE
            WRITE( MESG, '( A, I10 )' ) 'File status', FSTATUS,  'not supported for creating "' // TRIM( FNAME ) // '"'
            CALL M3WARN( PNAME, 0,0, MESG )
            CRTWRFOUT = .FALSE.
            RETURN
        END IF

        CALL NAMEVAL( FNAME, EQNAME )
        IERR = NF_CREATE( EQNAME, FSTAT, CDFIDWO )
        IF ( IERR .NE. 0 ) THEN
            MESG  = NF_STRERROR( IERR )
            CALL M3MESG( MESG )
            MESG = 'Path name "' // TRIM( EQNAME ) // '"'
            CALL M3MESG( MESG )
            WRITE( MESG, '( 3 A, I10 )' ) 'NF_OPEN(', TRIM( FNAME ), ') failure:  IERR=', IERR
            CALL M3WARN( PNAME, 0,0, MESG )
            CRTWRFOUT = .FALSE.
            RETURN
        ELSE
            MESG = 'Opening "'// TRIM(FNAME) // '" on path "' // TRIM(EQNAME ) // '"'
            CALL M3MESG( MESG )
        END IF


        !!...............  Copy/edit dimensions:

        IERR = NF_INQ( CDFIDW, NDIMS, NVARSW, NATTS, UDIM )
        IF ( IERR .NE. 0 ) THEN
            MESG  = NF_STRERROR( IERR )
            CALL M3MESG( MESG )
            WRITE( MESG, '( 3 A, I10 )' ) 'NF_INQ_NDIMS(', TRIM( FNAME ), ') failure:  IERR=', IERR
            CALL M3WARN( PNAME, 0,0, MESG )
            CRTWRFOUT = .FALSE.
            RETURN
        ELSE IF ( NDIMS .GT. MXWRFDIMS ) THEN
            MESG = 'File "' // TRIM( FNAME ) // '":  NDIMS exceeds MXWRFDIMS'
            CALL M3WARN( PNAME, 0,0, MESG )
            CRTWRFOUT = .FALSE.
            RETURN
        ELSE IF ( NVARSW .GT. MXWRFVARS ) THEN
            MESG = 'File "' // TRIM( FNAME ) // '":  NVARS exceeds MXWRFVARS'
            CALL M3WARN( PNAME, 0,0, MESG )
            CRTWRFOUT = .FALSE.
            RETURN
        END IF

        DO N = 1, NDIMS

            IERR = NF_INQ_DIM( CDFIDW, N, DNAMEWO( N ), DSIZEWO( N ) )
            IF ( IERR .NE. 0 ) THEN
                MESG  = NF_STRERROR( IERR )
                CALL M3MESG( MESG )
                WRITE( MESG, '( 3 A, I10 )' ) 'NF_INQ_DIM(', TRIM( FNAME ), ') failure:  IERR=', IERR
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
                CYCLE
            END IF

            IF ( DNAMEWO( N ) .EQ. 'west_east' ) THEN
                DSIZEWO( N ) = NCOLSX
                WEDIDX       = N
            ELSE  IF ( DNAMEWO( N ) .EQ. 'west_east_stag' ) THEN
                DSIZEWO( N ) = NCOLSX+1
                WEDIDS       = N
            ELSE  IF ( DNAMEWO( N ) .EQ. 'south_north' ) THEN
                DSIZEWO( N ) = NROWSX
                SNDIDX       = N
            ELSE  IF ( DNAMEWO( N ) .EQ. 'south_north_stag' ) THEN
                DSIZEWO( N ) = NROWSX+1
                SNDIDS       = N
            ELSE  IF ( DNAMEWO( N ) .EQ. 'Time' ) THEN
                NRECSWO      = DSIZEWO( N )
                DSIZEWO( N ) = NF_UNLIMITED
                TIMDID       = N
            END IF

            IERR = NF_DEF_DIM( CDFIDWO, DNAMEWO( N ), DSIZEWO( N ), N )
            IF ( IERR .NE. 0 ) THEN
                MESG  = NF_STRERROR( IERR )
                CALL M3MESG( MESG )
                WRITE( MESG, '( 3 A, I10 )' ) 'NF_DEF_DIMS(', TRIM( FNAME ), ') failure:  IERR=', IERR
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
                CYCLE
            END IF

        END DO

        IF ( EFLAG ) THEN
            CALL M3WARN( PNAME, 0,0, 'Dimension-definition failure for ' // FNAME )
            CRTWRFOUT = .FALSE.
            RETURN
        END IF

        !!...............  Compute grid-center Lat-Lon for WRF's screwed-up
        !!...............  CEN_LAT (etc.) grid descriptions:

        CENTX = XORIGX + 0.5D0 * XCELLX * DBLE( NCOLSX )
        CENTY = YORIGX + 0.5D0 * YCELLX * DBLE( NROWSX )

        CALL XY2XY( LATGRD3, 0.0D0,   0.0D0,  0.0D0,  0.0D0,  0.0D0, ARW_SPHERE,    &
                    GDTYPX,  P_ALPX, P_BETX, P_GAMX, XCENTX, YCENTX, ARW_SPHERE,    &
                    CENTX, CENTY, CENTLON, CENTLAT )
        RLAT = REAL( CENTLAT )
        RLON = REAL( CENTLON )
        WRITE( MESG, '( 2( A, F9.4 ) )' ) 'Grid center at LON=', RLON, ', LAT=', RLAT
        CALL M3MESG( MESG )

        !!...............  Copy global attributes:

        DO N = 1, NATTS

            IERR = NF_INQ_ATTNAME( CDFIDW, NF_GLOBAL, N, ANAME )
            IF ( IERR .NE. 0 ) THEN
                MESG  = NF_STRERROR( IERR )
                CALL M3MESG( MESG )
                WRITE( MESG, '( 3 A, I10 )' ) 'NF_INQ_ATTNAME(', TRIM( FNAME ), ') failure:  IERR=', IERR
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
                CYCLE
            END IF

            IF ( ANAME .EQ. 'WEST-EAST_PATCH_END_UNSTAG' ) THEN
                IERR = NF_PUT_ATT_INT( CDFIDWO, NF_GLOBAL, ANAME, NF_INT, 1, NCOLSX )
            ELSE IF ( ANAME .EQ. 'WEST-EAST_PATCH_END_STAG' ) THEN
                IERR = NF_PUT_ATT_INT( CDFIDWO, NF_GLOBAL, ANAME, NF_INT, 1, NCOLSX+1 )
            ELSE IF ( ANAME .EQ. 'SOUTH-NORTH_PATCH_END_UNSTAG' ) THEN
                IERR = NF_PUT_ATT_INT( CDFIDWO, NF_GLOBAL, ANAME, NF_INT, 1, NROWSX )
            ELSE IF ( ANAME .EQ. 'SOUTH-NORTH_PATCH_END_STAG' ) THEN
                IERR = NF_PUT_ATT_INT( CDFIDWO, NF_GLOBAL, ANAME, NF_INT, 1, NROWSX+1 )
            ELSE IF ( ANAME .EQ. 'CEN_LON' ) THEN
                IERR = NF_PUT_ATT_REAL( CDFIDWO, NF_GLOBAL, ANAME, NF_REAL, 1, RLON  )
            ELSE IF ( ANAME .EQ. 'CEN_LAT' ) THEN
                IERR = NF_PUT_ATT_REAL( CDFIDWO, NF_GLOBAL, ANAME, NF_REAL, 1, RLAT  )
            ELSE IF ( ANAME .EQ. 'MOAD_CEN_LAT' ) THEN
                IERR = NF_PUT_ATT_REAL( CDFIDWO, NF_GLOBAL, ANAME, NF_REAL, 1, RLAT  )
            ELSE
                IERR = NF_COPY_ATT( CDFIDW, NF_GLOBAL, ANAME, CDFIDWO, NF_GLOBAL )
            END IF

            IF ( IERR .NE. 0 ) THEN
                MESG  = NF_STRERROR( IERR )
                CALL M3MESG( MESG )
                WRITE( MESG, '( 5 A, I10 )' ) 'NF_*_ATT(', TRIM( FNAME ), COMMA, TRIM( ANAME ), ') failure:  IERR=', IERR
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
                CYCLE
            END IF

        END DO

        IF ( EFLAG ) THEN
            CALL M3WARN( PNAME, 0,0, 'Global-att-definition failure for ' // FNAME )
            CRTWRFOUT = .FALSE.
            RETURN
        END IF


        !!...............  Copy variable-definitions:

        DO V = 1, NVARSW

            IERR = NF_INQ_VAR( CDFIDW, V, VNAME, VTYPE, VDIMS, DIMID, NATTS )
            IF ( IERR .NE. 0 ) THEN
                MESG  = NF_STRERROR( IERR )
                CALL M3MESG( MESG )
                WRITE( MESG, '( 3 A, I5, A, I10 )' )        &
                        'NF_INQ_VAR(', TRIM( FNAME ), COLON, V,  ') failure:  IERR=', IERR
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
                CYCLE
            END IF

            DO N = 1, VDIMS
                VDIMSWO( N,V ) = DSIZEWO( DIMID( N ) )
            END DO

            IERR = NF_DEF_VAR( CDFIDWO, VNAME, VTYPE, VDIMS, DIMID, VID )
            IF ( IERR .NE. 0 ) THEN
                MESG  = NF_STRERROR( IERR )
                CALL M3MESG( MESG )
                WRITE( MESG, '( 5 A, I10 )' )        &
                        'NF_DEF_VAR(', TRIM( FNAME ), COLON, VNAME, ') failure:  IERR=', IERR
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
                CYCLE
            END IF

            DO N = 1, NATTS

                IERR = NF_INQ_ATTNAME( CDFIDW, V, N, ANAME )
                IF ( IERR .NE. 0 ) THEN
                    MESG  = NF_STRERROR( IERR )
                    CALL M3MESG( MESG )
                    WRITE( MESG, '( 5 A, I10 )' )        &
                        'NF_INQ_ATTNAME(', TRIM( FNAME ), COLON, VNAME, ') failure:  IERR=', IERR
                    CALL M3MESG( MESG )
                    EFLAG = .TRUE.
                    CYCLE
                END IF

                IERR = NF_COPY_ATT( CDFIDW, V, ANAME, CDFIDWO, VID )
                IF ( IERR .NE. 0 ) THEN
                    MESG  = NF_STRERROR( IERR )
                    CALL M3MESG( MESG )
                    WRITE( MESG, '( 5 A, I10 )' )        &
                        'NF_COPY_ATT(', TRIM( FNAME ), COLON, VNAME, ') failure:  IERR=', IERR
                    CALL M3MESG( MESG )
                    EFLAG = .TRUE.
                    CYCLE
                END IF

                IF ( ANAME .EQ. 'MemoryOrder' ) THEN
                    IERR = NF_GET_ATT_TEXT( CDFIDW, V, ANAME, MORDRWO( V ) )
                    IF ( IERR .NE. 0 ) THEN
                        MESG  = NF_STRERROR( IERR )
                        CALL M3MESG( MESG )
                        WRITE( MESG, '( 5 A, I10 )' )        &
                            'NF_GET_ATT_TEXT ""MemoryOrder', TRIM( FNAME ), COLON, VNAME, ') failure:  IERR=', IERR
                        CALL M3MESG( MESG )
                        EFLAG = .TRUE.
                        CYCLE
                    END IF
                END IF

            END DO

        END DO

        IERR = NF_ENDDEF( CDFIDWO )
        IF ( IERR .NE. 0 ) THEN
            MESG  = NF_STRERROR( IERR )
            CALL M3MESG( MESG )
            CALL M3WARN( PNAME, 0,0, 'Could not put "' // TRIM(FNAME) // '" into data mode' )
            CRTWRFOUT = .FALSE.
            RETURN
        END IF


        CRTWRFOUT = .TRUE.
        RETURN

    END FUNCTION CRTWRFOUT


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION COPYTIME( IVAR, IREC )

        INTEGER, INTENT( IN ) :: IVAR, IREC

        INTEGER         IERR
        INTEGER         INDX(2)
        INTEGER         CNTS(2)
        CHARACTER*19    WTIME
        CHARACTER*256   MESG

        INDX(1) = 1
        INDX(2) = IREC
        CNTS(1) = 19
        CNTS(2) = 1
        
        IERR = NF_GET_VARA_TEXT( CDFIDW, IVAR, INDX, CNTS, WTIME )
        IF ( IERR .NE. 0 ) THEN
            MESG  = NF_STRERROR( IERR )
            CALL M3MESG( MESG )
            WRITE( MESG, '( 2 A, I5, A, I10 )' )    &
                'NF_GET_VARA_TEXT( "WRFIFILE"', COLON, IVAR,  ') failure:  IERR=', IERR
            CALL M3MESG( MESG )
            COPYTIME = .FALSE.
            RETURN
        END IF
        
        IERR = NF_PUT_VARA_TEXT( CDFIDWO, IVAR, INDX, CNTS, WTIME )
        IF ( IERR .NE. 0 ) THEN
            MESG  = NF_STRERROR( IERR )
            CALL M3MESG( MESG )
            WRITE( MESG, '( 2 A, I5, A, I10 )' )    &
                'NF_PUT_VARA_TEXT( "WRFOFILE"', COLON, IVAR,  ') failure:  IERR=', IERR
            CALL M3MESG( MESG )
            COPYTIME = .FALSE.
            RETURN
        END IF

        WRITE( MESG, '( 3A,I10 )' ) 'Variable "Time" = ', WTIME,'" copied to "WRFOFILE" for timestep', IREC
        CALL M3MESG( MESG )
        COPYTIME = .TRUE.
        RETURN

    END FUNCTION COPYTIME


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION WNDWXYI( IVAR, IREC )

        INTEGER, INTENT( IN ) :: IVAR, IREC

        INTEGER         C, R, DIMS1, DIMS2, WNDW1, WNDW2
        INTEGER         IBUF(  VARDIM( 1,IVAR ),  VARDIM( 2,IVAR ) )
        INTEGER         OBUF( VDIMSWO( 1,IVAR ), VDIMSWO( 2,IVAR ) )
        CHARACTER*256   MESG

        DIMS1 = VARDIM( 1,IVAR )
        DIMS2 = VARDIM( 2,IVAR )

        WNDW1 = VDIMSWO( 1,IVAR )
        WNDW2 = VDIMSWO( 2,IVAR )
        
        IF ( .NOT.  READNCVAR( 'WRFIFILE', CDFIDW, IREC, VNAMEW( IVAR ), DIMS1, DIMS2, IBUF ) ) THEN
            WNDWXYI = .FALSE.
            RETURN
        END IF

        DO R = 1, WNDW2
        DO C = 1, WNDW1
            OBUF( C,R ) = IBUF( C+XOFFS1,  R+YOFFS1 )
        END DO
        END DO

        IF ( .NOT. WRITENCVAR( 'WRFOFILE', CDFIDWO, IREC, VNAMEW( IVAR ), WNDW1, WNDW2, OBUF ) ) THEN
            WNDWXYI = .FALSE.
        ELSE
            WNDWXYI = .TRUE.
            WRITE( MESG, '( 3A,I10 )' ) 'Variable "', TRIM( VNAMEW( IVAR ) ), '" windowed to "WRFOFILE" for timestep', IREC
            CALL M3MESG( MESG )
        END IF

        RETURN

    END FUNCTION WNDWXYI


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION WNDWXYR( IVAR, IREC )

        INTEGER, INTENT( IN ) :: IVAR, IREC

        INTEGER         C, R, DIMS1, DIMS2, WNDW1, WNDW2
        REAL            IBUF(  VARDIM( 1,IVAR ),  VARDIM( 2,IVAR ) )
        REAL            OBUF( VDIMSWO( 1,IVAR ), VDIMSWO( 2,IVAR ) )
        CHARACTER*256   MESG

        DIMS1 = VARDIM( 1,IVAR )
        DIMS2 = VARDIM( 2,IVAR )

        WNDW1 = VDIMSWO( 1,IVAR )
        WNDW2 = VDIMSWO( 2,IVAR )
        
        IF ( .NOT.  READNCVAR( 'WRFIFILE', CDFIDW, IREC, VNAMEW( IVAR ), DIMS1, DIMS2, IBUF ) ) THEN
            WNDWXYR = .FALSE.
            RETURN
        END IF

        DO R = 1, WNDW2
        DO C = 1, WNDW1
            OBUF( C,R ) = IBUF( C+XOFFS1,  R+YOFFS1 )
        END DO
        END DO

        IF ( .NOT. WRITENCVAR( 'WRFOFILE', CDFIDWO, IREC, VNAMEW( IVAR ), WNDW1, WNDW2, OBUF ) ) THEN
            WNDWXYR = .FALSE.
        ELSE
            WNDWXYR = .TRUE.
            WRITE( MESG, '( 3A,I10 )' ) 'Variable "', TRIM( VNAMEW( IVAR ) ), '" windowed to "WRFOFILE" for timestep', IREC
            CALL M3MESG( MESG )
        END IF

        RETURN

    END FUNCTION WNDWXYR


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION WNDWXYD( IVAR, IREC )

        INTEGER, INTENT( IN ) :: IVAR, IREC

        INTEGER         C, R, DIMS1, DIMS2, WNDW1, WNDW2
        REAL*8          IBUF(  VARDIM( 1,IVAR ),  VARDIM( 2,IVAR ) )
        REAL*8          OBUF( VDIMSWO( 1,IVAR ), VDIMSWO( 2,IVAR ) )
        CHARACTER*256   MESG

        DIMS1 = VARDIM( 1,IVAR )
        DIMS2 = VARDIM( 2,IVAR )

        WNDW1 = VDIMSWO( 1,IVAR )
        WNDW2 = VDIMSWO( 2,IVAR )
        
        IF ( .NOT.  READNCVAR( 'WRFIFILE', CDFIDW, IREC, VNAMEW( IVAR ), DIMS1, DIMS2, IBUF ) ) THEN
            WNDWXYD = .FALSE.
            RETURN
        END IF

        DO R = 1, WNDW2
        DO C = 1, WNDW1
            OBUF( C,R ) = IBUF( C+XOFFS1,  R+YOFFS1 )
        END DO
        END DO

        IF ( .NOT. WRITENCVAR( 'WRFOFILE', CDFIDWO, IREC, VNAMEW( IVAR ), WNDW1, WNDW2, OBUF ) ) THEN
            WNDWXYD = .FALSE.
        ELSE
            WNDWXYD = .TRUE.
            WRITE( MESG, '( 3A,I10 )' ) 'Variable "', TRIM( VNAMEW( IVAR ) ), '" windowed to "WRFOFILE" for timestep', IREC
            CALL M3MESG( MESG )
        END IF

        RETURN

    END FUNCTION WNDWXYD


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION WNDWXYZI( IVAR, IREC )

        INTEGER, INTENT( IN ) :: IVAR, IREC

        INTEGER         C, R, L, DIMS1, DIMS2, DIMS3, WNDW1, WNDW2
        INTEGER         IBUF(  VARDIM( 1,IVAR ),  VARDIM( 2,IVAR ),  VARDIM( 3,IVAR ) )
        INTEGER         OBUF( VDIMSWO( 1,IVAR ), VDIMSWO( 2,IVAR ), VDIMSWO( 3,IVAR ) )
        CHARACTER*256   MESG

        DIMS1 = VARDIM( 1,IVAR )
        DIMS2 = VARDIM( 2,IVAR )
        DIMS3 = VARDIM( 3,IVAR )

        WNDW1 = VDIMSWO( 1,IVAR )
        WNDW2 = VDIMSWO( 2,IVAR )
        
        IF ( .NOT.  READNCVAR( 'WRFIFILE', CDFIDW, IREC, VNAMEW( IVAR ), DIMS1, DIMS2, DIMS3, IBUF ) ) THEN
            WNDWXYZI = .FALSE.
            RETURN
        END IF

        DO L = 1, DIMS3
        DO R = 1, WNDW2
        DO C = 1, WNDW1
            OBUF( C,R,L ) = IBUF( C+XOFFS1,  R+YOFFS1, L )
        END DO
        END DO
        END DO

        IF ( .NOT. WRITENCVAR( 'WRFOFILE', CDFIDWO, IREC, VNAMEW( IVAR ), WNDW1, WNDW2, DIMS3, OBUF ) ) THEN
            WNDWXYZI = .FALSE.
        ELSE
            WNDWXYZI = .TRUE.
            WRITE( MESG, '( 3A,I10 )' ) 'Variable "', TRIM( VNAMEW( IVAR ) ), '" windowed to "WRFOFILE" for timestep', IREC
            CALL M3MESG( MESG )
        END IF

        RETURN

    END FUNCTION WNDWXYZI


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION WNDWXYZR( IVAR, IREC )

        INTEGER, INTENT( IN ) :: IVAR, IREC

        INTEGER         C, R, L, DIMS1, DIMS2, DIMS3, WNDW1, WNDW2
        REAL            IBUF(  VARDIM( 1,IVAR ),  VARDIM( 2,IVAR ),  VARDIM( 3,IVAR ) )
        REAL            OBUF( VDIMSWO( 1,IVAR ), VDIMSWO( 2,IVAR ), VDIMSWO( 3,IVAR ) )
        CHARACTER*256   MESG

        DIMS1 = VARDIM( 1,IVAR )
        DIMS2 = VARDIM( 2,IVAR )
        DIMS3 = VARDIM( 3,IVAR )

        WNDW1 = VDIMSWO( 1,IVAR )
        WNDW2 = VDIMSWO( 2,IVAR )
        
        IF ( .NOT.  READNCVAR( 'WRFIFILE', CDFIDW, IREC, VNAMEW( IVAR ), DIMS1, DIMS2, DIMS3, IBUF ) ) THEN
            WNDWXYZR = .FALSE.
            RETURN
        END IF

        DO L = 1, DIMS3
        DO R = 1, WNDW2
        DO C = 1, WNDW1
            OBUF( C,R,L ) = IBUF( C+XOFFS1, R+YOFFS1, L )
        END DO
        END DO
        END DO

        IF ( .NOT. WRITENCVAR( 'WRFOFILE', CDFIDWO, IREC, VNAMEW( IVAR ), WNDW1, WNDW2, DIMS3, OBUF ) ) THEN
            WNDWXYZR = .FALSE.
        ELSE
            WNDWXYZR = .TRUE.
            WRITE( MESG, '( 3A,I10 )' ) 'Variable "', TRIM( VNAMEW( IVAR ) ), '" windowed to "WRFOFILE" for timestep', IREC
            CALL M3MESG( MESG )
        END IF

        RETURN

    END FUNCTION WNDWXYZR


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION WNDWXYZD( IVAR, IREC )

        INTEGER, INTENT( IN ) :: IVAR, IREC

        INTEGER         C, R, L, DIMS1, DIMS2, DIMS3, WNDW1, WNDW2
        REAL*8          IBUF(  VARDIM( 1,IVAR ),  VARDIM( 2,IVAR ),  VARDIM( 3,IVAR ) )
        REAL*8          OBUF( VDIMSWO( 1,IVAR ), VDIMSWO( 2,IVAR ), VDIMSWO( 3,IVAR ) )
        CHARACTER*256   MESG

        DIMS1 = VARDIM( 1,IVAR )
        DIMS2 = VARDIM( 2,IVAR )
        DIMS3 = VARDIM( 3,IVAR )

        WNDW1 = VDIMSWO( 1,IVAR )
        WNDW2 = VDIMSWO( 2,IVAR )
        
        IF ( .NOT.  READNCVAR( 'WRFIFILE', CDFIDW, IREC, VNAMEW( IVAR ), DIMS1, DIMS2, DIMS3, IBUF ) ) THEN
            WNDWXYZD = .FALSE.
            RETURN
        END IF

        DO L = 1, VDIMSWO( 3, IVAR )
        DO R = 1, VDIMSWO( 2, IVAR )
        DO C = 1, VDIMSWO( 1, IVAR )
            OBUF( C,R,L ) = IBUF( C+XOFFS1, R+YOFFS1, L )
        END DO
        END DO
        END DO

        IF ( .NOT. WRITENCVAR( 'WRFOFILE', CDFIDWO, IREC, VNAMEW( IVAR ), WNDW1, WNDW2, DIMS3, OBUF ) ) THEN
            WNDWXYZD = .FALSE.
        ELSE
            WNDWXYZD = .TRUE.
            WRITE( MESG, '( 3A,I10 )' ) 'Variable "', TRIM( VNAMEW( IVAR ) ), '" windowed to "WRFOFILE" for timestep', IREC
            CALL M3MESG( MESG )
        END IF

        RETURN

    END FUNCTION WNDWXYZD


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION COPYVAR1I( IVAR, IREC )

        INTEGER, INTENT( IN ) :: IVAR, IREC

        INTEGER         VBUF
        CHARACTER*256   MESG

        IF      ( .NOT.  READNCVAR( 'WRFIFILE', CDFIDW,  IREC, VNAMEW( IVAR ), VBUF ) ) THEN
            COPYVAR1I = .FALSE.
        ELSE IF ( .NOT. WRITENCVAR( 'WRFOFILE', CDFIDWO, IREC, VNAMEW( IVAR ), VBUF ) ) THEN
            COPYVAR1I = .FALSE.
        ELSE
            COPYVAR1I = .TRUE.
            WRITE( MESG, '( 3A,I10 )' ) 'Variable "', TRIM( VNAMEW( IVAR ) ), '" copied to "WRFOFILE" for timestep', IREC
            CALL M3MESG( MESG )
        END IF

        RETURN

    END FUNCTION COPYVAR1I


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION COPYVAR1R( IVAR, IREC )

        INTEGER, INTENT( IN ) :: IVAR, IREC

        REAL            VBUF
        CHARACTER*256   MESG

        IF      ( .NOT.  READNCVAR( 'WRFIFILE', CDFIDW,  IREC, VNAMEW( IVAR ), VBUF ) ) THEN
            COPYVAR1R = .FALSE.
        ELSE IF ( .NOT. WRITENCVAR( 'WRFOFILE', CDFIDWO, IREC, VNAMEW( IVAR ), VBUF ) ) THEN
            COPYVAR1R = .FALSE.
        ELSE
            COPYVAR1R = .TRUE.
            WRITE( MESG, '( 3A,I10 )' ) 'Variable "', TRIM( VNAMEW( IVAR ) ), '" copied to "WRFOFILE" for timestep', IREC
            CALL M3MESG( MESG )
        END IF

        RETURN

    END FUNCTION COPYVAR1R


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION COPYVAR1D( IVAR, IREC )

        INTEGER, INTENT( IN ) :: IVAR, IREC

        REAL*8          VBUF
        CHARACTER*256   MESG

        IF      ( .NOT.  READNCVAR( 'WRFIFILE', CDFIDW,  IREC, VNAMEW( IVAR ), VBUF ) ) THEN
            COPYVAR1D = .FALSE.
        ELSE IF ( .NOT. WRITENCVAR( 'WRFOFILE', CDFIDWO, IREC, VNAMEW( IVAR ), VBUF ) ) THEN
            COPYVAR1D = .FALSE.
        ELSE
            COPYVAR1D = .TRUE.
            WRITE( MESG, '( 3A,I10 )' ) 'Variable "', TRIM( VNAMEW( IVAR ) ), '" copied to "WRFOFILE" for timestep', IREC
            CALL M3MESG( MESG )
        END IF

        RETURN

    END FUNCTION COPYVAR1D


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION COPYVAR2I( IVAR, IREC )

        INTEGER, INTENT( IN ) :: IVAR, IREC

        INTEGER         DIMS1
        INTEGER         VBUF( VARDIM( 1,IVAR ) )
        CHARACTER*256   MESG

        DIMS1 = VARDIM( 1,IVAR )

        IF      ( .NOT.  READNCVAR( 'WRFIFILE', CDFIDW,  IREC, VNAMEW( IVAR ), DIMS1, VBUF ) ) THEN
            COPYVAR2I = .FALSE.
        ELSE IF ( .NOT. WRITENCVAR( 'WRFOFILE', CDFIDWO, IREC, VNAMEW( IVAR ), DIMS1, VBUF ) ) THEN
            COPYVAR2I = .FALSE.
        ELSE
            COPYVAR2I = .TRUE.
            WRITE( MESG, '( 3A,I10 )' ) 'Variable "', TRIM( VNAMEW( IVAR ) ), '" copied to "WRFOFILE" for timestep', IREC
            CALL M3MESG( MESG )
        END IF

        RETURN

    END FUNCTION COPYVAR2I


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION COPYVAR2R( IVAR, IREC )

        INTEGER, INTENT( IN ) :: IVAR, IREC

        INTEGER         DIMS1
        REAL            VBUF( VARDIM( 1,IVAR ) )
        CHARACTER*256   MESG

        DIMS1 = VARDIM( 1,IVAR )

        IF      ( .NOT.  READNCVAR( 'WRFIFILE', CDFIDW,  IREC, VNAMEW( IVAR ), DIMS1, VBUF ) ) THEN
            COPYVAR2R = .FALSE.
        ELSE IF ( .NOT. WRITENCVAR( 'WRFOFILE', CDFIDWO, IREC, VNAMEW( IVAR ), DIMS1, VBUF ) ) THEN
            COPYVAR2R = .FALSE.
        ELSE
            COPYVAR2R = .TRUE.
            WRITE( MESG, '( 3A,I10 )' ) 'Variable "', TRIM( VNAMEW( IVAR ) ), '" copied to "WRFOFILE" for timestep', IREC
            CALL M3MESG( MESG )
        END IF

        RETURN

    END FUNCTION COPYVAR2R


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION COPYVAR2D( IVAR, IREC )

        INTEGER, INTENT( IN ) :: IVAR, IREC

        INTEGER         DIMS1
        REAL*8          VBUF( VARDIM( 1,IVAR ) )
        CHARACTER*256   MESG

        DIMS1 = VARDIM( 1,IVAR )

        IF      ( .NOT.  READNCVAR( 'WRFIFILE', CDFIDW,  IREC, VNAMEW( IVAR ), DIMS1, VBUF ) ) THEN
            COPYVAR2D = .FALSE.
        ELSE IF ( .NOT. WRITENCVAR( 'WRFOFILE', CDFIDWO, IREC, VNAMEW( IVAR ), DIMS1, VBUF ) ) THEN
            COPYVAR2D = .FALSE.
        ELSE
            COPYVAR2D = .TRUE.
            WRITE( MESG, '( 3A,I10 )' ) 'Variable "', TRIM( VNAMEW( IVAR ) ), '" copied to "WRFOFILE" for timestep', IREC
            CALL M3MESG( MESG )
        END IF

        RETURN

    END FUNCTION COPYVAR2D


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION COPYVAR3I( IVAR, IREC )

        INTEGER, INTENT( IN ) :: IVAR, IREC

        INTEGER         DIMS1, DIMS2
        INTEGER         VBUF( VARDIM( 1,IVAR ), VARDIM( 2,IVAR ) )
        CHARACTER*256   MESG

        DIMS1 = VARDIM( 1,IVAR )
        DIMS2 = VARDIM( 2,IVAR )

        IF      ( .NOT.  READNCVAR( 'WRFIFILE', CDFIDW,  IREC, VNAMEW( IVAR ), DIMS1, DIMS2, VBUF ) ) THEN
            COPYVAR3I = .FALSE.
        ELSE IF ( .NOT. WRITENCVAR( 'WRFOFILE', CDFIDWO, IREC, VNAMEW( IVAR ), DIMS1, DIMS2, VBUF ) ) THEN
            COPYVAR3I = .FALSE.
        ELSE
            COPYVAR3I = .TRUE.
            WRITE( MESG, '( 3A,I10 )' ) 'Variable "', TRIM( VNAMEW( IVAR ) ), '" copied to "WRFOFILE" for timestep', IREC
            CALL M3MESG( MESG )
        END IF

        RETURN

    END FUNCTION COPYVAR3I


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION COPYVAR3R( IVAR, IREC )

        INTEGER, INTENT( IN ) :: IVAR, IREC

        INTEGER         DIMS1, DIMS2
        REAL            VBUF( VARDIM( 1,IVAR ), VARDIM( 2,IVAR ) )
        CHARACTER*256   MESG

        DIMS1 = VARDIM( 1,IVAR )
        DIMS2 = VARDIM( 2,IVAR )

        IF      ( .NOT.  READNCVAR( 'WRFIFILE', CDFIDW,  IREC, VNAMEW( IVAR ), DIMS1, DIMS2, VBUF ) ) THEN
            COPYVAR3R = .FALSE.
        ELSE IF ( .NOT. WRITENCVAR( 'WRFOFILE', CDFIDWO, IREC, VNAMEW( IVAR ), DIMS1, DIMS2, VBUF ) ) THEN
            COPYVAR3R = .FALSE.
        ELSE
            COPYVAR3R = .TRUE.
            WRITE( MESG, '( 3A,I10 )' ) 'Variable "', TRIM( VNAMEW( IVAR ) ), '" copied to "WRFOFILE" for timestep', IREC
            CALL M3MESG( MESG )
        END IF

        RETURN

    END FUNCTION COPYVAR3R


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION COPYVAR3D( IVAR, IREC )

        INTEGER, INTENT( IN ) :: IVAR, IREC

        INTEGER         DIMS1, DIMS2
        REAL*8          VBUF( VARDIM( 1,IVAR ), VARDIM( 2,IVAR ) )
        CHARACTER*256   MESG

        DIMS1 = VARDIM( 1,IVAR )
        DIMS2 = VARDIM( 2,IVAR )

        IF      ( .NOT.  READNCVAR( 'WRFIFILE', CDFIDW,  IREC, VNAMEW( IVAR ), DIMS1, DIMS2, VBUF ) ) THEN
            COPYVAR3D = .FALSE.
        ELSE IF ( .NOT. WRITENCVAR( 'WRFOFILE', CDFIDWO, IREC, VNAMEW( IVAR ), DIMS1, DIMS2, VBUF ) ) THEN
            COPYVAR3D = .FALSE.
        ELSE
            COPYVAR3D = .TRUE.
            WRITE( MESG, '( 3A,I10 )' ) 'Variable "', TRIM( VNAMEW( IVAR ) ), '" copied to "WRFOFILE" for timestep', IREC
            CALL M3MESG( MESG )
        END IF

        RETURN

    END FUNCTION COPYVAR3D


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION COPYVAR4I( IVAR, IREC )

        INTEGER, INTENT( IN ) :: IVAR, IREC

        INTEGER         DIMS1, DIMS2, DIMS3
        INTEGER         VBUF( VARDIM( 1,IVAR ), VARDIM( 2,IVAR ), VARDIM( 3,IVAR ) )
        CHARACTER*256   MESG

        DIMS1 = VARDIM( 1,IVAR )
        DIMS2 = VARDIM( 2,IVAR )
        DIMS3 = VARDIM( 3,IVAR )

        IF      ( .NOT.  READNCVAR( 'WRFIFILE', CDFIDW,  IREC, VNAMEW( IVAR ), DIMS1, DIMS2, DIMS3, VBUF ) ) THEN
            COPYVAR4I = .FALSE.
        ELSE IF ( .NOT. WRITENCVAR( 'WRFOFILE', CDFIDWO, IREC, VNAMEW( IVAR ), DIMS1, DIMS2, DIMS3, VBUF ) ) THEN
            COPYVAR4I = .FALSE.
        ELSE
            COPYVAR4I = .TRUE.
            WRITE( MESG, '( 3A,I10 )' ) 'Variable "', TRIM( VNAMEW( IVAR ) ), '" copied to "WRFOFILE" for timestep', IREC
            CALL M3MESG( MESG )
        END IF

        RETURN

    END FUNCTION COPYVAR4I


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION COPYVAR4R( IVAR, IREC )

        INTEGER, INTENT( IN ) :: IVAR, IREC

        INTEGER         DIMS1, DIMS2, DIMS3
        REAL            VBUF( VARDIM( 1,IVAR ), VARDIM( 2,IVAR ), VARDIM( 3,IVAR ) )
        CHARACTER*256   MESG

        DIMS1 = VARDIM( 1,IVAR )
        DIMS2 = VARDIM( 2,IVAR )
        DIMS3 = VARDIM( 3,IVAR )

        IF      ( .NOT.  READNCVAR( 'WRFIFILE', CDFIDW,  IREC, VNAMEW( IVAR ), DIMS1, DIMS2, DIMS3, VBUF ) ) THEN
            COPYVAR4R = .FALSE.
        ELSE IF ( .NOT. WRITENCVAR( 'WRFOFILE', CDFIDWO, IREC, VNAMEW( IVAR ), DIMS1, DIMS2, DIMS3, VBUF ) ) THEN
            COPYVAR4R = .FALSE.
        ELSE
            COPYVAR4R = .TRUE.
            WRITE( MESG, '( 3A,I10 )' ) 'Variable "', TRIM( VNAMEW( IVAR ) ), '" copied to "WRFOFILE" for timestep', IREC
            CALL M3MESG( MESG )
        END IF

        RETURN

    END FUNCTION COPYVAR4R


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION COPYVAR4D( IVAR, IREC )

        INTEGER, INTENT( IN ) :: IVAR, IREC

        INTEGER         DIMS1, DIMS2, DIMS3
        REAL*8          VBUF( VARDIM( 1,IVAR ), VARDIM( 2,IVAR ), VARDIM( 3,IVAR ) )
        CHARACTER*256   MESG

        DIMS1 = VARDIM( 1,IVAR )
        DIMS2 = VARDIM( 2,IVAR )
        DIMS3 = VARDIM( 3,IVAR )

        IF      ( .NOT.  READNCVAR( 'WRFIFILE', CDFIDW,  IREC, VNAMEW( IVAR ), DIMS1, DIMS2, DIMS3, VBUF ) ) THEN
            COPYVAR4D = .FALSE.
        ELSE IF ( .NOT. WRITENCVAR( 'WRFOFILE', CDFIDWO, IREC, VNAMEW( IVAR ), DIMS1, DIMS2, DIMS3, VBUF ) ) THEN
            COPYVAR4D = .FALSE.
        ELSE
            COPYVAR4D = .TRUE.
            WRITE( MESG, '( 3A,I10 )' ) 'Variable "', TRIM( VNAMEW( IVAR ) ), '" copied to "WRFOFILE" for timestep', IREC
            CALL M3MESG( MESG )
        END IF

        RETURN

    END FUNCTION COPYVAR4D


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION DBLERR( P, Q )
        REAL*8, INTENT( IN ) :: P, Q
        DBLERR = ( (P - Q)**2  .GT.  1.0D-10*( P*P + Q*Q + 1.0D-5 ) )
        RETURN
    END FUNCTION DBLERR



END PROGRAM WRFWNDW
