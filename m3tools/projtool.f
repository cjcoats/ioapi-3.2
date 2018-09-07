
        PROGRAM PROJTOOL

    !!***********************************************************************
    !! Version "$Id: projtool.f 108 2018-09-07 18:59:37Z coats $"
    !! EDSS/Models-3 M3TOOLS.
    !! Copyright (C) 1992-2002 MCNC, (C) 1997-2013 Carlie J. Coats, Jr.,
    !! (C) 2002-2012 Baron Advanced Meteorological Systems. LLC., and
    !! (C) 2014 UNC Institute for the Environment.
    !! Distributed under the GNU GENERAL PUBLIC LICENSE version 2
    !! See file "GPL.txt" for conditions of use.
    !!.........................................................................
    !!  program body starts at line  128
    !!
    !!  DESCRIPTION:
    !!       Perform coordinate conversions and grid-related computations
    !!       for I/O API supported coordinate systems.
    !!
    !!  PRECONDITIONS REQUIRED:
    !!       "setenv GRIDDESC <pathname>" for using map projections by name.
    !!
    !!  SUBROUTINES AND FUNCTIONS CALLED:
    !!       GETMENU, GETNUM, GETDBLE, GETSTR, GETYN
    !!       USGS-derived GCTP package
    !!
    !!  REVISION  HISTORY:
    !!       Adapted 11/2002 by CJC from UTMTOOL (which it supersedes).
    !!       Version 06/2008 by CJC:  add Albers support
    !!       Version 02/2010 by CJC for I/O API v3.1:  Fortran-90 only;
    !!       USE M3UTILIO, and related changes.
    !!       Version 04/2011 by CJC:  bug-fixes; add <X,Y>-to-<C,R> operation
    !!       Version 05/2011 by CJC:  bug-fixes
    !!       Version 10/2012 by CJC:  bug-fix in M3MESG calls for cases 10,11
    !!       Version  8/2014 by CJC:  need to call SETPROJ() for cases 6,7
    !!       Version 12/2014 by CJC:  Version for I/O API-v3.2: bug-fix in SHOWGRID(),
    !!       menu-display; new "define output grid" menu item; USE MODGCTP/XY2XY()
    !!***********************************************************************

      USE M3UTILIO
      USE MODGCTP

      IMPLICIT NONE

C...........   PARAMETERS and their descriptions:

        REAL*8,       PARAMETER :: BAD   = -9.999D36
        CHARACTER*16, PARAMETER :: PNAME = 'PROJTOOL'
        CHARACTER*72, PARAMETER :: BAR   =
     &  '-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-'

        CHARACTER*60, PARAMETER :: CHOICES( 13 ) =
     & (/
     & 'Quit the program                                        ',    !!  mode =  1
     & 'Set up input  map projection by name                    ',    !!  mode =  2
     & 'Set up output map projection by name                    ',    !!  mode =  3
     & 'Set up input  grid &  projection by name                ',    !!  mode =  4
     & 'Set up output grid &  projection by name                ',    !!  mode =  5
     & 'Set up input  map projection by angles (etc.)           ',    !!  mode =  6
     & 'Set up output map projection by angles (etc.)           ',    !!  mode =  7
     & 'Define an input grid w.r.t. the input map projection    ',    !!  mode =  8
     & 'Convert from input X-Y to output X-Y                    ',    !!  mode =  9
     & 'Get output-projection grid corners for input grid       ',    !!  mode = 10
     & 'Get output-projection grid-corner-cell centers          ',    !!  mode = 11
     & 'Define an output grid w.r.t. the output map projection  ',    !!  mode = 12
     & 'Get output-grid <C,R> for input-coord <X,Y>             '     !!  mode = 13
     & /)

        CHARACTER*60, PARAMETER :: CPROMPT =
     & 'What operation do you want to do next?'

C...........   LOCAL VARIABLES and their descriptions:

        INTEGER :: LOGDEV
        INTEGER :: MODE, NMODES
        INTEGER :: ZONE
        REAL*8  :: XSW, YSW, XNW, YNW, XSE, YSE, XNE, YNE
        REAL*8  :: USW, VSW, UNW, VNW, USE, VSE, UNE, VNE
        REAL*8  :: XX, YY, UU, VV
        INTEGER :: CC, RR

        CHARACTER*16  :: PRNAM       ! GRIDDESC projection name
        CHARACTER*16  :: GDNAM       ! GRIDDESC grid name
        CHARACTER*256 :: MESG

        !!........  Input projection and grid:

        INTEGER :: GDTYP1      ! grid type:  1=LAT-LON, 2=UTM, ...
        REAL*8  :: P_ALP1      ! first, second, third map
        REAL*8  :: P_BET1      ! projection descriptive
        REAL*8  :: P_GAM1      ! parameters.
        REAL*8  :: XCENT1      ! lon for coord-system X=0
        REAL*8  :: YCENT1      ! lat for coord-system Y=0

        INTEGER :: NCOLS1      ! number of grid columns
        INTEGER :: NROWS1      ! number of grid rows
        INTEGER :: NTHIK1      ! boundary thickness (cells)
        REAL*8  :: XORIG1      ! X-coordinate origin of grid (map units)
        REAL*8  :: YORIG1      ! Y-coordinate origin of grid
        REAL*8  :: XCELL1      ! X-coordinate cell dimension
        REAL*8  :: YCELL1      ! Y-coordinate cell dimension

        !!........  output projection and grid:

        INTEGER :: GDTYP2      ! grid type:  1=LAT-LON, 2=UTM, ...
        REAL*8  :: P_ALP2      ! first, second, third map
        REAL*8  :: P_BET2      ! projection descriptive
        REAL*8  :: P_GAM2      ! parameters.
        REAL*8  :: XCENT2      ! lon for coord-system X=0
        REAL*8  :: YCENT2      ! lat for coord-system Y=0

        INTEGER :: NCOLS2      ! number of grid columns
        INTEGER :: NROWS2      ! number of grid rows
        INTEGER :: NTHIK2      ! boundary thickness (cells)
        REAL*8  :: XORIG2      ! X-coordinate origin of grid (map units)
        REAL*8  :: YORIG2      ! Y-coordinate origin of grid
        REAL*8  :: XCELL2      ! X-coordinate cell dimension
        REAL*8  :: YCELL2      ! Y-coordinate cell dimension

        LOGICAL :: INSET   = .FALSE.    ! has  input projection been set?
        LOGICAL :: OUTSET  = .FALSE.    ! has output projection been set?
        LOGICAL :: INGRID  = .FALSE.    ! has  input grid been set?
        LOGICAL :: OUTGRID = .FALSE.    ! has output grid been set?


C***********************************************************************
C   begin body of program PROJTOOL

        LOGDEV = INIT3()
        WRITE( *,'( 5X, A )' )
     &' ',
     &'Program PROJTOOL to provide coordinate conversion back and',
     &'forth among LAT-LON, UTM, LAMBERT, POLAR STEREOGRAPHIC,',
     &'TRANSVERSE MERCATOR, and EQUATORIAL MERCATOR coordinate',
     &'systems.',
     &' ',
     &'Note that according to the standard, UTM coordinates should',
     &'be specified in _meters_ instead of the UAM/EPS bastardized ',
     &'system which claims to be UTM but in fact uses *kilo*meters. ',
     &' ',
     &'Longitudes are specified in _signed_degrees_ (so that for',
     &'the US longitudes are negative).  NOTE that this is in',
     &'conformance with ISO STANDARD 6709, *not* the so-called',
     &'WMO "standard" for representation of longitudes.',
     &' ',
     &'See URL',
     &'https://www.cmascenter.org/ioapi/documentation/3.1/html#tools',
     &' ',
     &'Program copyright (C) 1992-2002 MCNC, (C) 1995-2013',
     &'Carlie J. Coats, Jr., (C) 2002-2010 Baron Advanced',
     &'Meteorological Systems, LLC., and (C) 2014 UNC Institute',
     &'for the Environment.  Released under Version 2 of the ',
     &'GNU General Public License. See enclosed GPL.txt, or',
     &'URL http://www.gnu.org/copyleft/gpl.html',
     &' ',
     &'Comments and questions are welcome and can be sent to',
     &' ',
     &'    Carlie J. Coats, Jr.    carlie@jyarborough.com',
     &'or',
     &'    UNC Institute for the Environment',
     &'    137 E. Franklin St. Suite 602 Room 613-C',
     &'    Campus Box 1105',
     &'    Chapel Hill, NC 27599-1105',
     &' ',
     &'Program version: ',
     &'$Id:: projtool.f 108 2018-09-07 18:59:37Z coats               $',
     &' '

        IF ( .NOT. GETYN( 'Continue with program?', .TRUE. ) )
     &      CALL M3EXIT( PNAME, 0, 0, 'Exit at user request', 0 )

        MODE = 1
        XX   = 0.0D0
        YY   = 0.0D0

        DO        !  head of event loop

            IF ( INSET ) THEN
                CALL SHOWPROJ( 'Input map projection parameters',
     &                         GDTYP1, P_ALP1, P_BET1, P_GAM1,
     &                         XCENT1, YCENT1 )
            END IF

            IF ( INGRID ) THEN
                CALL SHOWGRID( 'Input grid parameters',
     &                         NCOLS1, NROWS1,
     &                         XORIG1, YORIG1, XCELL1, YCELL1 )
            END IF

            IF ( OUTSET ) THEN
                CALL SHOWPROJ( 'Output map projection parameters',
     &                         GDTYP2, P_ALP2, P_BET2, P_GAM2,
     &                         XCENT2, YCENT2 )
            END IF

            IF ( OUTGRID ) THEN
                CALL SHOWGRID( 'Output grid parameters',
     &                         NCOLS2, NROWS2,
     &                         XORIG2, YORIG2, XCELL2, YCELL2 )
            END IF

            IF ( OUTGRID ) THEN
                NMODES = 13
            ELSE IF ( INGRID ) THEN
                NMODES = 12
            ELSE IF ( INSET .AND. OUTSET ) THEN
                NMODES = 12
            ELSE
                NMODES = 7
            END IF

            MODE = GETMENU( NMODES, 1+MOD( MODE, NMODES ),
     &                      CPROMPT, CHOICES )

            IF ( MODE .EQ. 1 ) THEN             ! exit

                EXIT

            ELSE IF ( MODE .EQ. 2 ) THEN        ! new input proj by name

                MESG = 'Enter GRIDDESC name for input projection'
                CALL GETSTR( MESG, 'LATLON', PRNAM )
                IF ( DSCOORD( PRNAM, GDTYP1,
     &                        P_ALP1, P_BET1, P_GAM1,
     &                        XCENT1, YCENT1 ) ) THEN
                    INSET = .TRUE.
                ELSE
                    MESG = 'Map projection "' // TRIM( PRNAM ) //
     &                     '"not found in GRIDDESC file'
                    CALL M3WARN( PNAME, 0, 0, MESG )
                END IF

            ELSE IF ( MODE .EQ. 3  ) THEN       ! new output proj by name

                MESG = 'Enter GRIDDESC name for output projection'
                CALL GETSTR( MESG, 'LATLON', PRNAM )
                IF ( DSCOORD( PRNAM, GDTYP2,
     &                        P_ALP2, P_BET2, P_GAM2,
     &                        XCENT2, YCENT2 ) ) THEN
                    OUTSET = .TRUE.
                ELSE
                    MESG = 'Map projection "' // TRIM( PRNAM ) //
     &                     '"not found in GRIDDESC file'
                    CALL M3WARN( PNAME, 0, 0, MESG )
                END IF

            ELSE IF ( MODE .EQ. 4  ) THEN       ! new  input grid by name

                MESG = 'Enter GRIDDESC name for input grid'
                CALL GETSTR( MESG, 'LATLON', GDNAM )
                IF ( DSCGRID( GDNAM,   PRNAM, GDTYP1,
     &                        P_ALP1, P_BET1, P_GAM1, XCENT1, YCENT1,
     &                        XORIG1, YORIG1, XCELL1, YCELL1,
     &                        NCOLS1, NROWS1, NTHIK1 ) ) THEN
                    INSET  = .TRUE.
                    INGRID = .TRUE.
                ELSE
                    MESG = 'Grid "' // TRIM( GDNAM ) //
     &                     '"not found in GRIDDESC file'
                    CALL M3WARN( PNAME, 0, 0, MESG )
                END IF

            ELSE IF ( MODE .EQ. 5  ) THEN       ! new output grid by name

                MESG = 'Enter GRIDDESC name for output grid'
                CALL GETSTR( MESG, 'LATLON', GDNAM )
                IF ( DSCGRID( GDNAM,  PRNAM, GDTYP2,
     &                        P_ALP2, P_BET2, P_GAM2, XCENT2, YCENT2,
     &                        XORIG2, YORIG2, XCELL2, YCELL2,
     &                        NCOLS2, NROWS2, NTHIK2 ) ) THEN
                    OUTSET  = .TRUE.
                    OUTGRID = .TRUE.
                ELSE
                    MESG = 'Grid "' // TRIM( GDNAM ) //
     &                     '"not found in GRIDDESC file'
                    CALL M3WARN( PNAME, 0, 0, MESG )
                END IF

            ELSE IF ( MODE .EQ. 6  ) THEN       ! new  input proj by angles

                CALL GETPROJ( GDTYP1,
     &                        P_ALP1, P_BET1, P_GAM1, XCENT1, YCENT1 )
                INSET  = .TRUE.

            ELSE IF ( MODE .EQ. 7  ) THEN       ! new output proj by angles

                CALL GETPROJ( GDTYP2,
     &                        P_ALP2, P_BET2, P_GAM2, XCENT2, YCENT2 )
                OUTSET   = .TRUE.

            ELSE IF ( MODE .EQ. 8  ) THEN       ! new input grid

                CALL GETGRID( XORIG1, YORIG1, XCELL1, YCELL1,
     &                        NCOLS1, NROWS1 )

                INGRID = .TRUE.

            ELSE IF ( MODE .EQ. 9  ) THEN       ! Coordinate conversion

                XX = GETDBLE( BAD, -BAD, XX,
     &                                'Enter input coord X' )

                YY = GETDBLE( BAD, -BAD, YY,
     &                                'Enter input coord Y' )

                CALL XY2XY( GDTYP2,P_ALP2,P_BET2,P_GAM2,XCENT2,YCENT2,
     &                      GDTYP1,P_ALP1,P_BET1,P_GAM1,XCENT1,YCENT1,
     &                      XX, YY, UU, VV )

                CALL M3MESG( BAR )
                CALL M3MESG( ' ' )
                WRITE( MESG, '( 2( A, F25.14, :, 2X ) )' )
     &                 'Input   X = ', XX,
     &                 'Input   Y = ', YY
                CALL M3MESG( MESG )
                WRITE( MESG, '( 2( A, F25.14, :, 2X ) )' )
     &                 'Output  X = ', UU,
     &                 'Output  Y = ', VV
                CALL M3MESG( MESG )
                CALL M3MESG( ' ' )
                CALL M3MESG( BAR )
                MODE = MODE - 1

            ELSE IF ( MODE .EQ. 10 ) THEN       ! compute grid corners

                IF ( .NOT.INGRID ) THEN
                    CALL M3MESG( 'You must define input grid first' )
                    CYCLE
                END IF

                XSW = XORIG1
                YSW = YORIG1
                XNW = XORIG1
                YNW = YORIG1 + DBLE( NROWS1 )*YCELL1
                XSE = XORIG1 + DBLE( NCOLS1 )*XCELL1
                YSE = YORIG1
                XNE = XORIG1 + DBLE( NCOLS1 )*XCELL1
                YNE = YORIG1 + DBLE( NROWS1 )*YCELL1

                CALL XY2XY( GDTYP2,P_ALP2,P_BET2,P_GAM2,XCENT2,YCENT2,
     &                      GDTYP1,P_ALP1,P_BET1,P_GAM1,XCENT1,YCENT1,
     &                      XSW, YSW, USW, VSW )

                CALL XY2XY( GDTYP2,P_ALP2,P_BET2,P_GAM2,XCENT2,YCENT2,
     &                      GDTYP1,P_ALP1,P_BET1,P_GAM1,XCENT1,YCENT1,
     &                      XNW, YNW, UNW, VNW )

                CALL XY2XY( GDTYP2,P_ALP2,P_BET2,P_GAM2,XCENT2,YCENT2,
     &                      GDTYP1,P_ALP1,P_BET1,P_GAM1,XCENT1,YCENT1,
     &                      XSE, YSE, USE, VSE )

                CALL XY2XY( GDTYP2,P_ALP2,P_BET2,P_GAM2,XCENT2,YCENT2,
     &                      GDTYP1,P_ALP1,P_BET1,P_GAM1,XCENT1,YCENT1,
     &                      XNE, YNE, UNE, VNE )

                CALL M3MESG( BAR )
                CALL M3MESG( ' ' )
                WRITE( MESG, '( 3( A, :, F25.14 ) )' )
     &              'Input  proj SW corner (X,Y) = (',
     &              XSW, ',', YSW, ')'
                CALL M3MESG( MESG )
                WRITE( MESG, '( 3( A, :, F25.14 ) )' )
     &              'Output proj SW corner (X,Y) = (',
     &              USW, ',', VSW, ')'
                CALL M3MESG( MESG )
                CALL M3MESG( ' ' )
                WRITE( MESG, '( 3( A, :, F25.14 ) )' )
     &              'Input  proj SE corner (X,Y) = (',
     &              XSE, ',', YSE, ')'
                CALL M3MESG( MESG )
                WRITE( MESG, '( 3( A, :, F25.14 ) )' )
     &              'Output proj SE corner (X,Y) = (',
     &              USE, ',', VSE, ')'
                CALL M3MESG( MESG )
                CALL M3MESG( ' ' )
                WRITE( MESG, '( 3( A, :, F25.14 ) )' )
     &              'Input  proj NW corner (X,Y) = (',
     &              XNW, ',', YNW, ')'
                CALL M3MESG( MESG )
                WRITE( MESG, '( 3( A, :, F25.14 ) )' )
     &              'Output proj NW corner (X,Y) = (',
     &              UNW, ',', VNW, ')'
                CALL M3MESG( MESG )
                CALL M3MESG( ' ' )
                WRITE( MESG, '( 3( A, :, F25.14 ) )' )
     &              'Input  proj NE corner (X,Y) = (',
     &              XNE, ',', YNE, ')'
                CALL M3MESG( MESG )
                WRITE( MESG, '( 3( A, :, F25.14 ) )' )
     &              'Output proj NE corner (X,Y) = (',
     &              UNE, ',', VNE, ')'
                CALL M3MESG( MESG )
                CALL M3MESG( ' ' )
                CALL M3MESG( BAR )


            ELSE IF ( MODE .EQ. 11 ) THEN       ! compute corner-cell centers

                IF ( .NOT.INGRID ) THEN
                    CALL M3MESG( 'You must define input grid first' )
                    CYCLE
                END IF

                XSW = XORIG1 + 0.5D0*XCELL1
                YSW = YORIG1 + 0.5D0*YCELL1
                XNW = XSW
                YNW = YSW + DBLE( NROWS1 - 1 )*YCELL1
                XSE = XSW + DBLE( NCOLS1 - 1 )*XCELL1
                YSE = YSW
                XNE = XSW + DBLE( NCOLS1 - 1 )*XCELL1
                YNE = YSW + DBLE( NROWS1 - 1 )*YCELL1

                CALL XY2XY( GDTYP2,P_ALP2,P_BET2,P_GAM2,XCENT2,YCENT2,
     &                      GDTYP1,P_ALP1,P_BET1,P_GAM1,XCENT1,YCENT1,
     &                      XSW, YSW, USW, VSW )

                CALL XY2XY( GDTYP2,P_ALP2,P_BET2,P_GAM2,XCENT2,YCENT2,
     &                      GDTYP1,P_ALP1,P_BET1,P_GAM1,XCENT1,YCENT1,
     &                      XNW, YNW, UNW, VNW )

                CALL XY2XY( GDTYP2,P_ALP2,P_BET2,P_GAM2,XCENT2,YCENT2,
     &                      GDTYP1,P_ALP1,P_BET1,P_GAM1,XCENT1,YCENT1,
     &                      XSE, YSE, USE, VSE )

                CALL XY2XY( GDTYP2,P_ALP2,P_BET2,P_GAM2,XCENT2,YCENT2,
     &                      GDTYP1,P_ALP1,P_BET1,P_GAM1,XCENT1,YCENT1,
     &                      XNE, YNE, UNE, VNE )

                CALL M3MESG( BAR )
                CALL M3MESG( ' ' )
                WRITE( MESG, '( 3( A, :, F25.14 ) )' )
     &              'Input  proj SW cell-center (X,Y) = (',
     &              XSW, ',', YSW, ')'
                CALL M3MESG( MESG )
                WRITE( MESG, '( 3( A, :, F25.14 ) )' )
     &              'Output proj SW cell-center (X,Y) = (',
     &              USW, ',', VSW, ')'
                CALL M3MESG( MESG )
                CALL M3MESG( ' ' )
                WRITE( MESG, '( 3( A, :, F25.14 ) )' )
     &              'Input  proj SE cell-center (X,Y) = (',
     &              XSE, ',', YSE, ')'
                CALL M3MESG( MESG )
                WRITE( MESG, '( 3( A, :, F25.14 ) )' )
     &              'Output proj SE cell-center (X,Y) = (',
     &              USE, ',', VSE, ')'
                CALL M3MESG( MESG )
                CALL M3MESG( ' ' )
                WRITE( MESG, '( 3( A, :, F25.14 ) )' )
     &              'Input  proj NW cell-center (X,Y) = (',
     &              XNW, ',', YNW, ')'
                CALL M3MESG( MESG )
                WRITE( MESG, '( 3( A, :, F25.14 ) )' )
     &              'Output proj NW cell-center (X,Y) = (',
     &              UNW, ',', VNW, ')'
                CALL M3MESG( MESG )
                CALL M3MESG( ' ' )
                WRITE( MESG, '( 3( A, :, F25.14 ) )' )
     &              'Input  proj NE cell-center (X,Y) = (',
     &              XNE, ',', YNE, ')'
                CALL M3MESG( MESG )
                WRITE( MESG, '( 3( A, :, F25.14 ) )' )
     &              'Output proj NE cell-center (X,Y) = (',
     &              UNE, ',', VNE, ')'
                CALL M3MESG( MESG )
                CALL M3MESG( ' ' )
                CALL M3MESG( BAR )

            ELSE IF ( MODE .EQ. 12 ) THEN       ! new output grid

                CALL GETGRID( XORIG2, YORIG2, XCELL2, YCELL2,
     &                        NCOLS2, NROWS2 )
                OUTGRID = .TRUE.

            ELSE IF ( MODE .EQ. 13 ) THEN       ! Coordinate-to-grid conversion

                XSW = GETDBLE( BAD, -BAD, XX,
     &                        'Enter input coord X' )

                YSW = GETDBLE( BAD, -BAD, YY,
     &                        'Enter input coord Y' )

                CALL XY2XY( GDTYP2,P_ALP2,P_BET2,P_GAM2,XCENT2,YCENT2,
     &                      GDTYP1,P_ALP1,P_BET1,P_GAM1,XCENT1,YCENT1,
     &                      XSW, YSW, USW, VSW )

                XX = ( USW - XORIG2 ) / XCELL2
                YY = ( VSW - YORIG2 ) / YCELL2
                IF ( XX .GE. 0.0D0 ) THEN
                    CC = 1 + INT( XX )
                ELSE
                    CC = -INT( -XX )
                END IF
                IF ( YY .GE. 0.0D0 ) THEN
                    RR = 1 + INT( YY )
                ELSE
                    RR = -INT( -YY )
                END IF


                CALL M3MESG( BAR )
                WRITE( MESG, '( 2( A, F25.14 , :, 2X ) )' )
     &             'Input   X = ', XSW,
     &             'Input   Y = ', YSW
                CALL M3MESG( MESG )
                WRITE( MESG, '( 2( A, F25.14, :, 2X ) )' )
     &             'Output  X = ', USW,
     &             'Output  Y = ', VSW
                CALL M3MESG( MESG )
                WRITE( MESG, '( A, F25.14, 2X, A, I5 )' )
     &             'Output  grid-normal X = ', XX, 'col =', CC
                CALL M3MESG( MESG )
                WRITE( MESG, '( A, F25.14, 2X, A, I5 )' )
     &             'Output  grid-normal Y = ', YY, 'row =', RR
                CALL M3MESG( MESG )
                CALL M3MESG( BAR )
                MODE = MODE - 1

            ELSE                                ! error

                MESG = '"Impossible Error" -- Unrecognized choice'
                CALL M3EXIT( PNAME, 0, 0, MESG, 2 )

            END IF                              !!  if mode=1,2,...,10, or not

        END DO          !!  end of event-loop

        MESG = 'Successful completion of program PROJTOOL'

        CALL M3EXIT( PNAME, 0, 0, MESG, 0 )


      CONTAINS    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


          SUBROUTINE GETPROJ( GDTYP, ALPHA, BETA, GAMMA, XCENT, YCENT )

          !*********************************************************
          !  FUNCTION:
          !       Get map projection defining parameters from user
          !**********************************************************

          !...........   ARGUMENTS and their descriptions:

          INTEGER*4, INTENT(  OUT) :: GDTYP  !  I/O API map projection code (input)
          REAL*8   , INTENT(  OUT) :: ALPHA  !  map projection parameter    (input)
          REAL*8   , INTENT(  OUT) :: BETA   !  map projection parameter    (input)
          REAL*8   , INTENT(  OUT) :: GAMMA  !  map projection parameter    (input)
          REAL*8   , INTENT(  OUT) :: XCENT  !  Cartesian origin longitude  (input)
          REAL*8   , INTENT(  OUT) :: YCENT  !  Cartesian origin latitude   (input)


          !...........   SCRATCH LOCAL VARIABLES and their descriptions:

          CHARACTER*64, PARAMETER :: GPROMPT =
     &                  'Enter map projection type'

          CHARACTER*64, PARAMETER :: GMENU( 7 ) =
     &    (/
     &    'Latitude-Longitude                   ',    !!  choice 1
     &    'Lambert Conformala Conic             ',    !!  choice 2
     &    'Universal Transverse Mercator        ',    !!  choice 3
     &    'Polar Sterographic                   ',    !!  choice 4
     &    '(General) Transverse Mercator        ',    !!  choice 5
     &    'Equatorial Mercator                  ',    !!  choice 6
     &    'Albers Equal-Area Conic              '     !!  choice 7
     &    /)
          INTEGER, PARAMETER :: GTYPES( 7 ) =
     &    (/
     &    LATGRD3,                              !!  choice 1
     &    LAMGRD3,                              !!  choice 2
     &    UTMGRD3,                              !!  choice 3
     &    POLGRD3,                              !!  choice 4
     &    TRMGRD3,                              !!  choice 5
     &    EQMGRD3,                              !!  choice 5
     &    ALBGRD3                               !!  choice 6
     &    /)

          !**************************************************************
          !   begin body of subroutine  GETPROJ

          GDTYP = GTYPES( GETMENU( 7, 1, GPROMPT, GMENU ) )

          IF ( GDTYP .EQ. LATGRD3 ) THEN
              ALPHA = 0.0D0
              BETA  = 0.0D0
              GAMMA = 0.0D0
              XCENT = 0.0D0
              YCENT = 0.0D0
          ELSE IF ( GDTYP .EQ. LAMGRD3 ) THEN
              ALPHA = GETDBLE( -90.0D0, 90.0D0, 30.0D0,
     &                         'Enter first secant angle   (deg N)' )
              BETA  = GETDBLE( ALPHA, 90.0D0, 60.0D0,
     &                         'Enter second secant angle  (deg W)' )
              GAMMA = GETDBLE( -180.0D0, 180.0D0, -90.0D0,
     &                         'Enter central meridian     (deg W)' )
              XCENT = GETDBLE(  -180.0D0, 180.0D0, GAMMA,
     &                         'Enter Cartesian-origin lon (deg W)' )
              YCENT = GETDBLE(  -90.0D0, 90.0D0, 0.5D0*(ALPHA+BETA),
     &                         'Enter Cartesian-origin lat (deg N)' )
          ELSE IF ( GDTYP .EQ. UTMGRD3 ) THEN
              ALPHA = DBLE( GETNUM( 1, 60, 17,
     &                         'Enter UTM zone number' ) )
              BETA  = 0.0D0
              GAMMA = 0.0D0
              XCENT = GETDBLE(  -9.999D36, 9.999D36, 0.0D0,
     &                         'Enter Cartesian-origin X (M)' )
              YCENT = GETDBLE(   -9.999D36, 9.999D36, 4235.0D3,
     &                         'Enter Cartesian-origin Y (M)' )
          ELSE IF ( GDTYP .EQ. POLGRD3 ) THEN
              ALPHA = DBLE( GETNUM(-1, 1, 1,
     &        'Enter 1 for North polar, -1 for South Polar' ) )
              BETA  = GETDBLE( -90.0D0, 90.0D0, 60.0D0,
     &                         'Enter latitude of true scale (deg N)' )
              GAMMA = GETDBLE( -180.0D0, 180.0D0, -90.0D0,
     &                         'Enter central meridian       (deg W)' )
              XCENT = GETDBLE(  -180.0D0, 180.0D0, GAMMA,
     &                         'Enter Cartesian-origin lon (deg W)' )
              YCENT = GETDBLE(  -90.0D0, 90.0D0, 90.0D0*ALPHA,
     &                         'Enter Cartesian-origin lat (deg N)' )
          ELSE IF ( GDTYP .EQ. TRMGRD3 ) THEN
              ALPHA = 0.0D0
              BETA  = GETDBLE( 0.0D0, 1.0D0, 1.0D0,
     &                         'Enter scale factor' )
              GAMMA = GETDBLE( -180.0D0, 180.0D0, -90.0D0,
     &                         'Enter central meridian       (deg W)' )
              XCENT = GETDBLE(  -180.0D0, 180.0D0, GAMMA,
     &                         'Enter Cartesian-origin lon (deg W)' )
              YCENT = GETDBLE(  -90.0D0, 90.0D0, 0.5D0*(ALPHA+BETA),
     &                         'Enter Cartesian-origin lat (deg N)' )
          ELSE IF ( GDTYP .EQ. EQMGRD3 ) THEN
              ALPHA = 0.0D0
              BETA  = GETDBLE( -90.0D0, 90.0D0, 60.0D0,
     &                         'Enter latitude of true scale (deg N)' )
              GAMMA = GETDBLE( -180.0D0, 180.0D0, -90.0D0,
     &                         'Enter central meridian       (deg W)' )
              XCENT = GETDBLE(  -180.0D0, 180.0D0, GAMMA,
     &                         'Enter Cartesian-origin lon (deg W)' )
              YCENT = GETDBLE(  -90.0D0, 90.0D0, 0.0D0,
     &                         'Enter Cartesian-origin lat (deg N)' )
          ELSE IF ( GDTYP .EQ. ALBGRD3 ) THEN
              ALPHA = GETDBLE( -90.0D0, 90.0D0, 30.0D0,
     &                         'Enter first secant angle   (deg N)' )
              BETA  = GETDBLE( ALPHA, 90.0D0, 60.0D0,
     &                         'Enter second secant angle  (deg W)' )
              GAMMA = GETDBLE( -180.0D0, 180.0D0, -90.0D0,
     &                         'Enter central meridian     (deg W)' )
              XCENT = GETDBLE(  -180.0D0, 180.0D0, GAMMA,
     &                         'Enter Cartesian-origin lon (deg W)' )
              YCENT = GETDBLE(  -90.0D0, 90.0D0, 0.5D0*(ALPHA+BETA),
     &                         'Enter Cartesian-origin lat (deg N)' )
          END IF

          RETURN

          END SUBROUTINE GETPROJ


      !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

          SUBROUTINE GETGRID( XORIG, YORIG,  XCELL, YCELL,
     &                        NCOLS, NROWS )

          !*********************************************************
          !  FUNCTION:
          !       Get grid defining parameters from user
          !**********************************************************

          !...........   ARGUMENTS and their descriptions:

          REAL*8 , INTENT(INOUT) :: XORIG, YORIG, XCELL, YCELL
          INTEGER, INTENT(INOUT) :: NCOLS, NROWS

          REAL*8    N, X

          !**************************************************************
          !   begin body of subroutine  GETGRID

          NCOLS = GETNUM( 1, 999999999, NCOLS,
     &                   'Enter number of grid cols' )
          NROWS = GETNUM( 1, 999999999, NROWS,
     &                   'Enter number of grid rows' )
          XORIG = GETDBLE( -9.999D36, 9.999D36, XORIG,
     &                   'Enter X at SW grid corner' )
          YORIG = GETDBLE( -9.999D36, 9.999D36, YORIG,
     &                   'Enter Y at SW grid corner' )
          XCELL = GETDBLE( -9.999D36, 9.999D36, XCELL,
     &                   'Enter DX cellsize' )
          YCELL = GETDBLE( -9.999D36, 9.999D36, XCELL,
     &                   'Enter DY cellsize' )

          RETURN

          END SUBROUTINE GETGRID


      !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

          SUBROUTINE SHOWPROJ( TEXT,
     &                         GDTYP, ALPHA, BETA, GAMMA, XCENT, YCENT )

          !*********************************************************
          !  FUNCTION:
          !       Display map projection defining parameters
          !**********************************************************

          !...........   ARGUMENTS and their descriptions:

          CHARACTER*(*), INTENT(IN   ) :: TEXT   !  projection description      (input)
          INTEGER*4    , INTENT(IN   ) :: GDTYP  !  I/O API map projection code (input)
          REAL*8       , INTENT(IN   ) :: ALPHA  !  map projection parameter    (input)
          REAL*8       , INTENT(IN   ) :: BETA   !  map projection parameter    (input)
          REAL*8       , INTENT(IN   ) :: GAMMA  !  map projection parameter    (input)
          REAL*8       , INTENT(IN   ) :: XCENT  !  Cartesian origin longitude  (input)
          REAL*8       , INTENT(IN   ) :: YCENT  !  Cartesian origin latitude   (input)


          !...........   SCRATCH LOCAL VARIABLES and their descriptions:

          CHARACTER*256   MESG

          !**************************************************************
          !   begin body of subroutine  SHOWPROJ

          IF ( GDTYP .EQ. LATGRD3 ) THEN

              CALL M3MSG2( TEXT )
              MESG = 'Projection type:  Lat-Lon'
              CALL M3MSG2( MESG )

          ELSE IF ( GDTYP .EQ. LAMGRD3 ) THEN

              CALL M3MSG2( TEXT )
              MESG = 'Projection type:  Lambert conformal conic'
              CALL M3MSG2( MESG )
              WRITE( MESG, '( A26, 2( :, 2X, F17.12 ) )' )
     &           'Secant angles', ALPHA, BETA
              CALL M3MSG2( MESG )
              WRITE( MESG, '( A26, 2( :, 2X, F17.12 ) )' )
     &           'Central meridian', GAMMA
              CALL M3MSG2( MESG )
              WRITE( MESG, '( A26, 2( :, 2X, F17.12 ) )' )
     &           'Cartesian origin LON-LAT', XCENT, YCENT
              CALL M3MSG2( MESG )

          ELSE IF ( GDTYP .EQ. UTMGRD3 ) THEN

              CALL M3MSG2( TEXT )
              MESG = 'Projection type:  UTM'
              CALL M3MSG2( MESG )
              WRITE( MESG, '( A26, I7 )' ) 'UTM Zone', NINT( ALPHA )
              CALL M3MSG2( MESG )
              WRITE( MESG, '( A26, 2( :, 2X, F17.12 ) )' )
     &           'UTM origin X-Y', XCENT, YCENT
              CALL M3MSG2( MESG )

          ELSE IF ( GDTYP .EQ. POLGRD3 ) THEN

              CALL M3MSG2( TEXT )
              IF ( ALPHA .GT. 0 ) THEN
                  MESG = 'Projection type:  North-Polar Stereographic'
              ELSE
                  MESG = 'Projection type:  South-Polar Stereographic'
              END IF
              CALL M3MSG2( MESG )
              WRITE( MESG, '( A26, 2( :, 2X, F17.12 ) )' )
     &           'Latitude of True Scale', BETA
              CALL M3MSG2( MESG )
              WRITE( MESG, '( A26, 2( :, 2X, F17.12 ) )' )
     &           'Central meridian', GAMMA
              CALL M3MSG2( MESG )
              WRITE( MESG, '( A26, 2( :, 2X, F17.12 ) )' )
     &           'Cartesian origin LON-LAT', XCENT, YCENT
              CALL M3MSG2( MESG )

          ELSE IF ( GDTYP .EQ. TRMGRD3 ) THEN

              CALL M3MSG2( TEXT )
              MESG = 'Projection type:  Transverse Mercator'
              CALL M3MSG2( MESG )
              WRITE( MESG, '( A26, 2( :, 2X, F17.12 ) )' )
     &           'Scale Factor', BETA
              CALL M3MSG2( MESG )
              WRITE( MESG, '( A26, 2( :, 2X, F17.12 ) )' )
     &           'Central meridian', GAMMA
              CALL M3MSG2( MESG )
              WRITE( MESG, '( A26, 2( :, 2X, F17.12 ) )' )
     &           'Cartesian origin LON-LAT', XCENT, YCENT
              CALL M3MSG2( MESG )

          ELSE IF ( GDTYP .EQ. EQMGRD3 ) THEN

              CALL M3MSG2( TEXT )
              MESG = 'Projection type:  Equatorial Mercator'
              CALL M3MSG2( MESG )
              WRITE( MESG, '( A26, 2( :, 2X, F17.12 ) )' )
     &           'Latitude of True Scale', BETA
              CALL M3MSG2( MESG )
              WRITE( MESG, '( A26, 2( :, 2X, F17.12 ) )' )
     &           'Central meridian', GAMMA
              CALL M3MSG2( MESG )
              WRITE( MESG, '( A26, 2( :, 2X, F17.12 ) )' )
     &           'Cartesian origin LON-LAT', XCENT, YCENT
              CALL M3MSG2( MESG )

          ELSE IF ( GDTYP .EQ. ALBGRD3 ) THEN

              CALL M3MSG2( TEXT )
              MESG = 'Projection type:  Albers Equal-Area conic'
              CALL M3MSG2( MESG )
              WRITE( MESG, '( A26, 2( :, 2X, F17.12 ) )' )
     &           'Secant angles', ALPHA, BETA
              CALL M3MSG2( MESG )
              WRITE( MESG, '( A26, 2( :, 2X, F17.12 ) )' )
     &           'Central meridian', GAMMA
              CALL M3MSG2( MESG )
              WRITE( MESG, '( A26, 2( :, 2X, F17.12 ) )' )
     &           'Cartesian origin LON-LAT', XCENT, YCENT
              CALL M3MSG2( MESG )

          ELSE

              WRITE( MESG, '( A, I5 )' )
     &           'Unsupported map projection type', GDTYP
              CALL M3MSG2( MESG )

          END IF

          CALL M3MSG2( ' ' )

          RETURN

          END SUBROUTINE SHOWPROJ


      !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

          SUBROUTINE SHOWGRID( TEXT,
     &                         NCOLS, NROWS,
     &                         XORIG, YORIG, XCELL, YCELL )

          !*********************************************************
          !  FUNCTION:
          !       Display grid defining parameters
          !**********************************************************

          IMPLICIT NONE

          !...........   ARGUMENTS and their descriptions:

          CHARACTER*(*), INTENT(IN   ) :: TEXT        !  projection description      (input)
          INTEGER      , INTENT(IN   ) :: NCOLS       ! number of grid columns
          INTEGER      , INTENT(IN   ) :: NROWS       ! number of grid rows
          REAL*8       , INTENT(IN   ) :: XORIG       ! X-coordinate origin of grid (map units)
          REAL*8       , INTENT(IN   ) :: YORIG       ! Y-coordinate origin of grid
          REAL*8       , INTENT(IN   ) :: XCELL       ! X-coordinate cell dimension
          REAL*8       , INTENT(IN   ) :: YCELL       ! Y-coordinate cell dimension

          !...........   SCRATCH LOCAL VARIABLES and their descriptions:

          CHARACTER*256   MESG

          !**************************************************************
          !   begin body of subroutine  SHOWGRID

          CALL M3MSG2( TEXT )
          WRITE( MESG, '( A20, 2( :, I10, 2X, A ) )' )
     &           'Dimensions', NCOLS, 'columns', NROWS, 'rows'
          CALL M3MSG2( MESG )
          WRITE( MESG, '( A20, 2( :, 2X, 1PE24.17 ) )' )
     &           'Lower-Left corner X Y', XORIG, YORIG
          CALL M3MSG2( MESG )
          WRITE( MESG, '( A20, 2( :, 2X, 1PE24.17 ) )' )
     &           'Cellsize DX DY', XCELL, YCELL
          CALL M3MSG2( MESG )

          CALL M3MSG2( ' ' )

          RETURN

          END SUBROUTINE SHOWGRID


        END PROGRAM PROJTOOL
