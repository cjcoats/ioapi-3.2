
PROGRAM PRESZ

    !!***********************************************************************
    !! Version "$Id:: presz.F 1703 2013-11-15 21:39:36Z coats@bdsl$"
    !! EDSS/Models-3 M3TOOLS.
    !! Copyright (C) 1992-2002 MCNC, (C) 1997-2013 Carlie J. Coats, Jr.,
    !! (C) 2002-2012 Baron Advanced Meteorological Systems. LLC., and
    !! (C) 2015 UNC Institute for the Environment.
    !! Distributed under the GNU GENERAL PUBLIC LICENSE version 2
    !! See file "GPL.txt" for conditions of use.
    !!.........................................................................
    !!  program body      starts at line   71
    !!  subroutine MAKEPZ starts at line  290
    !!
    !!  DESCRIPTION:
    !!       Builds multi-layer time-independent gridded file with
    !!       reference PRES, TA, and Z values.
    !!
    !!  PRECONDITIONS REQUIRED:
    !!       "setenv"s for output file, GRIDDESC file
    !!       "f90 presz.F -o presz -L/home/xcc/SunOS5 -lemstuff -lm3io -lnetcdf"
    !!       from a directory containing PARMS3.EXT, FDESC3.EXT, IODECL3.EXT
    !!
    !!  SUBROUTINES AND FUNCTIONS CALLED:
    !!       I/O API and utility routines; Lambert conversion routines from
    !!       libemstuff
    !!
    !!  REVISION  HISTORY:
    !!      prototype 7/1996 by CJC
    !!      Modified  9/1999 by CJC for enhanced portability
    !!      Version  11/2001 by CJC for I/O API Version 2.1
    !!      Version  11/2007 by CJC flash-screen/contact-info update
    !!      Version  02/2010 by CJC for I/O API v3.1:  Fortran-90 only;
    !!      USE M3UTILIO, and related changes.
    !!      Version  12/2013 by CJC:  PARAMETER CMENU(:)
    !!      Version  02/2015 by CJC for I/O API v3.2:  F90 free-format source,
    !!      use generics for "GET*()"
    !!***********************************************************************

    USE M3UTILIO

    IMPLICIT NONE

    !!.......   Parameter

    INTEGER, PARAMETER :: CTYPE( 6 ) =  &
        (/ LATGRD3, LAMGRD3, MERGRD3, STEGRD3, UTMGRD3, ALBGRD3 /)

    CHARACTER*16, PARAMETER :: NONE  = 'NONE'
    CHARACTER*16, PARAMETER :: PNAME = 'PRESZ'
    CHARACTER*20, PARAMETER :: CMENU( 6 ) = &
        (/  'lat-lon            ',          &   !  coordinate types menu item 1
            'Lambert Conformal  ',          &   !  coordinate types menu item 2
            'Mercator           ',          &   !  coordinate types menu item 3
            'Stereographic      ',          &   !  coordinate types menu item 4
            'UTM                ',          &   !  coordinate types menu item 5
            'Albers Equal-Area  '  /)           !  coordinate types menu item 6

    !!...........   LOCAL VARIABLES and their descriptions:

    INTEGER         L
    REAL            V
    REAL            P00, ZLP, PFAC, TS0, TLP      !!  constants defining MM5 reference atmosphere
    INTEGER         LOGDEV, ISTAT
    LOGICAL         EFLAG
    CHARACTER*16    ANAME
    CHARACTER*16    FNAME, TNAME
    CHARACTER*160   MESG

    !!***********************************************************************
    !!.......   First:  Initialize the I/O API:

    LOGDEV = INIT3()        !  initialization returns unit # for log
    EFLAG  = .FALSE.

    WRITE( *,'( 5X, A )' ) ' ',                                             &
'Program PRESZ to construct TIME-INDEPENDENT LAYERED GRIDDED I/O API',      &
' files containing an MM5-style reference atmosphere with layer-center',    &
'and layer-surface altitude and reference pressure, and layer-center',      &
'reference temperature, for a user specified coordinate system and grid.',  &
'',                                                                         &
'NOTE:  Currently, only hydrostatic Sigma-P vertical coordinate systems',   &
'and Lat-lon, Lambert, Mercator, Stereographic, and UTM horizontal',        &
'coordinate systems are supported.',                                        &
'',                                                                         &
'PRECONDITIONS REQUIRED:',                                                  &
'',                                                                         &
'    setenv <input terrain file>  <path-name, or "NONE">',                  &
'    setenv GRIDDESC              <path-name> (if no terrain file)',        &
'    setenv <output data   file>  <path-name>',                             &
'',                                                                         &
'Specifications for this grid may either come from a GRIDDESC file',        &
'(if it is a named grid), or may be entered interactively.',                &
'',                                                                         &
'THE PROGRAM WILL PROMPT YOU for the logical name of the terrain file,',    &
'or "NONE", the logical name of the output file, the reference pressure',   &
'P00 (millibars), reference sea-level temperature TS0 (K), andlapse rates', &
' ZLP (M/log(P)) and TLP (K/log(P)) for this standard atmosphere.',         &
'',                                                                         &
'If the terrain-file is "NONE", the program will prompt you for either',    &
'the GRIDDESC-name or the defining parameters of the output grid.',         &
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
'$$Id: presz.f90 1 2017-06-10 18:05:20Z coats $',&
' '

    IF ( .NOT. GETVAL( 'Continue with program?', .TRUE. ) ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'Program ended at user request', 0 )
    END IF

    MESG = 'Enter logical name for TERRAIN input file, or "NONE"'
    CALL GETSTR( MESG, 'INFILE', TNAME )

    MESG = 'Enter logical name for output file'
    CALL GETSTR( MESG, 'OUTFILE', FNAME )


    !!.......   If input file exists, open it, get its description, and re-use
    !!.......   the horizontal-grid part of its description:

    IF ( TNAME .NE. NONE ) THEN

        IF ( .NOT. OPEN3( TNAME, FSREAD3, PNAME ) ) THEN
            MESG = 'Could not open file "' // TRIM( TNAME ) // '" for input'
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
        ELSE IF ( .NOT. DESC3( TNAME ) ) THEN
            CALL M3EXIT( PNAME, 0, 0, 'Could not get terrain file description', 2 )
        END IF

    ELSE IF ( GETVAL( 'Specify grid by name from GRIDDESC file?', .TRUE. ) ) THEN

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
        GDTYP3D = CTYPE( GETVAL( 6, 2, 'Enter number for horizontal coordinate system type', CMENU ) )

        IF ( GDTYP3D .EQ. LATGRD3 ) THEN !  lat-lon:  no P_ALP, ...

            P_ALP3D = 0.0D0
            P_BET3D = 0.0D0
            P_GAM3D = 0.0D0
            XCENT3D = 0.0D0
            YCENT3D = 0.0D0

        ELSE IF ( GDTYP3D .EQ. LAMGRD3  .OR.    &
                  GDTYP3D .EQ. ALBGRD3  ) THEN      !!  Lambert or Albers conic projection

            P_ALP3D = GETVAL( -90.0D0, 90.0D0, 30.0D0, 'Enter secant angle     P_ALP' )
            P_BET3D = GETVAL( P_ALP3D, 90.0D0, 60.0D0, 'Enter secant angle     P_BET' )
            P_GAM3D = GETVAL( -180.0D0, 180.0D0, -90.0D0, 'Enter central meridian P_GAM' )
            XCENT3D = GETVAL( -180.0D0, 180.0D0, P_GAM3D, 'Enter X coord origin   XCENT' )
            YCENT3D = GETVAL(  -90.0D0,  90.0D0, 40.0D0, 'Enter Y coord origin   YCENT' )

        ELSE IF ( GDTYP3D .EQ. UTMGRD3 ) THEN       !!  UTM projection

            P_ALP3D = DBLE( GETVAL( 1, 60, 17, 'Enter UTM zone' ) )
            P_BET3D = 0.0D0
            P_GAM3D = 0.0D0
            XCENT3D = GETVAL( -999999999.0D0, 999999999.0D0, 0.0D0, 'Enter UTM offset XCENT' )
            YCENT3D = GETVAL( -999999999.0D0, 999999999.0D0, 0.0D0, 'Enter UTM offset YCENT' )

        ELSE

            CALL M3EXIT( PNAME, 0, 0, 'Only Lat-Lon, Lambert, UTM, and Albers supported', 2 )

        END IF  !  if descriptive angles relevant for this type

        NCOLS3D = GETVAL( 1, 999999999, 48, 'Enter number NCOLS of grid columns' )
        NROWS3D = GETVAL( 1, 999999999, 50, 'Enter number NROWS of grid rows' )
        NTHIK3D = GETVAL( 1, 999999999, 1, 'Enter bdy thickness NTHIK (cells)' )

        XCELL3D = GETVAL( 0.0D0, 9.0D36, 54000.0D0, 'Enter X cell size XCELL (meters)' )
        YCELL3D = GETVAL( 0.0D0, 9.0D36, XCELL3D, 'Enter Y cell size YCELL (meters)' )
        XORIG3D = GETVAL( -9.0D36, 9.0D36, XCELL3D*( DBLE( NCOLS3D ) - 0.5D0 ),    &
                     'Enter SW corner X coord for (1,1)-cell' )
        YORIG3D = GETVAL( -9.0D36, 9.0D36, YCELL3D*( DBLE( NROWS3D ) - 0.5D0 ),    &
                     'Enter SW corner Y coord for (1,1)-cell' )

    END IF      !  if specify horizontal grid by name, or interactively

    !!...........   PARAMETERS for MM5 reference hydrostatic atmosphere

    P00  = GETVAL(     0.0, 9999.0, 1012.5, 'Enter sea-level pressure    for atmosphere       (mb)' )
    ZLP  = GETVAL( BADVAL3,    0.0, -7.2E3, 'Enter Z-lapse rate          for atmosphere (M/log(P))' )
    TS0  = GETVAL(   200.0,  400.0,  290.0, 'Enter sea-level temperature for atmosphere       (mb)' )
    TLP  = GETVAL(     0.0,  100.0,   50.0, 'Enter T-lapse rate          for atmosphere (K/log(P))' )
    PFAC = 1.0 / ZLP


    !!.......   Now enter vertical coordinate structure:

    NLAYS3D = GETVAL( 1, MXLAYS3, 30, 'Enter number of layers' )
    VGTYP3D = VGSGPH3       ! hydrostatic sigma-P from PARMS3.EXT
    VGTOP3D = 100.0         ! model top (mb)

    VGLVS3D( 1 ) = GETVAL( 0.0, 1.0, 1.0, 'Enter sigma value for bottom of model')

    DO  L = 1, NLAYS3D
        WRITE( MESG, '( A, I3 )' ) 'Enter sigma value for top of layer', L
        V = 1.0 - ( FLOAT( L ) / FLOAT( NLAYS3D ) )**2
        VGLVS3D( L ) = GETVAL( 0.0, 1.0, V, MESG )
    END DO        !  end:  get horizontal grid specs.


    !!.......   Time step structure: zeros for time-independent file

    SDATE3D = 0
    STIME3D = 0
    TSTEP3D = 0

    !!.......   Variables and their descriptions; file description

    NVARS3D = 4
    VNAME3D( 1 ) = 'PRESH'
    UNITS3D( 1 ) = 'millibars'
    VDESC3D( 1 ) = 'layer-center reference pressure '
    VTYPE3D( 1 ) = M3REAL

    VNAME3D( 2 ) = 'PRESF'
    UNITS3D( 2 ) = 'millibars'
    VDESC3D( 2 ) = 'layer-top reference pressure '
    VTYPE3D( 2 ) = M3REAL

    VNAME3D( 3 ) = 'ZH'
    UNITS3D( 3 ) = 'meters'
    VDESC3D( 3 ) = 'layer-center elevation above terrain'
    VTYPE3D( 3 ) = M3REAL

    VNAME3D( 4 ) = 'ZF'
    UNITS3D( 4 ) = 'meters'
    VDESC3D( 4 ) = 'layer-top elevation above terrain'
    VTYPE3D( 4 ) = M3REAL

    VNAME3D( 5 ) = 'TA'
    UNITS3D( 5 ) = 'K'
    VDESC3D( 5 ) = 'layer-center reference temperature'
    VTYPE3D( 5 ) = M3REAL

    FTYPE3D      = GRDDED3
    FDESC3D      = ' '
    FDESC3D( 1 ) = 'MM5 reference atmosphere: pressures, temperatures, and altitudes'
    FDESC3D( 2 ) = 'Generated by sample program PRESZ'

    IF ( TNAME .EQ. NONE ) THEN
        FDESC3D( 3 ) = 'Computations are relative to reference sea level'
    ELSE
        FDESC3D( 3 ) = 'Computations are relative to terrain elevation'
    END IF
    WRITE( FDESC3D(4), '( A, 1PE13.6)' ) 'Reference sea level pressure (mb)   P00=', P00
    WRITE( FDESC3D(5), '( A, 1PE13.6)' ) 'Reference sea level temperature (K) TS0=', TS0
    WRITE( FDESC3D(6), '( A, 1PE13.6)' ) 'Reference Z-lapse rate (M/log(P)    ZLP=', ZLP
    WRITE( FDESC3D(7), '( A, 1PE13.6)' ) 'Reference T-lapse rate (K/log(P)    TLP=', TLP


    !!.......   Open file as "unknown" -- if it does not exist, create it;
    !!.......   else check header against description supplied in FDESC3.EXT;
    !!.......   open for output in any case.
    !!.......   Write reference-atmosphere constants as file-attributes.
    !!.......   Use subroutine MAKEPZ to allocate arrays for variables
    !!.......   PRES and Z, compute them, and write them to file FNAME.

    IF ( .NOT. OPEN3( FNAME, FSUNKN3, PNAME ) ) THEN

        EFLAG = .TRUE.
        MESG  = 'ERROR:  Could not open file "' // TRIM( FNAME ) // '" for output'
        CALL M3MESG( MESG )

    ELSE IF ( .NOT.WRATT3( FNAME, ALLVAR3, 'P00', M3REAL, 1, P00 ) ) THEN

        EFLAG = .TRUE.
        MESG  = 'ERROR:  Could not write attribute "P00" to "' // TRIM( FNAME ) // '"'
        CALL M3MESG( MESG )

    ELSE IF ( .NOT.WRATT3( FNAME, ALLVAR3, 'TS0', M3REAL, 1, TS0 ) ) THEN

        EFLAG = .TRUE.
        MESG  = 'ERROR:  Could not write attribute "TS0" to "' // TRIM( FNAME ) // '"'
        CALL M3MESG( MESG )

    ELSE IF ( .NOT.WRATT3( FNAME, ALLVAR3, 'ZLP', M3REAL, 1, ZLP ) ) THEN

        EFLAG = .TRUE.
        MESG  = 'ERROR:  Could not write attribute "ZLP" to "' // TRIM( FNAME ) // '"'
        CALL M3MESG( MESG )

    ELSE IF ( .NOT.WRATT3( FNAME, ALLVAR3, 'TLP', M3REAL, 1, TLP ) ) THEN

        EFLAG = .TRUE.
        MESG  = 'ERROR:  Could not write attribute "TLP" to "' // TRIM( FNAME ) // '"'
        CALL M3MESG( MESG )

    ELSE

        CALL MAKEPZ( FNAME , TNAME )    !  see below, in this file.

    END IF


    IF ( EFLAG ) THEN
        MESG  = 'Failure in program'
        ISTAT = 2
    ELSE
        MESG  = 'Success in program'
        ISTAT = 0
    END IF

    CALL M3EXIT( PNAME, 0, 0, MESG, ISTAT )


CONTAINS        !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    SUBROUTINE  MAKEPZ( FNAME , TNAME )

        IMPLICIT NONE

        !!...........   ARGUMENTS and their descriptions:

        CHARACTER*16, INTENT( IN ) :: FNAME   !  name of output file
        CHARACTER*16, INTENT( IN ) :: TNAME   !  name of input terrain file, or "NONE"

        !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

        REAL    HT   ( NCOLS3D, NROWS3D )
        REAL    PSFC ( NCOLS3D, NROWS3D )
        REAL    PRESH( NCOLS3D, NROWS3D, NLAYS3D )
        REAL    PRESF( NCOLS3D, NROWS3D, NLAYS3D )
        REAL    ZH   ( NCOLS3D, NROWS3D, NLAYS3D )
        REAL    ZF   ( NCOLS3D, NROWS3D, NLAYS3D )
        REAL    TA   ( NCOLS3D, NROWS3D, NLAYS3D )

        INTEGER         R, C, L         !  row, column, layer counters
        REAL            SH, SF, P, DP0       !  scratch variables
        CHARACTER*80    MESG


        !!***********************************************************************
        !!   begin body of subroutine  MAKEPZ

        IF ( TNAME .NE. NONE ) THEN      !  read HT; compute PSFC from HT

            IF ( .NOT. READ3( TNAME, 'HT', 1, 0, 0, HT ) ) THEN
                EFLAG = .TRUE.
                MESG  = 'Could not read "HT" from "' // TNAME
                CALL M3MESG( MESG )
                RETURN
            END IF

!$OMP       PARALLEL DO                                             &
!$OMP&          DEFAULT( NONE ),                                    &
!$OMP&           SHARED( NCOLS3D, NROWS3D, P00, PFAC, PSFC, HT ),   &
!$OMP&          PRIVATE( C, R )

            DO  R = 1, NROWS3D
            DO  C = 1, NCOLS3D
                PSFC( C, R ) = P00 * EXP( PFAC * HT( C,R ) )
            END DO
            END DO

        ELSE		!  set surface pressure to P00

!$OMP       PARALLEL DO                                     &
!$OMP&          DEFAULT( NONE ),                            &
!$OMP&           SHARED( NCOLS3D, NROWS3D, P00, PSFC ),     &
!$OMP&          PRIVATE( C, R )

            DO  R = 1, NROWS3D
            DO  C = 1, NCOLS3D
                HT  ( C, R ) = 0.0
                PSFC( C, R ) = P00
            END DO
            END DO

        END IF		!  if tname "NONE" or not

        DP0 = 1.0 / P00

!$OMP   PARALLEL DO                                                 &
!$OMP&      DEFAULT( NONE ),                                        &
!$OMP&       SHARED( NCOLS3D, NROWS3D, NLAYS3D, VGLVS3D, PSFC,      &
!$OMP&               PRESH, PRESF, ZH, ZF, DP0, ZLP, TS0, TLP ),    &
!$OMP&      PRIVATE( C, R, L, SH, SF, P )

        DO  L = 1, NLAYS3D

            SH = 0.5 * ( VGLVS3D( L ) + VGLVS3D( L+1 ) )
            SF = VGLVS3D( L+1 )

            DO  R = 1, NROWS3D
            DO  C = 1, NCOLS3D

                P = PSFC( C, R )
                PRESH( C, R, L ) = VGTOP3D + SH * ( P - VGTOP3D )
                PRESF( C, R, L ) = VGTOP3D + SF * ( P - VGTOP3D )

                P = 1.0 / P
                ZH   ( C, R, L ) = HT( C, R ) + ZLP * LOG( PRESH( C, R, L ) * P )
                ZF   ( C, R, L ) = HT( C, R ) + ZLP * LOG( PRESF( C, R, L ) * P )

                TA   ( C, R, L ) = TS0 + TLP*ALOG( PSFC( C,R ) * DP0 )

            END DO          !  end loop on cols C
            END DO         	!  end loop on rows R

        END DO		!  end loop on levels L


    !!.......   Write out results to file FNAME, then return:

        IF ( .NOT. WRITE3( FNAME, 'PRESH', 0, 0, PRESH ) ) THEN
            EFLAG = .TRUE. 
            MESG  = 'ERROR:  Writing "PRESH" to file "' // FNAME
            CALL M3MESG( MESG )
        END IF

        IF ( .NOT. WRITE3( FNAME, 'PRESF', 0, 0, PRESF ) ) THEN
            EFLAG = .TRUE. 
            MESG = 'ERROR:  Writing "PRESF" to file "' // FNAME
            CALL M3MESG( MESG )
        END IF

        IF ( .NOT. WRITE3( FNAME, 'ZH', 0, 0, ZH ) ) THEN
            EFLAG = .TRUE. 
            MESG = 'ERROR:  Writing "ZH" to file "' // FNAME
            CALL M3EXIT( 'PRESZ/MAKEPZ', 0, 0, MESG, 2 )
        END IF

        IF ( .NOT. WRITE3( FNAME, 'ZF', 0, 0, ZF ) ) THEN
            EFLAG = .TRUE. 
            MESG = 'ERROR:  Writing "ZF" to file "' // FNAME
            CALL M3MESG( MESG )
        END IF

        IF ( .NOT. WRITE3( FNAME, 'TA', 0, 0, TA ) ) THEN
            EFLAG = .TRUE. 
            MESG = 'ERROR:  Writing "TA" to file "' // FNAME
            CALL M3MESG( MESG )
        END IF

        RETURN

    END SUBROUTINE  MAKEPZ


END PROGRAM PRESZ

