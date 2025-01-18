
PROGRAM WRFGRIDDESC

    !!***************************************************************
    !! Version "$Id: wrfgriddesc.f90 1 2017-06-10 18:05:20Z coats $"
    !! EDSS/Models-3 M3TOOLS.
    !! Copyright (c) 2010 Baron Advanced Meteorological Systems
    !! and (C) 2015 UNC Institute for the Environment.
    !! Distributed under the GNU GENERAL PUBLIC LICENSE version 2
    !! See file "GPL.txt" for conditions of use.
    !!..............................................................
    !!  Program body starts at line  55
    !!
    !!  DESCRIPTION:
    !!      Read LL gridded input data, and interpolate to a finer LL grid
    !!      
    !!  PRECONDITIONS:
    !!      Output-grid is coarser than the input grid
    !!      output grid using a resampling algorithm.
    !!
    !!  REVISION  HISTORY:
    !!      Prototype  4/2009 by Carlie J. Coats, Jr., BAMS
    !!
    !!      Version  11/2015 by CJC for I/O API v3.2 M3TOOLS.
    !!***************************************************************

    USE M3UTILIO
    USE MODWRFIO

    IMPLICIT NONE

    !!......  PARAMETERS and their descriptions:


    CHARACTER*16, PARAMETER ::  PNAME = 'WRFGRIDDESC'
    CHARACTER*1 , PARAMETER ::  QUOTE = "'"
    CHARACTER*1 , PARAMETER ::  BLANK = ' '
    CHARACTER*64, PARAMETER ::  BAR   = &
  '-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-'


    !!......  LOCAL VARIABLES and their descriptions:

    INTEGER :: LDEV, GDEV, ISTAT
    LOGICAL :: EFLAG = .FALSE.
    
    CHARACTER*16    CRDNAME, CROGRID, DOTGRID, STXGRID, STYGRID

    CHARACTER*256   MESG


    !!--------------------------------------------------------------
    !!   begin body of program WRFGRIDDESC

    LDEV  = INIT3()

    WRITE( LDEV, '( 5X, A )' )                                              &
'Program WRFGRIDDESC to read data from a WRF-netCD-output file, and',       &
'create a GRIDDESC file for the grids it contains.',                        &
'',                                                                         &
'PRECONDITIONS REQUIRED:',                                                  &
'   setenv WRFFILE    <path name for  input WRF-netcdf file>',              &
'   setenv OUTDESC    <path name for output GRIDDESC file>',                &
'   setenv CRDNAME    <GRIDDESC name for WRF coordinate system>',           &
'   setenv CROGRID    <GRIDDESC name for     cross-point grid>',            &
'   setenv DOTGRID    <GRIDDESC name for       dot-point grid>',            &
'   setenv STXGRID    <GRIDDESC name for X-stagger-point grid>',            &
'   setenv STYGRID    <GRIDDESC name for Y-stagger-point grid>',            &
'',                                                                         &
'Program copyright (C) 2010 Baron Advanced Meteorological Systems, LLC.,',  &
'and (C) 2015 UNC Institute for the Environment.',                          &
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
'Program version:',                                                         &
'$Id: wrfgriddesc.f90 1 2017-06-10 18:05:20Z coats $',&
''

    IF ( .NOT. GETYN( 'Continue with program?', .TRUE. ) ) THEN
        MESG = 'Program terminated at user request'
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
    END IF


    !!...............  Get environment

    CALL ENVSTR( 'CRDNAME', 'Coordinate system name for WRF grids', 'WRF_FOO', CRDNAME, ISTAT )
    IF ( ISTAT .GT. 0 ) THEN
        EFLAG = .TRUE.
        CALL M3MESG( 'Bad environment variable "CRDNAME"' )
    END IF

    CALL ENVSTR( 'CROGRID', 'Cross-point ("mass-point") WRF grid', 'WRF_FOO_CRO', CROGRID, ISTAT )
    IF ( ISTAT .GT. 0 ) THEN
        EFLAG = .TRUE.
        CALL M3MESG( 'Bad environment variable "CROGRID"' )
    END IF

    CALL ENVSTR( 'DOTGRID', 'Dot-point WRF grid', 'WRF_FOO_DOT', DOTGRID, ISTAT )
    IF ( ISTAT .GT. 0 ) THEN
        EFLAG = .TRUE.
        CALL M3MESG( 'Bad environment variable "DOTGRID"' )
    END IF

    CALL ENVSTR( 'STXGRID', 'X-stagger-point ("U-point") WRF grid', 'WRF_FOO_STX', STXGRID, ISTAT )
    IF ( ISTAT .GT. 0 ) THEN
        EFLAG = .TRUE.
        CALL M3MESG( 'Bad environment variable "STXGRID"' )
    END IF

    CALL ENVSTR( 'STYGRID', 'Y-stagger-point ("V-point") WRF grid', 'WRF_FOO_STY', STYGRID, ISTAT )
    IF ( ISTAT .GT. 0 ) THEN
        EFLAG = .TRUE.
        CALL M3MESG( 'Bad environment variable "STYGRID"' )
    END IF


    !!...............  Get WRF file data:

    IF ( .NOT.OPENWRF( 'WRFFILE', BLANK, FSREAD3 ) ) THEN
        EFLAG = .TRUE.
        CALL M3MESG( 'Error(s) processing WRF file  "WRFFILE"' )
    END IF


    !!...............  Open output GRIDDESC file:

    GDEV = GETEFILE( 'OUTDESC', .FALSE., .TRUE., PNAME )
    IF ( GDEV .LT. 0 ) THEN
        EFLAG = .TRUE.
        CALL M3MESG( 'Could not open  "OUTDESC"' )
    END IF


    IF ( EFLAG ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'Error(s) setting up program', 2 )
    END IF


    !!...............  Write GRIDDESC file:

    WRITE( GDEV, '( 1X, A ) ' ) '!  coords --line:  name; type,  P-alpha, P-beta, P-gamma, xcent, ycent'

    WRITE( GDEV, '( 1X, 3A ) ' ) QUOTE, TRIM( CRDNAME ), QUOTE

    WRITE( GDEV, '( 1X, I5, 5( 2X, 1PD15.7 ) ) ' ) GDTYP1, P_ALP1, P_BET1, P_GAM1, XCENT1, YCENT1

    WRITE( GDEV, '( 1X, A ) ' )  BLANK

    WRITE( GDEV, '( 1X, 4A ) ' )       &
        QUOTE, BLANK, QUOTE, ' !  end coords.  grids:  name; xorig, yorig, xcell, ycell, ncols, nrows, nthik'

    WRITE( GDEV, '( 1X, A ) ' )  BLANK
    
    WRITE( GDEV, '( 1X, 3A ) ' ) QUOTE, TRIM( CROGRID ), QUOTE

    WRITE( GDEV, '( 1X, 3A, 4(1X, 1PE15.7), 3(1X, I5) )' )     &
        QUOTE, TRIM( CRDNAME ), QUOTE, XORIG1, YORIG1, XCELL1, YCELL1, NCOLS1, NROWS1, 1

    WRITE( GDEV, '( 1X, 3A ) ' ) QUOTE, TRIM( DOTGRID ), QUOTE

    WRITE( GDEV, '( 1X, 3A, 4(1X, 1PE15.7), 3(1X, I5) )' )     &
        QUOTE, TRIM( CRDNAME ), QUOTE, XORIG1-0.5D0*XCELL1, YORIG1-0.5D0*YCELL1, XCELL1, YCELL1, NCOLS1+1, NROWS1+1, 1

    WRITE( GDEV, '( 1X, 3A ) ' ) QUOTE, TRIM( STXGRID ), QUOTE

    WRITE( GDEV, '( 1X, 3A, 4(1X, 1PE15.7), 3(1X, I5) )' )     &
        QUOTE, TRIM( CRDNAME ), QUOTE, XORIG1-0.5D0*XCELL1, YORIG1, XCELL1, YCELL1, NCOLS1+1, NROWS1, 1

    WRITE( GDEV, '( 1X, 3A ) ' ) QUOTE, TRIM( STYGRID ), QUOTE

    WRITE( GDEV, '( 1X, 3A, 4(1X, 1PE15.7), 3(1X, I5) )' )     &
        QUOTE, TRIM( CRDNAME ), QUOTE, XORIG1, YORIG1-0.5D0*YCELL1, XCELL1, YCELL1, NCOLS1, NROWS1+1, 1

    WRITE( GDEV, '( 1X, 5A ) ' ) QUOTE, BLANK, QUOTE, ' !  end grids'


    CALL M3EXIT( PNAME, 0, 0, 'Success in program', 0 )


END PROGRAM WRFGRIDDESC

