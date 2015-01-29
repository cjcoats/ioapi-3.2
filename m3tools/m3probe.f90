
PROGRAM M3PROBE

    !!***************************************************************
    !!  Version "$Id: m3probe.f90 101 2015-01-16 16:52:50Z coats $"
    !!   EDSS/Models-3 M3TOOLS.
    !!   Copyright (C) 2010 Baron Advanced Meteorological Systems. LLC., and
    !!   (C) 2015 UNC Institute for the Environment.
    !!   Distributed under the GNU GENERAL PUBLIC LICENSE version 2
    !!   See file "GPL.txt" for conditions of use.
    !!..............................................................
    !!  program body starts at line  87
    !!
    !!  DESCRIPTION:
    !!      See splash screen
    !!
    !!  PRECONDITIONS:
    !!      REQUIRES 01/09/2013 or later version of M3UTILIO
    !!      See splash screen
    !!
    !!  REVISION  HISTORY:
    !!      Prototype  01/20012 by Carlie J. Coats, Jr., BAMS.
    !!      Version    01/2013 by CJC:   Use LASTTIME to compute EDATE:ETIME
    !!      Version    03/2013 by CJC:   increase MXPNT from 20 to 256
    !!      Version    01/2015 by CJC:   Changes for I/O API-3.2;
    !!      better error-checking, support for BNDARY3 files.
    !!***************************************************************

    USE M3UTILIO
    IMPLICIT NONE


    !!......  PARAMETERS and their descriptions:

    INTEGER,      PARAMETER ::  MXPNT = 256

    CHARACTER*16, PARAMETER ::  PNAME = 'M3PROBE'
    CHARACTER*16, PARAMETER ::  BLANK = ' '
    CHARACTER*64, PARAMETER ::  BAR   = &
    '-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-'


    !!......  LOCAL VARIABLES and their descriptions:

    INTEGER         LDEV, ISTAT, IMISS
    INTEGER         V, K, N, C, R, L
    INTEGER         K1, K2, K3, K4, C0, R0, NC, NR
    INTEGER         NMAX, UMAX
    INTEGER         RDEV                !  for ${REPORT}
    INTEGER         SDATE, STIME, EDATE, ETIME, NRECS, NVARS
    INTEGER         JDATE, JTIME, TSTEP

    INTEGER         NPNTS
    INTEGER         PNTS( MXPNT )
    CHARACTER*16    VNAME

    LOGICAL         EFLAG
    CHARACTER*256   MESG

    CHARACTER*72    PROMPT
    CHARACTER*144   MENU( MXVARS3 )

    !!     GRIDDESC name, parameters for output grid

    CHARACTER*16    GDNAM2
    INTEGER         FTYPE2
    INTEGER         GDTYP2
    INTEGER         NCOLS2
    INTEGER         NROWS2
    INTEGER         NLAYS2
    INTEGER         NTHIK2
    INTEGER         NVARS2
    INTEGER         NSIZE2
    REAL*8          P_ALP2
    REAL*8          P_BET2
    REAL*8          P_GAM2
    REAL*8          XCENT2
    REAL*8          YCENT2
    REAL*8          XORIG2
    REAL*8          YORIG2
    REAL*8          XCELL2
    REAL*8          YCELL2
    INTEGER         SDATE2
    INTEGER         STIME2
    INTEGER         TSTEP2
    INTEGER         EDATE2
    INTEGER         ETIME2

    REAL, ALLOCATABLE :: RBUF( : )

    !!--------------------------------------------------------------
    !!   begin body of program M3PROBE

    LDEV  = INIT3()
    EFLAG = .FALSE.

    WRITE( LDEV, '( 5X, A )' )  BLANK, BAR,                                 &
'Program M3PROBE to extract time series for a specified variable for a',    &
'specified set of points from a GRIDDED, BOUNDARY, or CUSTOM input file,',  &
'and write the resulting time series as columns to a (human-readable)',     &
'ASCII REPORT-file.',                                                       &
'',                                                                         &
'PRECONDITIONS REQUIRED:',                                                  &
'    setenv INFILE     <path name for  input gridded file>',                &
'    setenv REPORT     <path name for output ASCII file>',                  &
'',                                                                         &
'    at most 256 points in the list.',                                      &
'',                                                                         &
'THE PROGRAM WILL PROMPT YOU for starting and ending date time for the',    &
'report period, variable, and grid/vector coordinates for the selected',    &
'set of points.',                                                           &
'',                                                                         &
'See URL',                                                                  &
'',                                                                         &
'   https://www.cmascenter.org/ioapi/documentation/3.1/html/AA.html#tools', &
'',                                                                         &
'Copyright (C) 2012 Baron Advanced Meteorological Systems, LLC.,',          &
'(C) 2015 UNC Institute for the Environment.',                              &
'Released under Version 2 of the GNU General Public License. See',          &
'enclosed GPL.txt, or URL',                                                 &
''  ,                                                                       &
'    https://www.gnu.org/licenses/old-licenses/gpl-2.0.html',               &
''  ,                                                                       &
'Comments and questions are welcome and can be sent to'  ,                  &
'',                                                                         &
'    Carlie J. Coats, Jr.    cjcoats@email.unc.edu',                        &
'    UNC Institute for the Environment',                                    &
'    100 Europa Dr., Suite 490 Rm 405',                                     &
'    Campus Box 1105',                                                      &
'    Chapel Hill, NC 27599-1105',                                           &
'',                                                                         &
'Program version: ',                                                        &
'$Id: m3probe.f90 101 2015-01-16 16:52:50Z coats $',&
''

    IF ( .NOT. GETYN( 'Continue with program?', .TRUE. ) ) THEN
        MESG = 'Program terminated at user request'
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
    END IF


    !!...............  Open files:

    IF ( .NOT.OPEN3( 'INFILE', FSREAD3, PNAME ) ) THEN
        EFLAG = .TRUE.
        MESG  = 'ERROR:  Could not open "INFILE"'
        CALL M3MESG( MESG )
    ELSE IF ( .NOT.DESC3( 'INFILE' ) ) THEN
        EFLAG = .TRUE.
        MESG  = 'ERROR:  Could not get description for "INFILE"'
        CALL M3MESG( MESG )
    ELSE

        GDNAM2 = GDNAM3D
        FTYPE2 = FTYPE3D
        GDTYP2 = GDTYP3D
        NCOLS2 = NCOLS3D
        NROWS2 = NROWS3D
        NLAYS2 = NLAYS3D
        NTHIK2 = NTHIK3D
        NVARS2 = NVARS3D
        GDTYP2 = GDTYP3D
        P_ALP2 = P_ALP3D
        P_BET2 = P_BET3D
        P_GAM2 = P_GAM3D
        XCENT2 = XCENT3D
        YCENT2 = YCENT3D
        XORIG2 = XORIG3D
        YORIG2 = YORIG3D
        XCELL2 = XCELL3D
        YCELL2 = YCELL3D

        SDATE2 = SDATE3D
        STIME2 = STIME3D
        TSTEP2 = TSTEP3D

        CALL LASTTIME( SDATE2, STIME2, TSTEP2, MXREC3D, EDATE2, ETIME2 )

        IF      ( FTYPE2 .EQ. CUSTOM3 ) THEN
            NSIZE2 = NCOLS2 * NLAYS2
        ELSE IF ( FTYPE2 .EQ. GRDDED3 ) THEN
            NSIZE2 = NCOLS3D * NROWS2 * NLAYS2
        ELSE IF ( FTYPE2 .EQ. BNDARY3 ) THEN
            NSIZE2 = 2 * ( NCOLS2 + NROWS2 + 2 * NTHIK2 ) * NLAYS2 * NTHIK2
            K1     = ( NCOLS2 + NTHIK2 ) * NTHIK2
            K2     = K1 + ( NROWS2 + NTHIK2 ) * NTHIK2
            K3     = K1 + K2
            K4     = K3 + ( NROWS2 + NTHIK2 ) * NTHIK2
        ELSE
            EFLAG = .TRUE.
            WRITE( MESG, '( A, I10 )' )  'Unsupported file type', FTYPE2
            CALL M3MESG( MESG )
        END IF

        ALLOCATE( RBUF( NSIZE2 ),  STAT = ISTAT )
        IF ( ISTAT .NE. 0 ) THEN
            WRITE( MESG, '( A, I10 )' ) 'ERROR:  Allocation failure for work arrays:  STATUS=', ISTAT
            EFLAG = .TRUE.
            CALL M3MESG( MESG )
        END IF

        NMAX = 0
        UMAX = 0
        DO V = 1, NVARS2
            NMAX = MAX( NMAX, LEN_TRIM( VNAME3D( V ) ) )
            UMAX = MAX( UMAX, LEN_TRIM( UNITS3D( V ) ) )
        END DO

        DO V = 1, NVARS2
            MENU( V )(            1:NMAX       ) = VNAME3D( V )( 1:NMAX )
            MENU( V )( NMAX+1     :NMAX+2      ) = '('
            MENU( V )( NMAX+3     :NMAX+UMAX+2 ) = UNITS3D( V )( 1:UMAX )
            MENU( V )( NMAX+UMAX+3:NMAX+UMAX+5 ) = ') :'
            MENU( V )( NMAX+UMAX+6:            ) = VDESC3D( V )
            MENU( V )(          70:         72 ) = '...'
        END DO

    END IF              !  if not.open3(INFILE...); else...


    RDEV = GETEFILE( 'REPORT', .FALSE., .TRUE., PNAME )
    IF ( RDEV .LT. 0 ) THEN
        EFLAG = .TRUE.
        CALL M3MESG( 'ERROR:  Could not open file "REPORT"'  )
    END IF


    IF ( EFLAG ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'Fatal file-related error(s)', 2 )
    END IF


    !!...............  Get date&time, probe-point specs:

    IF ( TSTEP2 .GT. 0 ) THEN
        SDATE = GETNUM( SDATE2, EDATE2, SDATE2, 'Enter starting date  for ${REPORT}' )
        STIME = GETNUM(      0, 999999, STIME2, 'Enter starting time  for ${REPORT}' )
        EDATE = GETNUM( SDATE , EDATE2, EDATE2, 'Enter ending   date  for ${REPORT}' )
        ETIME = GETNUM(      0, 999999, ETIME2, 'Enter ending   time  for ${REPORT}' )
        TSTEP = GETNUM( TSTEP2, 999999, TSTEP2, 'Enter     time step  for ${REPORT}'     )
        NRECS = CURREC( EDATE, ETIME, SDATE, STIME, TSTEP, JDATE, JTIME )
    ELSE
        SDATE = 0
        STIME = 0
        TSTEP = 0
        NRECS = 1
    END IF

    CALL M3MESG( BLANK )
    CALL M3MESG( 'Available variables are:' )
    CALL M3PARAG( NVARS2, MENU )
    CALL GETSTR( 'Enter name of requested variable', VNAME3D( 1 ), VNAME )
    V = INDEX1( VNAME, NVARS2, VNAME3D )
    IF ( V .LE. 0 ) THEN
        EFLAG = .TRUE.
        MESG  = 'ERROR:  Variable "' // TRIM( VNAME ) // '" not available from "INFILE"'
        CALL M3MESG( MESG )
    ELSE IF ( VTYPE3D(V) .NE. M3REAL ) THEN
        EFLAG = .TRUE.
        MESG  = 'ERROR:  Variable "' // TRIM( VNAME ) // '" not of type REAL'
        CALL M3MESG( MESG )
    END IF

    NPNTS = 0
    CALL M3MESG( BLANK )
    CALL M3MESG( 'Now enter list of points to be probed' )

    DO K = 1, MXPNT

        IF      ( FTYPE2 .EQ. GRDDED3 ) THEN

            C = GETNUM( 0, NCOLS2, 1, 'Enter column for this point, or 0 to end list' )
            IF ( C .EQ. 0 )  EXIT
            R = GETNUM( 1, NROWS2, 1, 'Enter row    for this point' )
            IF ( NLAYS2 .GT. 1 ) THEN
                L = GETNUM( 1, NLAYS2, 1, 'Enter layer  for this point' )
            ELSE
                L = 1
            END IF
            NPNTS = K
            PNTS( K ) = ( L-1 )*NCOLS2*NROWS2 + ( R-1 )*NCOLS2 + C

        ELSE IF ( FTYPE2 .EQ. BNDARY3 ) THEN

            C = GETNUM( 1-NTHIK2, NCOLS2+NTHIK2, 1, 'Enter column for this point, or 0 to end list' )
            IF ( C .EQ. 0 )  EXIT
            R = GETNUM( 1-NTHIK2, NROWS2+NTHIK2, 1, 'Enter row    for this point' )
            IF ( NLAYS2 .GT. 1 ) THEN
                L = GETNUM( 1, NLAYS2, 1, 'Enter layer  for this point' )
            ELSE
                L = 1
            END IF
            NPNTS = K
            IF ( C .GT. 1 .AND. C .LE. NCOLS2 .AND. R .GE. 1 .AND. R .LE. NROWS2  ) THEN
                EFLAG = .TRUE.
                WRITE( MESG, '( A, I6, 2X, A, I6 )' )  'ERROR:  Non-boundary point:  C=', C, 'R=', R
                CALL M3MESG( MESG )
            ELSE IF ( C .LE. 0 .AND. R .GE. 1 ) THEN        !!  south boundary
                C0 = 1 - NTHIK2
                R0 = 1
                NC = NCOLS2 + NTHIK2
                NR = NTHIK2
                PNTS( K ) = ( L-1 )*K4 + ( R-R0 )*NC + C - C0 + 1
            ELSE IF ( R .LE. 0 .AND. R .LE. NROWS2 ) THEN   !!  west  boundary
                C0 = 1 - NTHIK2
                R0 = 1 - NTHIK2
                NC = NTHIK2
                NR = NROWS2 + NTHIK2
                PNTS( K ) = ( L-1 )*K4 + K3 + ( R-R0 )*NC + C - C0 + 1
            ELSE IF ( C .GT. NCOLS2 ) THEN                  !!  east  boundary
                C0 = NCOLS2 + 1
                R0 = 1
                NC = NTHIK2
                NR = NROWS2 + NTHIK2
                PNTS( K ) = ( L-1 )*K4 + K1 + ( R-R0 )*NC + C - C0 + 1
            ELSE IF ( R .GT. NROWS2 ) THEN                  !!  north boundary
                C0 = 1 - NTHIK2
                R0 = NROWS2 + 1
                NC = NCOLS2 + NTHIK2
                NR = NTHIK2
                PNTS( K ) = ( L-1 )*K4 + K2 + ( R-R0 )*NC + C - C0 + 1
            END IF

        ELSE IF ( FTYPE2 .EQ. CUSTOM3 ) THEN

            C = GETNUM( 0, NCOLS2, 1, 'Enter index for this point, or 0 to end list' )
            IF ( C .EQ. 0 )  EXIT
            IF ( NLAYS2 .GT. 1 ) THEN
                L = GETNUM( 1, NLAYS2, 1, 'Enter layer  for this point' )
            ELSE
                L = 1
            END IF
            NPNTS = K
            PNTS( K ) = ( L-1 )*NCOLS2 + C

        END IF          !!  if gridded; else if custom

    END DO


    IF ( NPNTS .EQ. 0 ) THEN
        CALL M3MESG( 'ERROR:  No points entered' )
    END IF
    IF ( EFLAG) THEN
        CALL M3EXIT( PNAME, 0, 0, 'ERROR:  Bad set-up', 2 )
    END IF


    !!...............  Processing loop

    CALL M3MESG( BAR )
    JDATE = SDATE
    JTIME = STIME

    DO N = 1, NRECS

        WRITE( MESG, '( A, I9.7, A, I6.6 )' ) 'Processing', JDATE, ':', JTIME
        CALL M3MESG( MESG )

        IF ( .NOT.READ3( 'INFILE', VNAME, ALLAYS3, JDATE, JTIME, RBUF ) ) THEN
            EFLAG = .TRUE.
        ELSE
            WRITE( RDEV, '( I9.7, A, I6.6, 256( 2X, 1PE14.6, : ) )' )     &
                JDATE, ':', JTIME, ( RBUF( PNTS( K ) ), K = 1, NPNTS )
        END IF

        CALL NEXTIME( JDATE, JTIME, TSTEP )

    END DO          !!  end loop on time steps for this variable


    IF ( EFLAG ) THEN
        MESG  = 'Failure in program'
        ISTAT = 2
    ELSE
        MESG  = 'Success in program'
        ISTAT = 0
    END IF

    CALL M3EXIT( PNAME, 0, 0, MESG, ISTAT )


END PROGRAM M3PROBE


