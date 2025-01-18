
PROGRAM M3TOTXT

    !!***************************************************************
    !!  Version "$Id: m3totxt.f90 1 2017-06-10 18:05:20Z coats $"
    !!   EDSS/Models-3 M3TOOLS.
    !!   Copyright (C) 1992-2002 MCNC,
    !!   (C) 1995-2002,2005-2013 Carlie J. Coats, Jr.,
    !!   (C) 2002-2010 Baron Advanced Meteorological Systems. LLC., and
    !!   (C) 2015 UNC Institute for the Environment.
    !!   Distributed under the GNU GENERAL PUBLIC LICENSE version 2
    !!   See file "GPL.txt" for conditions of use.
    !!..............................................................
    !!  program body starts at line  84
    !!
    !!  DESCRIPTION:
    !!      See splash screen
    !!
    !!  PRECONDITIONS:
    !!      REQUIRES I/O API v3.2 or later
    !!
    !!      See splash screen
    !!
    !!  REVISION  HISTORY:
    !!      Prototype  04/2011 by CJC
    !!      Version    01/2013 by CJC:   Use LASTTIME to compute EDATE:ETIME
    !!      Version    01/2015 by CJC for I/O API-3.2:  Fortran90 "free"
    !!      source-format, generic ENVLIST(); support for INTEGER, INTEGER*8,
    !!      and REAL*8 variables using CONTAINed routines *GRID2ASC()
    !!      call RUNSPEC() to get SDATE:STIME:TSTEP:NRECS
    !!***************************************************************

    USE M3UTILIO
    IMPLICIT NONE


    !!......  PARAMETERS and their descriptions:

    CHARACTER*16, PARAMETER ::  PNAME = 'M3TOTXT'
    CHARACTER*16, PARAMETER::   BLANK = ' '
    CHARACTER*64, PARAMETER::   BAR   = &
    '-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-'
    CHARACTER*64, PARAMETER::   DASH  = &
    '- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -'


    !!......  LOCAL VARIABLES and their descriptions:

    INTEGER         LDEV, ISTAT, IMISS
    INTEGER         V, K, N, C, R
    INTEGER         RDEV                !  for ${REPORT}
    INTEGER         SDATE, STIME, EDATE, ETIME, NRECS, NVARS
    INTEGER         JDATE, JTIME, TSTEP
    INTEGER         COL0, COL1, ROW0, ROW1, ILAY
    CHARACTER*16    VNAMES( MXVARS3 )
    CHARACTER*16    VUNITS( MXVARS3 )
    CHARACTER*80    VDESCS( MXVARS3 )
    INTEGER         VTYPES( MXVARS3 )

    LOGICAL         EFLAG
    CHARACTER*256   MESG

    !!     GRIDDESC name, parameters for output grid

    CHARACTER*16    GDNAM2
    INTEGER         GDTYP2
    INTEGER         NCOLS2
    INTEGER         NROWS2
    INTEGER         NLAYS2
    INTEGER         TSTEP2
    REAL*8          P_ALP2
    REAL*8          P_BET2
    REAL*8          P_GAM2
    REAL*8          XCENT2
    REAL*8          YCENT2
    REAL*8          XORIG2
    REAL*8          YORIG2
    REAL*8          XCELL2
    REAL*8          YCELL2

    !!--------------------------------------------------------------
    !!   begin body of program M3TOTXT

    LDEV  = INIT3()
    EFLAG = .FALSE.

    WRITE( LDEV, '( 5X, A )' )  BLANK, BAR,                                 &
'Program M3TOTXT to write contents of a specified 1-layer window',          &
'into a GRIDDED  M3IO file for specified variables for a specified',        &
'time period to a (human-readable) formatted-ASCII REPORT-file.',           &
'',                                                                         &
'PRECONDITIONS REQUIRED:',                                                  &
'    setenv  INFILE    <path name for  input gridded file>',                &
'    setenv  REPORT    <path name for output ASCII file>',                  &
'',                                                                         &
'    setenv  VARLIST   <comma-list of variable names>',                     &
'',                                                                         &
'THE PROGRAM WILL PROMPT YOU for the time step and starting and',           &
'ending date & time for the report period, the layer, and the',             &
'column and row boundaries for the output window.',                         &
'',                                                                         &
'See URL',                                                                  &
'',                                                                         &
'   https://www.cmascenter.org/ioapi/documentation/3.1/html/AA.html#tools', &
'',                                                                         &
'Program copyright (C) 2015 UNC Institute for the Environment.',            &
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
'$Id: m3totxt.f90 1 2017-06-10 18:05:20Z coats $',&
''

    IF ( .NOT. GETYN( 'Continue with program?', .TRUE. ) ) THEN
        MESG = 'Program terminated at user request'
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
    END IF


    !!...............  Open files:

    IF ( .NOT.OPEN3( 'INFILE', FSREAD3, PNAME ) ) THEN
        EFLAG = .TRUE.
        MESG  = 'Could not open "INFILE"'
        CALL M3MESG( MESG )
    ELSE IF ( .NOT.DESC3( 'INFILE' ) ) THEN
        EFLAG = .TRUE.
        MESG  = 'Could not get description for "INFILE"'
        CALL M3MESG( MESG )
    ELSE

        GDNAM2 = GDNAM3D
        GDTYP2 = GDTYP3D
        NCOLS2 = NCOLS3D
        NROWS2 = NROWS3D
        NLAYS2 = NLAYS3D
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
        TSTEP2 = TSTEP3D

    END IF              !  if not.open3(INFILE...); else...


    RDEV = GETEFILE( 'REPORT', .FALSE., .TRUE., PNAME )
    IF ( RDEV .LT. 0 ) THEN
        EFLAG = .TRUE.
        CALL M3MESG( 'ERROR:  Could not open file "REPORT"' )
    END IF


    !!...............  Get VARLIST from environment:

    IF ( .NOT.ENVLIST( 'VARLIST',                                       &
                       'comma-list of variable names for ${REPORT}',    &
                       MXVARS3, NVARS, VNAMES ) ) THEN
        MESG  = 'Failure getting environment-list "VARLIST"'
        EFLAG = .TRUE.
        CALL M3MSG2( MESG )
    ELSE IF ( NVARS .LE. 0 ) THEN
        MESG  = 'ERROR:  Empty environment-list "VARLIST"'
        EFLAG = .TRUE.
        CALL M3MSG2( MESG )
    ELSE

        DO V = 1, NVARS

            K = INDEX1( VNAMES( V ), NVARS3D, VNAME3D )

            IF ( K .LE. 0 ) THEN
                EFLAG = .TRUE.
                MESG  = 'ERROR:  Variable "' // TRIM( VNAMES( V ) ) // '" not found in ${INFILE}'
                CALL M3MSG2( MESG )
            ELSE IF ( VTYPE3D( K ) .NE. M3REAL ) THEN
                EFLAG = .TRUE.
                MESG  = 'ERROR:  Variable "' // TRIM( VNAMES( V ) ) // '" not of type REAL'
                CALL M3MSG2( MESG )
            ELSE
                VUNITS( V ) = UNITS3D( K )
                VDESCS( V ) = VDESC3D( K )
                VTYPES( V ) = VTYPE3D( K )
            END IF

        END DO

    END IF


    !!...............  Get date&time, grid-window specs:

    CALL RUNSPEC( 'INFILE', .FALSE., SDATE, STIME, TSTEP, NRECS )

    COL0 = GETNUM(    1, NCOLS2, NCOLS2/2, 'Enter starting column for ${REPORT}' )
    COL1 = GETNUM( COL0, NCOLS2, COL0+1,   'Enter final    column for ${REPORT}' )
    ROW0 = GETNUM(    1, NROWS2, NROWS2/2, 'Enter starting row    for ${REPORT}' )
    ROW1 = GETNUM( ROW0, NROWS2, ROW0+1,   'Enter final    row    for ${REPORT}' )
    ILAY = GETNUM(    1, NLAYS2,      1,   'Enter layer           for ${REPORT}' )


    IF ( EFLAG ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'Fatal setup error(s)', 2 )
    END IF


    !!...............  Processing loop

    DO V = 1, NVARS

        IF ( NVARS .GT. 1 )  WRITE( RDEV, '( 1X, A )' ) BAR
        WRITE( RDEV, '( /, 5X, 5 A, /, 5X, A, / )' )    &
            'Variable "', TRIM( VNAMES( V ) ), '" (',   &
            TRIM( VUNITS( V ) ), ')', TRIM( VDESCS( V ) )

        JDATE = SDATE
        JTIME = STIME

        DO N = 1, NRECS

            IF      ( VTYPES(V) .EQ. M3DBLE ) THEN

                CALL DGRID2ASC( VNAMES(V), ILAY, JDATE, JTIME )

            ELSE IF ( VTYPES(V) .EQ. M3INT  ) THEN

                CALL IGRID2ASC( VNAMES(V), ILAY, JDATE, JTIME )

            ELSE IF ( VTYPES(V) .EQ. M3REAL ) THEN

                CALL RGRID2ASC( VNAMES(V), ILAY, JDATE, JTIME )

            ELSE IF ( VTYPES(V) .EQ. M3INT8 ) THEN

                CALL LGRID2ASC( VNAMES(V), ILAY, JDATE, JTIME )

            END IF

            CALL NEXTIME( JDATE, JTIME, TSTEP )

        END DO          !!  end loop on time steps for this variable

        IF ( NVARS .GT. 1 )  WRITE( RDEV, '( 1X, A )' ) BAR

    END DO              !!  end loop on variables


    IF ( EFLAG ) THEN
        MESG  = 'ERROR:  Failure in program'
        ISTAT = 2
    ELSE
        MESG  = 'Success in program'
        ISTAT = 0
    END IF

    CALL M3EXIT( PNAME, 0, 0, MESG, ISTAT )


CONTAINS    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    SUBROUTINE  DGRID2ASC( VNAME, LAYER, JDATE, JTIME )

        CHARACTER*(*), INTENT(IN   ) :: VNAME
        INTEGER      , INTENT(IN   ) :: LAYER, JDATE, JTIME

        INTEGER     C, R
        REAL*8      RBUF( NCOLS2,NROWS2 )

        !!...............   body  ......................................

        IF ( .NOT.READ3( 'INFILE', VNAME, LAYER, JDATE, JTIME, RBUF ) ) THEN
            EFLAG = .TRUE.
            RETURN
        END IF

        IF ( TSTEP2 .GT. 0 ) THEN
            WRITE( RDEV, '( 5X, 3A, 2X, I9.7, A, I6.6 )' )                  &
                '# Variable "', TRIM( VNAMES( V ) ), '"  Date&Time', JDATE, ':', JTIME
        ELSE
            WRITE( RDEV, '( 5X, 3 A )' ) 'Variable "', TRIM( VNAMES( V ) ), '"'
        END IF
        WRITE( RDEV, '( 1X, A, T15, 99999( I5, :, 10X ) )' )                &
            '# ROW\COL:', ( C, C = COL0, COL1 )

        DO R = ROW0, ROW1
            WRITE( RDEV, '( 1X, I5, T15, 99999( 1PE14.7, :, 1X ) )' )       &
                R, ( RBUF( C,R ), C = COL0, COL1 )
        END DO

        IF ( N .LT. NRECS )  WRITE( RDEV, '( 1X, A, / )' ) DASH

        RETURN

    END SUBROUTINE  DGRID2ASC


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    SUBROUTINE  IGRID2ASC( VNAME, LAYER, JDATE, JTIME )

        CHARACTER*(*), INTENT(IN   ) :: VNAME
        INTEGER      , INTENT(IN   ) :: LAYER, JDATE, JTIME

        INTEGER     C, R
        INTEGER     IBUF( NCOLS2,NROWS2 )

        !!...............   body  ......................................

        IF ( .NOT.READ3( 'INFILE', VNAME, LAYER, JDATE, JTIME, IBUF ) ) THEN
            EFLAG = .TRUE.
            RETURN
        END IF

        IF ( TSTEP2 .GT. 0 ) THEN
            WRITE( RDEV, '( 5X, 3A, 2X, I9.7, A, I6.6 )' )                  &
                '# Variable "', TRIM( VNAMES( V ) ), '"  Date&Time', JDATE, ':', JTIME
        ELSE
            WRITE( RDEV, '( 5X, 3 A )' ) 'Variable "', TRIM( VNAMES( V ) ), '"'
        END IF
        WRITE( RDEV, '( 1X, A, T15, 99999( I5, :, 10X ) )' )                &
            '# ROW\COL:', ( C, C = COL0, COL1 )

        DO R = ROW0, ROW1
            WRITE( RDEV, '( 1X, I5, T15, 99999( I14, :, 1X ) )' )           &
                R, ( IBUF( C,R ), C = COL0, COL1 )
        END DO

        IF ( N .LT. NRECS )  WRITE( RDEV, '( 1X, A, / )' ) DASH

        RETURN

    END SUBROUTINE  IGRID2ASC


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    SUBROUTINE  LGRID2ASC( VNAME, LAYER, JDATE, JTIME )

        CHARACTER*(*), INTENT(IN   ) :: VNAME
        INTEGER      , INTENT(IN   ) :: LAYER, JDATE, JTIME

        INTEGER     C, R
        INTEGER*8   IBUF( NCOLS2,NROWS2 )

        !!...............   body  ......................................

        IF ( .NOT.READ3( 'INFILE', VNAME, LAYER, JDATE, JTIME, IBUF ) ) THEN
            EFLAG = .TRUE.
            RETURN
        END IF

        IF ( TSTEP2 .GT. 0 ) THEN
            WRITE( RDEV, '( 5X, 3A, 2X, I9.7, A, I6.6 )' )                  &
                '# Variable "', TRIM( VNAMES( V ) ), '"  Date&Time', JDATE, ':', JTIME
        ELSE
            WRITE( RDEV, '( 5X, 3 A )' ) 'Variable "', TRIM( VNAMES( V ) ), '"'
        END IF
        WRITE( RDEV, '( 1X, A, T15, 99999( I5, :, 15X ) )' )                &
            '# ROW\COL:', ( C, C = COL0, COL1 )

        DO R = ROW0, ROW1
            WRITE( RDEV, '( 1X, I5, T15, 99999( I19, :, 1X ) )' )           &
                R, ( IBUF( C,R ), C = COL0, COL1 )
        END DO

        IF ( N .LT. NRECS )  WRITE( RDEV, '( 1X, A, / )' ) DASH

        RETURN

    END SUBROUTINE  LGRID2ASC


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    SUBROUTINE  RGRID2ASC( VNAME, LAYER, JDATE, JTIME )

        CHARACTER*(*), INTENT(IN   ) :: VNAME
        INTEGER      , INTENT(IN   ) :: LAYER, JDATE, JTIME

        INTEGER     C, R
        REAL        RBUF( NCOLS2,NROWS2 )

        !!...............   body  ......................................

        IF ( .NOT.READ3( 'INFILE', VNAME, LAYER, JDATE, JTIME, RBUF ) ) THEN
            EFLAG = .TRUE.
            RETURN
        END IF

        IF ( TSTEP2 .GT. 0 ) THEN
            WRITE( RDEV, '( 5X, 3A, 2X, I9.7, A, I6.6 )' )                  &
                '# Variable "', TRIM( VNAMES( V ) ), '"  Date&Time', JDATE, ':', JTIME
        ELSE
            WRITE( RDEV, '( 5X, 3 A )' ) 'Variable "', TRIM( VNAMES( V ) ), '"'
        END IF
        WRITE( RDEV, '( 1X, A, T15, 99999( I5, :, 10X ) )' )                &
            '# ROW\COL:', ( C, C = COL0, COL1 )

        DO R = ROW0, ROW1
            WRITE( RDEV, '( 1X, I5, T15, 99999( 1PE14.7, :, 1X ) )' )       &
                R, ( RBUF( C,R ), C = COL0, COL1 )
        END DO

        IF ( N .LT. NRECS )  WRITE( RDEV, '( 1X, A, / )' ) DASH

        RETURN

    END SUBROUTINE  RGRID2ASC


END PROGRAM M3TOTXT

