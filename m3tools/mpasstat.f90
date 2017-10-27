
PROGRAM MPASSTAT

    !!***********************************************************************
    !!  Copyright (c) 2017 Carlie J. Coats, Jr.
    !!  Distributed under the GNU GENERAL PUBLIC LICENSE version 2
    !!  See file "GPL.txt" for conditions of use.
    !!.......................................................................
    !!
    !!  DESCRIPTION:
    !!      Compute statistics for a specified variable from an MPAS file,
    !!      optionally for a specified time period and/or layer-range.
    !!
    !!  REVISION  HISTORY:
    !!      Prototype  09/2017 by Carlie J. Coats, Jr., UNC IE
    !!
    !!.......................................................................

    USE MODMPASFIO
    USE MODNCFIO
    USE M3UTILIO

    IMPLICIT NONE

    !!...........   PARAMETERS and their descriptions:

    CHARACTER*1,  PARAMETER :: BLANK = ' '
    CHARACTER*80, PARAMETER :: BAR   = '-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-'
    CHARACTER*16, PARAMETER :: PNAME = 'MPASSTAT'

    !!...........   LOCAL VARIABLES and their descriptions:

    LOGICAL         EFLAG

    CHARACTER*256   MESG, RNAME

    INTEGER         ISTAT, LDEV, RDEV

    INTEGER         K, L, M, N, NN, P, Q, V, T

    INTEGER         SDATE, STIME
    INTEGER         EDATE, ETIME
    INTEGER         JDATE, JTIME

    INTEGER         NCELLS
    INTEGER         LAY0, LAY1, NLAYS
    INTEGER         REC0, REC1, NRECS

    CHARACTER*32    VNAME

    INTEGER         FVARS
    CHARACTER*32    VNAMES( MXVARS3 )         !!  data structures for DESCNCVAR()
    INTEGER         VTYPES( MXVARS3 )
    INTEGER         VNDIMS( MXVARS3 )
    INTEGER          VDIMS( 7,MXVARS3 )
    CHARACTER*32     DNAME( 7,MXVARS3 )
    CHARACTER*32     TNAME

    INTEGER              :: FSTEPS
    INTEGER, ALLOCATABLE :: FDATES( : )
    INTEGER, ALLOCATABLE :: FTIMES( : )

    REAL   , ALLOCATABLE :: WGRID1D( : )
    REAL   , ALLOCATABLE :: WGRID2D( :,: )

    REAL*8      FSUM, FSSQ
    REAL        FMAX, FMIN
    INTEGER     KMAX, KMIN, LMAX, LMIN, TMAX, TMIN


    !!***********************************************************************
    !!.......   First:  Initialize the I/O API:

    LDEV = INIT3()	!  initialization returns unit # for log
    EFLAG = .FALSE.

    WRITE( *, '( 5X, A )' ) BAR,                                                &
'Program "mpasstat" to compute statistics for the specified variable',          &
'for the for the specified layer range (if appropriate), and for the',          &
'specified time period (if appropriate), and write the result to the',          &
'program-log, or to a specified ASCII REPORT file.',                            &
'',                                                                             &
'PRECONDITIONS REQUIRED:',                                                      &
'    setenv  INFILE    <path name for  input MPAS-gridded file>',               &
'    setenv  REPORT    <path name for output REPORT file>',                     &
'                      or "LOG"',                                               &
'',                                                                             &
'    The requested variable must be of type REAL',                              &
'',                                                                             &
'    The requested variable must be subscripted by CELL, VERTEX, or',           &
'    EDGE (N), and optionally by TIME (T) and/or LAYER (L), in (Fortran)',      &
'    subscript order',                                                          &
'',                                                                             &
'        V( [L,] N [, T] )',                                                    &
'',                                                                             &
'THE PROGRAM WILL PROMPT YOU for the variable-name, and the starting',          &
'and ending date&time for the report period, and the layer range',              &
'(as appropriate).',                                                            &
'',                                                                             &
'Program copyright (C) 2017 Carlie J. Coats, Jr.',                              &
'Released under Version 2 of the GNU General Public License.',                  &
'See enclosed GPL.txt, or URL',                                                 &
''  ,                                                                           &
'    https://www.gnu.org/licenses/old-licenses/gpl-2.0.html',                   &
'', BAR, ''

    IF ( .NOT.INITMPGRID( 'INFILE' ) ) THEN
        CALL M3EXIT( PNAME, 0,0, 'Could not read header for "INFILE"', 2 )
    ELSE IF ( .NOT.DESCMPAS( 'INFILE', FSTEPS, FVARS, VNAMES,                   &
                             VTYPES, VNDIMS, VDIMS, DNAME ) ) THEN
        CALL M3EXIT( PNAME, 0,0, 'Could not read header for "INFILE"', 2 )
    END IF

    ALLOCATE( FDATES( FSTEPS ), FTIMES( FSTEPS ), STAT = ISTAT )
    IF ( ISTAT .NE. 0 ) THEN
        WRITE( MESG, '( A, I10 )' ) 'DATE&TIME buffer allocation failed:  STAT=', ISTAT
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
    END IF

    CALL M3MESG( 'The list of variables in this file is:' )
    DO  L = 1, FVARS
        WRITE( *, '( I3, ": ", A )' ) L, VNAMES( L )
    END DO
    WRITE( *, '(A)' ) ''
    V     = GETNUM( 1, FVARS, 1, 'Enter number for the variable to analyze' )
    VNAME = VNAMES( V )

    T = INDEX1( 'Time', VNDIMS( V ), DNAME( :,V ) )
    N = MAX( INDEX1( 'xtime', FVARS, VNAMES ) ,  &
             INDEX1( 'xTime', FVARS, VNAMES ) )

    IF ( VTYPES( V ) .NE. M3REAL  ) THEN

        CALL M3EXIT( PNAME, 0, 0, 'Variable not of type REEAL', 2 )

    ELSE IF ( T .LE. 0 ) THEN

        Call M3MESG( 'Variable "' // TRIM( VNAME ) // '" is time independent' )
        SDATE = IMISS3
        STIME = IMISS3
        EDATE = IMISS3
        ETIME = IMISS3

    ELSE IF ( N .LE. 0 ) THEN

        CALL M3EXIT( PNAME, 0, 0, 'Time-variable not found', 2 )

    ELSE IF ( .NOT.READMPSTEPS( 'INFILE', VNAMES( N ), NRECS, FSTEPS, FDATES, FTIMES ) )  THEN

        CALL M3EXIT( PNAME, 0, 0, 'Error readng time-variable "'//VNAMES( T )//'"', 2 )

    ELSE

        WRITE( MESG , '( 3A, I9.7, A, I6.6, A, I7.7, A, I6.6 )' )      &
            'Variable "', TRIM( VNAME ), '" has date&time range',      &
            FDATES(1), ':', FTIMES(1), ' -', FDATES(FSTEPS), ':', FTIMES(FSTEPS)
        Call M3MESG( MESG )

        SDATE = GETNUM( FDATES(1),    FDATES(FSTEPS), FDATES(1),      'Enter starting date (YYYYDDD)' )
        STIME = GETNUM( 0, 999999999, FTIMES(1),                      'Enter starting time  (HHMMSS)' )
        EDATE = GETNUM( SDATE,        FDATES(FSTEPS), FDATES(FSTEPS), 'Enter   ending date (YYYYDDD)' )
        ETIME = GETNUM( 0, 999999999, FTIMES(FSTEPS),                 'Enter   ending time  (HHMMSS)' )

        REC0  = FINDKEY( SDATE, STIME, FSTEPS, FDATES, FTIMES )
        REC1  = FINDKEY( EDATE, ETIME, FSTEPS, FDATES, FTIMES )
        NRECS = REC1 - REC0 + 1

        IF ( REC0 .LT. 1 .OR. REC1 .LT. 1 ) THEN
            CALL M3EXIT( PNAME, 0, 0, 'Requested dates&times not found', 2 )
        END IF

    END IF

    K = MAX( INDEX1( 'nCells',    VNDIMS( V ), DNAME( :,V ) ),  &
             INDEX1( 'nEdges',    VNDIMS( V ), DNAME( :,V ) ),  &
             INDEX1( 'nVertices', VNDIMS( V ), DNAME( :,V ) ) )
    IF ( K .LE. 0 ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'Variable not subscripted by CELL, EDGE, or VERTEX', 2 )
    ELSE IF ( K .EQ. 1 .AND. ( T .EQ. 0 .OR. T .EQ. 2 ) ) THEN
        NCELLS = VDIMS( K,V )
        NLAYS = 1
        LAY0  = 1
        LAY1  = 1
    ELSE IF ( K .EQ. 2 .AND. ( T .EQ. 0 .OR. T .EQ. 3 ) ) THEN
        NCELLS = VDIMS( 2,V )
        NLAYS  = VDIMS( 1,V )
        LAY0   = GETNUM(    1, NLAYS,     1, 'Enter start of layer range for analysis' )
        LAY1   = GETNUM( LAY0, NLAYS, NLAYS, 'Enter  end  of layer range for analysis' )
    ELSE
        CALL M3EXIT( PNAME, 0, 0, 'Bad subscripting for variable:  not ([LVL,]CELL[,TIME])', 2 )
    END IF


    !!...............   Allocate arrays:

    IF ( NLAYS .GT. 1 ) THEN
        ALLOCATE( WGRID2D( NLAYS, NCELLS ), STAT = ISTAT )
    ELSE
        ALLOCATE( WGRID1D( NCELLS ), STAT = ISTAT )
    END IF
    IF ( ISTAT .NE. 0 ) THEN
        WRITE( MESG, '( A, I10 )' ) 'Buffer allocation failed:  STAT=', ISTAT
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
    END IF


    !!...............   Open REPORT file:

    CALL ENVSTR( 'REPORT', 'GRIDDESC-name for CAMx grid', 'LOG', RNAME, ISTAT )
    IF ( ISTAT .GT. 0 ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'Bad environment variable "REPORT"', 2 )
    ELSE IF ( RNAME .EQ. 'LOG' ) THEN
        RDEV = LDEV
    ELSE
        RDEV = GETEFILE( 'REPORT', .FALSE., .TRUE., PNAME )
        IF ( RDEV .LT. 0 ) THEN
            CALL M3EXIT( PNAME, 0, 0, 'Failure opening REPORT file', 2 )
        END IF
    END IF


    !!...............   Process stats:  time independent or time dependent:

    FMAX =  BADVAL3     !!  accumulators for time aggregated statistics
    FMIN = -BADVAL3
    FSUM = 0.0D0
    FSSQ = 0.0D0

    IF ( SDATE .EQ. IMISS3 ) THEN

        IF ( NLAYS .EQ. 1 ) THEN

            IF ( .NOT.READMPAS( 'INFILE', VNAME, NCELLS, WGRID1D ) ) THEN
                EFLAG = .TRUE.
            ELSE
                CALL STAT1D( NCELLS, WGRID1D )
            END IF


        ELSE

            IF ( .NOT.READMPAS( 'INFILE', VNAME, NLAYS, NCELLS, WGRID2D ) ) THEN
                EFLAG = .TRUE.
            ELSE
                CALL STAT2D( NLAYS, NCELLS, LAY0, LAY1, WGRID2D )
            END IF

        END IF

    ELSE        !! time stepped case

        DO N = REC0, REC1

            JDATE = FDATES( N )
            JTIME = FTIMES( N )

            IF ( NLAYS .EQ. 1 ) THEN

                IF ( .NOT.READMPAS( 'INFILE', N, VNAME, NCELLS, WGRID1D ) ) THEN
                    EFLAG = .TRUE.
                ELSE
                    CALL STAT1DT( JDATE, JTIME, N, NCELLS, WGRID1D )
                END IF


            ELSE

                IF ( .NOT.READMPAS( 'INFILE', N, VNAME, NLAYS, NCELLS, WGRID2D ) ) THEN
                    EFLAG = .TRUE.
                ELSE
                    CALL STAT2DT( JDATE, JTIME, N, NLAYS, NCELLS, LAY0, LAY1, WGRID2D )
                END IF

            END IF

        END DO

        IF ( NLAYS .EQ. 1 ) THEN
            CALL STAT1DF()
        ELSE
            CALL STAT2DF()
        END IF

    END IF


    !!.......   Clean up and exit

    ! CALL SHUTMPGRID()

    IF ( EFLAG ) THEN
        MESG  = 'Failure(s) in program'
        ISTAT = 2
    ELSE
        MESG  = 'Successful completion of program'
        ISTAT = 0
    END IF
    CALL M3EXIT( PNAME, 0, 0, MESG, 0 )


CONTAINS


    SUBROUTINE STAT2DT( JDATE, JTIME, N, NLAYS, NCELLS, LAY0, LAY1, VBUF )

        INTEGER, INTENT(IN   ) :: JDATE, JTIME, N   !!  date&time, timestep-number
        INTEGER, INTENT(IN   ) :: NLAYS, NCELLS     !!  dimensions
        INTEGER, INTENT(IN   ) :: LAY0, LAY1        !!  layer-range
        REAL   , INTENT(IN   ) :: VBUF( NLAYS, NCELLS )

        REAL*8      VSUM, VSSQ, VBAR, VSIG, DIV
        REAL        VMAX, VMIN
        INTEGER     IMIN, IMAX, JMIN, JMAX

        INTEGER     I, J

        VSUM = 0.0D0
        VSSQ = 0.0D0
        VMAX = VBUF( LAY0,1 )
        VMIN = VBUF( LAY0,1 )
        IMAX = 1
        IMIN = 1
        JMAX = LAY0
        JMIN = LAY0

        DO I = 1, NCELLS
        DO J = LAY0, LAY1
            VSUM = VSUM + VBUF( K,L )
            VSSQ = VSSQ + VBUF( K,L )**2
            IF ( VBUF( K,L ) .GT. VMAX ) THEN
                VMAX = VBUF( K,L )
                IMAX = I
                JMAX = J
            ELSE IF ( VBUF( K,L ) .LT. VMIN ) THEN
                VMIN = VBUF( K,L )
                IMIN = I
                JMIN = J
            END IF
        END DO
        END DO

        IF ( N .EQ. 1 ) THEN
            FSUM = VSUM
            FSSQ = VSSQ
            FMAX = VMAX
            KMAX = IMAX
            LMAX = JMAX
            TMAX = N
            FMIN = VMIN
            KMIN = IMIN
            LMIN = JMIN
            TMIN = N

            WRITE( RDEV, '( 2X, A, 2X, A, /, 4 A16, 2 A10, A16, 2 A10)' )   &
                'Variable:', VNAME,                                         &
                'Date&Time', 'MEAN', 'SIGMA', 'MAX @', 'CELL', 'LAYER', 'MIN @', 'CELL', 'LAYER'

        ELSE
            FSUM = FSUM + VSUM
            FSSQ = FSSQ + VSSQ
            IF ( VMAX .GT. FMAX ) THEN
                FMAX = VMAX
                LMAX = JMAX
                KMAX = IMAX
                TMAX = N
            END IF
            IF ( VMIN .LT. FMIN ) THEN
                FMIN = VMIN
                LMIN = JMIN
                KMIN = IMIN
                TMIN = N
            END IF
        END IF

        DIV  = 1.0D0 / DBLE( NCELLS * ( LAY1 - LAY0 + 1 ) )
        VBAR = VSUM * DIV
        VSIG = SQRT( MAX( VSSQ * DIV - VBAR**2, 0.0D0 ) )
        WRITE( RDEV, '(I9.7, A, I6.6, 3( 2X, 1PD14.6 ), 2 I10, 2X, 1PD14.6, 2 I10 )' )     &
            JDATE, ':', JTIME, VBAR, VSIG, VMAX, IMAX, JMAX, VMIN, IMIN, JMIN

        RETURN

    END SUBROUTINE STAT2DT


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    SUBROUTINE STAT2D( NLAYS, NCELLS, LAY0, LAY1, VBUF )

        INTEGER, INTENT(IN   ) :: NLAYS, NCELLS      !!  dimensions
        INTEGER, INTENT(IN   ) :: LAY0, LAY1        !!  layer-range
        REAL   , INTENT(IN   ) :: VBUF( NLAYS, NCELLS )

        REAL*8      VSUM, VSSQ, VBAR, VSIG, DIV
        REAL        VMAX, VMIN
        INTEGER     IMIN, IMAX, JMIN, JMAX

        INTEGER     I, J

        VSUM = 0.0D0
        VSSQ = 0.0D0
        VMAX = VBUF( LAY0,1 )
        VMIN = VBUF( LAY0,1 )
        IMAX = 1
        IMIN = 1
        JMAX = LAY0
        JMIN = LAY0

        DO I = 1, NCELLS
        DO J = LAY0, LAY1
            VSUM = VSUM + VBUF( K,L )
            VSSQ = VSSQ + VBUF( K,L )**2
            IF ( VBUF( K,L ) .GT. VMAX ) THEN
                VMAX = VBUF( K,L )
                IMAX = I
                JMAX = J
            ELSE IF ( VBUF( K,L ) .LT. VMIN ) THEN
                VMIN = VBUF( K,L )
                IMIN = I
                JMIN = J
            END IF
        END DO
        END DO

        WRITE( RDEV, '( 2X, A, 2X, A, /, 3 A16, 2 A10, A16, 2 A10)' )   &
            'Variable:', VNAME,                                         &
            'MEAN', 'SIGMA', 'MAX @', 'CELL', 'LAYER', 'MIN @', 'CELL', 'LAYER'

        DIV  = 1.0D0 / DBLE( NCELLS * ( LAY1 - LAY0 + 1 ) )
        VBAR = VSUM * DIV
        VSIG = SQRT( MAX( VSSQ * DIV - VBAR**2, 0.0D0 ) )
        WRITE( RDEV, '( 3( 2X, 1PD14.6 ), 2 I10, 2X, 1PD14.6, 2 I10 )' )     &
            VBAR, VSIG, VMAX, IMAX, JMAX, VMIN, IMIN, JMIN

        RETURN

    END SUBROUTINE STAT2D


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-



    SUBROUTINE STAT1DT( JDATE, JTIME, N, NCELLS, VBUF )

        INTEGER, INTENT(IN   ) :: JDATE, JTIME, N   !!  date&time, timestep-number
        INTEGER, INTENT(IN   ) :: NCELLS             !!  dimensions
        REAL   , INTENT(IN   ) :: VBUF( NCELLS )

        REAL*8      VSUM, VSSQ, VBAR, VSIG, DIV
        REAL        VMAX, VMIN
        INTEGER     IMIN, IMAX, JMIN, JMAX

        INTEGER     I, J

        VSUM = 0.0D0
        VSSQ = 0.0D0
        VMAX = VBUF( LAY0 )
        VMIN = VBUF( LAY0 )
        IMAX = 1
        IMIN = 1
        JMAX = LAY0
        JMIN = LAY0

        DO I = 1, NCELLS
            VSUM = VSUM + VBUF( K )
            VSSQ = VSSQ + VBUF( K )**2
            IF ( VBUF( K ) .GT. VMAX ) THEN
                VMAX = VBUF( K )
                IMAX = I
            ELSE IF ( VBUF( K ) .LT. VMIN ) THEN
                VMIN = VBUF( K )
                IMIN = I
            END IF
        END DO

        IF ( N .EQ. 1 ) THEN
            FSUM = VSUM
            FSSQ = VSSQ
            FMAX = VMAX
            KMAX = IMAX
            LMAX = JMAX
            TMAX = N
            FMIN = VMIN
            KMIN = IMIN
            LMIN = JMIN
            TMIN = N

            WRITE( RDEV, '( 2X, A, 2X, A, /, 4 A16, 2 A10, A16, 2 A10 )' )   &
                'Variable:', VNAME,                                          &
                'Date&Time', 'MEAN', 'SIGMA', 'MAX @', 'CELL', 'LAYER', 'MIN @', 'CELL K', 'LAYER'

        ELSE
            FSUM = FSUM + VSUM
            FSSQ = FSSQ + VSSQ
            IF ( VMAX .GT. FMAX ) THEN
                FMAX = VMAX
                LMAX = JMAX
                KMAX = IMAX
                TMAX = N
            END IF
            IF ( VMIN .LT. FMIN ) THEN
                FMIN = VMIN
                LMIN = JMIN
                KMIN = IMIN
                TMIN = N
            END IF
        END IF

        DIV  = 1.0D0 / DBLE( NCELLS )
        VBAR = VSUM * DIV
        VSIG = SQRT( MAX( VSSQ * DIV - VBAR**2, 0.0D0 ) )
        WRITE( RDEV, '(I9.7, A, I6.6, 3( 2X, 1PD14.6 ), 2 I10, 2X, 1PD14.6, 2 I10 )' )     &
            JDATE, ':', JTIME, VBAR, VSIG, VMAX, IMAX, JMAX, VMIN, IMIN, JMIN

        RETURN

    END SUBROUTINE STAT1DT


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    SUBROUTINE STAT1D( NCELLS, VBUF )

        INTEGER, INTENT(IN   ) :: NCELLS      !!  dimensions
        REAL   , INTENT(IN   ) :: VBUF( NCELLS )

        REAL*8      VSUM, VSSQ, VBAR, VSIG, DIV
        REAL        VMAX, VMIN
        INTEGER     IMIN, IMAX

        INTEGER     I, J

        VSUM = 0.0D0
        VSSQ = 0.0D0
        VMAX = VBUF( 1 )
        VMIN = VBUF( 1 )
        IMAX = 1
        IMIN = 1

        DO I = 1, NCELLS
            VSUM = VSUM + VBUF( K )
            VSSQ = VSSQ + VBUF( K )**2
            IF ( VBUF( K ) .GT. VMAX ) THEN
                VMAX = VBUF( K )
                IMAX = I
            ELSE IF ( VBUF( K ) .LT. VMIN ) THEN
                VMIN = VBUF( K )
                IMIN = I
            END IF
        END DO

        WRITE( RDEV, '( 2X, A, 2X, A, /, 3 A16, A10, A16, A10 )' )  &
            'Variable:', VNAME,                                     &
            'MEAN', 'SIGMA', 'MAX @', 'CELL', 'MIN @', 'CELL'

        DIV  = 1.0D0 / DBLE( NCELLS )
        VBAR = VSUM * DIV
        VSIG = SQRT( MAX( VSSQ * DIV - VBAR**2, 0.0D0 ) )
        WRITE( RDEV, '( 3( 2X, 1PD14.6 ), I10, 2X, 1PD14.6, I10 )' )     &
            VBAR, VSIG, VMAX, IMAX, VMIN, IMIN

        RETURN

    END SUBROUTINE STAT1D


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    SUBROUTINE STAT1DF

        REAL*8      FBAR, FSIG, DIV

        DIV = 1.0 / DBLE( NRECS * NCELLS )

        FBAR = FSUM * DIV
        FSIG = SQRT( MAX( FSSQ * DIV - FBAR**2, 0.0D0 ) )

        WRITE( RDEV, '( /, 2X, A )' ) 'FULL layer/time-period statistics:'

        WRITE( RDEV, '( 2( A8, 1PD14.6, / ), 2( A8, 1PD14.6, A, I9.7, A, I6.6, 2X, A, I10, / ) )' )     &
            ' MEAN:', FBAR,                                                      &
            'SIGMA:', FSIG,                                                      &
            '  MAX:', FMAX, ' at', FDATES(TMAX), ':', FTIMES(TMAX), 'K=', KMAX,    &
            '  MIN:', FMIN, ' at', FDATES(TMIN), ':', FTIMES(TMIN), 'K=', KMIN

        RETURN

    END SUBROUTINE STAT1DF


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    SUBROUTINE STAT2DF

        REAL*8      FBAR, FSIG, DIV

        DIV = 1.0 / DBLE( NRECS * NCELLS * ( LAY1 - LAY0 + 1 ) )

        FBAR = FSUM * DIV
        FSIG = SQRT( MAX( FSSQ * DIV - FBAR**2, 0.0D0 ) )

        WRITE( RDEV, '( /, 2X, A )' ) 'FULL layer/time-period statistics:'

        WRITE( RDEV, '( 2( A8, 1PD14.6, / ), 2( A8, 1PD14.6, A, I9.7, A, I6.6, 2( 2X, A, I10 ), / ) )' )     &
            ' MEAN:', FBAR,                                                                  &
            'SIGMA:', FSIG,                                                                  &
            '  MAX:', FMAX, ' at', FDATES(TMAX), ':', FTIMES(TMAX), 'K=', KMAX, 'L=', LMAX,    &
            '  MIN:', FMIN, ' at', FDATES(TMIN), ':', FTIMES(TMIN), 'K=', KMIN, 'L=', LMIN

        RETURN

    END SUBROUTINE STAT2DF

END PROGRAM MPASSTAT

