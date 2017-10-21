
PROGRAM MPASTOM3

    !!***********************************************************************
    !!  Copyright (c) 2017 UNC Institute for the Environment
    !!  Distributed under the GNU GENERAL PUBLIC LICENSE version 2
    !!  See file "GPL.txt" for conditions of use.
    !!.......................................................................
    !!
    !!  DESCRIPTION:
    !!      Interpolate data from MPAS-format netCDF files to I/O API
    !!      gridded output files, with optional emissions-unit correction
    !!
    !!  REVISION  HISTORY:
    !!      Prototype  10/2017 by Carlie J. Coats, Jr., UNC IE
    !!
    !!.......................................................................

    USE MODMPASFIO
    USE MODNCFIO
    USE M3UTILIO

    IMPLICIT NONE

    !!...........   PARAMETERS and their descriptions:

    CHARACTER*1,  PARAMETER :: BLANK = ' '
    CHARACTER*80, PARAMETER :: BAR   = '-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-'
    CHARACTER*16, PARAMETER :: PNAME = 'MPASTOM3'

    !!...........   LOCAL VARIABLES and their descriptions:

    LOGICAL         EFLAG, TFLAG, LFLAG, IFLAG
    INTEGER         ISTAT, LDEV

    CHARACTER*256   MESG

    INTEGER         I, K, L, M, N, P, Q, V, T

    INTEGER         REC0, REC1, NRECS, TSTEP
    INTEGER         SDATE, STIME
    INTEGER         EDATE, ETIME
    INTEGER         JDATE, JTIME

    INTEGER         NVARS1      ! number of output variables
    INTEGER         NCOLS1      ! number of grid columns
    INTEGER         NROWS1      ! number of grid rows
    INTEGER         NLAYS1      ! number of layers
    INTEGER         GDTYP1      ! grid type:  1=LAT-LON, 2=UTM, ...
    REAL*8          P_ALP1      ! first, second, third map
    REAL*8          P_BET1      ! projection descriptive
    REAL*8          P_GAM1      ! parameters.
    REAL*8          XCENT1      ! lon for coord-system X=0
    REAL*8          YCENT1      ! lat for coord-system Y=0
    REAL*8          XORIG1      ! X-coordinate origin of grid (map units)
    REAL*8          YORIG1      ! Y-coordinate origin of grid
    REAL*8          XCELL1      ! X-coordinate cell dimension
    REAL*8          YCELL1      ! Y-coordinate cell dimension
    REAL            AREA1       ! cell-area (M^2)

    INTEGER         MPVARS
    INTEGER         MPLAYS
    INTEGER         NCELLS
    CHARACTER*32    MPNAMES( MXVARS3 )         !!  data structures for DESCNCMPAR()
    CHARACTER*32    MPUNITS( MXVARS3 )         !!  data structures for DESCNCMPAR()
    INTEGER         MPTYPES( MXVARS3 )
    INTEGER         MPNDIMS( MXVARS3 )
    INTEGER          MPDIMS( 7,MXVARS3 )
    CHARACTER*32      DNAME( 7,MXVARS3 )

    INTEGER              :: FSTEPS
    INTEGER, ALLOCATABLE :: FDATES( : )
    INTEGER, ALLOCATABLE :: FTIMES( : )

    REAL   , ALLOCATABLE :: INGRID2D( : )
    REAL   , ALLOCATABLE :: INGRID3D( :,: )

    REAL, ALLOCATABLE ::  LAT( :,: )
    REAL, ALLOCATABLE ::  LON( :,: )

    REAL, ALLOCATABLE ::  OUTGRD2D( :,: )
    REAL, ALLOCATABLE ::  OUTGRD3D( :,:,: )
    REAL, ALLOCATABLE ::   XPBUF3D( :,:,: )

    CHARACTER*32    INNAME( MXVARS3 )
    CHARACTER*16    VNAMES( MXVARS3 )
    CHARACTER*16    VUNITS( MXVARS3 )
    CHARACTER*80    VDESCS( MXVARS3 )
    LOGICAL         EMFLAG( MXVARS3 )

    !!***********************************************************************
    !!.......   First:  Initialize the I/O API:

    LDEV = INIT3()	!  initialization returns unit # for log
    EFLAG = .FALSE.

    WRITE( *, '( 5X, A )' ) BAR,                                                &
'Program "mpastom3" to interpolate MPAS-gridded variables from ${MPFILE}',      &
'',                                                                             &
'',                                                                             &
'',                                                                             &
'',                                                                             &
'PRECONDITIONS REQUIRED:',                                                      &
'    setenv  LLFILE    <path name for  input M3IO gridded Lat-Lon file>',       &
'    setenv  MPFILE    <path name for  input MPAS-gridded file>',               &
'    setenv  OUTFILE   <path name for output M3IO gridded file>',               &
'',                                                                             &
'    Lat-Lon coordinate ${LLFILE}s not yet supported.',                         &
'',                                                                             &
'    The requested variables must be of type REAL, and subscripted by',         &
'    MPAS cell N and optionally by TIME (T) and/or LAYER (L), in (Fortran)',    &
'    subscript order',                                                          &
'',                                                                             &
'        V( [L,] N [, T] )',                                                    &
'',                                                                             &
'    All variables must have the same dimensionality:  either layered',         &
'    or not, and either time stepped or not.',                                  &
'',                                                                             &
'    Requested starting, ending date&time must be in ${MPFILE}',                &
'    For time stepped variables, ${MPFILE" must have regular timestep.',        &
'',                                                                             &
'THE PROGRAM WILL PROMPT YOU for the variable-names, and the starting',         &
'and ending date&time for the interpolation period.',                           &
'',                                                                             &
'Program copyright (C) 2017 UNC Institute for the Environment.',                &
'Released under Version 2 of the GNU General Public License.',                  &
'See enclosed GPL.txt, or URL',                                                 &
''  ,                                                                           &
'    https://www.gnu.org/licenses/old-licenses/gpl-2.0.html',                   &
'', BAR, ''


    !!...............  Open and get description for input files

    IF ( .NOT.OPEN3( 'LLFILE', FSREAD3, PNAME ) ) THEN
        MESG = 'Could not open "LLFILE"'
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
    ELSE IF ( .NOT.DESC3( 'LLFILE' ) ) THEN
        MESG = 'Could not get file description for "LLFILE"'
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
    ELSE IF ( GDTYP3D .EQ. LATGRD3 ) THEN
        MESG = 'Output-grid type Lat-Lon not supported'
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
    ELSE
        NCOLS1 = NCOLS3D
        NROWS1 = NROWS3D
        NLAYS1 = NLAYS3D
        GDTYP1 = GDTYP3D
        P_ALP1 = P_ALP3D
        P_BET1 = P_BET3D
        P_GAM1 = P_GAM3D
        XCENT1 = XCENT3D
        YCENT1 = YCENT3D
        XORIG1 = XORIG3D
        YORIG1 = YORIG3D
        XCELL1 = XCELL3D
        YCELL1 = YCELL3D
        AREA1  = XCELL1 * YCELL1    !  used for emissions-data area-based renormalization
    END IF

    IF ( .NOT.INITMPGRID( 'INFILE' ) ) THEN
        CALL M3EXIT( PNAME, 0,0, 'Could not read header for "INFILE"', 2 )
    ELSE IF ( .NOT.DESCMPAS( 'INFILE', FSTEPS, MPVARS, MPNAMES, MPTYPES,        &
                             MPUNITS, MPNDIMS, MPDIMS, DNAME ) ) THEN
        CALL M3EXIT( PNAME, 0,0, 'Could not read header for "INFILE"', 2 )
    ELSE
        NCELLS = MPCELLS
        T = MAX( INDEX1( 'xtime', MPVARS, VNAMES ) ,  &
                 INDEX1( 'xTime', MPVARS, VNAMES ) )
    END IF

    ALLOCATE( LAT( NCOLS1,NROWS1 ), LON( NCOLS1,NROWS1 ),       &
              FDATES( FSTEPS ), FTIMES( FSTEPS ), STAT = ISTAT )
    IF ( ISTAT .NE. 0 ) THEN
        WRITE( MESG, '( A, I10 )' ) 'LL/DATE buffer allocation failed:  STAT=', ISTAT
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
    END IF

    IF ( .NOT.READ3( 'INFILE', VNAMES( T ), N, FSTEPS, FDATES, FTIMES ) )  THEN
        CALL M3EXIT( PNAME, 0, 0, 'Error readng time-variable "'//VNAMES( T )//'"', 2 )
    END IF


    !!.......   Get list of variables to process:

    T = MAX( INDEX1( 'xtime', MPVARS, VNAMES ) ,  &
             INDEX1( 'xTime', MPVARS, VNAMES ) )
    IF ( T .LE. 0 ) THEN
        TFLAG = .FALSE.
        IFLAG = .TRUE.
        CALL M3MESG( 'MPFILE is time independent' )
    ELSE IF ( .NOT.READMPSTEPS( 'MPFILE', MPNAMES( T ), N, FSTEPS, FDATES, FTIMES ) )  THEN
        CALL M3EXIT( PNAME, 0, 0, 'Error readng time-variable "'//VNAMES( T )//'"', 2 )
    END IF

    CALL M3MESG( 'The list of variables in this file is:' )
    DO  L = 1, MPVARS
        WRITE( *, '( I3, ": ", A )' ) L, MPNAMES( L )
    END DO

    L = 36
    DO I = 1, MXVARS3

        V = GETNUM( 0, MPVARS, L+1, 'Enter number for the variable to interpolate, or 0 to quit.' )
        IF ( V .EQ. 0 )  EXIT

        K = INDEX1( 'nCells',    MPNDIMS( V ), DNAME( :,V ) )
        IF ( K .LE. 0 ) THEN
            EFLAG = .TRUE.
            CALL M3MESG( 'Variables must have MPAS cell-dimension "nCells".' )
        ELSE IF ( I .EQ. 1 ) THEN
            LFLAG = ( K .EQ. 2 )
            IF ( LFLAG ) THEN
                CALL M3MESG( 'Variable is layered' )
                MPLAYS = MPDIMS( K,V )
            ELSE IF ( K .EQ. 1 ) THEN
                CALL M3MESG( 'Variable is not layered' )
                MPLAYS = 1
            ELSE
                EFLAG = .TRUE.
                CALL M3MESG( 'Bad layer subcript-order for variable' )
            END IF
        ELSE IF ( ( K .NE. 2 ) .AND. LFLAG ) THEN
            CALL M3MESG( 'INCONSISTENCY: Variable is not layered' )
        ELSE IF ( ( K .EQ. 2 ) .AND. .NOT.LFLAG ) THEN
            CALL M3MESG( 'INCONSISTENCY: Variable is layered' )
        ELSE IF ( LFLAG ) THEN
            IF ( MPLAYS .NE. MPDIMS( K,V ) ) THEN
                EFLAG = .TRUE.
                CALL M3MESG( 'INCONSISTENCY: bad layer-dimensionhing for variable' )
            END IF
        END IF

        IF ( MPTYPES( V ) .NE. M3REAL ) THEN
            EFLAG = .TRUE.
            CALL M3MESG( 'Variable not of type REAL' )
        END IF

        N = INDEX1( 'Time', MPNDIMS( V ), DNAME( :,V ) )

        IF ( I .EQ. 1 ) THEN
            TFLAG = ( N .GT. 0 )
            IF ( TFLAG ) THEN
                CALL M3MESG( 'Variable is time stepped' )
            ELSE
                CALL M3MESG( 'Variable is time independent' )
            END IF
        ELSE IF ( TFLAG .AND. N .LE. 0 ) THEN
            EFLAG = .TRUE.
            CALL M3MESG( 'INCONSISTENCY: Variable is time independent' )
        ELSE IF ( N .GT. 0 ) THEN
            EFLAG = .TRUE.
            CALL M3MESG( 'INCONSISTENCY: Variable is time stepped' )
        END IF

        CALL GETSTR( 'Enter output name for variable', MPNAMES( V ), VNAMES( I ) )
        CALL GETSTR( 'Enter units       for variable', MPUNITS( V ), VUNITS( I ) )
        CALL GETSTR( 'Enter description for variable', BLANK       , VDESCS( I ) )
        EMFLAG( I ) = GETYN( 'Rescale as an emissions-variable?', .TRUE. )
        INNAME( I ) = MPNAMES( V )

        NVARS1 = I

    END DO      !  end loop getting variables to process

    IF ( EFLAG ) THEN
        CALL M3EXIT( PNAME, 0,0, 'Fatal error(s) in setup for variables', 0 )
    END IF


    !!.......   Get timestep info:

    IF ( TFLAG ) THEN
        SDATE = GETNUM( FDATES(1),    FDATES(FSTEPS), FDATES(1),      'Enter starting date (YYYYDDD)' )
        STIME = GETNUM( 0, 999999999, FTIMES(1),                      'Enter starting time  (HHMMSS)' )
        EDATE = GETNUM( SDATE,        FDATES(FSTEPS), FDATES(FSTEPS), 'Enter   ending date (YYYYDDD)' )
        ETIME = GETNUM( 0, 999999999, FTIMES(FSTEPS),                 'Enter   ending time  (HHMMSS)' )

        REC0  = FINDKEY( SDATE, STIME, FSTEPS, FDATES, FTIMES )
        REC1  = FINDKEY( EDATE, ETIME, FSTEPS, FDATES, FTIMES )
        NRECS = REC1 - REC0 + 1
        TSTEP = SEC2TIME( SECSDIFF( FDATES(1), FTIMES(1), FDATES(2), FTIMES(2) ) )
    END IF

    !!.......   Allocate I/O arrays:

    IF ( LFLAG ) THEN
        ALLOCATE( INGRID3D( MPLAYS,NCELLS ),           &
                  OUTGRD3D( NCOLS1,NROWS1,MPLAYS ),     &
                   XPBUF3D( MPLAYS,NCOLS1,NROWS1 ), STAT = ISTAT )
    ELSE
        ALLOCATE( INGRID2D( NCELLS ),                  &
                  OUTGRD2D( NCOLS1,NROWS1 ), STAT = ISTAT )
    END IF
    IF ( ISTAT .NE. 0 ) THEN
        WRITE( MESG, '( A, I10 )' ) 'LL/DATE buffer allocation failed:  STAT=', ISTAT
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
    END IF


    !!.......   Create the output file, borrowing file description from LL2D:

    NLAYS3D = MPLAYS
    NVARS3D = NVARS1
    VNAME3D( 1:NVARS1 ) = VNAMES( 1:NVARS1 )
    UNITS3D( 1:NVARS1 ) = VUNITS( 1:NVARS1 )
    VDESC3D( 1:NVARS1 ) = VDESCS( 1:NVARS1 )
    VTYPE3D( 1:NVARS1 ) = M3REAL
    FDESC3D( : ) = BLANK
    FDESC3D( 1 ) = 'Variables interpolated from MPAS file'
    FDESC3D( 2 ) = 'MPAS mesh ' // MESH_ID
    IF ( TFLAG ) THEN
        SDATE3D = SDATE
        STIME3D = STIME
        TSTEP3D = TSTEP
    ELSE
        SDATE3D = 0
        STIME3D = 0
        TSTEP3D = 0
    END IF
    VGTYP3D = IMISS3
    VGTOP3D = IMISS3
    DO L = 1, MIN( MPLAYS, MXLAYS3 )+1
        VGLVS3D(L ) = FLOAT( L-1 ) / FLOAT( MPLAYS )
    END DO

    IF ( .NOT.OPEN3( '', FSUNKN3, PNAME ) ) THEN
        MESG = 'Could not open "OUTFILE"'
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
    END IF


    !!....... Process interpolation:

    IF ( TFLAG ) THEN

        DO N = REC0, REC1

            JDATE = FDATES( N )
            JTIME = FTIMES( N )

            DO V = 1, NVARS1

                IF ( LFLAG ) THEN

                    IF ( .NOT.READMPAS( 'INFILE', N, INNAME(V), MPLAYS, NCELLS, INGRID3D ) ) THEN
                        EFLAG = .TRUE.
                        CYCLE
                    END IF

                    IF ( EMFLAG( V ) ) THEN
                        IF ( .NOT.MPINTERP( NCOLS1, NROWS1, AREA1, LAT, LON, MPLAYS, INGRID3D, XPBUF3D ) ) THEN
                            EFLAG = .TRUE.
                            CYCLE
                        END IF
                    ELSE
                        IF ( .NOT.MPINTERP( NCOLS1, NROWS1, LAT, LON, MPLAYS, INGRID3D, XPBUF3D ) ) THEN
                            EFLAG = .TRUE.
                            CYCLE
                        END IF
                    END IF

                    CALL XPOSE( NCOLS1, NROWS1, MPLAYS, XPBUF3D, OUTGRD3D )

                    IF ( .NOT.WRITE3( 'OUTFILE', VNAMES( V ), JDATE, JTIME, OUTGRD3D ) ) THEN
                        EFLAG = .TRUE.
                    END IF

                ELSE

                    IF ( .NOT.READMPAS( 'INFILE', N, INNAME(V), NCELLS, INGRID2D ) ) THEN
                        EFLAG = .TRUE.
                        CYCLE
                    END IF

                    IF ( EMFLAG( V ) ) THEN
                        IF ( .NOT.MPINTERP( NCOLS1, NROWS1, AREA1, LAT, LON, INGRID2D, OUTGRD2D ) ) THEN
                            EFLAG = .TRUE.
                            CYCLE
                        END IF
                    ELSE
                        IF ( .NOT.MPINTERP( NCOLS1, NROWS1, LAT, LON, INGRID2D, OUTGRD2D ) ) THEN
                            EFLAG = .TRUE.
                            CYCLE
                        END IF
                    END IF

                    IF ( .NOT.WRITE3( 'OUTFILE', VNAMES( V ), JDATE, JTIME, OUTGRD2D ) ) THEN
                        EFLAG = .TRUE.
                    END IF

                END IF

            END DO      !!  end loop on output-variables

        END DO      !!  end loop on time steps

    ELSE

        DO V = 1, NVARS1

            IF ( LFLAG ) THEN

                IF ( .NOT.READMPAS( 'INFILE', INNAME(V), MPLAYS, NCELLS, INGRID3D ) ) THEN
                    EFLAG = .TRUE.
                    CYCLE
                END IF

                IF ( EMFLAG( V ) ) THEN
                    IF ( .NOT.MPINTERP( NCOLS1, NROWS1, AREA1, LAT, LON, MPLAYS, INGRID3D, XPBUF3D ) ) THEN
                        EFLAG = .TRUE.
                        CYCLE
                    END IF
                ELSE
                    IF ( .NOT.MPINTERP( NCOLS1, NROWS1, LAT, LON, MPLAYS, INGRID3D, XPBUF3D ) ) THEN
                        EFLAG = .TRUE.
                        CYCLE
                    END IF
                END IF

                CALL XPOSE( NCOLS1, NROWS1, MPLAYS, XPBUF3D, OUTGRD3D )

                IF ( .NOT.WRITE3( 'OUTFILE', VNAMES( V ), 0,0, OUTGRD3D ) ) THEN
                    EFLAG = .TRUE.
                END IF

            ELSE

                IF ( .NOT.READMPAS( 'INFILE', INNAME(V), NCELLS, INGRID2D ) ) THEN
                    EFLAG = .TRUE.
                    CYCLE
                END IF

                IF ( EMFLAG( V ) ) THEN
                    IF ( .NOT.MPINTERP( NCOLS1, NROWS1, AREA1, LAT, LON, INGRID2D, OUTGRD2D ) ) THEN
                        EFLAG = .TRUE.
                        CYCLE
                    END IF
                ELSE
                    IF ( .NOT.MPINTERP( NCOLS1, NROWS1, LAT, LON, INGRID2D, OUTGRD2D ) ) THEN
                        EFLAG = .TRUE.
                        CYCLE
                    END IF
                END IF

                IF ( .NOT.WRITE3( 'OUTFILE', VNAMES( V ), 0,0, OUTGRD2D ) ) THEN
                    EFLAG = .TRUE.
                END IF

            END IF

        END DO      !!  end loop on output-variables


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

    SUBROUTINE XPOSE( NC, NR, NL, X, Y )
        INTEGER, INTENT(IN   ) :: NC, NR, NL
        REAL,    INTENT(IN   ) :: X( NL,NC,NR )
        REAL,    INTENT(  OUT) :: Y( NC,NR,NL )

        INTEGER     C, R, L

!$OMP    PARALLEL DO DEFAULT( NONE ),               &
!$OMP&                SHARED( NC, NR, NL, Y, X ),   &
!$OMP&               PRIVATE( C, R, L )

        DO R = 1, NR
            DO L = 1, NL
            DO C = 1, NC
                Y( C,R,L ) = X( L,C,R )
            END DO
            END DO
        END DO

        RETURN

    END SUBROUTINE XPOSE


END PROGRAM MPASTOM3
