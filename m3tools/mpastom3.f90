
PROGRAM MPASTOM3

    !!***********************************************************************
    !!  Version "$Id: mpastom3.f90 36 2017-10-24 17:32:20Z coats $"
    !!  EDSS/Models-3 M3TOOLS.
    !!  Copyright (c) 2017 UNC Institute for the Environment and Carlie J. Coats, Jr.
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

    REAL*8, PARAMETER :: PI     = 3.141592653589793238462643383279d0
    REAL*8, PARAMETER :: PI180  = PI / 180.0d0

    CHARACTER*1,  PARAMETER :: BLANK = ' '
    CHARACTER*80, PARAMETER :: BAR   = '-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-'
    CHARACTER*16, PARAMETER :: PNAME = 'MPASTOM3'

    !!...........   LOCAL VARIABLES and their descriptions:

    LOGICAL         EFLAG, TFLAG, LFLAG, IFLAG, LATLON
    INTEGER         ISTAT, LDEV

    CHARACTER*256   MESG

    INTEGER         C, R, I, K, L, M, N, P, Q, V, T

    INTEGER         REC0, REC1, NRECS, TSTEP
    INTEGER         SDATE, STIME
    INTEGER         EDATE, ETIME
    INTEGER         JDATE, JTIME

    INTEGER         NCOLS       ! number of output-grid columns
    INTEGER         NROWS       ! number of output-grid rows
    REAL            CAREA       ! output-grid cell-area (M^2)
    REAL            ASQ         ! scratch-variables

    INTEGER         MPVARS      ! MPAS input-file dimensions and tables
    INTEGER         MPLAYS
    INTEGER         NCELLS
    INTEGER         MPRECS
    CHARACTER*32    MPNAMES( MXVARS3 )         !!  data structures for DESCNCMPAR()
    CHARACTER*32    MPUNITS( MXVARS3 )         !!  data structures for DESCNCMPAR()
    INTEGER         MPTYPES( MXVARS3 )
    INTEGER         MPNDIMS( MXVARS3 )
    INTEGER          MPDIMS( 7,MXVARS3 )
    CHARACTER*32      DNAME( 7,MXVARS3 )

    INTEGER, ALLOCATABLE :: FDATES( : )         !! (MPRECS)
    INTEGER, ALLOCATABLE :: FTIMES( : )

    REAL, ALLOCATABLE ::     LAT( :,: )            !!  (NCOLS,NROWS)
    REAL, ALLOCATABLE ::     LON( :,: )
    REAL, ALLOCATABLE ::  LLAREA( :,: )

    REAL   , ALLOCATABLE :: INGRID2D( : )       !!  (NCELLS)
    REAL   , ALLOCATABLE :: INGRID3D( :,: )     !!  (MPLAYS,NCELLS)

    REAL, ALLOCATABLE ::  OUTGRD2D( :,: )       !!  (NCOLS,NROWS)
    REAL, ALLOCATABLE ::  OUTGRD3D( :,:,: )     !!  (NCOLS,NROWS,MPLAYS)
    REAL, ALLOCATABLE ::   XPGRD3D( :,:,: )     !!  (MPLAYS,NCOLS,NROWS)

    INTEGER         NVARS                   ! number of output variables
    CHARACTER*32    INAMES( MXVARS3 )       ! input  MPAS-file variable-names
    CHARACTER*16    VNAMES( MXVARS3 )       ! output M3IO-file variable-names
    CHARACTER*16    VUNITS( MXVARS3 )
    CHARACTER*80    VDESCS( MXVARS3 )
    LOGICAL         EMFLAG( MXVARS3 )       ! do emissions-style area-weighted re-scaling?

    !!***********************************************************************
    !!.......   First:  Initialize the I/O API:

    LDEV  = INIT3()	!  initialization returns unit # for log
    EFLAG = .FALSE.

    WRITE( *, '( 5X, A )' ) BAR,                                            &
'Program "mpastom3" to interpolate MPAS-gridded variables from input',      &
'file ${MPFILE} to the grid of I/O API input Lat-Lon file ${LLFILE},',      &
'for a specified time step sequence if appropriate, and write the result',  &
'to a GRIDDED M3IO output file ${OUTFILE}.',                                &
'',                                                                         &
'PRECONDITIONS REQUIRED:',                                                  &
'    setenv  LLFILE    <path name for  input M3IO gridded Lat-Lon file>',   &
'                      e.g., a GRID_CRO_2D file from MCIP',                 &
'    setenv  MPFILE    <path name for  input MPAS-gridded file>',           &
'    setenv  OUTFILE   <path name for output M3IO gridded file>',           &
'',                                                                         &
'    The requested MPAS variables must be of type REAL, and subscripted',   &
'    by MPAS cell N and optionally by TIME (T) and/or LAYER (L), in ',      &
'    (Fortran) subscript order',                                            &
'',                                                                         &
'        V( [L,] N [, T] )',                                                &
'',                                                                         &
'    All variables must have the same dimensionality:  either layered',     &
'    or not, and either time stepped or not.',                              &
'',                                                                         &
'    Requested starting, ending date&time must be in ${MPFILE}',            &
'    For time stepped variables, ${MPFILE" must have regular timestep.',    &
'',                                                                         &
'THE PROGRAM WILL PROMPT YOU for the MPAS-input variables by number,',      &
'for the output I/O API variable-names, units, and descriptions,',          &
'whether to re-scale by the ratio of M3IO cell-area to MPAS cell-area',     &
'(as needed by emissions data), and, if appropriate, for the starting',     &
'and ending dates&times for the interpolation period.',                     &
'',                                                                         &
'Program copyright (C) 2017 UNC Institute for the Environment and',         &
'Carlie J. Coats, Jr.',                                                     &
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
'$Id: mpastom3.f90 36 2017-10-24 17:32:20Z coats $',&
BLANK, BAR, BLANK

    IF ( .NOT. GETYN( 'Continue with program?', .TRUE. ) ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'Program terminated at user request', 2 )
    END IF


    !!...............  Open and get description for input files

    IF ( .NOT.OPEN3( 'LLFILE', FSREAD3, PNAME ) ) THEN
        MESG = 'Could not open "LLFILE"'
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
    ELSE IF ( .NOT.DESC3( 'LLFILE' ) ) THEN
        MESG = 'Could not get file description for "LLFILE"'
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
    ELSE
        NCOLS  = NCOLS3D
        NROWS  = NROWS3D
        CAREA  = XCELL3D * YCELL3D    !  used for emissions-data area-based renormalization
        LATLON = ( GDTYP3D .EQ. LATGRD3 )
    END IF

    IF ( .NOT.INITMPGRID( 'MPFILE' ) ) THEN
        CALL M3EXIT( PNAME, 0,0, 'Could not read header for "MPFILE"', 2 )
    ELSE IF ( .NOT.DESCMPAS( 'MPFILE', MPRECS, MPVARS, MPNAMES, MPTYPES,        &
                             MPUNITS, MPNDIMS, MPDIMS, DNAME ) ) THEN
        CALL M3EXIT( PNAME, 0,0, 'Could not read header for "MPFILE"', 2 )
    ELSE
        NCELLS = MPCELLS
        T = MAX( INDEX1( 'xtime', MPVARS, MPNAMES ) ,        &
                 INDEX1( 'xTime', MPVARS, MPNAMES ) )
    END IF

    ALLOCATE( LAT( NCOLS,NROWS ), LON( NCOLS,NROWS ),       &
              FDATES( MPRECS ), FTIMES( MPRECS ), STAT = ISTAT )
    IF ( ISTAT .NE. 0 ) THEN
        WRITE( MESG, '( A, I10 )' ) 'LAT/LON/DATE buffer allocation failed:  STAT=', ISTAT
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
    END IF

    IF ( T .LE. 0 ) THEN
        TFLAG = .FALSE.
        IFLAG = .TRUE.
        CALL M3MESG( 'MPFILE is time independent' )
    ELSE IF ( .NOT.READMPSTEPS( 'MPFILE', MPNAMES( T ), N, MPRECS, FDATES, FTIMES ) )  THEN
        CALL M3EXIT( PNAME, 0, 0, 'Error reading time-variable "'//VNAMES( T )//'" from MPFILE', 2 )
    END IF

    IF ( .NOT.READ3( 'LLFILE', 'LAT', 1, 0, 0, LAT ) )  THEN
        CALL M3EXIT( PNAME, 0, 0, 'Error reading variable "LAT" from LLFILE', 2 )
    END IF

    IF ( .NOT.READ3( 'LLFILE', 'LON', 1, 0, 0, LON ) )  THEN
        CALL M3EXIT( PNAME, 0, 0, 'Error reading variable "LON" from LLFILE', 2 )
    END IF

    IF ( LATLON ) THEN
        ALLOCATE( LLAREA( NCOLS,NROWS ),  STAT = ISTAT )
        IF ( ISTAT .NE. 0 ) THEN
            WRITE( MESG, '( A, I10 )' ) 'LAT/LON/DATE buffer allocation failed:  STAT=', ISTAT
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
        END IF
        CAREA = CAREA * REARTH**2 * PI180**2        !!  cell-area at equator
        DO R = 1, NROWS
            ASQ = CAREA * COS( PI180*LAT( 1,R ) ) * REARTH**2
            DO C = 1, NCOLS
                LLAREA( C,R ) = ASQ
            END DO
        END DO
    END IF


    !!.......   Get list of variables to process:

    CALL M3MESG( BLANK )
    CALL M3MESG( 'The list of variables in this file is:' )
    DO  L = 1, MPVARS
        WRITE( *, '( I3, ": ", A )' ) L, MPNAMES( L )
    END DO

    IFLAG = .FALSE.     !!  are LFLAG, TFLAG initialized?
    L     = 37          !!  number of MPAS header-variables

    DO I = 1, MXVARS3

        L = MOD( L, MPVARS ) + 1
        V = GETNUM( 0, MPVARS, L, 'Enter number for the variable to interpolate, or 0 to quit.' )
        IF ( V .EQ. 0 )  EXIT
        
        INAMES( I ) = MPNAMES( V )

        K = INDEX1( 'nCells', MPNDIMS( V ), DNAME( :,V ) )
        N = INDEX1( 'Time',   MPNDIMS( V ), DNAME( :,V ) )

        IF ( .NOT.IFLAG ) THEN      !!  initialize LFLAG, TFLAG

            IFLAG = .TRUE.
            LFLAG = ( K .EQ. 2 )
            IF ( LFLAG ) THEN
                CALL M3MESG( 'Variable "' //TRIM( INAMES( I ) ) // '" is layered' )
                MPLAYS = MPDIMS( 1,V )
            ELSE IF ( K .EQ. 1 ) THEN
                CALL M3MESG( 'Variable "' //TRIM( INAMES( I ) ) // '" is not layered' )
                MPLAYS = 1
            ELSE
                EFLAG = .TRUE.
                CALL M3MESG( 'Bad layer subcript-order for variable' )
            END IF
            
            TFLAG = ( N .GT. 0 )
            IF ( TFLAG ) THEN
                CALL M3MESG( 'Variable "' //TRIM( INAMES( I ) ) // '" is time stepped' )
            ELSE
                CALL M3MESG( 'Variable "' //TRIM( INAMES( I ) ) // '" is time independent' )
            END IF

        END IF      !!  if initializing LFLAG, TFLAG

        IF ( MPNDIMS( V ) .GT. 3 ) THEN
            EFLAG = .TRUE.
            CALL M3MESG( 'Incorrect number of dimensions for variable "' //TRIM( INAMES( I ) ) // '"' )
        END IF

        IF ( K .LE. 0 ) THEN
            EFLAG = .TRUE.
            CALL M3MESG( 'Variables must have MPAS cell-dimension "nCells".' )
        ELSE IF ( ( K .NE. 2 ) .AND. LFLAG ) THEN
            CALL M3MESG( 'INCONSISTENCY: Variable "' //TRIM( INAMES( I ) ) // '" is not layered' )
        ELSE IF ( ( K .EQ. 2 ) .AND. .NOT.LFLAG ) THEN
            CALL M3MESG( 'INCONSISTENCY: Variable "' //TRIM( INAMES( I ) ) // '" is layered' )
        ELSE IF ( LFLAG ) THEN
            IF ( MPLAYS .NE. MPDIMS( 1,V ) ) THEN
                EFLAG = .TRUE.
                CALL M3MESG( 'INCONSISTENCY: bad layer-dimensioning for variable "' //TRIM( INAMES( I ) ) // '"' )
            END IF
        END IF

        IF ( MPTYPES( V ) .NE. M3REAL ) THEN
            EFLAG = .TRUE.
            CALL M3MESG( 'Variable "' //TRIM( INAMES( I ) ) // '" not of type REAL' )
        END IF

        IF ( N .LE. 1 ) THEN
            EFLAG = .TRUE.
            CALL M3MESG( 'Bad time-dimensioning for variable "' //TRIM( INAMES( I ) ) // '"' )
        ELSE IF ( N .LE. 0  .AND. TFLAG  ) THEN
            EFLAG = .TRUE.
            CALL M3MESG( 'INCONSISTENCY: Variable is time independent' )
        ELSE IF ( N .GT. 0 .AND. .NOT.TFLAG ) THEN
            EFLAG = .TRUE.
            CALL M3MESG( 'INCONSISTENCY: Variable is time stepped' )
        END IF

        MESG = 'Interpolated from MPAS-file variable "' // TRIM( INAMES( I ) ) // '"'
        CALL GETSTR( 'Enter output name for variable', MPNAMES( V ), VNAMES( I ) )
        CALL GETSTR( 'Enter units       for variable', MPUNITS( V ), VUNITS( I ) )
        CALL GETSTR( 'Enter description for variable', MESG        , VDESCS( I ) )
        EMFLAG( I ) = GETYN( 'Rescale by cell-areas as an emissions-variable?', .TRUE. )

        NVARS = I

    END DO      !  end loop getting variables to process

    IF ( EFLAG ) THEN
        CALL M3EXIT( PNAME, 0,0, 'Fatal error(s) in setup for variables', 0 )
    END IF

    !!.......   Allocate I/O arrays:

    IF ( LFLAG ) THEN
        ALLOCATE( INGRID3D( MPLAYS, NCELLS      ),      &
                   XPGRD3D( MPLAYS, NCOLS,NROWS ),      &
                  OUTGRD3D( NCOLS,NROWS, MPLAYS ), STAT = ISTAT )
    ELSE
        ALLOCATE( INGRID2D( NCELLS ),                  &
                  OUTGRD2D( NCOLS,NROWS ), STAT = ISTAT )
    END IF
    IF ( ISTAT .NE. 0 ) THEN
        WRITE( MESG, '( A, I10 )' ) 'I/O buffer allocation failed:  STAT=', ISTAT
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
    END IF


    !!.......   Get timestep info:

    IF ( TFLAG ) THEN
        SDATE = GETNUM( FDATES(1),    FDATES(MPRECS), FDATES(1),      'Enter starting date (YYYYDDD)' )
        STIME = GETNUM( 0, 999999999, FTIMES(1),                      'Enter starting time  (HHMMSS)' )
        EDATE = GETNUM( SDATE,        FDATES(MPRECS), FDATES(MPRECS), 'Enter   ending date (YYYYDDD)' )
        ETIME = GETNUM( 0, 999999999, FTIMES(MPRECS),                 'Enter   ending time  (HHMMSS)' )

        REC0  = FINDKEY( SDATE, STIME, MPRECS, FDATES, FTIMES )
        REC1  = FINDKEY( EDATE, ETIME, MPRECS, FDATES, FTIMES )
        NRECS = REC1 - REC0 + 1
        TSTEP = SEC2TIME( SECSDIFF( FDATES(1), FTIMES(1), FDATES(2), FTIMES(2) ) )

        SDATE3D = SDATE
        STIME3D = STIME
        TSTEP3D = TSTEP
    ELSE
        SDATE3D = 0
        STIME3D = 0
        TSTEP3D = 0
    END IF


    !!.......   Create the output file, borrowing most of file description from LL2D:

    VNAME3D( 1:NVARS ) = VNAMES( 1:NVARS )
    UNITS3D( 1:NVARS ) = VUNITS( 1:NVARS )
    VDESC3D( 1:NVARS ) = VDESCS( 1:NVARS )
    VTYPE3D( 1:NVARS ) = M3REAL
    NLAYS3D = MPLAYS
    NVARS3D = NVARS
    VGTYP3D = IMISS3
    VGTOP3D = IMISS3
    DO L = 1, MIN( MPLAYS, MXLAYS3 )+1
        VGLVS3D(L ) = FLOAT( L-1 ) / FLOAT( MPLAYS )
    END DO
    FDESC3D( : ) = BLANK
    FDESC3D( 1 ) = 'Variables interpolated from MPAS file'
    FDESC3D( 2 ) = 'MPAS mesh ' // MESH_ID

    IF ( .NOT.OPEN3( 'OUTFILE', FSUNKN3, PNAME ) ) THEN
        MESG = 'Could not open "OUTFILE"'
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
    END IF


    !!....... Process interpolation:

    IF ( TFLAG ) THEN       !! time stepped

        DO N = REC0, REC1

            JDATE = FDATES( N )
            JTIME = FTIMES( N )

            DO V = 1, NVARS

                IF ( LFLAG ) THEN      !!  if multi-layer

                    IF ( .NOT.READMPAS( 'MPFILE', N, INAMES(V), MPLAYS, NCELLS, INGRID3D ) ) THEN
                        EFLAG = .TRUE.
                        CYCLE
                    END IF
                    
                    IF ( LATLON .AND. EMFLAG( V ) ) THEN
                        IF ( .NOT.MPINTERP( NCOLS, NROWS, LLAREA, LAT, LON, MPLAYS, INGRID3D, XPGRD3D ) ) THEN
                            EFLAG = .TRUE.
                            CYCLE
                        END IF
                    ELSE IF ( EMFLAG( V ) ) THEN
                        IF ( .NOT.MPINTERP( NCOLS, NROWS, CAREA, LAT, LON, MPLAYS, INGRID3D, XPGRD3D ) ) THEN
                            EFLAG = .TRUE.
                            CYCLE
                        END IF
                    ELSE
                        IF ( .NOT.MPINTERP( NCOLS, NROWS, LAT, LON, MPLAYS, INGRID3D, XPGRD3D ) ) THEN
                            EFLAG = .TRUE.
                            CYCLE
                        END IF
                    END IF

                    CALL XPOSE( NCOLS, NROWS, MPLAYS, XPGRD3D, OUTGRD3D )

                    IF ( .NOT.WRITE3( 'OUTFILE', VNAMES( V ), JDATE, JTIME, OUTGRD3D ) ) THEN
                        EFLAG = .TRUE.
                    END IF

                ELSE        !!  single-layer

                    IF ( .NOT.READMPAS( 'MPFILE', N, INAMES(V), NCELLS, INGRID2D ) ) THEN
                        EFLAG = .TRUE.
                        CYCLE
                    END IF

                    IF ( LATLON .AND. EMFLAG( V ) ) THEN
                        IF ( .NOT.MPINTERP( NCOLS, NROWS, LLAREA, LAT, LON, INGRID2D, OUTGRD2D ) ) THEN
                            EFLAG = .TRUE.
                            CYCLE
                        END IF
                    ELSE IF ( EMFLAG( V ) ) THEN
                        IF ( .NOT.MPINTERP( NCOLS, NROWS, CAREA, LAT, LON, INGRID2D, OUTGRD2D ) ) THEN
                            EFLAG = .TRUE.
                            CYCLE
                        END IF
                    ELSE
                        IF ( .NOT.MPINTERP( NCOLS, NROWS, LAT, LON, INGRID2D, OUTGRD2D ) ) THEN
                            EFLAG = .TRUE.
                            CYCLE
                        END IF
                    END IF

                    IF ( .NOT.WRITE3( 'OUTFILE', VNAMES( V ), JDATE, JTIME, OUTGRD2D ) ) THEN
                        EFLAG = .TRUE.
                    END IF

                END IF      !!  if multi-layer; else single-layer

            END DO      !!  end loop on output-variables

        END DO      !!  end loop on time steps

    ELSE       !! time independent

        DO V = 1, NVARS

            IF ( LFLAG ) THEN      !!  if multi-layer

                IF ( .NOT.READMPAS( 'MPFILE', INAMES(V), MPLAYS, NCELLS, INGRID3D ) ) THEN
                    EFLAG = .TRUE.
                    CYCLE
                END IF

                IF ( LATLON .AND. EMFLAG( V ) ) THEN
                    IF ( .NOT.MPINTERP( NCOLS, NROWS, LLAREA, LAT, LON, MPLAYS, INGRID3D, XPGRD3D ) ) THEN
                        EFLAG = .TRUE.
                        CYCLE
                    END IF
                ELSE IF ( EMFLAG( V ) ) THEN
                    IF ( .NOT.MPINTERP( NCOLS, NROWS, CAREA, LAT, LON, MPLAYS, INGRID3D, XPGRD3D ) ) THEN
                        EFLAG = .TRUE.
                        CYCLE
                    END IF
                ELSE
                    IF ( .NOT.MPINTERP( NCOLS, NROWS, LAT, LON, MPLAYS, INGRID3D, XPGRD3D ) ) THEN
                        EFLAG = .TRUE.
                        CYCLE
                    END IF
                END IF

                CALL XPOSE( NCOLS, NROWS, MPLAYS, XPGRD3D, OUTGRD3D )

                IF ( .NOT.WRITE3( 'OUTFILE', VNAMES( V ), 0,0, OUTGRD3D ) ) THEN
                    EFLAG = .TRUE.
                END IF

            ELSE        !!  single-layer data

                IF ( .NOT.READMPAS( 'MPFILE', INAMES(V), NCELLS, INGRID2D ) ) THEN
                    EFLAG = .TRUE.
                    CYCLE
                END IF

                IF ( LATLON .AND. EMFLAG( V ) ) THEN
                    IF ( .NOT.MPINTERP( NCOLS, NROWS, LLAREA, LAT, LON, INGRID2D, OUTGRD2D ) ) THEN
                        EFLAG = .TRUE.
                        CYCLE
                    END IF
                ELSE IF ( EMFLAG( V ) ) THEN
                    IF ( .NOT.MPINTERP( NCOLS, NROWS, CAREA, LAT, LON, INGRID2D, OUTGRD2D ) ) THEN
                        EFLAG = .TRUE.
                        CYCLE
                    END IF
                ELSE
                    IF ( .NOT.MPINTERP( NCOLS, NROWS, LAT, LON, INGRID2D, OUTGRD2D ) ) THEN
                        EFLAG = .TRUE.
                        CYCLE
                    END IF
                END IF

                IF ( .NOT.WRITE3( 'OUTFILE', VNAMES( V ), 0,0, OUTGRD2D ) ) THEN
                    EFLAG = .TRUE.
                END IF

            END IF      !!  if multi-layer; else single-layer

        END DO      !!  end loop on output-variables

    END IF       !! if time stepped; else time independent


    !!.......   Clean up and exit

    IF ( EFLAG ) THEN
        MESG  = 'Failure(s) in program'
        ISTAT = 2
    ELSE
        MESG  = 'Successful completion of program'
        ISTAT = 0
    END IF
    CALL M3EXIT( PNAME, 0, 0, MESG, 0 )


CONTAINS    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    SUBROUTINE XPOSE( NC, NR, NL, XP, YG )
        INTEGER, INTENT(IN   ) :: NC, NR, NL
        REAL,    INTENT(IN   ) :: XP( NL,NC,NR )
        REAL,    INTENT(  OUT) :: YG( NC,NR,NL )

        INTEGER     C, R, L

!$OMP    PARALLEL DO DEFAULT( NONE ),               &
!$OMP&                SHARED( NC, NR, NL, YG, XP ), &
!$OMP&               PRIVATE( C, R, L )

        DO R = 1, NR
            DO L = 1, NL
            DO C = 1, NC
                YG( C,R,L ) = XP( L,C,R )
            END DO
            END DO
        END DO

        RETURN

    END SUBROUTINE XPOSE


END PROGRAM MPASTOM3
