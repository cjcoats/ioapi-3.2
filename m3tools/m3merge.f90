
PROGRAM  M3MERGE

    !***********************************************************************
    ! Version "$Id: m3merge.f90 214 2021-12-14 12:54:34Z coats $"
    ! EDSS/Models-3 M3TOOLS.
    ! Copyright (C) 1992-2002 MCNC,
    ! (C) 1995-2002,2005-2013,2021 Carlie J. Coats, Jr.,
    ! and (C) 2002-2010 Baron Advanced Meteorological Systems. LLC.
    ! Distributed under the GNU GENERAL PUBLIC LICENSE version 2
    ! See file "GPL.txt" for conditions of use.
    !.........................................................................
    !  program body starts at line  99
    !
    !  FUNCTION:
    !       Merges selected variables from a set of input files for a
    !       specified time period, and writes them to the output file,
    !       with optional renaming in the process.
    !
    !  PRECONDITIONS REQUIRED:
    !       Merges selected layers of selected variables from a set of
    !       gridded, boundary, or custom  files over a common time period,
    !       with optional renaming.
    !       Horizontal and vertical grid structures must tbe the same for
    !       all files.
    !       Files have a common time period including the duration of the
    !       merge.
    !       setenv <logical names>  <path-names>
    !
    !  SUBROUTINES AND FUNCTIONS CALLED:
    !       Models-3 I/O API.
    !
    !  REVISION  HISTORY:
    !      Prototype  6/2001 by CJC
    !       Version  11/2001 by CJC for I/O API Version 2.1
    !       Version  11/2005 by CJC:  eliminate unused vbles
    !       Version   9/2008 by CJC:  fix VDESC as CHAR*80 instead of CHAR*16
    !       Version  02/2010 by CJC for I/O API v3.1:  Fortran-90 only;
    !       USE M3UTILIO, and related changes.
    !       Version  01/2014 by CJC:  bug-fix for time independent files.
    !       Version  10/2021 by CJC:  free ".f90" source format for IOAPI-4.0
    !       Version  12/2021 by CJC:  bug-fix for date-prompt
    !***********************************************************************

    USE M3UTILIO

    IMPLICIT NONE

    !...........   PARAMETERS and their descriptions:

    CHARACTER*16, PARAMETER :: PNAME = 'M3MERGE'

    !...........   LOCAL VARIABLES and their descriptions:

    CHARACTER*16    INNAMES( MXFILE3 )

    CHARACTER*16    ANAME, FNAME
    INTEGER         VTYPE( MXVARS3 ) ! variable type:  M3(INT|REAL|DBLE)
    CHARACTER*16    VFILE( MXVARS3 )
    CHARACTER*16    VNAMI( MXVARS3 )
    CHARACTER*16    VNAMO( MXVARS3 )
    CHARACTER*16    UNITS( MXVARS3 )
    CHARACTER*80    VDESC( MXVARS3 )

    INTEGER         NCOLS
    INTEGER         NROWS
    INTEGER         NLAYS
    INTEGER         NTHIK
    INTEGER         NVARS
    INTEGER         FTYPE
    INTEGER         FSIZE
    REAL*8          P_ALP      ! first, second, third map
    REAL*8          P_BET      ! projection descriptive
    REAL*8          P_GAM      ! parameters.

    REAL*8          XCENT      ! lon for coord-system X=0
    REAL*8          YCENT      ! lat for coord-system Y=0
    REAL*8          XORIG      ! X-coordinate origin of grid (map units)
    REAL*8          YORIG      ! Y-coordinate origin of grid
    REAL*8          XCELL      ! X-coordinate cell dimension
    REAL*8          YCELL      ! Y-coordinate cell dimension

    INTEGER         VGTYP      !  vertical coordinate type (VGSIGP3, ...)
    REAL            VGTOP      !  model-top, for sigma coord types.
    REAL            VGLVS( MXLAYS3 + 1 )  !  vertical coord values.

    CHARACTER*16    GDNAM      ! grid name             (length NAMLEN3=16)

    INTEGER         SDATE, STIME, TSTEP, DURATN, JDATE, JTIME

    INTEGER         I, N, L, V, F, STEP, NSTEPS
    INTEGER         STATUS

    REAL,    ALLOCATABLE::   INBUF( : )

    LOGICAL         EFLAG
    CHARACTER*256   MESG


    !***********************************************************************
    !   begin body of program M3MERGE

    EFLAG = .FALSE.         !  no errors yet
    I     = INIT3()
    WRITE( *, '( 5X, A )' ) ' ',                                        &
'Program M3MERGE to merge selected variables from a set of gridded',    &
'boundary, or custom files over a commmon grid and time period.',       &
' ',                                                                    &
'THE PROGRAM WILL PROMPT YOU for the logical names of the input files', &
'and the output file, the variables and layers to extract, the names',  &
'by which they should be called in the output file, and the time step', &
'sequence to be processed.',                                            &
'Use output time step 0 for tine independent output.',                  &
'Default responses are indicated in square brackets [LIKE THIS],',      &
'and may be accepted by hitting the RETURN key.',                       &
' ',                                                                    &
'PRECONDITIONS REQUIRED:',                                              &
' ',                                                                    &
'    setenv <first input name>    <path-names>',                        &
'    ...',                                                              &
'    setenv <last  input name>    <path-names>',                        &
'    setenv <output name>         <path-names>',                        &
' ',                                                                    &
'See URLs  https://cjcoats.github.io/ioapi/AA.html#tools or',           &
'  https://www.cmascenter.org/ioapi/documentation/all_versions/html/AA.html#tools', &
' ',                                                                    &
'Program copyright (C) 1992-2002 MCNC, (C) 1995-2013, 2021',            &
'Carlie J. Coats, Jr., (C) 2002-2010 Baron Advanced',                   &
'Meteorological Systems, LLC., and (C) 2014-2018 UNC',                  &
'Institute for the Environment.',                                       &
'Released under Version 2 of the GNU General Public License.',          &
'See enclosed GPL.txt, or URL',                                         &
'https://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html',            &
' ',                                                                    &
'Comments and questions are welcome and can be sent to',                &
' ',                                                                    &
'    Carlie J. Coats, Jr.    carlie@jyarborough.com',                   &
'or',                                                                   &
'    UNC Institute for the Environment',                                &
'    100 Europa Dr., Suite 490',                                        &
'    Campus Box 1105',                                                  &
'    Chapel Hill, NC 27599-1105',                                       &
' ',                                                                    &
'Program version: ',                                                    &
'$Id:: m3merge.f90 214 2021-12-14 12:54:34Z coats                   $', &
' '

    IF ( .NOT. GETYN( 'Continue with program?', .TRUE. ) ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'Program terminated at user request', 2 )
    END IF

    DO  F = 1, 9
        WRITE( INNAMES( F ), '( A, I1 )' ) 'INFILE', F
    END DO

    DO  F = 10, MIN( MXFILE3, 99 )
        WRITE( INNAMES( F ), '( A, I2 )' ) 'INFILE', F
    END DO

    DO  F = 100, MXFILE3
        WRITE( INNAMES( F ), '( A, I3 )' ) 'INFILE', F
    END DO

    !...............  Open/Process the first input file

    INNAMES( 1 ) = PROMPTMFILE(  'Enter first input file', FSREAD3, INNAMES( 1 ), PNAME )

    IF ( .NOT. DESC3( INNAMES( 1 ) ) ) THEN
        MESG = 'Could not get file description for ' // INNAMES(1)
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
    END IF

    NCOLS = NCOLS3D
    NROWS = NROWS3D
    NLAYS = NLAYS3D
    NTHIK = NTHIK3D
    P_ALP = P_ALP3D
    P_BET = P_BET3D
    P_GAM = P_GAM3D
    XCENT = XCENT3D
    YCENT = YCENT3D
    XORIG = XORIG3D
    YORIG = YORIG3D
    XCELL = XCELL3D
    YCELL = YCELL3D
    VGTYP = VGTYP3D
    VGTOP = VGTOP3D
    GDNAM = GDNAM3D

    FTYPE = FTYPE3D
    IF ( FTYPE .EQ. GRDDED3 ) THEN
        FSIZE = NCOLS * NROWS * NLAYS
    ELSE IF ( FTYPE .EQ. BNDARY3 ) THEN
        FSIZE = 2*ABS( NTHIK )*NLAYS*( NCOLS + NROWS + 2*NTHIK )
    ELSE IF ( FTYPE .EQ. CUSTOM3 ) THEN
        FSIZE = NCOLS * NLAYS
    ELSE
        WRITE( MESG, '( A, I3 )' ) 'Unsupported file type', FTYPE
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
    END IF

    SDATE  = SDATE3D
    STIME  = STIME3D
    TSTEP  = TSTEP3D
    NSTEPS = MXREC3D
    DO  L = 1, NLAYS + 1
        VGLVS( L ) = VGLVS3D( L )
    END DO

    WRITE( *, '( /5X, A, 120( /5X, I3, 6A, : ) )' ) 'Variables in this file are:',                  &
        (  V, ':  ', VNAME3D(V), ' (', TRIM( UNITS3D(V) ), '): ', TRIM( VDESC3D(V) ), V=1, NVARS3D )
    WRITE( *,* )

    F = 1
    N = 0
    IF ( GETYN( 'Incorporate all variables, without renaming?',&
                .TRUE. ) ) THEN
        DO  I = 1, NVARS3D
            VNAMI(N+I) = VNAME3D(I)
            VNAMO(N+I) = VNAME3D(I)
            VDESC(N+I) = VDESC3D(I)
            VTYPE(N+I) = VTYPE3D( I )
            UNITS(N+I) = UNITS3D( I )
            VFILE(N+I) = INNAMES( F )
        END DO
        N = N + NVARS3D
        GO TO  22
    END IF

    F = 1
    N = 0
    I = 1
11  CONTINUE

    I = GETNUM( 0, NVARS3D, I,&
        'Enter # for next variable to extract (0 to quit)' )

    IF ( I .EQ. 0 ) GO TO 12

    N = N + 1
    VNAMI(N) = VNAME3D(I)
    ANAME    = VNAME3D(I)
    VDESC(N) = VDESC3D(I)
    VTYPE(N) = VTYPE3D( I )
    UNITS(N) = UNITS3D( I )
    VFILE(N) = INNAMES( 1 )
    CALL GETSTR( 'Enter output name for this variable',&
                 ANAME, VNAMO(N) )

    IF ( N .LT. MXVARS3 ) GO TO 11
    MESG = 'I/O API max number of variables now selected'
    CALL M3MSG2( MESG )
    GO TO 44

12  CONTINUE        !  exit from get-variables loop


    !...............  Open/Process the rest of the input data files

22  CONTINUE        !  get rest of the input files

    F = F + 1
    INNAMES( F ) = PROMPTMFILE( 'Enter next input file, or "NONE"', FSREAD3, INNAMES( F ), PNAME )

    IF ( INNAMES( F ) .EQ. 'NONE' ) GO TO 44

    IF ( .NOT. DESC3( INNAMES( F ) ) ) THEN
        MESG = 'Could not get file description for ' // INNAMES(F)
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
    ELSE IF ( .NOT.FILCHK3( INNAMES( F ),  FTYPE, NCOLS, NROWS, NLAYS, NTHIK3D ) ) THEN
        MESG = 'Inconsistent dimensions  for ' // INNAMES( F )
        EFLAG = .TRUE.
        CALL M3MESG( MESG )
    ELSE IF ( .NOT.GRDCHK3( INNAMES( F ),                           &
                            P_ALP, P_BET, P_GAM, XCENT, YCENT,      &
                            XORIG, YORIG, XCELL, YCELL,             &
                            NLAYS, VGTYP, VGTOP, VGLVS ) ) THEN
        MESG = 'Inconsistent coord/grid  for ' // INNAMES( F )
        EFLAG = .TRUE.
        CALL M3MESG( MESG )
    ELSE IF ( FTYPE.EQ.BNDARY3 .AND. NTHIK3D.NE.NTHIK ) THEN
        MESG = 'Inconsistent NTHIK for ' // INNAMES( F )
        EFLAG = .TRUE.
        CALL M3MESG( MESG )
    END IF

    WRITE( *, '( /5X, A, 120( /5X, I3, 6A, : ) )' )  'Variables in this file are:',                  &
        (  V, ':  ', VNAME3D(V), ' (', TRIM( UNITS3D(V) ), '): ', TRIM( VDESC3D(V) ), V=1, NVARS3D )
    WRITE( *,* )

    I = 1
    L = 1
    IF ( N + NVARS3D .LE. MXVARS3 ) THEN
        IF ( GETYN( 'Merge all variables, without renaming?', .TRUE. ) ) THEN
            DO  I = 1, NVARS3D
                VNAMI(N+I) = VNAME3D(I)
                VNAMO(N+I) = VNAME3D(I)
                VDESC(N+I) = VDESC3D(I)
                VTYPE(N+I) = VTYPE3D( I )
                UNITS(N+I) = UNITS3D( I )
                VFILE(N+I) = INNAMES( F )
            END DO
            N = N + NVARS3D
            GO TO  34
        END IF
    END IF

33  CONTINUE

    I = GETNUM( 0, NVARS3D, I, 'Enter # for next variable to extract (0 to quit)' )

    IF ( I .EQ. 0 ) GO TO 34

    N = N + 1
    VNAMI(N) = VNAME3D(I)
    ANAME    = VNAME3D(I)
    VDESC(N) = VDESC3D(I)
    VTYPE(N) = VTYPE3D( I )
    UNITS(N) = UNITS3D( I )
    VFILE(N) = INNAMES( F )
    CALL GETSTR( 'Enter output name for this variable/layer', ANAME, VNAMO(N) )

    IF ( N .LT. MXVARS3 ) GO TO 33
    MESG = 'I/O API max number of variables now selected'
    CALL M3MSG2( MESG )
    GO TO 44

34  CONTINUE        !  exit from get-variables loop

    IF ( N .EQ. MXVARS3 ) THEN
        MESG = 'I/O API max number of variables now selected'
        CALL M3MSG2( MESG )
        GO TO 44
    END IF

    GO TO 22    !  to head of get-files loop

44  CONTINUE        !  exit from get-files loop

    IF ( EFLAG ) THEN
        MESG = 'Fatal input-setup/configuraton errors'
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
    END IF

    NVARS = N

    SDATE = GETNUM( 0,   9999999, SDATE, 'Enter starting DATE for the run (HHMMSS)' )

    STIME = GETNUM( 0,    235959, STIME, 'Enter starting TIME   (HHMMSS)' )
   
    TSTEP = GETNUM( 0, 999999999, TSTEP, 'Enter OUTPUT TIME STEP (HHMMSS)' )

    IF ( TSTEP .GT. 0 ) THEN
        I      = SEC2TIME( NSTEPS * TIME2SEC( TSTEP ) )
        DURATN = GETNUM( 0,999999999,I, 'Enter RUN DURATION (HHMMSS)' )
        NSTEPS = TIME2SEC( DURATN ) / TIME2SEC( TSTEP )
    ELSE
        NSTEPS = 1
    END IF


    !...............  Build the output file:

    SDATE3D = SDATE
    STIME3D = STIME
    TSTEP3D = TSTEP
    NCOLS3D = NCOLS
    NROWS3D = NROWS
    NLAYS3D = NLAYS
    P_ALP3D = P_ALP
    P_BET3D = P_BET
    P_GAM3D = P_GAM
    XCENT3D = XCENT
    YCENT3D = YCENT
    XORIG3D = XORIG
    YORIG3D = YORIG
    XCELL3D = XCELL
    YCELL3D = YCELL
    VGTYP3D = VGTYP
    VGTOP3D = VGTOP
    GDNAM3D = GDNAM
    DO  L = 1, NLAYS+1
        VGLVS3D( L ) = VGLVS( L )
    END DO

    NVARS3D = NVARS
    DO  V = 1, NVARS3D
        VNAME3D( V ) = VNAMO( V )
        VTYPE3D( V ) = VTYPE( V )
        UNITS3D( V ) = UNITS( V )
        VDESC3D( V ) = VDESC( V )
    END DO

    FNAME = PROMPTMFILE(  'Enter output file', FSUNKN3, 'OUTFILE', PNAME )

    ALLOCATE( INBUF ( FSIZE ), STAT = STATUS )

    IF ( STATUS .NE. 0 ) THEN
        WRITE( MESG, '( A, I10)' ) 'Buffer allocation failed:  STAT=', STATUS
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
    END IF


    !...............  Perform the merge

    JDATE  = SDATE
    JTIME  = STIME

    DO  STEP = 1, NSTEPS

        DO  V = 1, NVARS

            IF ( .NOT. READ3( VFILE( V ), VNAMI( V ), ALLAYS3, JDATE, JTIME, INBUF ) ) THEN
                MESG = 'Could not read "' // TRIM( VNAMI(V) ) // '" from "' // TRIM( VFILE( V ) ) // '"'
                CALL M3EXIT( PNAME, JDATE, JTIME, MESG, 2 )
            END IF  !  if read failed

            IF ( .NOT. WRITE3( FNAME, VNAMO( V ), JDATE, JTIME, INBUF ) ) THEN
                MESG = 'Could not write "' // TRIM( VNAMO(V) ) // '" to "' // TRIM( FNAME ) // '"'
                CALL M3EXIT( PNAME, JDATE, JTIME, MESG, 2 )
            END IF  !  if write failed

        END DO              !  end loop on variables V for this time step

        CALL NEXTIME( JDATE, JTIME, TSTEP )

    END DO          !  end loop on output time steps


    CALL M3EXIT( PNAME, 0, 0, 'Successful completion of program M3MERGE', 0 )

END PROGRAM  M3MERGE


