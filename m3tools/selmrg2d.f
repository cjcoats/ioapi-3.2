
        PROGRAM SELMRG2D

C***********************************************************************
C Version "$Id: selmrg2d.f 435 2016-11-22 18:10:58Z coats $"
C EDSS/Models-3 M3TOOLS.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
C (C) 2002-2010 Baron Advanced Meteorological Systems. LLC.
C Distributed under the GNU GENERAL PUBLIC LICENSE version 2
C See file "GPL.txt" for conditions of use.
C.........................................................................
C  program body starts at line  93
C
C  DESCRIPTION:
C       Merges selected layers of selected variables from a set of
C       gridded files over a common time period, with optional renaming.
C       Horizontal grid must tbe the same grid for all files.
C
C  PRECONDITIONS REQUIRED:
C       setenv <logical names>  <path-names>
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       I/O API
C
C  REVISION  HISTORY:
C       Prototype 2/2000 by Carlie J. Coats, Jr., NCSC
C       Version  11/2001 by CJC for I/O API Version 2.1
C       Version  11/2005 by CJC:  eliminate unused vbles and functions
C       Version   9/2008 by CJC:  VDESC should be CHARACTER*80 instead of *16
C       Version  02/2010 by CJC for I/O API v3.1:  Fortran-90 only;
C       USE M3UTILIO, and related changes.
C***********************************************************************

      USE M3UTILIO

      IMPLICIT NONE

C...........   PARAMETERS and their descriptions:

       CHARACTER*16, PARAMETER :: PNAME = 'SELMRG2D'

C...........   LOCAL VARIABLES and their descriptions:

        CHARACTER*16    INNAMES( MXFILE3 )
        CHARACTER*16    ANAME, FNAME
        INTEGER         VTYPE( MXVARS3 ) ! variable type:  M3(INT|REAL|DBLE)
        INTEGER         VLAYR( MXVARS3 )
        CHARACTER*16    VFILE( MXVARS3 )
        CHARACTER*16    VNAMI( MXVARS3 )
        CHARACTER*16    VNAMO( MXVARS3 )
        CHARACTER*16    UNITS( MXVARS3 )
        CHARACTER*80    VDESC( MXVARS3 )

        INTEGER         NCOLS
        INTEGER         NROWS
        INTEGER         NLAYS
        INTEGER         NVARS
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
        REAL            VGLVS( MXLAYS3+1 )

        CHARACTER*16    GDNAM      ! grid name             (length NAMLEN3=16)

        INTEGER         FLAYS( MXFILE3 )

        INTEGER         SDATE, STIME, TSTEP, DURATN, NSTEPS
        INTEGER         JDATE, JTIME

        INTEGER         I, N, L, V, F, STEP
        INTEGER         STATUS

        REAL,    ALLOCATABLE::   INBUF( :, : )

        LOGICAL         EFLAG
        CHARACTER*256   MESG
        CHARACTER*256   CBUF


C***********************************************************************
C   begin body of program SELMRG2D

        I = INIT3()
        WRITE( *, '( 5X, A )' )
     &' ',
     &'Program SELMRG2D to merge selected layers of selected',
     &'variables from a set of gridded files over a commmon grid',
     &'and time period.',
     &' ',
     &'THE PROGRAM WILL PROMPT YOU for the logical names of the input',
     &'input files and the output file, the variables and layers to',
     &'extract, the names by which they should be called in the ',
     &'output file, and the time step sequence to be processed.',
     &'Default responses are indicated in square brackets',
     &'[LIKE THIS], and may be accepted by hitting the RETURN key.',
     &' ',
     &'PRECONDITIONS REQUIRED:',
     &' ',
     &'    setenv <first input name>    <path-names>',
     &'    ...',
     &'    setenv <last  input name>    <path-names>',
     &'    setenv <output name>         <path-names>',
     &' ',
     &'See URL',
     &'https://www.cmascenter.org/ioapi/documentation/3.1/html#tools',
     &' ',
     &'Program copyright (C) 1992-2002 MCNC, (C) 1995-2013',
     &'Carlie J. Coats, Jr., and (C) 2002-2010 Baron Advanced',
     &'Meteorological Systems, LLC.  Released under Version 2',
     &'of the GNU General Public License. See enclosed GPL.txt, or',
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
     &'$Id:: selmrg2d.f 435 2016-11-22 18:10:58Z coats               $',
     &' '


        IF ( .NOT. GETYN( 'Continue with program?', .TRUE. ) ) THEN
            CALL M3EXIT( PNAME, 0, 0,
     &                   'Program terminated at user request', 2 )
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

C...............  Open/Process the first input file

        INNAMES( 1 ) = PROMPTMFILE(  'Enter first input file', FSREAD3,
     &                                INNAMES( 1 ), PNAME )

        IF ( .NOT. DESC3( INNAMES( 1 ) ) ) THEN
            MESG = 'Could not get file description for ' // INNAMES(1)
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
        END IF

        IF ( FTYPE3D .NE. GRDDED3 ) THEN
            MESG = 'File "' // TRIM( INNAMES(1) ) //
     &             '" not a gridded file'
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
        END IF

        FLAYS( 1 ) = NLAYS3D
        NCOLS = NCOLS3D
        NROWS = NROWS3D
        NLAYS = NLAYS3D
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
        VGLVS( 1:NLAYS+1 ) = VGLVS3D( 1:NLAYS+1 )
        GDNAM = GDNAM3D

        SDATE  = SDATE3D
        STIME  = STIME3D
        TSTEP  = TSTEP3D
        NSTEPS = MXREC3D

        WRITE( *, '( /5X, A, 120( /5X, I2, 6A, : ) )' )
     &      'Variables in this file are:',
     &      (  V, ':  ',
     &         VNAME3D(V), ' (',
     &         TRIM( UNITS3D(V) ), '): ',
     &         TRIM( VDESC3D(V) ), V=1, NVARS3D )
        WRITE( *,* )
        F = 1
        N = 0
        I = 1
11      CONTINUE

            I = GETNUM( 0, NVARS3D, I,
     &          'Enter # for next variable to extract (0 to quit)' )

            IF ( I .EQ. 0 ) GO TO 12

            N = N + 1
            IF ( NLAYS3D .EQ. 1 ) THEN
                VNAMI(N) = VNAME3D(I)
                ANAME    = VNAME3D(I)
                VLAYR(N) = 1
                VDESC(N) = VDESC3D(I)
            ELSE
                MESG = 'Enter layer to extract from ' // VNAME3D(I)
                VLAYR(N) = GETNUM( 1, NLAYS3D, 1, MESG )
                VNAMI(N) = VNAME3D(I)
                WRITE( CBUF, '( A, I4, A, 2X, A )' )
     &                   'Layer ', VLAYR(N), ':', TRIM( VDESC3D(I) )
                ANAME = CBUF
                VDESC(N) = CBUF
            END IF
            VTYPE(N) = VTYPE3D( I )
            UNITS(N) = UNITS3D( I )
            VFILE(N) = INNAMES( 1 )
            WRITE( CBUF, '( A, I3.3 )' ) TRIM( VNAME3D(I) ), VLAYR(N)
            CALL GETSTR( 'Enter output name for this variable/layer',
     &                   ANAME, VNAMO(N) )

            IF ( N .LT. MXVARS3 ) GO TO 11
            MESG = 'I/O API max number of variables now selected'
            CALL M3MSG2( MESG )
            GO TO 44

12      CONTINUE        !  exit from get-variables loop


C...............  Open/Process the rest of the input data files

22      CONTINUE        !  get rest of the input files

            F = F + 1
            INNAMES( F ) = PROMPTMFILE(
     &                        'Enter next input file, or "NONE"',
     &                        FSREAD3, INNAMES( F ), PNAME )

            IF ( INNAMES( F ) .EQ. 'NONE' ) GO TO 44

            IF ( .NOT. DESC3( INNAMES( F ) ) ) THEN
                MESG = 'Could not get file description for ' //
     &                 INNAMES(F)
                CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
            ELSE IF ( .NOT.FILCHK3( INNAMES( F ),  GRDDED3,
     &                              NCOLS, NROWS, NLAYS, NTHIK3D) ) THEN
                MESG = 'Inconsistent dimensions  for ' // INNAMES( F )
                EFLAG = .TRUE.
                CALL M3MESG( MESG )
            ELSE IF ( .NOT.GRDCHK3( INNAMES( F ),
     &                      P_ALP, P_BET, P_GAM, XCENT, YCENT,
     &                      XORIG, YORIG, XCELL, YCELL,
     &                      NLAYS3D, VGTYP3D, VGTOP3D, VGLVS3D ) ) THEN
                MESG = 'Inconsistent coord/grid  for ' // INNAMES( F )
                EFLAG = .TRUE.
                CALL M3MESG( MESG )
            END IF

            WRITE( *, '( /5X, A, 120( /5X, I2, 6A, : ) )' )
     &          'Variables in this file are:',
     &          (  V, ':  ',
     &             VNAME3D(V), ' (',
     &             TRIM( UNITS3D(V) ), '): ',
     &             TRIM( VDESC3D(V) ), V=1, NVARS3D )
            WRITE( *,* )
            I = 1
            L = 1
33          CONTINUE

                I = GETNUM( 0, NVARS3D, I,
     &          'Enter # for next variable to extract (0 to quit)' )

                IF ( I .EQ. 0 ) GO TO 34

                N = N + 1
                IF ( NLAYS3D .EQ. 1 ) THEN
                    VNAMI(N) = VNAME3D(I)
                    ANAME    = VNAME3D(I)
                    VLAYR(N) = 1
                    VDESC(N) = VDESC3D(I)
                ELSE
                    MESG = 'Enter layer to extract from ' // VNAME3D(I)
                    VLAYR(N) = GETNUM( 1, NLAYS3D, 1, MESG )
                    VNAMI(N) = VNAME3D(I)
                    WRITE( CBUF, '( A, I4, A, 2X, A )' )
     &                   'Layer ', VLAYR(N), ':', TRIM( VDESC3D(I) )
                    ANAME = CBUF
                    VDESC(N) = CBUF
                END IF
                VTYPE(N) = VTYPE3D( I )
                UNITS(N) = UNITS3D( I )
                VFILE(N) = INNAMES( F )
                CALL GETSTR(
     &              'Enter output name for this variable/layer',
     &               ANAME, VNAMO(N) )

                IF ( N .LT. MXVARS3 ) GO TO 33
                MESG = 'I/O API max number of variables now selected'
                CALL M3MSG2( MESG )
                GO TO 44

34          CONTINUE        !  exit from get-variables loop

            GO TO  22

44      CONTINUE        !  exit from get-files loop

        NVARS = N

        SDATE = GETNUM( 0, 9999999, SDATE,
     &                  'Enter starting DATE for the run (HHMMSS)' )

        STIME = GETNUM( 0, 235959, STIME,
     &                  'Enter starting TIME (HHMMSS)' )

        TSTEP = GETNUM( 0, 235959, TSTEP,
     &                  'Enter OUTPUT TIME STEP (HHMMSS)' )

        I = SEC2TIME( NSTEPS * TIME2SEC( TSTEP ) )
        DURATN = GETNUM( 0,99999999, I, 'Enter RUN DURATION (HHMMSS)' )


C...............  Build the output file:

        NCOLS3D = NCOLS
        NROWS3D = NROWS
        NLAYS3D = 1
        P_ALP3D = P_ALP
        P_BET3D = P_BET
        P_GAM3D = P_GAM
        XCENT3D = XCENT
        YCENT3D = YCENT
        XORIG3D = XORIG
        YORIG3D = YORIG
        XCELL3D = XCELL
        YCELL3D = YCELL
        VGTYP3D = IMISS3
        VGTOP3D = BADVAL3
        GDNAM3D = GDNAM
        VGLVS3D( 1 ) = BADVAL3
        VGLVS3D( 2 ) = BADVAL3

        NVARS3D = NVARS
        DO  V = 1, NVARS3D
            VNAME3D( V ) = VNAMO( V )
            VTYPE3D( V ) = VTYPE( V )
            UNITS3D( V ) = UNITS( V )
            VDESC3D( V ) = VDESC( V )
        END DO

        FNAME = PROMPTMFILE(  'Enter output file', FSUNKN3,
     &                        'OUTFILE', PNAME )

        ALLOCATE( INBUF ( NCOLS, NROWS ), STAT = STATUS )

        IF ( STATUS .NE. 0 ) THEN
            WRITE( MESG, '( A, I10)' )
     &           'Buffer allocation failed:  STAT=', STATUS
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
        END IF


C...............  Perform the merge

        NSTEPS = TIME2SEC( DURATN ) / TIME2SEC( TSTEP )
        JDATE  = SDATE
        JTIME  = STIME

        DO  STEP = 1, NSTEPS

            DO  V = 1, NVARS

               IF ( .NOT. READ3( VFILE( V ), VNAMI( V ), VLAYR( V ),
     &                            JDATE, JTIME, INBUF ) ) THEN
                   MESG = 'Could not read "' // TRIM( VNAMI(V) ) //
     &                    '" from "' // TRIM( VFILE( V ) ) // '"'
                   CALL M3EXIT( PNAME, JDATE, JTIME, MESG, 2 )
               END IF  !  if read failed

               IF ( .NOT. WRITE3( FNAME, VNAMO( V ),
     &                            JDATE, JTIME, INBUF ) ) THEN
                    MESG = 'Could not write "' // TRIM( VNAMO(V) ) //
     &                     '" to "' // TRIM( FNAME ) // '"'
                    CALL M3EXIT( PNAME, JDATE, JTIME, MESG, 2 )
                END IF  !  if write failed

            END DO              !  end loop on variables V for this time step

            CALL NEXTIME( JDATE, JTIME, TSTEP )

        END DO          !  end loop on output time steps


        CALL M3EXIT( PNAME, 0, 0,
     &               'Successful completion of program SELMRG2D', 0 )
C       STOP

        END PROGRAM SELMRG2D

