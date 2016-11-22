
        PROGRAM PRESTERP

C***********************************************************************
C Version "$Id: presterp.f 435 2016-11-22 18:10:58Z coats $"
C EDSS/Models-3 M3TOOLS.
C Copyright (C) 1992-2002 MCNC, (C) 1995-2002,2005-2013 Carlie J. Coats, Jr.,
C and (C) 2002-2010 Baron Advanced Meteorological Systems. LLC.
C Distributed under the GNU GENERAL PUBLIC LICENSE version 2
C See file "GPL.txt" for conditions of use.
C.........................................................................
C  program body starts at line  100
C
C  DESCRIPTION:
C       Interpolate specified file from sigma levels to pressure levels,
C       using coefficient and subscript tables from PRES_*_3D
C
C  PRECONDITIONS REQUIRED:
C       3D input file having same input grid as PRES_*_3D
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       GETNUM,   GETREAL,  GETYN,    INDEX1, PROMPTMFILE,
C       SECSDIFF, SEC2TIME, TIME2SEC
C
C  REVISION  HISTORY:
C       Prototype 7/4/99 by CJC
C       Version  11/2001 by CJC for I/O API Version 2.1
C       Version  11/2005 by CJC:  eliminate unused vbles
C       Version  02/2010 by CJC for I/O API v3.1:  Fortran-90 only;
C       USE M3UTILIO, and related changes.
C***********************************************************************

      USE M3UTILIO

      IMPLICIT NONE

C...........   PARAMETERS and their descriptions:

        CHARACTER*16, PARAMETER :: BLANK16 = ' '
        CHARACTER*16, PARAMETER :: PNAME   = 'PRESTERP'

C...........   EXTERNAL FUNCTIONS and their descriptions:

        INTEGER :: IARGC

C...........   LOCAL VARIABLES and their descriptions:

        INTEGER         LOGDEV  !  unit number for log file
        INTEGER         ARGCNT  !  number of command-line args, from IARGC()
        CHARACTER*256   ENVBUF  !  value from command line arguments

        INTEGER         STATUS
        LOGICAL         EFLAG, AFLAG
        LOGICAL ::      PFLAG( 3 ) = .FALSE.

        CHARACTER*16    INAME   !  logical name of the input data file
        CHARACTER*16    GNAME   !  logical name of the input pressure file
        CHARACTER*16    FNAME   !  logical name of the output file

        INTEGER         NCOLS   !  grid dimensions, from file headers
        INTEGER         NROWS   !  grid dimensions, from file headers
        INTEGER         NLAYS   !  grid dimensions, from file headers
        INTEGER         NSTEPS  !  number of vbles in INAME
        INTEGER         VGTYP   !  vertical coord type
        REAL            VGTOP

        INTEGER         PLAYS   !  number of    pressure levels from GNAME
        REAL            PRESF( MXLAYS3 + 1 ) !  pressure levels from GNAME

        REAL*8          P_ALP      ! first, second, third map
        REAL*8          P_BET      ! projection descriptive
        REAL*8          P_GAM      ! parameters.

        REAL*8          XCENT      ! lon for coord-system X=0
        REAL*8          YCENT      ! lat for coord-system Y=0
        REAL*8          XORIG      ! X-coordinate origin of grid (map units)
        REAL*8          YORIG      ! Y-coordinate origin of grid
        REAL*8          XCELL      ! X-coordinate cell dimension
        REAL*8          YCELL      ! Y-coordinate cell dimension

        INTEGER         JDATE, JTIME, TSTEP
        INTEGER         L, V, T

        CHARACTER*256   MESG    !  buffer for messages

        REAL,    ALLOCATABLE::    VSIG( :, :, : )
        INTEGER, ALLOCATABLE::    PDEX( :, :, : )
        REAL,    ALLOCATABLE::    PLIN( :, :, : )
        REAL,    ALLOCATABLE::    PLOG( :, :, : )
        REAL,    ALLOCATABLE::    PPOW( :, :, : )
        REAL,    ALLOCATABLE::    VPRS( :, :, : )

        INTEGER         ITYPE( MXVARS3 )        !  interpolation type
        REAL            BADV( MXVARS3 ), VV     !  bad-value token


C***********************************************************************
C   begin body of program PRESTERP

        LOGDEV = INIT3()

        WRITE( *,92000 )
     &  ' ',
     &  'Program PRESTERP to interpolate from sigma level input file',
     &  'to pressure levels, using coefficients and indices obtained',
     &  'from PRES_CRO_3D or PRES_DOT_3D.  Both files must have the',
     &  'same horizontal grid, and must be of type GRIDDED.',
     &  ' ',
     &  'You need to have assigned logical names to the physical file',
     &  'names of both files, according to Models-3 conventions, ',
     &  'using the operation ',
     &  ' ',
     &  '     setenv <lname> <pname>',
     &  ' ',
     &  'USAGE:',
     &  '     presterp [INFILE PRESFILE OUTFILE [<interp-type>]]',
     &  ' ',
     &  'where INFILE, PRESFILE, and OUTFILE are optional arguments ',
     &  'for the logical names of the input data, the pressure-level,',
     &  'and the output files, and  <interp-type> is the type of ',
     &  'interpolation desired:',
     &  ' ',
     &  '     LIN   for linear-in-pressure interpolation',
     &  '     LOG   for log(pressure)      interpolation',
     &  '     POW   for pressure^alpha     interpolation',
     &  ' ',
     &  '(and then answer the prompts).',
     &  'NOTE:  If you specify the interpolation-type interactively ',
     &  '(at the prompt), you will specify it separately for each ',
     &  'variable, rather than for all of them at once.',
     &' ',
     &'See URL',
     &'https://www.cmascenter.org/ioapi/documentation/3.1/html#tools',
     &  ' ',
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
     &'$Id:: presterp.f 435 2016-11-22 18:10:58Z coats               $',
     &' '

        IF ( .NOT.GETYN( 'Continue with program?', .TRUE. ) ) THEN
            CALL M3EXIT( PNAME, 0, 0,
     &                   'Terminated at user request', 2 )
        END IF

        ARGCNT = IARGC()
        IF ( ARGCNT .GE. 3 ) THEN

            CALL GETARG( 1, ENVBUF )
            INAME = ENVBUF( 1:16 )
            IF ( .NOT. OPEN3( INAME, FSREAD3, PNAME ) ) THEN
                MESG = 'Could not open input file "'
     &                    // TRIM( INAME ) // '"'
                CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
            END IF

            CALL GETARG( 2, ENVBUF )
            GNAME = ENVBUF( 1:16 )
            IF ( .NOT. OPEN3( GNAME, FSREAD3, PNAME ) ) THEN
                MESG = 'Could not open input file "'
     &                    // TRIM( GNAME ) // '"'
                CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
            END IF

            IF ( ARGCNT .EQ. 4 ) THEN
                CALL GETARG( 4, ENVBUF )
                CALL LUSTR( ENVBUF )
                IF ( ENVBUF( 1:3 ) .EQ. 'LIN' ) THEN
                    T = 1
                    PFLAG( 1 ) = .TRUE.
                ELSE IF ( ENVBUF( 1:3 ) .EQ. 'LOG' ) THEN
                    T = 2
                    PFLAG( 2 ) = .TRUE.
                ELSE IF ( ENVBUF( 1:3 ) .EQ. 'POW' ) THEN
                    T = 3
                    PFLAG( 3 ) = .TRUE.
                ELSE
                    MESG = 'Unknown interpolation type ' // ENVBUF
                    CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
                END IF
                DO  V = 1, MXVARS3
                    ITYPE( V ) = T
                END DO
            END IF

        ELSE IF ( ARGCNT .EQ. 0 ) THEN

            INAME = PROMPTMFILE( 'Enter logical name for INPUT FILE',
     &                           FSREAD3, 'AFILE', PNAME )

            GNAME = PROMPTMFILE( '            ... for PRESSURE FILE',
     &                           FSREAD3, 'PFILE', PNAME )

        ELSE
            CALL M3EXIT( PNAME, 0, 0,
     &      'USAGE:  presterp [INFILE PRESFILE OUTFILE]', 2 )
        END IF


        !!  Get input file descriptions and check file headers.

        IF ( .NOT. DESC3( GNAME ) ) THEN
            MESG = 'Could not get description for "' // GNAME
            CALL M3EXIT( PNAME, 0, 0,MESG, 2 )
        END IF
        NCOLS = NCOLS3D
        NROWS = NROWS3D
        PLAYS = NLAYS3D
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
        DO  L = 1, PLAYS+1
            PRESF( L ) = VGLVS3D( L )
        END DO

        IF ( .NOT. DESC3( INAME ) ) THEN
            MESG = 'Could not get description for "' // INAME
            CALL M3EXIT( PNAME, 0, 0,MESG, 2 )
        ELSE IF ( .NOT.FILCHK3( INAME,  GRDDED3,
     &                  NCOLS, NROWS, NLAYS3D, NTHIK3D ) ) THEN
            MESG = 'Inconsistent dimensions  for ' // INAME
            EFLAG = .TRUE.
            CALL M3MESG( MESG )
        ELSE IF ( .NOT.GRDCHK3( INAME,
     &                  P_ALP, P_BET, P_GAM, XCENT, YCENT,
     &                  XORIG, YORIG, XCELL, YCELL,
     &                  NLAYS3D, VGTYP3D, VGTOP3D, VGLVS3D ) ) THEN
            MESG = 'Inconsistent coord/grid  for ' // INAME
            EFLAG = .TRUE.
            CALL M3MESG( MESG )
        END IF

        JDATE  = SDATE3D
        JTIME  = STIME3D
        TSTEP  = TSTEP3D
        NSTEPS = MXREC3D
        NLAYS  = NLAYS3D


        !!  Create output file  Reuse description for INAME, except for
        !!  pressure levels obtained from GNAME.

        VGTYP3D = VGTYP
        VGTOP3D = VGTOP
        DO  L = 1, PLAYS+1
            VGLVS3D( L ) = PRESF( L )
        END DO
        NLAYS3D = PLAYS

        IF ( ARGCNT .GE. 3 ) THEN

            CALL GETARG( 3, ENVBUF )
            FNAME = ENVBUF( 1:16 )
            IF ( .NOT. OPEN3( FNAME, FSUNKN3, PNAME ) ) THEN
                MESG = 'Could not open input file "'
     &                    // TRIM( FNAME ) // '"'
                CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
            END IF

        ELSE

            FNAME = PROMPTMFILE( 'Enter logical name for OUTPUT FILE',
     &                           FSUNKN3, 'OFILE', PNAME )

        END IF


        !!  Get pressure-interpolation flags for each variable, and
        !!  allocate working buffers.

        IF ( ARGCNT .LT. 4 ) THEN

            WRITE( *,92000 )
     &  ' ',
     &  'Three types of pressure-level interpolation are supported:',
     &  ' ',
     &  '    1:  Linear-in-pressure interpolation',
     &  '    2:  Log-interpolation    (linear in log(P) )',
     &  '    3:  Power-interpolation  (linear in P**GAMMA)',
     &  ' ',
     &  'For each variable you need to specify the type., as well as',
     &  'the value used to indicate BAD (i.e. below-terrain) 3-D grid',
     &  'locations.',
     &  ' '
            VV = BADVAL3
            DO  V = 1, NVARS3D
                MESG = 'Enter interpolation type for "' //
     &                  TRIM( VNAME3D(V) ) // '"'
                T = GETNUM( 1, 3, 1, MESG )
                ITYPE( V ) = T
                PFLAG( T ) = .TRUE.
                MESG = 'Enter BELOW-GROUND value for "' //
     &                  TRIM( VNAME3D(V) ) // '"'
                VV = GETREAL( BADVAL3, -BADVAL3, 0.0, MESG )
                BADV( V ) = VV
            END DO

        ELSE

            DO  V = 1, NVARS3D
                BADV( V ) = BADVAL3
            END DO

        END IF

        ALLOCATE ( PDEX( NCOLS, NROWS, PLAYS ),
     &             PLIN( NCOLS, NROWS, PLAYS ),
     &             PLOG( NCOLS, NROWS, PLAYS ),
     &             PPOW( NCOLS, NROWS, PLAYS ),
     &             VPRS( NCOLS, NROWS, PLAYS ),
     &             VSIG( NCOLS, NROWS, NLAYS ), STAT = STATUS )

        IF ( STATUS .NE. 0 ) THEN
            WRITE( MESG, '( A, I10 )' )
     &      'Allocation failed:  STAT=', STATUS
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
        END IF


        !!  Main processing loops:

        AFLAG = .FALSE.
        DO  T = 1, NSTEPS

            EFLAG = .FALSE.

            IF ( .NOT. READ3( GNAME, 'PDEX', ALLAYS3, JDATE, JTIME,
     &                        PDEX ) ) THEN
                MESG = 'Could not read PDEX from ' // PNAME
                CALL M3WARN( PNAME, 0, 0, MESG )
                EFLAG = .TRUE.
                AFLAG = .TRUE.
            END IF

            IF ( PFLAG( 1 ) ) THEN
                IF ( .NOT. READ3( GNAME, 'CLIN', ALLAYS3,
     &                            JDATE, JTIME, PLIN ) ) THEN
                    MESG = 'Could not read PLIN from ' // GNAME
                    CALL M3WARN( PNAME, 0, 0, MESG )
                    EFLAG = .TRUE.
                    AFLAG = .TRUE.
                END IF
            END IF

            IF ( PFLAG( 2 ) ) THEN
                IF ( .NOT. READ3( GNAME, 'CLOG', ALLAYS3,
     &                            JDATE, JTIME, PLOG ) ) THEN
                    MESG = 'Could not read PLOG from ' // GNAME
                    CALL M3WARN( PNAME, 0, 0, MESG )
                    EFLAG = .TRUE.
                    AFLAG = .TRUE.
                END IF
            END IF

            IF ( PFLAG( 3 ) ) THEN
                IF ( .NOT. READ3( GNAME, 'CPOW', ALLAYS3,
     &                            JDATE, JTIME, PPOW ) ) THEN
                    MESG = 'Could not read PPOW from ' // GNAME
                    CALL M3WARN( PNAME, 0, 0, MESG )
                    EFLAG = .TRUE.
                    AFLAG = .TRUE.
                END IF
            END IF

            IF ( EFLAG ) CYCLE

            DO  V = 1, NVARS3D

                IF ( .NOT. READ3( INAME, VNAME3D( V ), ALLAYS3,
     &                            JDATE, JTIME, VSIG ) ) THEN
                    MESG = 'Could not read "' // VNAME3D( V ) //
     &                     '" from ' // INAME
                    CALL M3WARN( PNAME, 0, 0, MESG )
                    AFLAG = .TRUE.
                    CYCLE
                END IF

                IF      ( ITYPE( V ) .EQ. 1 ) THEN
                    CALL PTERP( NCOLS, NROWS, NLAYS, PLAYS, BADV( V ),
     &                          PDEX,  PLIN,  VSIG,  VPRS )
                ELSE IF ( ITYPE( V ) .EQ. 2 ) THEN
                    CALL PTERP( NCOLS, NROWS, NLAYS, PLAYS, BADV( V ),
     &                          PDEX,  PLOG,  VSIG,  VPRS )
                ELSE IF ( ITYPE( V ) .EQ. 3 ) THEN
                    CALL PTERP( NCOLS, NROWS, NLAYS, PLAYS, BADV( V ),
     &                          PDEX,  PPOW,  VSIG,  VPRS )
                END IF

                IF ( .NOT.WRITE3( FNAME, VNAME3D( V ),
     &                            JDATE, JTIME, VPRS ) ) THEN
                    MESG = 'Could not write "' // VNAME3D( V ) //
     &                     '" to ' // FNAME
                    CALL M3WARN( PNAME, 0, 0, MESG )
                    AFLAG = .TRUE.
                END IF

            END DO              !  end loop on variables V

            CALL NEXTIME( JDATE, JTIME, TSTEP )

        END DO          !  end loop on timesteps T

        CALL M3EXIT( PNAME, 0, 0,
     &               'Successful completion of program PRESTERP', 0 )

!..........................   FORMAT STATEMENTS  ....................
!...........   Informational (LOG) message formats... 92xxx

92000 FORMAT( 5X, A )

!.........................................................................

      CONTAINS

            SUBROUTINE  PTERP( NCOLS, NROWS, NLAYS, PLAYS, BADVAL,
     &                         NDEX,  COEF,  VSIG,  VPRS )
            INTEGER         NCOLS, NROWS, NLAYS, PLAYS
            REAL            BADVAL
            INTEGER         NDEX( NCOLS, NROWS, PLAYS )
            REAL            COEF( NCOLS, NROWS, PLAYS )
            REAL            VPRS( NCOLS, NROWS, PLAYS )
            REAL            VSIG( NCOLS, NROWS, NLAYS )

            INTEGER         C, R, L, K
            REAL            P, Q

            !!..........................................................

            DO  L = 1, PLAYS
            DO  R = 1, NROWS
            DO  C = 1, NCOLS
                P = COEF( C,R,L )
                IF ( P .LE. 1.0 ) THEN
                    K = NDEX( C,R,L )
                    Q = 1.0 - P
                    VPRS( C,R,L ) = P*VSIG( C,R,K ) + Q*VSIG( C,R,K+1 )
                ELSE
                    VPRS( C,R,L ) = BADVAL
                END IF
            END DO
            END DO
            END DO

            RETURN

            END SUBROUTINE  PTERP

        END PROGRAM PRESTERP

