
PROGRAM  BCWNDW

    !!***********************************************************************
    !! Version "$Id: bcwndw.f90 117 2019-06-15 14:56:29Z coats $"
    !! EDSS/Models-3 M3TOOLS.
    !! Copyright (C) 1992-2002 MCNC, (C) 1995-2002,2005-2013 Carlie J. Coats, Jr.,
    !! and (C) 2002-2010 Baron Advanced Meteorological Systems. LLC.,
    !! and (C) 2015 UNC Institute for the Environment
    !! Distributed under the GNU GENERAL PUBLIC LICENSE version 2
    !! See file "GPL.txt" for conditions of use.
    !!.........................................................................
    !!  program body starts at line  89
    !!
    !!  FUNCTION:
    !!       Window a subrectangle of the grid from gridded input file
    !!       specified time period, and write it to the output file.
    !!
    !!  PRECONDITIONS REQUIRED:
    !!       Machine with stack-allocated AUTO local variables (e.g., CRAY)
    !!       consistency with FORIO:PARMS3.EXT for name and description lengths.
    !!
    !!  SUBROUTINES AND FUNCTIONS CALLED:
    !!       Models-3 I/O.
    !!
    !!  REVISION  HISTORY:
    !!      Prototype 5/96 by CJC
    !!
    !!      Modified  9/99 by CJC for enhanced portability
    !!
    !!      Version  11/2001 by CJC for I/O API Version 2.1
    !!
    !!      Version  02/2010 by CJC for I/O API v3.1:  Fortran-90 only;
    !!      USE M3UTILIO, and related changes.
    !!
    !!      Version  01/2015 by CJC for I/O API v3.2:  F90 free-format source
    !!
    !!      Version  06/2019 by CJC:  Bugfix for RUNLEN
    !!***********************************************************************

    USE M3UTILIO
    IMPLICIT NONE

    !!...........   EXTERNAL FUNCTIONS and their descriptions:

    INTEGER :: IARGC

    !!...........   PARAMETERS and their descriptions:

    CHARACTER*16, PARAMETER :: PNAME   = 'BCWNDW'
    CHARACTER*16, PARAMETER :: BLANK16 = ' '

    !!...........   LOCAL VARIABLES and their descriptions:

    INTEGER         LOGDEV  !  unit number for log file
    INTEGER         ARGCNT  !  number of command-line args, from IARGC()
    CHARACTER*256   ENVBUF  !  value from command line arguments
    CHARACTER*256   MESG    !  message buffer for M3EXIT(), etc.

    CHARACTER*16    INAME   !  logical name of the  input file
    CHARACTER*16    WNAME   !  logical name of the output file

    INTEGER         NTHIK   ! boundary thickness dimension
    INTEGER         NCOLS   ! grid dimensions, from INAME header
    INTEGER         NROWS   ! grid dimensions, from INAME header
    INTEGER         NLAYS   ! grid dimensions, from INAME header
    INTEGER         NVARS   !  number of vbles in WNAME
    INTEGER         SDATE   !  starting date, from user
    INTEGER         STIME   !  starting time, from user
    INTEGER         JDATE   !  current date
    INTEGER         JTIME   !  current time
    INTEGER         EDATE   !  ending date
    INTEGER         ETIME   !  ending time
    INTEGER         TSTEP   !  time step, from INAME header
    INTEGER         TSECS
    INTEGER         RUNLEN  !  duration, HHMMSS from user
    INTEGER         NSTEPS  !  duration in TSTEPs
    INTEGER         I       !  scratch variables
    INTEGER         LOCOL   !  window boundary
    INTEGER         HICOL   !  window boundary
    INTEGER         LOROW   !  window boundary
    INTEGER         HIROW   !  window boundary
    INTEGER         ISTAT       !  allocation-status
    LOGICAL         EFLAG

    !!.........................................................................
    !!   begin body of program  BCWNDW

    EFLAG  = .FALSE.
    LOGDEV = INIT3()
    WRITE ( LOGDEV,'( 5X , A )' ) ' ',                                      &
'Program BCWNDW to construct BOUNDARY CONDITIONs for windowed models',      &
'on a specified subgrid from "normal" GRIDDED (e.g., concentration)',       &
'file output for a specified time period to a specified subgrid and',       &
'write them to a BOUNDARY file.',                                           &
'',                                                                         &
'USAGE:  m3wndw [INFILE OUTFILE]  (and then answer the prompts). ',         &
'',                                                                         &
'PRECONDITIONS REQUIRED:',                                                  &
'',                                                                         &
'    setenv <input  file>    <path-name>',                                  &
'    setenv <output file>    <path-name>',                                  &
'',                                                                         &
'    All variables in the input file are REAL or INTEGER',                  &
'',                                                                         &
'THE PROGRAM WILL PROMPT YOU for the logical names of the input and',       &
'output files, if these are not supplied on the command line, and',         &
'for the specifications (low- and high- column and row) for the',           &
'grid whose boundary-file is being constructed and for the duration',       &
'RUNLEN (HHMMSS)',                                                          &
'Note that RUNLEN=0 for single-step runs (a "fencepost problem")',          &
'',                                                                         &
'See URL',                                                                  &
'',                                                                         &
'   https://www.cmascenter.org/ioapi/documentation/3.1/html/AA.html#tools', &
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
'$Id: bcwndw.f90 117 2019-06-15 14:56:29Z coats $',&
' '

    ARGCNT = IARGC()

    IF ( ARGCNT .EQ. 0 ) THEN       !  get names from user

        MESG  = 'Enter logical name for  INPUT FILE'
        INAME = PROMPTMFILE( MESG, FSREAD3, 'INFILE', PNAME )

    ELSE IF ( ARGCNT .EQ. 2 ) THEN

        CALL GETARG( 1, ENVBUF )
        INAME = ENVBUF( 1:16 )
        IF ( .NOT. OPEN3( INAME, FSREAD3, PNAME ) ) THEN
            MESG = 'Could not open input file ' // INAME
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
        END IF

        CALL GETARG( 2, ENVBUF )
        WNAME = ENVBUF( 1:16 )

    ELSE

        CALL M3EXIT( PNAME, 0, 0, 'usage:  m3wndw [INFILE OUTFILE]', 2 )

    END IF

    IF ( .NOT. DESC3( INAME ) ) THEN
        MESG = 'Could not get description of input file ' // INAME
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
    ELSE IF ( FTYPE3D .NE. GRDDED3 ) THEN

        WRITE( MESG, '(3 A, I4, 2X, A )' )      &
             'Input file "', TRIM( INAME ),     &
             '" has type', FTYPE3D, '(type GRDDED3==1 required)'
        CALL M3EXIT( PNAME, 0, 0, MESG, 3 )

    END IF

    NCOLS  = NCOLS3D
    NROWS  = NROWS3D
    NLAYS  = NLAYS3D
    NVARS  = NVARS3D
    SDATE  = SDATE3D
    STIME  = STIME3D
    TSTEP  = TSTEP3D


    !!.......   Get starting date and time, and duration:

    IF ( TSTEP .EQ. 0 ) THEN        !  time-independent file

        SDATE  = 0
        STIME  = 0
        NSTEPS = 1

    ELSE                            !  time-dependent file

        CALL LASTTIME( SDATE, STIME, TSTEP, MXREC3D, EDATE, ETIME )
        SDATE  = GETNUM( SDATE3D, 9999999, SDATE3D, 'Enter starting date (YYYYDDD) for run' )
        STIME  = GETNUM(       0,  239999, STIME3D, 'Enter starting time (HHMMSS) for run' )
        TSECS  = SEC2TIME( MXREC3D * TIME2SEC( TSTEP3D ) )
        RUNLEN = GETNUM( 0, 999999999, TSECS, 'Enter duration (HHMMSS) for run' )
        JDATE = SDATE
        JTIME = STIME
        CALL NEXTIME( JDATE, JTIME, RUNLEN )
        NSTEPS = CURREC( JDATE,JTIME,SDATE,STIME,TSTEP,EDATE,ETIME )

    END IF          !  time-independent file, or not


    !!.......   Build description for the output file, and create accordingly:
    !!.......   Re-use most of the input-file description.

    SDATE3D = SDATE
    STIME3D = STIME

    WRITE ( *, '( 5X, 5A, I5, 2X, A, I5 )' )        &
        'Input file "', TRIM( INAME ),              &
        '" has grid "', TRIM( GDNAM3D ),            &
        '" with', NCOLS3D, 'cols and', NROWS3D, 'rows.'
    WRITE ( *,'( 5X , A )' )                        &
        ' ',                                        &
        TRIM( MESG ),                               &
        'Now enter the window specifications.  These will be of the',   &
        'form GRID-NAME, LOCOL, HICOL, LOROW, HIROW, where',            &
        ' ',                                        &
        '        LOCOL <= col <= HICOL',            &
        '        LOROW <= row <= HIROW',            &
        ' '

    MESG = 'WNDW_' // GDNAM3D
    CALL GETSTR( 'Enter name for windowed grid', MESG( 1:16 ),  GDNAM3D )

    I = -MIN( NCOLS-1, NROWS-1 ) / 2
    NTHIK   = GETNUM( 1, I, 1, 'Enter boundary thickness NTHIK' )
    NTHIK3D = NTHIK
    FTYPE3D = BNDARY3

    LOCOL   = GETNUM( NTHIK, NCOLS - NTHIK, 1, 'Enter LOCOL' )
    HICOL   = GETNUM( LOCOL, NCOLS - NTHIK, 1, 'Enter HICOL' )
    LOROW   = GETNUM( NTHIK, NROWS - NTHIK, 1, 'Enter LOROW' )
    HIROW   = GETNUM( LOROW, NROWS - NTHIK, 1, 'Enter HIROW' )

    XORIG3D = XORIG3D + DBLE( LOCOL - 1 ) * XCELL3D
    YORIG3D = YORIG3D + DBLE( LOROW - 1 ) * YCELL3D
    NCOLS3D = HICOL - LOCOL + 1
    NROWS3D = HIROW - LOROW + 1

    IF ( ARGCNT .EQ. 0 ) THEN
        WNAME = PROMPTMFILE( 'Enter logical name for OUTPUT FILE', FSUNKN3, 'OUTFILE', PNAME )
    ELSE    !  argcnt = 2:
        IF ( .NOT. OPEN3( WNAME, FSUNKN3, PNAME ) ) THEN
            CALL M3EXIT( PNAME, 0, 0, 'Could not open output file ' // WNAME, 2 )
        END IF
    END IF          !  if argcnt zero, or 2


    !!.......   Process this period in the input file:

    JDATE = SDATE
    JTIME = STIME

    DO  I = 1, NSTEPS

        CALL BCSTEP( NCOLS, NROWS, NLAYS, NVARS,            &
                     LOCOL, HICOL, LOROW, HIROW, NTHIK,     &
                     JDATE, JTIME, INAME, WNAME, LOGDEV )

        CALL NEXTIME( JDATE, JTIME, TSTEP )

    END DO        !  end loop on time steps



    IF ( EFLAG ) THEN
        MESG  = 'Failure in program'
        ISTAT = 2
    ELSE
        MESG  = 'Success in program'
        ISTAT = 0
    END IF

    CALL M3EXIT( PNAME, 0, 0, MESG, ISTAT )


CONTAINS    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    SUBROUTINE  BCSTEP( NCOLS, NROWS, NLAYS, NVARS,     &
                LOCOL, HICOL, LOROW, HIROW, NTHIK,      &
                JDATE, JTIME, INAME, WNAME, LOGDEV )

        IMPLICIT NONE


        !!...........   ARGUMENTS and their descriptions:

        INTEGER     , INTENT( IN ) :: NCOLS   ! input horiz grid dimension
        INTEGER     , INTENT( IN ) :: NROWS   ! input horiz grid dimension
        INTEGER     , INTENT( IN ) :: NLAYS   ! vertical dimension
        INTEGER     , INTENT( IN ) :: NVARS   ! vbles    dimension
        INTEGER     , INTENT( IN ) :: LOCOL   !  left   window boundary
        INTEGER     , INTENT( IN ) :: HICOL   !  right  window boundary
        INTEGER     , INTENT( IN ) :: LOROW   !  bottom window boundary
        INTEGER     , INTENT( IN ) :: HIROW   !  top    window boundary
        INTEGER     , INTENT( IN ) :: NTHIK   !  boundary thickness
        INTEGER     , INTENT( IN ) :: JDATE   ! current model date
        INTEGER     , INTENT( IN ) :: JTIME   ! current model time
        CHARACTER*16, INTENT( IN ) :: INAME   !  logical name of the  input file
        CHARACTER*16, INTENT( IN ) :: WNAME   !  logical name of the output file
        INTEGER     , INTENT( IN ) :: LOGDEV  ! unit number for output

        !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         I, C, R, L, V, T
        REAL            GRID( NCOLS, NROWS, NLAYS )
        REAL            BNDY( 2*NTHIK*( NCOLS3D + NROWS3D + 2*NTHIK ), NLAYS )

        CHARACTER*256   MESG


        !!***********************************************************************
        !!   begin body of subroutine  BCSTEP
        !!   NOTE:  READ3() and WRITE3() are thread-safe

!$OMP    PARALLEL DO                                                    &
!$OMP&       DEFAULT( NONE ),                                           &
!$OMP&        SHARED( NVARS, NLAYS, NTHIK,  LOROW, HIROW, GRID,         &
!$OMP&                INAME, WNAME, DATE, JTIME, VNAME3D ),             &
!$OMP&       PRIVATE( V, L, C, R, I, BNDY )

        DO  V = 1, NVARS

            IF ( .NOT. READ3( INAME, VNAME3D( V ), ALLAYS3, JDATE, JTIME, GRID ) ) THEN
                EFLAG = .TRUE.
                MESG  = 'Read failure:  file ' // INAME
                CALL M3MESG( MESG )
                CYCLE
            END IF              !  if read3() worked, or not


            !!.......   Extract the boundary from this subgrid:

            DO  L = 1, NLAYS

                I = 0           !  perimeter subscript for this layer

                DO  R = LOROW - NTHIK, LOROW - 1        !  south bdy component
                DO  C = LOCOL, HICOL + NTHIK
                    I = I + 1
                    BNDY( I,L ) = GRID( C,R,L )
                END DO
                END DO

                DO  R = LOROW, HIROW + NTHIK            !  east bdy
                DO  C = HICOL + 1, HICOL + NTHIK
                    I = I + 1
                    BNDY( I,L ) = GRID( C,R,L )
                END DO
                END DO

                DO  R = HIROW + 1, HIROW + NTHIK        !  north
                DO  C = LOCOL - NTHIK, HICOL
                    I = I + 1
                    BNDY( I,L ) = GRID( C,R,L )
                END DO
                END DO

                DO  R = LOROW - NTHIK, HIROW            !  west
                DO  C = LOCOL - NTHIK, LOCOL - 1
                    I = I + 1
                    BNDY( I,L ) = GRID( C,R,L )
                END DO
                END DO

            END DO      !  end loop on layers

            IF ( .NOT. WRITE3( WNAME, VNAME3D( V ), JDATE, JTIME, BNDY ) ) THEN
                EFLAG = .TRUE.
                MESG  = 'ERROR:  Write failure:  file ' // WNAME
                CALL M3MESG( MESG )
            END IF              !  if read3() worked, or not

        END DO      !  end loop on variables

        RETURN

    END SUBROUTINE  BCSTEP


END PROGRAM  BCWNDW

