
PROGRAM  M3WNDW

    !!***********************************************************************
    !! Version "$Id: m3wndw.f90 117 2019-06-15 14:56:29Z coats $"
    !! EDSS/Models-3 M3TOOLS.
    !! Copyright (C) 1992-2002 MCNC,
    !! (C) 1995-2002,2005-2014 Carlie J. Coats, Jr.,
    !! and (C) 2002-2010 Baron Advanced Meteorological Systems. LLC.,
    !! and (C) 2015-2016 UNC Institute for the Environment
    !! Distributed under the GNU GENERAL PUBLIC LICENSE version 2
    !! See file "GPL.txt" for conditions of use.
    !!.........................................................................
    !!  program body starts at line  111
    !!
    !!  FUNCTION:
    !!       Window a subrectangle of the grid from gridded input file
    !!       specified time period, and write it to the output file.
    !!
    !!  PRECONDITIONS REQUIRED:
    !!
    !!  SUBROUTINES AND FUNCTIONS CALLED:
    !!       Models-3 I/O.
    !!
    !!  REVISION  HISTORY:
    !!       Prototype 5/1996 by CJC
    !!
    !!       Modified 10/1999 by CJC -- Fortran standards conformance
    !!
    !!       Version  11/2001 by CJC for I/O API Version 2.1
    !!
    !!       Version  10/2006 by CJC -- remove unused FLTERR()
    !!
    !!       Version  02/2010 by CJC for I/O API v3.1:  Fortran-90 only;
    !!       USE M3UTILIO, and related changes.  Inline WNDWSTEP().
    !!       Bug-fix  01/2014 by CJC for GRDCHK3() call
    !!
    !!      Version  02/2015 by CJC for I/O API v3.2:  F90 free-format source;
    !!      expand WNDW for M3DBLE and M3INT8 variables.
    !!
    !!      Version  06/2016 by CJC:  copy CMAQ metadata, if present
    !!
    !!      Version  06/2019 by CJC:  Bugfix for RUNLEN
    !!***********************************************************************

    USE M3UTILIO
    USE MODATTS3

    IMPLICIT NONE

    !!...........   EXTERNAL FUNCTIONS and their descriptions:

     INTEGER :: IARGC

    !!...........   PARAMETERS and their descriptions:

    CHARACTER*16, PARAMETER :: BLANK = ' '
    CHARACTER*16, PARAMETER :: PNAME = 'M3WNDW'
    CHARACTER*72, PARAMETER :: BAR   = &
    '-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-'

    !!...........   LOCAL VARIABLES and their descriptions:

    INTEGER         LOGDEV  !  unit number for log file
    INTEGER         ARGCNT  !  number of command-line args, from IARGC()
    INTEGER         ISTAT   !  status for ALLOCATE()
    CHARACTER*256   ENVBUF  !  value from command line arguments
    CHARACTER*256   MESG    !  message buffer for M3EXIT(), etc.

    CHARACTER*16    INAME   !  logical name of the  input file
    CHARACTER*16    WNAME   !  logical name of the output file
    CHARACTER*16    CNAME   !  output coordinate system name

    INTEGER         NCOLS   ! grid dimensions, from INAME header
    INTEGER         NROWS   ! grid dimensions, from INAME header
    INTEGER         NLAYS   ! grid dimensions, from INAME header
    INTEGER         NVARS   !  number of vbles in WNAME
    INTEGER         SDATE   !  starting date, from user
    INTEGER         STIME   !  starting time, from user
    INTEGER         EDATE   ! ending date
    INTEGER         ETIME   ! ending time
    INTEGER         JDATE   !  current date
    INTEGER         JTIME   !  current time
    INTEGER         TSTEP   !  time step, from INAME header
    INTEGER         RUNLEN  !  duration, HHMMSS from user
    INTEGER         NSTEPS  !  duration in TSTEPs
    INTEGER         I       !  scratch variables
    INTEGER         LOCOL   !  window boundary
    INTEGER         HICOL   !  window boundary
    INTEGER         LOROW   !  window boundary
    INTEGER         HIROW   !  window boundary

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

    LOGICAL         EFLAG, CFLAG

    REAL, ALLOCATABLE :: WNDW( : )

    !!.........................................................................
    !!   begin body of program  M3WNDW

    EFLAG  = .FALSE.
    LOGDEV = INIT3()
    WRITE ( *,'( 5X, A )' ) '', BAR, '',                                &
'Program M3WNDW to window variables to a specified subgrid from a ',    &
'GRIDDED Models-3 file for a specified time period, and write them',    &
'to another such file.', ' ',                                           &
'You need to have set environment variables for the input and output',  &
'file logical names, and optionally for the GRIDDESC file.',            &
'You will be asked to select the window into the input grid, either',   &
'by GRIDDESC grid-name or by entering the window-boundary row and',     &
'column numbers, and time period to be windowed.',                      &
'Note that RUNLEN=0 for single-step runs  (a "fencepost" problem)',     &
' ',                                                                    &
'USAGE:  m3wndw [INFILE OUTFILE]   (and then answer the prompts). ',    &
' ',                                                                    &
'Program copyright (C) 1992-2002 MCNC,',                                &
'(C) 1995-2013 Carlie J. Coats, Jr.,',                                  &
'(C) 2002-2010 Baron Advanced Meteorological Systems, LLC., and',       &
'(C) 2015 UNC Institute for the Environment.',                          &
'Released under Version 2',                                             &
'of the GNU General Public License. See enclosed GPL.txt,',             &
'or URL',                                                               &
' ',                                                                    &
'    https://www.gnu.org/licenses/old-licenses/gpl-2.0.html',           &
' ',                                                                    &
'See URL',                                                              &
' ',                                                                    &
'    https://www.cmascenter.org/ioapi/documentation/3.1/html#tools',    &
' ',                                                                    &
'Comments and questions are welcome and can be sent to',                &
'',                                                                     &
'    Carlie J. Coats, Jr.    carlie@jyarborough.com',                   &
'or',                                                                   &
'    UNC Institute for the Environment',                                &
'    100 Europa Dr., Suite 490 Rm 405',                                 &
'    Campus Box 1105',                                                  &
'    Chapel Hill, NC 27599-1105',                                       &
'',                                                                     &
'Program version: ',                                                    &
'$Id: m3wndw.f90 117 2019-06-15 14:56:29Z coats $',&
' '

    ARGCNT = IARGC()

    IF ( ARGCNT .EQ. 0 ) THEN       !  get names from user

        INAME = PROMPTMFILE( 'Enter logical name for  INPUT FILE', FSREAD3, 'INFILE', PNAME )

    ELSE IF ( ARGCNT .EQ. 2 ) THEN

        CALL GETARG( 1, ENVBUF )
        INAME = ENVBUF( 1:16 )
        IF ( .NOT. OPEN3( INAME, FSREAD3, PNAME ) ) THEN
            MESG = 'Could not open input file ' // INAME
            CALL M3EXIT( PNAME, 0, 0, MESG, 3 )
        END IF

        CALL GETARG( 2, ENVBUF )
        WNAME = ENVBUF( 1:16 )

    ELSE

        CALL M3EXIT( PNAME, 0, 0, 'USAGE:  m3wndw [INFILE OUTFILE]', 2 )

    END IF

    IF ( .NOT. DESC3( INAME ) ) THEN
        MESG = 'Could not get description of input file ' // INAME
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
    END IF

    IF ( ISCMAQ( INAME ) ) THEN
        CFLAG = ENVYN( 'COPY_META', 'Copy CMAQ metadata to output file?', .TRUE., ISTAT )
        IF ( ISTAT .GT. 0 ) THEN
            CALL M3EXIT( PNAME, 0, 0, 'Bad environment variable "COPY_META"', 2 )
        ELSE IF ( .NOT.CFLAG ) THEN
            CONTINUE
        ELSE IF ( .NOT.GETCMAQ( INAME ) ) THEN
            CALL M3EXIT( PNAME, 0, 0, 'Could not get CMAQ metadata for ' // INAME, 2 )
        END IF
    END IF

    IF ( FTYPE3D .NE. GRDDED3 ) THEN

        WRITE( MESG, 94010 )                    &
            'Input file "' // TRIM( INAME ) //  &
            '" has type', FTYPE3D,              &
            '(type GRDDED3==1 required)'
        CALL M3EXIT( PNAME, 0, 0, MESG, 3 )

    END IF

    NCOLS  = NCOLS3D
    NROWS  = NROWS3D
    NLAYS  = NLAYS3D
    NVARS  = NVARS3D
    SDATE  = SDATE3D
    STIME  = STIME3D
    TSTEP  = TSTEP3D
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
    CALL LASTTIME( SDATE, STIME, TSTEP, MXREC3D, EDATE, ETIME )


    !!.......   Get starting date and time, and duration:

    IF ( TSTEP .EQ. 0 ) THEN        !  time-independent file

        SDATE  = 0
        STIME  = 0
        NSTEPS = 1

    ELSE                            !  time-dependent file

        SDATE  = GETNUM( SDATE3D, 9999999, SDATE3D, 'Enter starting date (YYYYDDD) for run' )
        STIME  = GETNUM(       0,  239999, STIME3D, 'Enter starting time  (HHMMSS) for run' )
        RUNLEN = SEC2TIME( SECSDIFF( SDATE, STIME, EDATE, ETIME ) )
        RUNLEN = GETNUM( 0, 999999999, RUNLEN,      'Enter duration (HHMMSS) for run' )
        JDATE = SDATE
        JTIME = STIME
        CALL NEXTIME( JDATE, JTIME, RUNLEN )
        NSTEPS = CURREC( JDATE, JTIME, SDATE, STIME, TSTEP, EDATE, ETIME )
 
    END IF          !  time-independent file, or not


    !!.......   Build description for the output file, and create accordingly:
    !!.......   Re-use most of the input-file description.

    SDATE3D = SDATE
    STIME3D = STIME

    WRITE ( MESG,94010 )                                                &
        'Input file "' // TRIM( INAME ) //                              &
        '" has grid "' // TRIM( GDNAM3D ) //                            &
        '" with', NCOLS3D, 'cols and', NROWS3D, 'rows.'
    WRITE ( *, '( 5X, A )' ) ' ',                                       &
        TRIM( MESG ),                                                   &
        'Now enter the window specifications.  These will be of the',   &
        'form GRID-NAME, and (if the grid is not in the current ',      &
        'GRIDDESC, the LOCOL, HICOL, LOROW, HIROW relative to the',     &
        'input file grid, where',                                       &
        ' ',                                                            &
        '        LOCOL <= col <= HICOL',                                &
        '        LOROW <= row <= HIROW',                                &
        ' '
    MESG = 'WNDW_' // GDNAM3D
    CALL GETSTR( 'Enter name for windowed grid', MESG( 1:16 ),  GDNAM3D )

    IF ( DSCGRID( GDNAM3D, CNAME, GDTYP3D,        &
                  P_ALP3D, P_BET3D,P_GAM3D, XCENT3D, YCENT3D,        &
                  XORIG3D, YORIG3D, XCELL3D, YCELL3D,        &
                  NCOLS3D, NROWS3D, NTHIK3D ) ) THEN

        EFLAG = .FALSE.

        IF ( GDTYP1 .NE. GDTYP3D ) THEN
            MESG = 'Coordinate system type mismatch'
            CALL M3MSG2( MESG )
            EFLAG = .TRUE.
        ELSE IF ( .NOT.GRDCHK3( GDNAM3D,        &
                        P_ALP1, P_BET1, P_GAM1, XCENT1, YCENT1,        &
                        XORIG3D, YORIG3D, XCELL1, YCELL1,        &
                        1, VGTYP3D, VGTOP3D, VGLVS3D ) ) THEN
            MESG = 'Inconsistent coord/grid  for ' // GDNAM3D
            EFLAG = .TRUE.
            CALL M3MESG( MESG )
        END IF

        IF ( EFLAG ) THEN
            MESG = 'Bad setup for this run'
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
        END IF

        LOCOL   = 1 + NINT( ( XORIG3D - XORIG1 ) / XCELL3D )
        HICOL   = LOCOL + NCOLS3D - 1
        LOROW   = 1 + NINT( ( YORIG3D - YORIG1 ) / YCELL3D )
        HIROW   = LOROW + NROWS3D - 1

    ELSE

        LOCOL   = GETNUM( 1,     NCOLS3D, 1, 'Enter LOCOL' )
        HICOL   = GETNUM( LOCOL, NCOLS3D, 1, 'Enter HICOL' )
        LOROW   = GETNUM( 1,     NROWS3D, 1, 'Enter LOROW' )
        HIROW   = GETNUM( LOROW, NROWS3D, 1, 'Enter HIROW' )

        XORIG3D = XORIG3D + DBLE( LOCOL - 1 ) * XCELL3D
        YORIG3D = YORIG3D + DBLE( LOROW - 1 ) * YCELL3D
        NCOLS3D = HICOL - LOCOL + 1
        NROWS3D = HIROW - LOROW + 1

    END IF


    !!...............   Allocate I/O Buffer:

    ALLOCATE ( WNDW( 2*NCOLS3D*NROWS3D*NLAYS*NVARS ), STAT = ISTAT )

    IF ( ISTAT .NE. 0 ) THEN
        WRITE( MESG, '( A, I10 )' ) 'Memory allocation failed:  STAT=', ISTAT
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
    END IF


    !!...............   Get output file:

    IF ( ARGCNT .EQ. 0 ) THEN
        WNAME = PROMPTMFILE( 'Enter logical name for OUTPUT FILE', FSUNKN3, 'OUTFILE', PNAME )
    ELSE    !  argcnt = 2:
        IF ( .NOT. OPEN3( WNAME, FSUNKN3, PNAME ) ) THEN
            MESG = 'Could not open output file ' // WNAME
            CALL M3EXIT( PNAME, SDATE, STIME, MESG, 2 )
        END IF
    END IF          !  if argcnt zero, or 2


    !!.......   Process this period in the input file:

    JDATE = SDATE
    JTIME = STIME

    DO  I = 1, NSTEPS

        IF ( .NOT. XTRACT3( INAME, ALLVAR3, 1, NLAYS,       &
                            LOROW, HIROW, LOCOL, HICOL,     &
                            JDATE, JTIME, WNDW ) ) THEN
            EFLAG = .TRUE.
            CALL M3MESG( 'ERROR:  Read-failure' )
        ELSE IF ( .NOT. WRITE3( WNAME, ALLVAR3, JDATE, JTIME, WNDW ) ) THEN
            EFLAG = .TRUE.
            CALL M3MESG( 'ERROR:  Write-failure' )
        END IF              !  if read3() worked, or not

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


    !!..........................   FORMAT STATEMENTS  ....................
    !!...........   Internal bufferring formats  ......... 94xxx

94010   FORMAT ( 10 ( A, :, I7, :, 2X ) )


END PROGRAM M3WNDW

