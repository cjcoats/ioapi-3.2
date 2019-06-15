
PROGRAM  M3TSHIFT

    !!***********************************************************************
    !! Version "$Id: m3tshift.f90 117 2019-06-15 14:56:29Z coats $"
    !! EDSS/Models-3 M3TOOLS.
    !! Copyright (C) 1992-2002 MCNC,
    !! (C) 1995-2002,2005-2014, 2017 Carlie J. Coats, Jr.,
    !! (C) 2002-2010 Baron Advanced Meteorological Systems. LLC.,
    !! and (C) 2014-2016 UNC Institute for the Environment
    !! Distributed under the GNU GENERAL PUBLIC LICENSE version 2
    !! See file "GPL.txt" for conditions of use.
    !!.........................................................................
    !!  program body starts at line  93
    !!
    !!  FUNCTION:
    !!       extracts a subset of variables from the input file for a
    !!       specified time period, and writes them to the output file.
    !!
    !!  PRECONDITIONS REQUIRED:
    !!       Machine with stack-allocated AUTO local variables (e.g., CRAY)
    !!       consistency with FORIO:PARMS3.EXT for name and description lengths.
    !!
    !!  SUBROUTINES AND FUNCTIONS CALLED:
    !!       GETNUM, NEXTIME, STATSTEP, Models-3 I/O.
    !!
    !!  REVISION  HISTORY:
    !!       Prototype 1/1995 by CJC
    !!       Version   5/1995 by CJC:  command line arguments
    !!       Modified 10/1999 by CJC:  Fortran standards conformance
    !!       Version  11/2001 by CJC for I/O API Version 2.1
    !!       Version  02/2010 by CJC for I/O API v3.1:  Fortran-90 only;
    !!       USE M3UTILIO, and related changes.
    !!
    !!      Version  02/2015 by CJC for I/O API v3.2:  F90 free-format source;
    !!      inlined SUBROUTINE TSHIFT, explicit ALLOCATE. Support for M3INT8 variables.
    !!
    !!      Version  06/2016 by CJC:  copy CMAQ metadata, if present
    !!
    !!      Version  09/2017 by CJC for I/O API v3.2:  Enhanced default RUNLEN
    !!***********************************************************************

    USE M3UTILIO
    USE MODATTS3

    IMPLICIT NONE

    !!...........   EXTERNAL FUNCTIONS and their descriptions:

     INTEGER :: IARGC

    !!...........   PARAMETERS and their descriptions:

    CHARACTER*16, PARAMETER :: PNAME = 'M3TSHIFT'
    CHARACTER*16, PARAMETER :: BLANK = ' '
    CHARACTER*64, PARAMETER :: BAR   = '-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-'

    !!...........   LOCAL VARIABLES and their descriptions:

    INTEGER         LOGDEV  !  unit number for log file
    INTEGER         ARGCNT  !  number of command-line args, from IARGC()
    CHARACTER*256   ENVBUF  !  value from command line arguments
    CHARACTER*256   MESG    !  for M3WARN(), M3EXIT()

    CHARACTER*16    INAME   !  logical name of the  input file
    CHARACTER*16    ONAME   !  logical name of the output file

    INTEGER         SIZE    ! volume of one variable
    INTEGER         SDATE   !  starting  input date, from user
    INTEGER         STIME   !  starting  input time, from user
    INTEGER         TDATE   !  starting output date, from user
    INTEGER         TTIME   !  starting output time, from user
    INTEGER         EDATE   ! ending date
    INTEGER         ETIME   ! ending time
    INTEGER         JDATE   !  current  input date
    INTEGER         JTIME   !  current  input time
    INTEGER         KDATE   !  current output date
    INTEGER         KTIME   !  current output time
    INTEGER         TSTEP   !  time step, from INAME header
    INTEGER         TSOUT   !  output time step
    INTEGER         RUNLEN  !  duration, HHMMSS from user
    INTEGER         NSTEPS  !  duration in TSTEPs
    INTEGER         I       !  loop counter (time step #)
    INTEGER         ISTAT

    LOGICAL         EFLAG, CFLAG
    
    REAL*8, ALLOCATABLE :: RBUF ( : )   !!  over-size except for M3DBLE, M3INT8...

    !!.........................................................................
    !!   begin body of program  M3TSHIFT

    EFLAG  = .FALSE.
    LOGDEV = INIT3()
    WRITE( *,'( 5X, A )' ) BLANK, BAR, BLANK,                               &
'Program M3TSHIFT to copy a selected time period from a Models-3 file',     &
'to a different time period in a different Models-3 file.',                 &
'',                                                                         &
'You need to have set environment variables for the input and output',      &
'file logical names.  You will be asked to select the time period to be',   &
'copied and the start of the time period to receive the results.',          &
'Note that RUNLEN=0 for single-step runs (a "fencepost" problem)',          &
'',                                                                         &
'USAGE:  m3tproc [INFILE OUTFILE]     (and then answer the prompts).',      &
'',                                                                         &
'See URL',                                                                  &
'',                                                                         &
'   https://www.cmascenter.org/ioapi/documentation/3.1/html/AA.html#tools', &
'',                                                                         &
'Program copyright (C) 1992-2002 MCNC, (C) 1995-2013 Carlie J. Coats, Jr.', &
'(C) 2002-2010 Baron Advanced Meteorological Systems, LLC., and',           &
'(C) 2014-2016 UNC Institute for the Environment.',                         &
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
'$Id: m3tshift.f90 117 2019-06-15 14:56:29Z coats $',&
' '

    ARGCNT = IARGC()

    IF ( ARGCNT .EQ. 1  .OR.  ARGCNT .GT. 2 ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'usage:  m3tshift [INFILE OUTFILE]', 2 )
    END IF

    IF ( ARGCNT .EQ. 0 ) THEN       !  get names from user

        INAME = PROMPTMFILE( 'Enter INPUT FILE name', FSREAD3, 'INFILE', PNAME )

    ELSE        !  argcnt 2

        CALL GETARG( 1, ENVBUF )
        INAME = ENVBUF( 1:16 )
        IF ( .NOT. OPEN3( INAME, FSREAD3, PNAME ) ) THEN
            MESG = 'Could not open input file "' // TRIM( INAME ) // '"'
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
        END IF

        CALL GETARG( 2, ENVBUF )
        ONAME = ENVBUF( 1:16 )

    END IF


    IF ( .NOT. DESC3( INAME ) ) THEN
        MESG = 'Could not get description of input file "' // TRIM( INAME ) // '"'
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

    IF ( FTYPE3D .EQ. CUSTOM3 ) THEN
        SIZE = NCOLS3D * NLAYS3D * NVARS3D
    ELSE IF ( FTYPE3D .EQ. GRDDED3 ) THEN
        SIZE = NCOLS3D * NROWS3D * NLAYS3D * NVARS3D
    ELSE IF ( FTYPE3D .EQ. BNDARY3 ) THEN
        SIZE = 2 * NTHIK3D * NLAYS3D * NVARS3D * ( NCOLS3D + NROWS3D + 2 )
    ELSE IF ( FTYPE3D .EQ. IDDATA3 ) THEN
        SIZE = 1 + NROWS3D * NVARS3D * ( NLAYS3D + 1 )
    ELSE IF ( FTYPE3D .EQ. PROFIL3 ) THEN
        SIZE = 1 + NROWS3D * ( 7 + NVARS3D * NLAYS3D )
    ELSE IF ( FTYPE3D .EQ. GRNEST3 ) THEN
        SIZE = 1 + NROWS3D * ( 12 + NCOLS3D * NVARS3D )
    ELSE
        WRITE( MESG, '( 3A, I5 )' ) 'Input file "', TRIM( INAME ), '" has unsupported type', FTYPE3D
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
    END IF

    SDATE  = SDATE3D
    STIME  = STIME3D
    TSTEP  = TSTEP3D
    CALL LASTTIME( SDATE, STIME, TSTEP, MXREC3D, EDATE, ETIME )


    !!.......   Get starting date and time, and duration:

    IF ( TSTEP .EQ. 0 ) THEN        !  time-independent file

        SDATE  = 0
        STIME  = 0
        TDATE  = 0
        TTIME  = 0
        NSTEPS = 1

    ELSE                            !  time-dependent file

        SDATE  = GETNUM( SDATE3D, 9999999, SDATE3D, 'Enter starting (source) date (YYYYDDD) for run' )
        STIME  = GETNUM(       0,  239999, STIME3D, 'Enter starting (source) time  (HHMMSS) for run' )

        TDATE  = GETNUM( 0, 9999999, SDATE3D, 'Enter target date (YYYYDDD) for run' )
        TTIME  = GETNUM( 0, 239999,  STIME3D, 'Enter target time  (HHMMSS) for run' )
        TSOUT  = GETNUM( 0, 999999999, TSTEP, 'Enter output time step (HHMMSS) for run' )

        IF ( TSOUT .NE. 0 ) THEN
            RUNLEN = SEC2TIME( SECSDIFF( SDATE, STIME, EDATE, ETIME ) )
            RUNLEN = GETNUM( 0, 999999999, RUNLEN, 'Enter duration (HHMMSS) for run' )
            JDATE  = SDATE
            JTIME  = STIME
            CALL NEXTIME( JDATE, JTIME, RUNLEN )
            NSTEPS = CURREC( JDATE, JTIME, SDATE, STIME, TSTEP, EDATE, ETIME )
        ELSE
            NSTEPS = 1
        END IF
        IF ( TSOUT .NE. TSTEP ) THEN
            CALL M3WARN( PNAME, 0, 0, 'Input and output time steps are unequal.' )
        END IF

    END IF          !  time-independent file, or not


    !!.......   Build description for the output file, and create accordingly:
    !!.......   Re-use all but the starting date&time of the input-file description.

    SDATE3D = TDATE
    STIME3D = TTIME
    TSTEP3D = TSOUT

    IF ( ARGCNT .EQ. 0 ) THEN
        ONAME = PROMPTMFILE( 'Enter OUTPUT FILE name', FSUNKN3, 'OUTFILE', PNAME )
    ELSE    !  argcnt = 2:
        IF ( .NOT. OPEN3( ONAME, FSUNKN3, PNAME ) ) THEN
            MESG = 'Could not open output file "' // TRIM( ONAME ) // '"'
            CALL M3EXIT( PNAME, SDATE, STIME, MESG, 2 )
        END IF
    END IF      !  if argcnt zero, or 2


    !!...............   Allocate I/O Buffer:

    ALLOCATE ( RBUF( SIZE ), STAT = ISTAT )

    IF ( ISTAT .NE. 0 ) THEN
        WRITE( MESG, '( A, I10 )' ) 'Memory allocation failed:  STAT=', ISTAT
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
    END IF


    !!.......   Process this period in the input file:

    JDATE = SDATE
    JTIME = STIME
    KDATE = TDATE
    KTIME = TTIME

    DO  I = 1, NSTEPS

        IF ( .NOT.READ3( INAME, ALLVAR3, ALLAYS3, JDATE, JTIME, RBUF ) ) THEN

            MESG = 'Read failure:  file  ' // INAME
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            EFLAG = .TRUE.

        ELSE IF ( .NOT. WRITE3( ONAME, ALLVAR3, KDATE, KTIME, RBUF ) ) THEN

            MESG = 'Write failure:  file ' // ONAME
            CALL M3WARN( PNAME, KDATE, KTIME, MESG )
            EFLAG = .TRUE.

        END IF              !  if read3() worked, or not

        CALL NEXTIME( JDATE, JTIME, TSTEP )
        CALL NEXTIME( KDATE, KTIME, TSOUT )

    END DO        !  end loop on time steps


    IF ( EFLAG ) THEN
        MESG  = 'Failure in program'
        ISTAT = 2
    ELSE
        MESG  = 'Success in program'
        ISTAT = 0
    END IF

    CALL M3EXIT( PNAME, 0, 0, MESG, ISTAT )


END PROGRAM M3TSHIFT

