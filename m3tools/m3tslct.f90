
PROGRAM  M3TSLCT

    !!***********************************************************************
    !! Version "$Id: m3tslct.f90 258 2024-01-23 19:26:26Z coats $"
    !! EDSS/Models-3 M3TOOLS.
    !! Copyright (C) 2024 UNC Institute for the Environment
    !! Distributed under the GNU GENERAL PUBLIC LICENSE version 2
    !! See file "GPL.txt" for conditions of use.
    !!.........................................................................
    !!  program body starts at line 57
    !!
    !!  FUNCTION:
    !!
    !!  REVISION  HISTORY:
    !!      Prototype 2/2024 by Carlie J./ Coats, Jr.
    !!***********************************************************************

    USE M3UTILIO

    IMPLICIT NONE

    CHARACTER*16, PARAMETER ::BLANK  = ' '
    CHARACTER*16, PARAMETER :: PNAME = 'M3TSLCT'
    CHARACTER*72, PARAMETER :: BAR   =    &
    '-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-'


    !!...........   LOCAL VARIABLES and their descriptions:

    INTEGER         I, N, V
    INTEGER         SDATE   !  starting  input date, from user
    INTEGER         STIME   !  starting  input time, from user
    INTEGER         EDATE   !    ending  input date, from user
    INTEGER         ETIME   !    ending  input time, from user
    INTEGER         JDATE   !  current  input date
    INTEGER         JTIME   !  current  input time
    INTEGER         KDATE   !  scratch  date
    INTEGER         KTIME   !  scratch  time
    INTEGER         TSTEP
    INTEGER         ASTEP
    INTEGER         LOGDEV  !  unit number for log file
    INTEGER         VSIZE   !  volume of one variable
    INTEGER         TFILE   !  unit number for ${IN_TIMES)
    INTEGER         ISTAT
    INTEGER         NVARS
    INTEGER         VINDEX( MXVARS3 )
    INTEGER         VTYPES( MXVARS3 )
    CHARACTER*16    VNAMES( MXVARS3 )
    CHARACTER*16    VUNITS( MXVARS3 )
    CHARACTER*16    VDESCS( MXVARS3 )
    CHARACTER*16    ONAMES( MXVARS3 )

    LOGICAL         EFLAG
    
    REAL, ALLOCATABLE :: VBUF( : )

    CHARACTER*256   MESG

    !!.........................................................................
    !!   begin body of program  M3TSLCT

    LOGDEV = INIT3()
    EFLAG  = .FALSE.
    WRITE ( *, '( 5X,  A )' ) BLANK, BAR, BLANK,                            &
'Program M3TSLCT to extract selected dates&times of a list of variables',   &
'from a GRIDDED input file them, and write them to selected output',        &
'dates-&-times.',                                                           &
'',                                                                         &
'PRECONDITIONS REQUIRED:',                                                  &
'',                                                                         &
'   setenv IN_FILE      <path name for GRIDDED input file>',                &
'   setenv IN_TIMES     <path name for ASCII extraction dates&times>',      &
'   setenv IN_VBLES     <comma-list of  input-vbles for extraction>',       &
'   setenv OUTVBLES     <comma-list of output-vbles for extraction>',       &
'   setenv AGGSTEP      <analysis-window (HHMMSS) [240000]',                &
'   setenv OUTFILE      <path name for output file>',                       &
'',                                                                         &
'   IN_TIMES is an ASCII list-formatted file as from program M3MXFIND',     &
'listing the analysis-window date&time ADATE:ATIME and the dates&times',    &
'MDATE:MTIME for the maximum of IN_VBLE within the analysis-window',        &
'starting at ADATE:ATIME, formatted as below:',                             &
'',                                                                         &
'   ADATE(YYYYDDD), ATIME(HHMMSS), MDATE(YYYYDDD), MTIME(HHMMSS)',          &
'',                                                                         &
'',                                                                         &
'See URL',                                                                  &
'',                                                                         &
'   https://www.cmascenter.org/ioapi/documentation/3.1/html/AA.html#tools', &
'',                                                                         &
'Program copyright (C) 1992-2002 MCNC, (C) 1995-2013 Carlie J. Coats, Jr.', &
'(C) 2003-2010 Baron Advanced Meteorological Systems, LLC., and',           &
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
'$Id: m3tslct.f90 258 2024-01-23 19:26:26Z coats $',&
' '

    !!...............  Open and get description for input file

    IF ( .NOT. OPEN3( 'IN_FILE', FSREAD3, PNAME ) ) THEN
        MESG = 'Could not open input file "IN_FILE"'
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
    ELSE IF ( .NOT. DESC3( 'IN_FILE' ) ) THEN
        MESG = 'Could not get description of  "IN_FILE"'
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
    ELSE IF ( TSTEP3D .EQ. 0 ) THEN
        MESG = 'Input file "IN_FILE" is time-independent: no output written.'
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
    ELSE IF ( FTYPE3D .NE. GRDDED3 ) THEN
        MESG = 'Input file "IN_FILE" is not of type GRIDDED.'
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
    ELSE
        VSIZE = NCOLS3D*NROWS3D*NLAYS3D
        JDATE = SDATE3D
        JTIME = STIME3D
        TSTEP = TSTEP3D
        CALL LASTTIME( JDATE,JTIME,TSTEP,MXREC3D, KDATE,KTIME )
    END IF

    !!...............  Get environment:

    ASTEP = ENVINT( 'AGGSTEP', 'Analysis step  [HHMMSS]', 240000, ISTAT )
    IF ( ISTAT .GT. 0 ) THEN
        EFLAG = .TRUE.
        MESG  = 'Bad environment variable "AGGSTEP"'
        CALL M3MESG( MESG )
    END IF
    
    IF ( .NOT.STRLIST( 'IN_VBLES', 'Input variable-names for extraction', NVARS3D, NVARS, VNAMES ) ) THEN
        EFLAG = .TRUE.
        MESG  = 'Bad environment list "IN_VBLES"'
        CALL M3MESG( MESG )
    ELSE  IF ( .NOT.STRLIST( 'OUTVBLES', 'Output variable-names for extraction', NVARS3D, N, ONAMES ) ) THEN
        EFLAG = .TRUE.
        MESG  = 'Bad environment list "OUTVBLES"'
        CALL M3MESG( MESG )
    ELSE IF ( N .NE. NVARS ) THEN
        EFLAG = .TRUE.
        MESG  = 'Mis-matched lists "IIN_VBLES" and "OUTVBLES"'
        CALL M3MESG( MESG )
    ELSE

        DO  V = 1, NVARS
            I = INDEX1( VNAMES( V ), NVARS3D, VNAME3D )
            IF ( I .LE. 0 ) THEN
                EFLAG = .TRUE.
                MESG = 'Variable "' // TRIM( VNAMES( V ) ) // '" not found in "IN_FILE"'
                CALL M3MESG( MESG )
            ELSE
                VINDEX( V ) = I
                VUNITS( V ) = UNITS3D( I )
                VDESCS( V ) = VDESC3D( I )
                VTYPES( V ) = VTYPE3D( I )
            END IF
        END DO

    END IF
    
    TFILE = GETEFILE( 'IN_TIMES', .TRUE., .TRUE., PNAME )
    IF ( TFILE .LT. 0 ) THEN
        EFLAG = .TRUE.
        MESG  = 'Could not open input file "IN_TIMES"'
        CALL M3MESG( MESG )
    ELSE
        READ( TFILE,*,IOSTAT=ISTAT ) SDATE, STIME
        IF ( ISTAT .NE. 0 ) THEN
            EFLAG = .TRUE.
            WRITE( MESG, '(A,I9)' ) 'Could not read line 1 from "IN_TIMES":  IOSTAT', ISTAT
            CALL M3MESG( MESG )            
        END IF
        REWIND( TFILE )
    END IF

    IF ( EFLAG ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'Fatal environment-variable error(s)', 2 )
    END IF

    !!...............  Allocate buffer for I/O ("double-sized"  for M3DBLE variables)

    ALLOCATE( VBUF( 2*VSIZE ), STAT = ISTAT )
    IF ( ISTAT .NE. 0 ) THEN
        WRITE( MESG, '(A,I9)' ) 'Allocation failure:  STATUS=', ISTAT
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )        
    END IF

    !!...............  Open output file, borrowing most of the description;

    SDATE3D = SDATE
    STIME3D = STIME
    TSTEP3D = ASTEP
    NVARS3D = NVARS
    VNAME3D( 1:NVARS ) = ONAMES( 1:NVARS )
    UNITS3D( 1:NVARS ) = VUNITS( 1:NVARS )
    VDESC3D( 1:NVARS ) = VDESCS( 1:NVARS )
    VTYPE3D( 1:NVARS ) = VTYPES( 1:NVARS )

    IF ( .NOT. OPEN3( 'OUTFILE', FSUNKN3, PNAME ) ) THEN
        MESG = 'Could not open output file "OUTFILE"'
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
    END IF

    !!...............  Process time step sequence from TFILE
    
    DO N = 1, 1999999999
    
        READ( TFILE, *, END=999, IOSTAT=ISTAT ) JDATE, JTIME, KDATE, KTIME
        IF ( ISTAT .NE. 0 ) THEN
            WRITE( MESG, '(A,I9,2X,A,I9)' ) 'Failure reading "IN_TIMES" at line', N, 'STATUS=', ISTAT
            EFLAG = .TRUE.
            CYCLE
        END IF
        
        DO V = 1, NVARS

            IF ( .NOT.READ3( 'IN_FILE', VNAMES(V), ALLAYS3, KDATE, KTIME, VBUF ) ) THEN
                EFLAG = .TRUE.
                MESG  ='Could not read "'//TRIM( VNAMES(V) )//'" from "IN_FILE"'
                CALL M3MESG( MESG )    

            ELSE IF ( .NOT.WRITE3( 'OUTFILE', ONAMES(V), KDATE, KTIME, VBUF ) ) THEN

                EFLAG = .TRUE.
                MESG  ='Could not write "'//TRIM( ONAMES(V) )//'" to "OUTFILE"'
                CALL M3MESG( MESG )    

            END IF

        END DO
    
    END DO
    
999 CONTINUE

    !!...............  Completion

    IF ( EFLAG ) THEN
        MESG  = 'Failure in program'
        ISTAT = 2
    ELSE
        MESG  = 'Success in program'
        ISTAT = 0
    END IF

    CALL M3EXIT( PNAME, 0, 0, MESG, ISTAT )


END PROGRAM  M3TSLCT
