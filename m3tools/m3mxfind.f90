
PROGRAM  M3MXFIND

    !!***********************************************************************
    !! Version "$Id: m3mxfind.f90 258 2024-01-23 19:26:26Z coats $"
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
    CHARACTER*16, PARAMETER :: PNAME = 'M3MXFIND'
    CHARACTER*72, PARAMETER :: BAR   =    &
    '-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-'


    !!...........   LOCAL VARIABLES and their descriptions:

    INTEGER         SDATE   !  starting  input date, from user
    INTEGER         STIME   !  starting  input time, from user
    INTEGER         EDATE   !    ending  input date, from user
    INTEGER         ETIME   !    ending  input time, from user
    INTEGER         TSTEP   !  input time step, from IFILE header
    INTEGER         ASTEP   !  analysis time step
    INTEGER         NRECS   !  duration, output time steps
    INTEGER         ARECS   !  aggregation-window duration in TSTEPs
    INTEGER         JDATE   !  current  input date
    INTEGER         JTIME   !  current  input time
    INTEGER         KDATE   !  scratch  date
    INTEGER         KTIME   !  scratch  time
    INTEGER         LOGDEV  !  unit number for log file
    INTEGER         VSIZE   !  volume of one variable
    INTEGER         OFILE   !  unit number for output file
    INTEGER         VTYPE, ISTAT
    INTEGER         I, N, ISECS
    LOGICAL         EFLAG

    CHARACTER*16    VNAME
    CHARACTER*16    ANAME   !  scratch buffer for variable names
    CHARACTER*256   MESG

    !!.........................................................................
    !!   begin body of program  M3MXFIND

    LOGDEV = INIT3()
    EFLAG  = .FALSE.
    WRITE ( *, '( 5X,  A )' ) BLANK, BAR, BLANK,                            &
'Program M3MXFIND to find the (time stepped) analysis-window MAX for a',    &
'selected variable IN_VBLE from file IN_FILE and write the result to',      &
'an ASCII output file listing the analysis-window date&time ADATE:ATIME',   &
'and the date&time MDATE:MTIME for the maximum of IN_VBLE within that',     &
'analysis window, formatted',                                               &
'',                                                                         &
'    ADATE(YYYYDDD), ATIME(HHMMSS), MDATE(YYYYDDD), MTIME(HHMMSS)',         &
'    ... ',                                                                 &
'',                                                                         &
'PRECONDITIONS REQUIRED:',                                                  &
'',                                                                         &
'   setenv IN_FILE      <path name for GRIDDED input file>',                &
'   setenv IN_VBLE      <variable-name for analysis>',                      &
'   setenv AGGSTEP      <analysis-window (HHMMSS) [240000]',                &
'   setenv OUTFILE      <path name for output file>',                       &
'   setenv SDATE        <starting date for analysis> [SDATE3D from file]',  &
'   setenv STIME        <starting time for analysis> [STIME3D from file]',  &
'   setenv EDATE        <starting date for analysis> [from file]',          &
'   setenv ETIME        <starting time for analysis> [from file]',          &
'',                                                                         &
'Default values are the values coming from the input file IN_FILE',         &
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
'$Id: m3mxfind.f90 258 2024-01-23 19:26:26Z coats $',&
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
        ANAME = VNAME3D( 1 )
        JDATE = SDATE3D
        JTIME = STIME3D
        TSTEP = TSTEP3D
        CALL LASTTIME( JDATE,JTIME,TSTEP,MXREC3D, KDATE,KTIME )
    END IF

    !!...............  Get environment:

    SDATE = ENVINT( 'SDATE', 'Start date [YYYYDD]', JDATE, ISTAT )
    IF ( ISTAT .GT. 0 ) THEN
        EFLAG = .TRUE.
        MESG  = 'Bad environment variable "SDATE"'
        CALL M3MESG( MESG )
    END IF

    STIME = ENVINT( 'STIME', 'Start TIME  [HHMMSS]', JTIME, ISTAT )
    IF ( ISTAT .GT. 0 ) THEN
        EFLAG = .TRUE.
        MESG  = 'Bad environment variable "STIME"'
        CALL M3MESG( MESG )
    END IF

    EDATE = ENVINT( 'EDATE', 'Ending date [YYYYDD]', KDATE, ISTAT )
    IF ( ISTAT .GT. 0 ) THEN
        EFLAG = .TRUE.
        MESG  = 'Bad environment variable "EDATE"'
        CALL M3MESG( MESG )
    END IF

    ETIME = ENVINT( 'ETIME', 'Ending TIME  [HHMMSS]', KTIME, ISTAT )
    IF ( ISTAT .GT. 0 ) THEN
        EFLAG = .TRUE.
        MESG  = 'Bad environment variable "ETIME"'
        CALL M3MESG( MESG )
    END IF

    ASTEP = ENVINT( 'AGGSTEP', 'Analysis step  [HHMMSS]', 240000, ISTAT )
    IF ( ISTAT .GT. 0 ) THEN
        EFLAG = .TRUE.
        MESG  = 'Bad environment variable "AGGSTEP"'
        CALL M3MESG( MESG )
    END IF

    CALL ENVSTR( 'IN_VBLE', 'Variable-name for MAX-analysis', ANAME, VNAME, ISTAT )
    IF ( ISTAT .GT. 0 ) THEN
        EFLAG = .TRUE.
        MESG  = 'Bad environment variable "IN_VBLE"'
        CALL M3MESG( MESG )
    END IF
    
    OFILE = GETEFILE( 'OUTFILE', .FALSE., .TRUE., PNAME )
    IF ( OFILE .LT. 0 ) THEN
        EFLAG = .TRUE.
        MESG  = 'Could not open output file "OUTFILE"'
        CALL M3MESG( MESG )
    END IF

    IF ( EFLAG ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'Fatal environment-variable error(s)', 2 )
    END IF
    
    I = INDEX1( VNAME, NVARS3D, VNAME3D )
    IF ( I .LE. 0 ) THEN
        MESG = 'Input variable "'//TRIM( VNAME )//'" is not in IN_FILE.'
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
    ELSE
        VTYPE = VTYPE3D( I )
    END IF
    
    NRECS = SECSDIFF( SDATE, STIME, EDATE, ETIME ) / TIME2SEC( ASTEP )
    ARECS = TIME2SEC( ASTEP ) / TIME2SEC( TSTEP )

    !!...............  Process time step sequence:
    
    JDATE = SDATE
    JTIME = STIME
    DO N = 1, NRECS

        WRITE( MESG, '( A, I9.7, A, I6.6 )' ) 'Processing', JDATE, ':', JTIME
        CALL M3MESG( MESG )

        IF ( VTYPE .EQ. M3INT ) THEN
            CALL IMAX( VNAME, VSIZE, JDATE, JTIME, KDATE, KTIME )
        ELSE IF ( VTYPE .EQ. M3REAL ) THEN
            CALL RMAX( VNAME, VSIZE, JDATE, JTIME, KDATE, KTIME )
        ELSE IF ( VTYPE .EQ. M3DBLE ) THEN
            CALL DMAX( VNAME, VSIZE, JDATE, JTIME, KDATE, KTIME )
        ELSE IF ( VTYPE .EQ. M3INT8 ) THEN
            CALL LMAX( VNAME, VSIZE, JDATE, JTIME, KDATE, KTIME )
        END IF
        
        WRITE( OFILE, '( 5X, 2( I7.7, A, I6.6, :, A ) )' ) JDATE, ", ", JTIME, ", ", KDATE, ", ", KTIME

        CALL NEXTIME( JDATE, JTIME, ASTEP )

    END DO

    CALL M3EXIT( PNAME, 0,0, 'Success in program', 0 )


CONTAINS        !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-'


    SUBROUTINE IMAX( VNAME, VSIZE, JDATE, JTIME, KDATE, KTIME )
        
        CHARACTER(*), INTENT(IN   ) :: VNAME
        INTEGER     , INTENT(IN   ) :: VSIZE
        INTEGER     , INTENT(IN   ) :: JDATE, JTIME
        INTEGER     , INTENT(  OUT) :: KDATE, KTIME
        
        INTEGER     IDATE, ITIME, ISTEP, N
        INTEGER     VMAX
        INTEGER     VBUF( VSIZE )
        LOGICAL     EFLAG
        
        IDATE = JDATE
        ITIME = JTIME
        VMAX  = -1999999999
        EFLAG = .FALSE.

        DO ISTEP = 1, ARECS
        
            IF ( .NOT.READ3( 'IN_FILE', VNAME, ALLAYS3, IDATE, ITIME, VBUF ) ) THEN

                EFLAG = .TRUE.

            ELSE

                DO N = 1, VSIZE
                    IF ( VBUF( N ) .GT. VMAX ) THEN
                        VMAX = VBUF( N )
                        KDATE = JDATE
                        KTIME = JTIME
                    END IF
                END DO

            END IF
            
            CALL NEXTIME( IDATE, ITIME, TSTEP )
        
        END DO
        
        IF ( EFLAG ) THEN
            CALL M3EXIT( PNAME, JDATE, JTIME, 'READ-failure', 2 )
        END IF
        
        RETURN
        
    END SUBROUTINE IMAX

    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-'

   SUBROUTINE RMAX( VNAME, VSIZE, JDATE, JTIME, KDATE, KTIME )
        
        CHARACTER(*), INTENT(IN   ) :: VNAME
        INTEGER     , INTENT(IN   ) :: VSIZE
        INTEGER     , INTENT(IN   ) :: JDATE, JTIME
        INTEGER     , INTENT(  OUT) :: KDATE, KTIME
        
        INTEGER     IDATE, ITIME, ISTEP, N
        REAL        VMAX
        REAL        VBUF( VSIZE )
        LOGICAL     EFLAG
        
        IDATE = JDATE
        ITIME = JTIME
        VMAX  = BADVAL3
        EFLAG = .FALSE.

        DO ISTEP = 1, ARECS
        
            IF ( .NOT.READ3( 'IN_FILE', VNAME, ALLAYS3, IDATE, ITIME, VBUF ) ) THEN

                EFLAG = .TRUE.

            ELSE

                DO N = 1, VSIZE
                    IF ( VBUF( N ) .GT. VMAX ) THEN
                        VMAX = VBUF( N )
                        KDATE = JDATE
                        KTIME = JTIME
                    END IF
                END DO

            END IF
            
            CALL NEXTIME( IDATE, ITIME, TSTEP )
        
        END DO
        
        IF ( EFLAG ) THEN
            CALL M3EXIT( PNAME, JDATE, JTIME, 'READ-failure', 2 )
        END IF
        
        RETURN
        
    END SUBROUTINE RMAX

    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-'

   SUBROUTINE DMAX( VNAME, VSIZE, JDATE, JTIME, KDATE, KTIME )
        
        CHARACTER(*), INTENT(IN   ) :: VNAME
        INTEGER     , INTENT(IN   ) :: VSIZE
        INTEGER     , INTENT(IN   ) :: JDATE, JTIME
        INTEGER     , INTENT(  OUT) :: KDATE, KTIME
        
        INTEGER     IDATE, ITIME, ISTEP, N
        REAL*8      VMAX
        REAL*8      VBUF( VSIZE )
        LOGICAL     EFLAG
        
        IDATE = JDATE
        ITIME = JTIME
        VMAX  = BADVAL3
        EFLAG = .FALSE.

        DO ISTEP = 1, ARECS
        
            IF ( .NOT.READ3( 'IN_FILE', VNAME, ALLAYS3, IDATE, ITIME, VBUF ) ) THEN

                EFLAG = .TRUE.

            ELSE

                DO N = 1, VSIZE
                    IF ( VBUF( N ) .GT. VMAX ) THEN
                        VMAX = VBUF( N )
                        KDATE = JDATE
                        KTIME = JTIME
                    END IF
                END DO

            END IF
            
            CALL NEXTIME( IDATE, ITIME, TSTEP )
        
        END DO
        
        IF ( EFLAG ) THEN
            CALL M3EXIT( PNAME, JDATE, JTIME, 'READ-failure', 2 )
        END IF
        
        RETURN
        
    END SUBROUTINE DMAX

    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-'

   SUBROUTINE LMAX( VNAME, VSIZE, JDATE, JTIME, KDATE, KTIME )
        
        CHARACTER(*), INTENT(IN   ) :: VNAME
        INTEGER     , INTENT(IN   ) :: VSIZE
        INTEGER     , INTENT(IN   ) :: JDATE, JTIME
        INTEGER     , INTENT(  OUT) :: KDATE, KTIME
        
        INTEGER     IDATE, ITIME, ISTEP, N
        INTEGER*8   VMAX
        INTEGER*8   VBUF( VSIZE )
        LOGICAL     EFLAG
        
        IDATE = JDATE
        ITIME = JTIME
        VMAX  = -1999999999999999_8
        EFLAG = .FALSE.

        DO ISTEP = 1, ARECS
        
            IF ( .NOT.READ3( 'IN_FILE', VNAME, ALLAYS3, IDATE, ITIME, VBUF ) ) THEN

                EFLAG = .TRUE.

            ELSE

                DO N = 1, VSIZE
                    IF ( VBUF( N ) .GT. VMAX ) THEN
                        VMAX = VBUF( N )
                        KDATE = JDATE
                        KTIME = JTIME
                    END IF
                END DO

            END IF
            
            CALL NEXTIME( IDATE, ITIME, TSTEP )
        
        END DO
        
        IF ( EFLAG ) THEN
            CALL M3EXIT( PNAME, JDATE, JTIME, 'READ-failure', 2 )
        END IF
        
        RETURN
        
    END SUBROUTINE LMAX


END PROGRAM  M3MXFIND
    
