
        PROGRAM AIRNOW2M3

C***********************************************************************
C Version "$Id: airnow2m3.f 94 2018-03-28 20:38:33Z coats $"
C EDSS/Models-3 M3TOOLS.
C Copyright (C) 1992-2002 MCNC, (C) 1995-2002,2005-2013 Carlie J. Coats, Jr.,
C and (C) 2002-2010 Baron Advanced Meteorological Systems. LLC.
C Distributed under the GNU GENERAL PUBLIC LICENSE version 2
C See file "GPL.txt" for conditions of use.
C.........................................................................
C  program body starts at line  101
C
C  DESCRIPTION:
C       Read an ASCII  monitor-location file and a monitor-data file, and
C       produce an I/O API monitor-data file.
C
C  PRECONDITIONS REQUIRED:
C       Input ASCII files have agreed-upon AIRNOW format
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       I/O API
C       READHDR starts at line 536
C       SKIPBLK starts at line 714
C
C  REVISION  HISTORY:
C       10/1999:  Prototype by Carlie J. Coats, Jr., MCNC, for TNRCC data
C       08/2000:  Version for EPA AIRNOW data also performs gridding--CJC
C       09/2000:  Gridding functionality split out into program OBS_GRID--CJC
C       03/2002:  Version for PAVE/I/O API distribution
C
C       Version 02/2010 by CJC for I/O API v3.1:  Fortran-90 only;
C       USE M3UTILIO, and related changes.
C***********************************************************************

      USE M3UTILIO
      IMPLICIT NONE


C...........   PARAMETERS and their descriptions:

        REAL,         PARAMETER::    D60     = 1.0 / 60.0
        REAL,         PARAMETER::    D3600   = 1.0 / 3600.0
        CHARACTER*16, PARAMETER::    NCF_OBS = 'NCF_OBS'

       CHARACTER*16, PARAMETER :: PNAME = 'AIRNOW2M3'

C...........   LOCAL VARIABLES and their descriptions:

        INTEGER         LDEV    !  unit number for log file
        INTEGER         MDEV    !  unit number for monitor-location file
        INTEGER         DDEV    !  unit number for monitor-data     file

        REAL            XORIG, YORIG, XCELL, YCELL
        REAL            P_ALP, P_BET, P_GAM, XCENT, YCENT

        INTEGER         M, N, P !  loop counters and subscripts
        INTEGER         NMONS   !  number of monitors present
        INTEGER         NACTS   !  number of monitors present
        INTEGER         NHRS    !  number of time steps to process
        INTEGER         JDATE, JTIME
        INTEGER         JDATE, JTIME
        INTEGER         SDATE, STIME
        INTEGER         NCOLS, NROWS

        INTEGER         TZONE, DTIME
        INTEGER         YYYY000
        INTEGER         YEAR, MONTH, DAY, HOUR, MINS     !  file-header params
        INTEGER         TSTEP, NUMT, AVGT

        INTEGER         R, C
        INTEGER         I, J, L, II, JJ, KK, LL   !  scratch integers

        INTEGER         STATUS  !  I/O, ALLOCATE status

        LOGICAL         EFLAG

        INTEGER, ALLOCATABLE::   MONS( : )
        INTEGER, ALLOCATABLE::   MONU( : )
        INTEGER, ALLOCATABLE::   MDEX( : )
        REAL,    ALLOCATABLE::   LATU( : )
        REAL,    ALLOCATABLE::   LONU( : )
        REAL,    ALLOCATABLE::    LAT( : )
        REAL,    ALLOCATABLE::    LON( : )
        REAL,    ALLOCATABLE::     O3( :, : )

        CHARACTER*1     FLAG
        CHARACTER*256   LINE
        CHARACTER*256   BUFF
        CHARACTER*256   MESG
        CHARACTER*16    VNAME


C***********************************************************************
C   begin body of program dummy

        WRITE( *, '( 5X, A )' )
     &' ',
     &'Program AIRNOW2M3 to read ASCII air quality monitor location',
     &'and observation-data files, and produce an "observation format"',
     &'I/O API file.', ' ',
     &'You need to have assigned logical names to the physical file',
     &'names of the input files according to Models-3 conventions,',
     &'using the operation, and also for the program control input',
     &'RADIUS that controls the filtering radius used in producing',
     &'the OBS_CRO_2D gridded-observations file.',
     &' ',
     &'    "setenv MONITORS <path-name for monitor locations  file>".',
     &'    "setenv MON_OBS  <path-name for monitor obs input  file>".',
     &'    "setenv NCF_OBS  <path-name for netCDF  obs output file>".',
     &' ',
     &'    setenv RADIUS    <filtering radius (km)>',
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
     &'$Id:: airnow2m3.f 94 2018-03-28 20:38:33Z coats               $',
     &' '

        LDEV = INIT3()
        IF ( .NOT.GETYN( 'Continue with program?', .TRUE. ) ) THEN
            CALL M3EXIT( PNAME, 0, 0,
     &                   'Terminated at user request.', 2 )
        END IF
        MDEV = GETEFILE( 'MONITORS', .TRUE., .TRUE., PNAME )

        DDEV = GETEFILE( 'MON_OBS',  .TRUE., .TRUE., PNAME )


        !!  Current values, for defaults:

        CALL GETDTTIME( JDATE, JTIME )
        IF ( ISDSTIME( JDATE ) ) THEN   !  time zone (hour offset)
            II = 3                      !  relative to GMT
        ELSE                            !  for Atlantic Time
            II = 4
        END IF

        SDATE = GETNUM ( 0, 9999999, JDATE, 'Enter starting date' )
        STIME = GETNUM ( 0,  999999,     0, 'Enter starting time' )
        NHRS  = GETNUM ( 0,  999999,    24, 'Enter number of hours' )
        TZONE = GETNUM ( -23,    23,    II, 'Enter input time zone' )

        !!  NOTE that the file lists data as being "on the hour"
        !!  relative to the time zone (usually Atlantic Daylight Time
        !!  or Atlantic Standard Time)
        !!  the file actually means that the hour listed is the
        !!  _start_ of a 1-hour period over which the data is
        !!  _averaged_.
        !!  We re-interpret the time-stamps into _centered_ time
        !!  intervals specified in GMT

        YYYY000 = 1000 * ( SDATE / 1000 )
        DTIME   = 10000 * TZONE + 3000          !  central half-hour
        JDATE   = SDATE
        JTIME   = STIME
        CALL NEXTIME( JDATE, JTIME, DTIME )     !  input time-zone adjustment


C...........  Count monitors in monitor-location file;
C...........  copy header lines from monitor-location file and
C...........  monitor-observation file to FDESC3 file header:

        M = 0
        N = 0
        L = 1
        FDESC3D(L) = 'Ozone monitor data from AIRNOW monitor sites'

11      CONTINUE        !  loop counting monitors before allocation
            READ( MDEV, '(A)', END = 12, IOSTAT = STATUS ) LINE
            M = M + 1
            IF ( STATUS .NE. 0 ) THEN

                WRITE( MESG, '(A, I9, 2X, A, I9, 2X, A )' )
     &              'I/O error', STATUS, 'encountered at line', M,
     &              'of MONITOR-LOCATION FILE'
                CALL M3EXIT( PNAME, 0, 0, MESG, 2 )

            ELSE IF ( LINE(1:1) .EQ. '#' ) THEN

                L = L + 1
                IF ( L .LE. MXDESC3 )  FDESC3D( L ) = LINE

            ELSE IF ( LINE(1:1) .NE. '#' ) THEN

                N = N + 1

            END IF
            GO TO 11

12      CONTINUE        !  end loop copying file header lines
        REWIND( MDEV )

        M = 0

21      CONTINUE        !  loop counting monitors before allocation
            READ( DDEV, '(A)', END = 22, IOSTAT = STATUS ) LINE
            M = M + 1
            IF ( STATUS .NE. 0 ) THEN

                WRITE( MESG, '(A, I9, 2X, A, I9, 2X, A )' )
     &              'I/O error', STATUS, 'encountered at line', M,
     &              'of MONITOR-OBSERVATIONS FILE'
                CALL M3EXIT( PNAME, 0, 0, MESG, 2 )

            ELSE IF ( LINE(1:1) .EQ. '#' ) THEN

                L = L + 1
                IF ( L .LE. MXDESC3 )  FDESC3D( L ) = LINE
                GO TO  21

            END IF

22      CONTINUE        !  end loop copying file header lines
        REWIND( DDEV )
        NMONS = N
        DO  M = L+1, MXDESC3
            FDESC3D( M ) = ' '
        END DO

C...........  Create/Open output file:

        P_ALP3D = 0.0D0
        P_BET3D = 0.0D0
        P_GAM3D = 0.0D0
        XCENT3D = 0.0D0
        YCENT3D = 0.0D0
        XORIG3D = 0.0D0
        YORIG3D = 0.0D0
        XCELL3D = 0.0D0
        YCELL3D = 0.0D0
        FTYPE3D = CUSTOM3
        SDATE3D = JDATE
        STIME3D = JTIME
        TSTEP3D = 10000
        NCOLS3D = NMONS
        NROWS3D = 1
        NLAYS3D = 1
        NTHIK3D = 1
        GDTYP3D = LATGRD3
        VGTYP3D = IMISS3
        VGTOP3D = BADVAL3

        N = 1
        VNAME3D(N) = 'MONITOR_ID'
        UNITS3D(N) = 'none'
        VDESC3D(N) = 'SSCCCNNNN for state SS, county CCC, site NNNN'
        VTYPE3D(N) = M3INT

        N = N + 1
        VNAME3D(N) = 'LAT'
        UNITS3D(N) = 'deg'
        VDESC3D(N) = 'Latitude for monitor'
        VTYPE3D(N) = M3REAL

        N = N + 1
        VNAME3D(N) = 'LON'
        UNITS3D(N) = 'deg'
        VDESC3D(N) = 'Longitude for monitor'
        VTYPE3D(N) = M3REAL

        N = N + 1
        VNAME3D(N) = 'O3'
        UNITS3D(N) = 'ppmv'
        VDESC3D(N) = 'monitor-observation value, or MISSING < -9.9E36'
        VTYPE3D(N) = M3REAL

        NVARS3D = N
        GDNAM3D = 'LATLON'

        IF ( .NOT. OPEN3( 'NCF_OBS', FSUNKN3, PNAME ) ) THEN
            CALL M3EXIT( PNAME, 0, 0,
     &                   'Could not open file "NCF_OBS"', 2 )
        END IF


C...........  Allocate working arrays:

        ALLOCATE( MONS( NMONS ),
     &            MONU( NMONS ),
     &            MDEX( NMONS ),
     &            LONU( NMONS ),
     &            LATU( NMONS ),
     &             LAT( NMONS ),
     &             LON( NMONS ),
     &              O3( NMONS, NHRS ), STAT = STATUS )

        IF ( STATUS .NE. 0 ) THEN
            CALL M3EXIT( PNAME, 0, 0, 'Buffer allocation failure', 2 )
        END IF

        DO  M = 1, NHRS
        DO  N = 1, NMONS
            O3 ( N,M ) = BADVAL3
        END DO
        END DO
        DO  N = 1, NMONS
            MDEX( N ) = N
        END DO


C...........  Read and sort ID's in monitor-location file:

        M = 0
        N = 0
        EFLAG = .FALSE.

70      CONTINUE        !  loop reading monitor locations
            READ( MDEV, '(A)', END = 77, IOSTAT=STATUS ) LINE
            M = M + 1
            IF ( STATUS .NE. 0 ) THEN

                WRITE( MESG, '(A, I9, 2X, A, I9, 2X, A )' )
     &              'I/O error', STATUS,
     &              'encountered reading line', M,
     &              'of MONITOR-LOCATION FILE'
                CALL M3EXIT( PNAME, 0, 0, MESG, 2 )

            ELSE IF ( LINE( 1:1 ) .NE. '#' ) THEN

                N = N + 1

                II = STR2INT( LINE( 1:9 ) )
                IF ( II .LT. 0 ) THEN
                   WRITE( MESG, '(3 A, I9, 2X, A)' )
     &              'Bad Monitor-ID "', BUFF( 1:11 ),
     &              '" at line', M, 'of LOC-FILE'
                    CALL M3MSG2( MESG )
                    EFLAG = .TRUE.
                ELSE
                    MONU( N ) = II
                END IF

                II = STR2INT( LINE( 11:13 ) )
                JJ = STR2INT( LINE( 15:17 ) )
                KK = STR2INT( LINE( 19:21 ) )
                IF ( MIN( II,JJ,KK ) .LT. 0 ) THEN
                   WRITE( MESG, '(3 A, I9, 2X, A)' )
     &              'Bad LAT "', LINE( 11:21 ),
     &              '" at line', M, 'of LOC-FILE'
                    CALL M3MSG2( MESG )
                    EFLAG = .TRUE.
                ELSE
                    LATU( N ) = D3600*FLOAT( 3600*II + 60*JJ + KK )
                END IF
                II = STR2INT( LINE( 23:25 ) )
                JJ = STR2INT( LINE( 27:29 ) )
                KK = STR2INT( LINE( 31:33 ) )
                IF ( MIN( II,JJ,KK ) .LT. 0 ) THEN
                   WRITE( MESG, '(3 A, I9, 2X, A)' )
     &              'Bad LON "', LINE( 23:33 ),
     &              '" at line', M, 'of LOC-FILE'
                    CALL M3MSG2( MESG )
                    EFLAG = .TRUE.
                ELSE
                    LONU( N ) = -D3600*FLOAT( 3600*II + 60*JJ + KK )
                END IF

            END IF

            GO TO 70

77      CONTINUE        !  end loop reading monitor locations

        IF ( EFLAG ) THEN
            CALL M3EXIT( PNAME, 0, 0,
     &                   'Error reading LOC-file', 2 )
        ELSE IF ( N .NE. NMONS ) THEN
            CALL M3EXIT( PNAME, 0, 0,
     &                   'Error counting IDs in LOC-file', 2 )
        END IF

        CALL SORTI1( NMONS, MDEX, MONU )
        DO  N = 1, NMONS
            M = MDEX( N )
            MONS( N ) = MONU( M )
            LAT ( N ) = LATU( M )
            LON ( N ) = LONU( M )
        END DO
        REWIND( MDEV )



C...........  Read monitor-observation file:  first the header, then the data

        M = 0               !!  line number

80      CONTINUE
            CALL SKIPBLK( DDEV, M )
            CALL READHDR( DDEV, SDATE, STIME, NHRS, NMONS, YYYY000,
     &                    M, VNAME )
            IF ( VNAME .NE. 'OZONE' ) THEN
                GO TO 80
            END IF

90      CONTINUE        !  loop reading monitor data

            !!  first, the data-value line
            !!  (This is a redundant format that has both
            !!  in-line "missing" tokens and paired
            !!  out-of-line missing/valid flags):
            !!  Need to read ** TWO  ** lines per monitor-site !!

            READ( DDEV, '(A)', IOSTAT=STATUS ) BUFF
            M = M + 1
            IF ( STATUS .NE. 0 ) THEN
                WRITE( MESG, '(A, I9, 2X, A, I9, 2X, A )' )
     &              'I/O error', STATUS,
     &              'encountered reading line', M,
     &              'of MONITOR-OBSERVATION FILE'
                CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
            END IF

            IF ( BUFF( 1:8 ) .EQ. 'END_DATA' ) GO TO 99

            LL = STR2INT( BUFF( 22:30 ) )        !  monitor ID
            IF ( LL .LT. 0 ) THEN
                WRITE( MESG, '(3 A, I9, 2X, A)' )
     &              'Bad Monitor-ID "', BUFF( 22:30 ),
     &              '" at line', M, 'of OBS-FILE'
                CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
            END IF

            N  = FIND1( LL, NMONS, MONS )           !  subscript
            IF ( N .LT. 1 ) THEN
                WRITE( MESG, '(A, I11, A, I9, 2X, A)' )
     &              'Monitor-ID "', LL,
     &              '" not found at line', M, 'of OBS-FILE'
                CALL M3MSG2( MESG )
                GO TO  90
            END IF

            READ( BUFF( 32:256 ), *, IOSTAT=STATUS )
     &          ( O3 ( N,P ), P = 1, NHRS )
            IF ( STATUS .NE. 0 ) THEN
                WRITE( MESG, '(A, I9, 2X, A, I9, 2X, A )' )
     &              'I/O error', STATUS,
     &              'reading monitor data at line', M,
     &              'of MONITOR-OBSERVATION FILE'
                CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
            END IF

            READ( DDEV, '(A)', END = 99, IOSTAT=STATUS ) BUFF
            M = M + 1
            II = STR2INT( BUFF( 22:30 ) )        !  monitor ID
            IF ( II .LT. 0 ) THEN
                WRITE( MESG, '(3 A, I9, 2X, A)' )
     &              'Bad Monitor-ID "', BUFF( 22:30 ),
     &              '" at line', M, 'of OBS-FILE'
                CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
            ELSE IF ( LL .NE. II ) THEN
                WRITE( MESG, '(3 A, I9, 2X, A)' )
     &              'Bad Monitor-ID "', BUFF( 22:30 ),
     &              '" at line', M, 'of OBS-FILE'
                CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
            END IF

            DO  P = 1, NHRS
                FLAG = BUFF( 5*P + 30 : 5*P + 30 )
                IF ( FLAG .NE. 'G'  .AND.
     &               FLAG .NE. 'E'  .AND.
     &               FLAG .NE. 'K'  .AND.
     &               FLAG .NE. 'R'  ) THEN
                    O3 ( N,P ) = BADVAL3
                END IF
            END DO

91          CONTINUE        !  tail of loop reading monitor data
            GO TO 90

99      CONTINUE        !  end loop reading monitor data

        IF ( EFLAG ) THEN
            CALL M3EXIT( PNAME, 0, 0,
     &                   'Error reading OBS-file', 2 )
        END IF


C...........  Write output file:

        DO  P = 1, NHRS

            DO  N=1, NMONS
                IF ( O3 ( N,P ) .LT. 0.0 ) THEN
                    O3( N,P ) = BADVAL3
                ELSE
                    O3( N,P ) = 1.0E-3*O3( N,P )  !  ppb --> ppm
                END IF
            END DO

            IF ( .NOT.WRITE3( NCF_OBS, 'MONITOR_ID',
     &                        JDATE, JTIME, MONS ) ) THEN
                MESG = 'Error writing variable MONITOR_ID'
                CALL M3EXIT( PNAME, JDATE, JTIME, MESG, 2 )
            END IF

            IF ( .NOT.WRITE3( NCF_OBS, 'LAT',
     &                        JDATE, JTIME, LAT ) ) THEN
                MESG = 'Error writing variable '
                CALL M3EXIT( PNAME, JDATE, JTIME, MESG, 2 )
            END IF

            IF ( .NOT.WRITE3( NCF_OBS, 'LON',
     &                        JDATE, JTIME, LON ) ) THEN
                MESG = 'Error writing variable '
                CALL M3EXIT( PNAME, JDATE, JTIME, MESG, 2 )
            END IF

            IF ( .NOT.WRITE3( NCF_OBS, 'O3',
     &                        JDATE, JTIME, O3(1,P) ) ) THEN
                MESG = 'Error writing variable '
                CALL M3EXIT( PNAME, JDATE, JTIME, MESG, 2 )
            END IF

            CALL NEXTIME( JDATE, JTIME, 10000 )
            CALL M3MSG2( ' ' )

        END DO          !  end loop on output time steps


        CALL M3EXIT( PNAME, 0, 0,
     &       'Successful completion of program AIRNOW2M3', 0 )


      CONTAINS  !!-----------------------------------------------------------------


        SUBROUTINE READHDR( DDEV, SDATE, STIME, NHRS, NMONS, YYYY000,
     &                      M, VNAME )
        IMPLICIT NONE
        INTEGER,      INTENT(IN)::       DDEV
        INTEGER,      INTENT(IN)::       SDATE, STIME
        INTEGER,      INTENT(IN)::       NHRS, NMONS, YYYY000
        INTEGER,      INTENT(INOUT)::    M
        CHARACTER*16, INTENT(OUT)::      VNAME

        CHARACTER*24, PARAMETER::  RNAME = 'AIRNOW2M3/READHDR'

        INTEGER        YEAR, MONTH, DAY, HOUR, MINS, TSTEP, NUMT, AVGT
        INTEGER        II, JJ, JDATE, JTIME, TSTEP, NUMT, AVGT, STATUS
        CHARACTER*256  BUFF, MESG

        INTEGER  JSTEP3, JULIAN, LBLANK, STR2INT
        EXTERNAL JSTEP3, JULIAN, LBLANK, STR2INT

        !..................................................

        YEAR  = -9999
        MONTH = -9999
        DAY   = -9999
        HOUR  = -9999
        MINS  = -9999
        TSTEP = -9999
        NUMT  = -9999
        AVGT  = -9999

100     CONTINUE        !  loop reading file header

            READ( DDEV, '(A)', IOSTAT=STATUS ) BUFF
            M = M + 1

            IF ( STATUS .NE. 0 ) THEN

                WRITE( MESG, '(A, I9, 2X, A, I9, 2X, A )' )
     &              'I/O error', STATUS,
     &              'encountered reading line', M,
     &              'of MONITOR-OBSERVATION FILE'
                CALL M3EXIT( RNAME, 0, 0, MESG, 2 )

            ELSE IF ( BUFF( 1:10 ) .EQ. 'BEGIN_DATA' ) THEN

                GO TO 111

            ELSE IF ( BUFF( 1:9 ) .EQ. 'START_DTG' ) THEN

                II    = 10 + LBLANK( BUFF( 11:100 ) )
                YEAR  = STR2INT( BUFF( II+ 1:II+ 4 ) )
                MONTH = STR2INT( BUFF( II+ 5:II+ 6 ) )
                DAY   = STR2INT( BUFF( II+ 7:II+ 8 ) )
                HOUR  = STR2INT( BUFF( II+ 9:II+10 ) )
                MINS  = STR2INT( BUFF( II+11:II+12 ) )

                JJ = MIN( YEAR, MONTH, DAY, HOUR, MINS )
                IF ( JJ .LT. 0 ) THEN
                   WRITE( MESG, '(A, I9, 2X, A, 2X, A )' )
     &                  'Bad "START_DTG"  at line', M,
     &                  'of MONITOR-OBSERVATION FILE:', BUFF( 1:40 )
                    CALL M3EXIT( RNAME, JDATE, JTIME, MESG, 2 )
                END IF

                JDATE = YYYY000 + JULIAN( YEAR, MONTH, DAY )
                JTIME = 10000 * HOUR + 100 * MINS
                JJ    = JSTEP3( JDATE, JTIME, SDATE, STIME, 10000 )
                WRITE( MESG, '( A, I9, A, I6.6 )' )
     &                  'Run starting date and time', SDATE, ':', STIME
                CALL M3MSG2( MESG )
                WRITE( MESG, '( A, I9, A, I6.6 )' )
     &                  'File starting date and time', JDATE, ':', JTIME
                CALL M3MSG2( MESG )

                IF ( JDATE .NE. SDATE ) THEN
                   WRITE( MESG, '(A, I9, 2X, A, 2X, A )' )
     &                  'Wrong date-field in "START_DTG"  at line', M,
     &                  'of MONITOR-OBSERVATION FILE:', BUFF( 1:40 )
                        CALL M3EXIT( RNAME, JDATE, JTIME, MESG, 2 )
                ELSE IF ( JJ .NE. 1 ) THEN
                   WRITE( MESG, '(A, I9, 2X, A, 2X, A )' )
     &                  'Wrong time-field in "START_DTG"  at line', M,
     &                  'of MONITOR-OBSERVATION FILE:', BUFF( 1:40 )
                        CALL M3EXIT( RNAME, JDATE, JTIME, MESG, 2 )
                END IF

            ELSE IF ( BUFF( 1:8 ) .EQ. 'INTERVAL' ) THEN

                TSTEP = STR2INT( BUFF( 10:40 ) )
                IF ( TSTEP .NE. 60 ) THEN
                   WRITE( MESG, '(A, I9, 2X, A, 2X, A )' )
     &                  'Bad "INTERVAL"  at line', M,
     &                   'of MONITOR-OBSERVATION FILE:', BUFF( 1:40 )
                    CALL M3EXIT( RNAME, 0, 0, MESG, 2 )
                END IF

            ELSE IF ( BUFF( 1:8 ) .EQ. 'NUMSTEPS' ) THEN

                NUMT = STR2INT( BUFF( 10:40 ) )
                IF ( NUMT .LT. 0 ) THEN
                   WRITE( MESG, '(A, I9, 2X, A, 2X, A )' )
     &                  'Bad "NUMSTEPS"  at line', M,
     &                  'of MONITOR-OBSERVATION FILE:', BUFF( 1:40 )
                    CALL M3EXIT( RNAME, 0,0,MESG,2 )
                ELSE IF ( NUMT .LT. NHRS ) THEN
                   WRITE( MESG, '(A, I9, 2X, A, 2X, A )' )
     &                  'Too-small "NUMSTEPS"  at line', M,
     &                  'of MONITOR-OBSERVATION FILE:', BUFF( 1:40 )
                    CALL M3EXIT( RNAME, 0, 0, MESG, 2 )
                END IF
                WRITE( MESG, '(A, I4 )' )
     &                  'Number of input time steps', NUMT
                CALL M3MSG2( MESG )

            ELSE IF ( BUFF( 1:8 ) .EQ. 'AVG_TIME' ) THEN

                AVGT = STR2INT( BUFF( 10:40 ) )
                IF ( AVGT .NE. 60 ) THEN
                   WRITE( MESG, '(A, I9, 2X, A, 2X, A )' )
     &                  'Bad "AVG_TIME"  at line', M,
     &                  'of MONITOR-OBSERVATION FILE:', BUFF( 1:40 )
                    CALL M3EXIT( RNAME, 0, 0, MESG, 2 )
                END IF

            ELSE IF ( BUFF( 1:8 ) .EQ. 'STATIONS' ) THEN

                JJ = STR2INT( BUFF( 10:40 ) )
                IF ( JJ .LT. 0 ) THEN
                   WRITE( MESG, '(A, I9, 2X, A, 2X, A )' )
     &                  'Bad "STATIONS"  at line', M,
     &                  'of MONITOR-OBSERVATION FILE:', BUFF( 1:40 )
                    CALL M3EXIT( RNAME, 0, 0, MESG, 2 )
                ELSE IF ( JJ .GT. NMONS ) THEN
                   WRITE( MESG, '(A, I9, 2X, A, 2X, A )' )
     &                  'Too-big "STATIONS"  at line', M,
     &                  'of MONITOR-OBSERVATION FILE:', BUFF( 1:40 )
                    CALL M3EXIT( RNAME, 0, 0, MESG, 2 )
                END IF

            ELSE IF ( BUFF( 1:5 ) .EQ. 'UNITS' ) THEN

                II = 7 + LBLANK( BUFF( 7:100 ) )
                IF ( BUFF (II:II+3 ) .NE. 'PPB ' ) THEN
                   WRITE( MESG, '(A, I9, 2X, A, 2X, A )' )
     &                  'Bad "UNITS"  at line', M,
     &                  'of MONITOR-OBSERVATION FILE:', BUFF( 1:40 )
                    CALL M3EXIT( RNAME, 0, 0, MESG, 2 )
                END IF

            ELSE IF ( BUFF( 1:8 ) .EQ. 'VARIABLE' ) THEN

                II = 10 + LBLANK( BUFF( 10:100 ) )
                VNAME = BUFF (II:II+15 )
                MESG = 'Reading header for variable ' // VNAME
                CALL M3MSG2( MESG )

            ELSE IF ( BUFF( 1:16 ) .EQ. '!         ID    ' ) THEN

                DO  JJ = 1, NUMT
                    CALL M3MSG2( BUFF( 27+5*JJ : 31+5*JJ ) )
                END DO
            ELSE
                II = INDEX( BUFF, 'START HOUR' )
                IF ( II .GT. 0 ) THEN
                    MESG = 'Input time period '// TRIM( BUFF(II:256) )
                    CALL M3MSG2( MESG )
                END IF
            END IF

            GO TO 100

111     CONTINUE        !  end loop reading file header

        RETURN
        END SUBROUTINE READHDR


        !!---------------------------------------------------------!!

        SUBROUTINE SKIPBLK( DDEV, M )
        IMPLICIT NONE
        INTEGER, INTENT(INOUT)::        DDEV, M

        CHARACTER*24, PARAMETER::  PNAME = 'AIRNOW2M3/SKIPBLK'

        INTEGER        STATUS
        CHARACTER*256  BUFF, MESG

200     CONTINUE        !  loop reading file header

            READ( DDEV, '(A)', IOSTAT=STATUS ) BUFF
            M = M + 1

            IF ( STATUS .NE. 0 ) THEN

                WRITE( MESG, '(A, I9, 2X, A, I9, 2X, A )' )
     &              'I/O error', STATUS,
     &              'encountered reading line', M,
     &              'of MONITOR-OBSERVATION FILE'
                CALL M3EXIT( PNAME, 0,0, MESG, 2 )

            ELSE IF ( BUFF( 1:11 ) .NE. 'BEGIN_GROUP' ) THEN

                GO TO 200

            END IF

        RETURN
        END SUBROUTINE SKIPBLK

      END PROGRAM AIRNOW2M3

