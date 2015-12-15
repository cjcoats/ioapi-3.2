
        SUBROUTINE  STATCUST( NCOLS, NLAYS, NVARS,
     &                        JDATE, JTIME, NTHRES, THRESH,
     &                        INNAME, VNAMES, VTYPES, RDEV )

C***********************************************************************
C Version "$Id: statcust.f 163 2015-02-24 06:48:57Z coats $"
C EDSS/Models-3 M3TOOLS
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (C) 2003-2013 Baron Advanced Meteorological Systems,
C (C) 2007-2013 Carlie J. Coats, Jr., and
C (C) 2014 UNC Institute for the Environment.
C Distributed under the GNU GENERAL PUBLIC LICENSE version 2
C See file "GPL.txt" for conditions of use.
C.........................................................................
C  subroutine body starts at line  91
C
C  FUNCTION:
C       Statistics report to RDEV on variables VNAMES  from file
C       INNAME.
C       and on the results of using GRIDOPS to apply the operations
C       OPNAME( * ) to them.
C
C  PRECONDITIONS REQUIRED:
C       Valid dates and times JDATE:JTIME
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       Models-3 I/O:  M3ERR(), READ3(), WRITE3()
C
C  REVISION  HISTORY:
C       Prototype 3/93 by CJC
C       Modified  9/99 by CJC for enhanced portability
C
C       Version 02/2010 by CJC for I/O API v3.1:  Fortran-90 only;
C       USE M3UTILIO, and related changes.
C
C       Version 02/2010 by CJC:  SIZE bug-fix
C
C       Version 06/2011 by CJC:  in-line STATC()
C
C       Version 12/2013 by CJC:  INTENT for arguments
C
C       Version  02/2015 by CJC: Support for M3INT8 variables
C***********************************************************************

      USE M3UTILIO
      IMPLICIT NONE

C...........   PARAMETERS and their descriptions:

        CHARACTER*16, PARAMETER :: PNAME = 'M3STAT:STATCUST'

C...........   ARGUMENTS and their descriptions:

        INTEGER     , INTENT(IN) :: NCOLS   ! grid dimensions, from INNAME header
        INTEGER     , INTENT(IN) :: NLAYS   ! grid dimensions, from INNAME header
        INTEGER     , INTENT(IN) :: NVARS   !  number of vbles to be totaled
        INTEGER     , INTENT(IN) :: JDATE   ! current model date
        INTEGER     , INTENT(IN) :: JTIME   ! current model time
        INTEGER     , INTENT(IN) :: NTHRES( NVARS )     ! number of tests per vble
        REAL        , INTENT(IN) :: THRESH( 10,NVARS )  ! thresholds for counting
        CHARACTER*16, INTENT(IN) :: INNAME              !  input file logical name
        CHARACTER*16, INTENT(IN) :: VNAMES( NVARS )     !  list of vble names
        INTEGER     , INTENT(IN) :: VTYPES( NVARS )     ! number of tests per vble
        INTEGER     , INTENT(IN) :: RDEV    ! unit number for output

        INTEGER, EXTERNAL :: LEN2

C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        REAL            GRID( NCOLS, NLAYS )
        DOUBLEPRECISION DGRD( NCOLS, NLAYS )
        INTEGER         IGRD( NCOLS, NLAYS )
        INTEGER*8       LGRD( NCOLS, NLAYS )
        INTEGER         C, L, M, N, V, SIZE
        INTEGER         MC, ML      !  indexes for maximum
        INTEGER         NC, NL      !  indexes for minimum
        REAL            T
        REAL            AMAX
        REAL            AMIN
        REAL*8          ASUM
        REAL*8          ASSQ
        REAL*8          DNOM
        INTEGER         ECNT

        CHARACTER*20    MCBUF
        CHARACTER*20    MLBUF
        CHARACTER*20    NCBUF
        CHARACTER*20    NLBUF

        CHARACTER*120   MESG

C***********************************************************************
C   begin body of subroutine  STATCUST

        IF ( JDATE .NE. 0 .OR. JTIME .NE. 0 ) THEN
            WRITE( RDEV,92000 )
     &          INNAME, JDATE, JTIME, DT2STR( JDATE, JTIME )
        ELSE
            WRITE( RDEV,92010 ) INNAME
        END IF

        SIZE = NCOLS*NLAYS

        DO  111  V = 1, NVARS

            IF ( VTYPES( V ) .EQ. M3REAL ) THEN

                IF ( .NOT. READ3( INNAME, VNAMES( V ), ALLAYS3,
     &                            JDATE, JTIME, GRID ) ) THEN

                    MESG = 'Read failure:  file ' // INNAME //
     &                     ' variable ' // VNAMES( V )
                    CALL M3EXIT( PNAME, JDATE, JTIME,
     &                           MESG, 2 )
                    GO TO 111

                END IF              !  if read3() worked, or not

            ELSE IF ( VTYPES( V ) .EQ. M3INT ) THEN

                IF ( .NOT. READ3( INNAME, VNAMES( V ), ALLAYS3,
     &                            JDATE, JTIME, IGRD ) ) THEN

                    MESG = 'Read failure:  file ' // INNAME //
     &                     ' variable ' // VNAMES( V )
                    CALL M3EXIT( PNAME, JDATE, JTIME,
     &                           MESG, 2 )
                    GO TO 111

                END IF              !  if read3() worked, or not

                CALL INTG2REAL( SIZE, IGRD, GRID )

            ELSE IF ( VTYPES( V ) .EQ. M3INT8 ) THEN

                IF ( .NOT. READ3( INNAME, VNAMES( V ), ALLAYS3,
     &                            JDATE, JTIME, LGRD ) ) THEN

                    MESG = 'Read failure:  file ' // INNAME //
     &                     ' variable ' // VNAMES( V )
                    CALL M3EXIT( PNAME, JDATE, JTIME,
     &                           MESG, 2 )
                    GO TO 111

                END IF              !  if read3() worked, or not

                CALL INT82REAL( SIZE, LGRD, GRID )

            ELSE IF ( VTYPES( V ) .EQ. M3DBLE ) THEN

                IF ( .NOT. READ3( INNAME, VNAMES( V ), ALLAYS3,
     &                            JDATE, JTIME, DGRD ) ) THEN

                    MESG = 'Read failure:  file ' // INNAME //
     &                     ' variable ' // VNAMES( V )
                    CALL M3EXIT( PNAME, JDATE, JTIME,
     &                           MESG, 2 )
                    GO TO 111

                END IF              !  if read3() worked, or not

                CALL DBLE2REAL( SIZE, DGRD, GRID )

            ELSE

                MESG = 'Unknown data type for' //
     &                 ' variable ' // VNAMES( V )
                CALL M3EXIT( PNAME, JDATE, JTIME, MESG, 2 )
                GO TO 111

            END IF

C...........   Construct 3-D CUSTOM stats: max, min and their locations,
C...........   mean, and sigma

            MC   = 1
            ML   = 1
            NC   = 1
            NL   = 1
            T    = GRID( 1,1 )
            AMAX = T
            AMIN = T
            ASUM = 0.0D0
            ASSQ = 0.0D0

            DO   L = 1, NLAYS   !  3-D traversal:  all other layers
            DO   C = 1, NCOLS
                T    = GRID( C,L )
                ASUM = ASUM + T
                ASSQ = ASSQ + T*T
                IF ( T .GT. AMAX ) THEN
                    AMAX = T
                    MC   = C
                    ML   = L
                ELSE IF ( T .LT. AMIN ) THEN
                    AMIN = T
                    NC   = C
                    NL   = L
                END IF
            END DO
            END DO

            DNOM = 1.0 / DBLE( NCOLS * NLAYS )
            ASUM = DNOM * ASUM
            ASSQ = SQRT( MAX( ASSQ * DNOM - ASUM * ASUM , 0.0d0 ) )

            WRITE( MCBUF,94010 ) MC
            WRITE( MLBUF,94010 ) ML
            WRITE( NCBUF,94010 ) NC
            WRITE( NLBUF,94010 ) NL

            WRITE( RDEV,92010 )
     &          VNAMES( V ),
     &          'Max   ', AMAX, ' @(c,l)=(',
     &          MCBUF( LEN2( 1,20,MCBUF )+1 : 20 ),
     &          MLBUF( LEN2( 1,20,MLBUF )+1 : 20 ), ')',
     &          'Min   ', AMIN, ' @(c,l)=(',
     &          NCBUF( LEN2( 1,20,NCBUF )+1 : 20 ),
     &          NLBUF( LEN2( 1,20,NLBUF )+1 : 20 ), ')',
     &          'Mean  ', ASUM,
     &          'Sigma ', ASSQ


C...........   For each threshold level, count the number of times the
C...........   grid value exceeds the threshold, and report it:

            DO N = 1, NTHRES( V )   !  count threshold excesses:
                ECNT = 0
                T    = THRESH( N,V )
                DO   L = 1, NLAYS
                DO   C = 1, NCOLS
                    IF ( GRID( C,L ) .GE. T )  ECNT = ECNT + 1
                END DO
                END DO
                WRITE( RDEV,92020 ) T, ECNT, DNOM * DBLE( ECNT )
            END DO

111     CONTINUE        !  end loop on variables

        RETURN

C******************  FORMAT  STATEMENTS   ******************************

C...........   Informational (LOG) message formats... 92xxx

92000   FORMAT ( //5X, 'File:  ', A, :,
     &            /5X, 'Date and time:', I7.7, ':', I6.6, 2X, A )

92010   FORMAT ( / , 'Variable:  ', A, 2X, 'Array statistics',
     &           2( /9X, A, 1PE12.5, A, A, ',', A, A ),
     &           2( /9X, A, 1PE12.5 ) )

92020   FORMAT ( 9X , 'Number of times ', 1PE12.5,
     &           2X, 'exceeded:', I8,
     &           2X, 'fraction:', F10.8 )

C...........   Internal buffering formats............ 94xxx

94010   FORMAT( I20 )

        END SUBROUTINE  STATCUST

