
SUBROUTINE  STATGRID( NCOLS, NROWS, NLAYS, NVARS,       &
                      JDATE, JTIME, NTHRES, THRES,      &
                      INNAME, VNAMES, VTYPES, LOGDEV )

    !***********************************************************************
    ! Version "$Id: statgrid.f90 280 2025-04-12 15:34:39Z coats $"
    ! EDSS/Models-3 M3TOOLS.
    ! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
    ! (C) 2003-2013 Baron Advanced Meteorological Systems,
    ! (C) 2007-2013,2021-2024 Carlie J. Coats, Jr., and
    ! (C) 2014 UNC Institute for the Environment.
    ! Distributed under the GNU GENERAL PUBLIC LICENSE version 2
    ! See file "GPL.txt" for conditions of use.
    !.........................................................................
    !  subroutine body starts at line  100
    !
    !  FUNCTION:
    !       Statistics report to LOGDEV on variables VNAMES  from file
    !       INNAME.
    !       and on the results of using GRIDOPS to apply the operations
    !       OPNAME( * ) to them.
    !
    !  PRECONDITIONS REQUIRED:
    !       Valid dates and times JDATE:JTIME
    !       Stack-allocation operating environment (such as CRAY)
    !
    !  SUBROUTINES AND FUNCTIONS CALLED:
    !       Models-3 I/O:  M3ERR(), READ3(), WRITE3()
    !
    !  REVISION  HISTORY:
    !       Prototype 3/93 by CJC
    !       Modified  9/99 by CJC for enhanced portability
    !
    !       Version 02/2010 by CJC for I/O API v3.1:  Fortran-90 only;
    !       USE M3UTILIO, and related changes.
    !
    !       Version 02/2010 by CJC:  SIZE bug-fix
    !
    !       Version  02/2015 by CJC: Support for M3INT8 variables
    !
    !       Version  10/2021 by CJC:  free ".f90" source format for IOAPI-4.0
    !
    !       Version  4/2025 by CJC:  in-line STATG()
    !***********************************************************************

    USE M3UTILIO
    IMPLICIT NONE

    !...........   PARAMETERS and their descriptions:

    CHARACTER*16, PARAMETER :: PNAME = 'M3STAT:STATGRID'

    !...........   ARGUMENTS and their descriptions:

    INTEGER     , INTENT(IN) :: NCOLS   ! grid dimensions, from INNAME header
    INTEGER     , INTENT(IN) :: NROWS   ! grid dimensions, from INNAME header
    INTEGER     , INTENT(IN) :: NLAYS   ! grid dimensions, from INNAME header
    INTEGER     , INTENT(IN) :: NVARS   !  number of vbles to be totaled
    INTEGER     , INTENT(IN) :: JDATE   ! current model date
    INTEGER     , INTENT(IN) :: JTIME   ! current model time
    INTEGER     , INTENT(IN) :: NTHRES( NVARS )     ! number of tests per vble
    REAL        , INTENT(IN) :: THRES ( 10,NVARS )  ! thresholds for counting
    CHARACTER*16, INTENT(IN) :: INNAME              !  input file logical name
    CHARACTER*16, INTENT(IN) :: VNAMES( NVARS )     !  list of vble names
    INTEGER     , INTENT(IN) :: VTYPES( NVARS )     ! number of tests per vble
    INTEGER     , INTENT(IN) :: LOGDEV              ! unit number for output


    !...........   SCRATCH LOCAL VARIABLES and their descriptions:

    REAL            GRID( NCOLS, NROWS, NLAYS )
    INTEGER         IGRD( NCOLS, NROWS, NLAYS )
    INTEGER*8       LGRD( NCOLS, NROWS, NLAYS )
    REAL*8          DGRD( NCOLS, NROWS, NLAYS )
    INTEGER         C, R, K, L, V, SIZE     !  col, row, level, variable, counters
    INTEGER         MC, MR, ML              !  indexes for maximum
    INTEGER         NC, NR, NL      !  indexes for minimum
    REAL            T
    REAL            AMAX
    REAL            AMIN
    REAL*8          ASUM, BSUM, CSUM
    REAL*8          ASSQ, BSSQ, CSSQ
    REAL*8          DNOM
    INTEGER         ECNT

    CHARACTER*20    MCBUF
    CHARACTER*20    MRBUF
    CHARACTER*20    MLBUF
    CHARACTER*20    NCBUF
    CHARACTER*20    NRBUF
    CHARACTER*20    NLBUF
    CHARACTER*24    DTBUF
    CHARACTER*120   MESG


    !***********************************************************************
    !   begin body of subroutine  STATGRID

    IF ( JDATE .NE. 0 .OR. JTIME .NE. 0 ) THEN
        DTBUF = DT2STR( JDATE, JTIME )
        WRITE( LOGDEV,92010 ) INNAME, JDATE, JTIME, DTBUF
    ELSE
        WRITE( LOGDEV,92010 ) INNAME
    END IF

    SIZE = NCOLS*NROWS*NLAYS

    DO  V = 1, NVARS

        IF ( VTYPES( V ) .EQ. M3REAL ) THEN

            IF ( .NOT. READ3( INNAME, VNAMES( V ), ALLAYS3, JDATE, JTIME, GRID ) ) THEN
                MESG = 'Read failure:  file ' // INNAME // ' variable ' // VNAMES( V )
                CALL M3EXIT( PNAME, JDATE, JTIME, MESG, 2 )
            END IF              !  if read3() worked, or not

        ELSE IF ( VTYPES( V ) .EQ. M3INT ) THEN

            IF ( .NOT. READ3( INNAME, VNAMES( V ), ALLAYS3, JDATE, JTIME, IGRD ) ) THEN
                MESG = 'Read failure:  file ' // INNAME // ' variable ' // VNAMES( V )
                CALL M3EXIT( PNAME, JDATE, JTIME, MESG, 2 )
            END IF              !  if read3() worked, or not

            CALL INTG2REAL( SIZE, IGRD, GRID )

        ELSE IF ( VTYPES( V ) .EQ. M3INT8 ) THEN

            IF ( .NOT. READ3( INNAME, VNAMES( V ), ALLAYS3, JDATE, JTIME, LGRD ) ) THEN
                MESG = 'Read failure:  file ' // INNAME // ' variable ' // VNAMES( V )
                CALL M3EXIT( PNAME, JDATE, JTIME, MESG, 2 )
            END IF              !  if read3() worked, or not

            CALL INT82REAL( SIZE, LGRD, GRID )

        ELSE IF ( VTYPES( V ) .EQ. M3DBLE ) THEN

            IF ( .NOT. READ3( INNAME, VNAMES( V ), ALLAYS3, JDATE, JTIME, DGRD ) ) THEN
                MESG = 'Read failure:  file ' // INNAME // ' variable ' // VNAMES( V )
                CALL M3EXIT( PNAME, JDATE, JTIME, MESG, 2 )
            END IF              !  if read3() worked, or not

            CALL DBLE2REAL( SIZE, DGRD, GRID )

        ELSE

            MESG = 'Unknown data type for variable ' // VNAMES( V )
            CALL M3EXIT( PNAME, JDATE, JTIME, MESG, 2 )

        END IF

        !...........   Construct 3-D GRID stats: max, min and their locations,
        !...........   mean, and sigma

        MC   = 1
        MR   = 1
        ML   = 1
        NC   = 1
        NR   = 1
        NL   = 1
        T    = GRID( 1,1,1 )
        AMAX = T
        AMIN = T
        ASUM = 0.0
        ASSQ = 0.0

        DO  L = 1, NLAYS   !  3-D traversal:  all other layers

            BSUM = 0.0
            BSSQ = 0.0

            DO  R = 1, NROWS

                CSUM = 0.0
                CSSQ = 0.0

                DO  C = 1, NCOLS

                    T    = GRID( C,R,L )
                    CSUM = CSUM + T
                    CSSQ = CSSQ + T*T
                    IF ( T .GT. AMAX ) THEN
                        AMAX = T
                        MC   = C
                        MR   = R
                        ML   = L
                    ELSE IF ( T .LT. AMIN ) THEN
                        AMIN = T
                        NC   = C
                        NR   = R
                        NL   = L
                    END IF

                END DO          !  end loop on C

                BSUM = BSUM + CSUM
                BSSQ = BSSQ + CSSQ

            END DO          !  end loop on R

            ASUM = ASUM + BSUM
            ASSQ = ASSQ + BSSQ

        END DO          !  end loop on L

        DNOM = 1.0 / DBLE( NCOLS * NROWS * NLAYS )
        ASUM = DNOM * ASUM
        ASSQ = SQRT( MAX( ASSQ * DNOM - ASUM * ASUM , 0.0d0 ) )
        WRITE( MCBUF,94010 ) MC
        WRITE( MRBUF,94010 ) MR
        WRITE( MLBUF,94010 ) ML
        WRITE( NCBUF,94010 ) NC
        WRITE( NRBUF,94010 ) NR
        WRITE( NLBUF,94010 ) NL

        WRITE( LOGDEV,92020 )                               &
            VNAMES( V ), ' 3-D grid statistics' ,           &
            'Max   ', AMAX, ' @(c,r,l)=(',                  &
                MCBUF( LEN2( 1,20,MCBUF )+1 : 20 ),         &
                MRBUF( LEN2( 1,20,MRBUF )+1 : 20 ),         &
                MLBUF( LEN2( 1,20,MLBUF )+1 : 20 ), ')',    &
            'Min   ', AMIN, ' @(c,r,l)=(',                  &
                NCBUF( LEN2( 1,20,NCBUF )+1 : 20 ),         &
                NRBUF( LEN2( 1,20,NRBUF )+1 : 20 ),         &
                NLBUF( LEN2( 1,20,NLBUF )+1 : 20 ), ')',    &
            'Mean  ', ASUM,                                 &
            'Sigma ', ASSQ


        !...........   For each threshold level, count the number of times the
        !...........   grid value exceeds the threshold, and report it:

        DO   K = 1, NTHRES( V )    !  count threshold excesses:
            ECNT = 0
            T    = THRES( K,V )
            DO   L = 1, NLAYS
                DO   R = 1, NROWS
                DO   C = 1, NCOLS
                    IF ( GRID( C,R,L ) .GE. T )  ECNT = ECNT + 1
                END DO
                END DO
            END DO
            WRITE( LOGDEV,92030 ) T, ECNT, DNOM * DBLE( ECNT )
        END DO  

    END DO          !  end loop on variables

    RETURN

    !******************  FORMAT  STATEMENTS   ******************************

    !...........   Informational (LOG) message formats... 92xxx

92010 FORMAT ( //5X, 'File:  ', A, :,                   &
              /5X, 'Date and time:', I9.7, ':', I6.6,   &
               2X, '(', A, ')' )

92020 FORMAT ( /5X , 'Variable:  ', A,                          &
               /9X, A,                                          &
               2( /9X, A, 1PE12.5, A, A, ',', A, ',', A, A ),   &
               2( /9X, A, 1PE12.5 ) )

92030 FORMAT ( 9X , 'Number of times ', 1PE12.5,                &
               2X, 'exceeded:', I8,                             &
               2X, 'fraction:', F10.8 )


    !...........   Internal buffering formats............ 94xxx

94010 FORMAT( I20 )


END SUBROUTINE  STATGRID

