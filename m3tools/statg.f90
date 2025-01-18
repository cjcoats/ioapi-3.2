
SUBROUTINE  STATG( NCOLS, NROWS, NLAYS, GRID,   &
                   NEPS, EPS, LABEL, LOGDEV )

    !***********************************************************************
    ! Version "$Id: statg.f90 203 2021-10-14 18:02:11Z coats $"
    ! EDSS/Models-3 M3TOOLS.
    ! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
    ! (C) 2002-2007 Baron Advanced Meteorological Systems. LLC.,
    ! (C) 2021 Carlie J. Coats, Jr.
    ! Distributed under the GNU GENERAL PUBLIC LICENSE version 2
    ! See file "GPL.txt" for conditions of use.
    !.........................................................................
    !  subroutine body starts at line  76
    !
    !  FUNCTION:
    !       Produce statistics report to LOGDEV
    !
    !  PRECONDITIONS REQUIRED:
    !
    !  SUBROUTINES AND FUNCTIONS CALLED:
    !       Models-3 I/O:  LEN2()
    !
    !  REVISION  HISTORY:
    !       Prototype 5/95 by CJC adapted from "stats.f"
    !       Version 3/2007 by CJC: REAL*8 accumulators
    !       Version  12/2013 by CJC: INTENT for arguments
    !       Version  10/2021 by CJC:  free ".f90" source format for IOAPI-4.0
    !***********************************************************************

    IMPLICIT NONE

    !...........   ARGUMENTS and their descriptions:

    INTEGER, INTENT(IN) :: NCOLS    !  grid dimensions, from INNAME header
    INTEGER, INTENT(IN) :: NROWS    !  grid dimensions, from INNAME header
    INTEGER, INTENT(IN) :: NLAYS    !  grid dimensions, from INNAME header
    REAL   , INTENT(IN) :: GRID( NCOLS, NROWS, NLAYS )  !  the grid.
    INTEGER, INTENT(IN) :: LOGDEV   !  unit number for stats report
    INTEGER, INTENT(IN) :: NEPS     !  number of thresholds
    REAL   , INTENT(IN) :: EPS(*)   !  thresholds for threshold-fraction reports
    CHARACTER*(*), INTENT(IN) :: LABEL   !  legend text


    !...........   EXTERNAL FUNCTION:  number of leading blanks

    INTEGER, EXTERNAL :: LEN2


    !...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER         C, R, L, V      !  col, row, level, variable, counters
    INTEGER         MC, MR, ML      !  indexes for maximum
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

    !***********************************************************************
    !   begin body of subroutine  STATG

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

    WRITE( LOGDEV,92010 )                               &
        LABEL, ' 3-D grid statistics' ,                 &
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

    DO   V = 1, NEPS    !  count threshold excesses:
        ECNT = 0
        T    = EPS( V )
        DO   L = 1, NLAYS
            DO   R = 1, NROWS
            DO   C = 1, NCOLS
                IF ( GRID( C,R,L ) .GE. T )  ECNT = ECNT + 1
            END DO
            END DO
        END DO
        WRITE( LOGDEV,92020 ) T, ECNT, DNOM * DBLE( ECNT )
    END DO          !  end loop on V

    RETURN

    !******************  FORMAT  STATEMENTS   ******************************

    !...........   Informational (LOG) message formats... 92xxx

92010 FORMAT ( /5X , 'Variable:  ', A,                          &
               /9X, A,                                          &
               2( /9X, A, 1PE12.5, A, A, ',', A, ',', A, A ),   &
               2( /9X, A, 1PE12.5 ) )

92020 FORMAT ( 9X , 'Number of times ', 1PE12.5,                &
               2X, 'exceeded:', I8,                             &
               2X, 'fraction:', F10.8 )


    !...........   Internal buffering formats............ 94xxx

94010 FORMAT( I20 )

END SUBROUTINE  STATG

