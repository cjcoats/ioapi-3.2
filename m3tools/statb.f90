

SUBROUTINE  STATB( SIZE, NCOLS, NROWS, NLAYS, NTHIK, BDRY,      &
                   NEPS, EPS, LABEL, LOGDEV )

    !***********************************************************************
    ! Version "$Id: statb.f90 203 2021-10-14 18:02:11Z coats $"
    ! EDSS/Models-3 M3TOOLS.
    ! Copyright (C) 1992-2002 MCNC,
    ! (C) 1995-2002,2005-2013,2021 Carlie J. Coats, Jr.,
    ! (C) 2002-2010 Baron Advanced Meteorological Systems. LLC.
    ! Distributed under the GNU GENERAL PUBLIC LICENSE version 2
    ! See file "GPL.txt" for conditions of use.
    !.........................................................................
    !  subroutine body starts at line  71
    !
    !  FUNCTION:
    !       Produce statistics report to LOGDEV
    !
    !  PRECONDITIONS REQUIRED:
    !       Stack-allocation operating environment (such as CRAY)
    !       number of columns, rows, and levels at most 99
    !
    !  SUBROUTINES AND FUNCTIONS CALLED:
    !       none
    !
    !  REVISION  HISTORY:
    !       Prototype 3/1993 by CJC
    !       Version   3/2007 by CJC: REAL*8 accumulators
    !       Version  11/2007 by CJC: Bug-fix for misplaced initialization
    !       of perimeter-counter K
    !       Version  12/2013 by CJC: INTENT for arguments
    !       Version  10/2021 by CJC:  free ".f90" source format for IOAPI-4.0
    !***********************************************************************

    IMPLICIT NONE

    !...........   ARGUMENTS and their descriptions:

    INTEGER, INTENT(IN) :: SIZE     !  horiz size,       from INNAME header
    INTEGER, INTENT(IN) :: NCOLS    !  grid dimensions,  from INNAME header
    INTEGER, INTENT(IN) :: NROWS    !  grid dimensions,  from INNAME header
    INTEGER, INTENT(IN) :: NLAYS    !  grid dimensions,  from INNAME header
    INTEGER, INTENT(IN) :: NTHIK    !  bdry thickness,   from INNAME header
    REAL   , INTENT(IN) :: BDRY( SIZE, NLAYS )  !  the grid.
    INTEGER, INTENT(IN) :: NEPS     !  number of thresholds
    REAL   , INTENT(IN) :: EPS( * ) !  thresholds for threshold-fraction reports
    INTEGER, INTENT(IN) :: LOGDEV   !  unit number for stats report

    CHARACTER*(*), INTENT(IN) :: LABEL   !  legend text


    !...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER         C, R, L, V, K   !  col, row, level, vble, cell counters
    INTEGER         MC, MR, ML      !  indexes for maximum
    INTEGER         NC, NR, NL      !  indexes for minimum
    REAL            T
    REAL            AMAX
    REAL            AMIN
    REAL*8          ASUM
    REAL*8          ASSQ
    REAL*8          DNOM
    INTEGER         ECNT


    !***********************************************************************
    !   begin body of subroutine  STATB

    !...........   Construct 3-D BDRY stats: max, min and their locations,
    !...........   mean, and sigma

    MC   = 1
    MR   = 1 - NTHIK
    ML   = 1
    NC   = 1
    NR   = MR
    NL   = 1
    T    = BDRY( 1,1 )
    AMAX = T
    AMIN = T
    ASUM = 0.0
    ASSQ = 0.0

    DO  155  L = 1, NLAYS   !  traversal:  all layers, all edges

        K = 0
        DO R = 1 - NTHIK, 0       !  south edge
        DO C = 1, NCOLS + NTHIK
            K    = K + 1
            T    = BDRY( K,L )
            ASUM = ASUM + T
            ASSQ = ASSQ + T*T
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
        END DO
        END DO

        DO  R = 1, NROWS + NTHIK       ! east edge
        DO  C = NCOLS + 1, NCOLS + NTHIK
            K    = K + 1
            T    = BDRY( K,L )
            ASUM = ASUM + T
            ASSQ = ASSQ + T*T
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
        END DO
        END DO

        DO R = NROWS + 1, NROWS + NTHIK   ! north edge
        DO C = 1 - NTHIK, 0
            K    = K + 1
            T    = BDRY( K,L )
            ASUM = ASUM + T
            ASSQ = ASSQ + T*T
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
        END DO
        END DO

        DO R = 1 - NTHIK, NROWS       !  west edge
        DO C = 1 - NTHIK, 0
            K    = K + 1
            T    = BDRY( K,L )
            ASUM = ASUM + T
            ASSQ = ASSQ + T*T
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
        END DO
        END DO

155 CONTINUE    !  end loop on levels

    DNOM = 1.0 / DBLE( NCOLS * NROWS * NLAYS )
    ASUM = DNOM * ASUM
    ASSQ = SQRT( MAX( ASSQ * DNOM - ASUM * ASUM , 0.0d0 ) )

    IF ( MAX( NCOLS, NROWS, NLAYS ) .LT. 100 - NTHIK ) THEN
        WRITE( LOGDEV,92010 )                                   &
            LABEL, ' 3-D boundary statistics' ,                 &
            'Max   ', AMAX, ' @(c,r,l)=(', MC, MR, ML, ')',     &
            'Min   ', AMIN, ' @(c,r,l)=(', NC, NR, NL, ')',     &
            'Mean  ', ASUM,                                     &
            'Sigma ', ASSQ
    ELSE
        WRITE( LOGDEV,92011 )                                   &
            LABEL, ' 3-D boundary statistics' ,                 &
            'Max   ', AMAX, ' @(c,r,l)=(', MC, MR, ML, ')',     &
            'Min   ', AMIN, ' @(c,r,l)=(', NC, NR, NL, ')',     &
            'Mean  ', ASUM,                                     &
            'Sigma ', ASSQ
    END IF


    !...........   For each threshold level, count the number of times the
    !...........   grid value exceeds the threshold, and report it:

    DO  199  V = 1, NEPS    !  count threshold excesses:
        ECNT = 0
        T    = EPS( V )
        DO L = 1, NLAYS
        DO K = 1, SIZE
            IF ( BDRY( K,L ) .GE. T )  ECNT = ECNT + 1
        END DO
        END DO
        WRITE( LOGDEV,92020 ) T, ECNT, DNOM * DBLE( ECNT )
199 CONTINUE

    RETURN

    !******************  FORMAT  STATEMENTS   ******************************

    !...........   Informational (LOG) message formats... 92xxx

92010 FORMAT ( /5X , 'Variable:  ', A, A,                           &
            2( /9X, A, 1PE12.5, A, I2, ',', I2, ',', I2, A ),       &
            2( /9X, A, 1PE12.5 ) )

92011 FORMAT ( /5X , 'Variable:  ', A, A,                           &
           2( /9X, A, 1PE12.5, A, I3, ',', I3, ',', I3, A ),        &
           2( /9X, A, 1PE12.5 ) )

92020 FORMAT ( 9X , 'Number of times ', 1PE12.5,                    &
               2X, 'exceeded:', I8,                                 &
               2X, 'fraction:', F10.8 )

END SUBROUTINE  STATB

