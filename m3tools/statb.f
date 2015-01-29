

        SUBROUTINE  STATB( SIZE, NCOLS, NROWS, NLAYS, NTHIK, BDRY,
     &                     NEPS, EPS, LABEL, LOGDEV )

C***********************************************************************
C Version "$Id: statb.f 101 2015-01-16 16:52:50Z coats $"
C EDSS/Models-3 M3TOOLS.
C Copyright (C) 1992-2002 MCNC, (C) 1995-2002,2005-2013 Carlie J. Coats, Jr.,
C and (C) 2002-2010 Baron Advanced Meteorological Systems. LLC.
C Distributed under the GNU GENERAL PUBLIC LICENSE version 2
C See file "GPL.txt" for conditions of use.
C.........................................................................
C  subroutine body starts at line  71
C
C  FUNCTION:
C       Produce statistics report to LOGDEV
C
C  PRECONDITIONS REQUIRED:
C       Stack-allocation operating environment (such as CRAY)
C       number of columns, rows, and levels at most 99
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       none
C
C  REVISION  HISTORY:
C       Prototype 3/1993 by CJC
C       Version   3/2007 by CJC: REAL*8 accumulators
C       Version  11/2007 by CJC: Bug-fix for misplaced initialization
C       of perimeter-counter K
C       Version  12/2013 by CJC: INTENT for arguments
C***********************************************************************

      IMPLICIT NONE

C...........   ARGUMENTS and their descriptions:

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


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

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


C***********************************************************************
C   begin body of subroutine  STATB

C...........   Construct 3-D BDRY stats: max, min and their locations,
C...........   mean, and sigma

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
            DO  112  R = 1 - NTHIK, 0       !  south edge
            DO  111  C = 1, NCOLS + NTHIK
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
111         CONTINUE
112         CONTINUE

            DO  122  R = 1, NROWS + NTHIK       ! east edge
            DO  121  C = NCOLS + 1, NCOLS + NTHIK
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
121         CONTINUE
122         CONTINUE

            DO  133  R = NROWS + 1, NROWS + NTHIK   ! north edge
            DO  132  C = 1 - NTHIK, 0
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
132         CONTINUE
133         CONTINUE

            DO  144  R = 1 - NTHIK, NROWS       !  west edge
            DO  143  C = 1 - NTHIK, 0
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
143         CONTINUE
144         CONTINUE

155     CONTINUE    !  end loop on levels

        DNOM = 1.0 / DBLE( NCOLS * NROWS * NLAYS )
        ASUM = DNOM * ASUM
        ASSQ = SQRT( MAX( ASSQ * DNOM - ASUM * ASUM , 0.0d0 ) )

        IF ( MAX( NCOLS, NROWS, NLAYS ) .LT. 100 - NTHIK ) THEN
            WRITE( LOGDEV,92010 )
     &          LABEL, ' 3-D boundary statistics' ,
     &          'Max   ', AMAX, ' @(c,r,l)=(', MC, MR, ML, ')',
     &          'Min   ', AMIN, ' @(c,r,l)=(', NC, NR, NL, ')',
     &          'Mean  ', ASUM,
     &          'Sigma ', ASSQ
        ELSE
            WRITE( LOGDEV,92011 )
     &          LABEL, ' 3-D boundary statistics' ,
     &          'Max   ', AMAX, ' @(c,r,l)=(', MC, MR, ML, ')',
     &          'Min   ', AMIN, ' @(c,r,l)=(', NC, NR, NL, ')',
     &          'Mean  ', ASUM,
     &          'Sigma ', ASSQ
        END IF


C...........   For each threshold level, count the number of times the
C...........   grid value exceeds the threshold, and report it:

        DO  199  V = 1, NEPS    !  count threshold excesses:
            ECNT = 0
            T    = EPS( V )
            DO  188  L = 1, NLAYS
            DO  177  K = 1, SIZE
                IF ( BDRY( K,L ) .GE. T )  ECNT = ECNT + 1
177         CONTINUE
188         CONTINUE
            WRITE( LOGDEV,92020 ) T, ECNT, DNOM * DBLE( ECNT )
199     CONTINUE

        RETURN

C******************  FORMAT  STATEMENTS   ******************************

C...........   Informational (LOG) message formats... 92xxx

92010   FORMAT ( /5X , 'Variable:  ', A, A,
     &           2( /9X, A, 1PE12.5, A, I2, ',', I2, ',', I2, A ),
     &           2( /9X, A, 1PE12.5 ) )

92011   FORMAT ( /5X , 'Variable:  ', A, A,
     &           2( /9X, A, 1PE12.5, A, I3, ',', I3, ',', I3, A ),
     &           2( /9X, A, 1PE12.5 ) )

92020   FORMAT ( 9X , 'Number of times ', 1PE12.5,
     &           2X, 'exceeded:', I8,
     &           2X, 'fraction:', F10.8 )

        END SUBROUTINE  STATB

