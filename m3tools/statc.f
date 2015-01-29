
        SUBROUTINE  STATC( NCOLS, NLAYS, GRID,
     &                     NEPS, EPS, LABEL, RDEV )

C***********************************************************************
C Version "$Id: statc.f 101 2015-01-16 16:52:50Z coats $"
C EDSS/Models-3 M3TOOLS.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
C (C) 2002-2007 Baron Advanced Meteorological Systems. LLC.
C Distributed under the GNU GENERAL PUBLIC LICENSE version 2
C See file "GPL.txt" for conditions of use.
C.........................................................................
C  subroutine body starts at line  74
C
C  FUNCTION:
C       Produce statistics report to RDEV
C
C  PRECONDITIONS REQUIRED:
C       Stack-allocation operating environment (such as CRAY)
C       number of columns, rows, and levels at most 99
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       Models-3 I/O:  M3ERR(), READ3(), WRITE3()
C
C  REVISION  HISTORY:
C       Prototype 3/1993 by CJC
C       Version   3/2007 by CJC: REAL*8 accumulators
C       Version  12/2013 by CJC: INTENT for arguments
C***********************************************************************

      IMPLICIT NONE


C...........   ARGUMENTS and their descriptions:

        INTEGER      , INTENT(IN) :: NCOLS      !  grid dimensions, from INNAME header
        INTEGER      , INTENT(IN) :: NLAYS      !  grid dimensions, from INNAME header
        REAL         , INTENT(IN) :: GRID( NCOLS, NLAYS )   !  the grid.
        INTEGER      , INTENT(IN) :: RDEV       !  unit number for stats report
        INTEGER      , INTENT(IN) :: NEPS       !  number of thresholds
        REAL         , INTENT(IN) :: EPS( * )   !  thresholds for threshold-fraction reports
        CHARACTER*(*), INTENT(IN) :: LABEL      !  legend text


C...........   EXTERNAL FUNCTION:  number of leading blanks

        INTEGER, EXTERNAL :: LEN2


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         C, L, V     !  col, level, variable, counters
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

C***********************************************************************
C   begin body of subroutine  STATC

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

        DO  133  L = 1, NLAYS   !  3-D traversal:  all other layers
        DO  111  C = 1, NCOLS
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
111     CONTINUE
133     CONTINUE

        DNOM = 1.0 / DBLE( NCOLS * NLAYS )
        ASUM = DNOM * ASUM
        ASSQ = SQRT( MAX( ASSQ * DNOM - ASUM * ASUM , 0.0d0 ) )

        WRITE( MCBUF,94010 ) MC
        WRITE( MLBUF,94010 ) ML
        WRITE( NCBUF,94010 ) NC
        WRITE( NLBUF,94010 ) NL

        WRITE( RDEV,92010 )
     &          LABEL, ' Array statistics' ,
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

        DO  199  V = 1, NEPS    !  count threshold excesses:
            ECNT = 0
            T    = EPS( V )
            DO  188  L = 1, NLAYS
            DO  166  C = 1, NCOLS
                IF ( GRID( C,L ) .GE. T )  ECNT = ECNT + 1
166         CONTINUE
188         CONTINUE
            WRITE( RDEV,92020 ) T, ECNT, DNOM * DBLE( ECNT )
199     CONTINUE

        RETURN

C******************  FORMAT  STATEMENTS   ******************************

C...........   Informational (LOG) message formats... 92xxx

92010   FORMAT ( / , 'Variable:  ', A, A,
     &           2( /9X, A, 1PE12.5, A, A, ',', A, A ),
     &           2( /9X, A, 1PE12.5 ) )

92020   FORMAT ( 9X , 'Number of times ', 1PE12.5,
     &           2X, 'exceeded:', I8,
     &           2X, 'fraction:', F10.8 )

C...........   Internal buffering formats............ 94xxx

94010   FORMAT( I20 )

        END SUBROUTINE  STATC

