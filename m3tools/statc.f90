
SUBROUTINE  STATC( NCOLS, NLAYS, GRID, NEPS, EPS, LABEL, RDEV )

    !***********************************************************************
    ! Version "$Id: statc.f90 203 2021-10-14 18:02:11Z coats $"
    ! EDSS/Models-3 M3TOOLS.
    ! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
    ! (C) 2021 Carlie J. Coats, Jr.,, and
    ! (C) 2002-2007 Baron Advanced Meteorological Systems. LLC.
    ! Distributed under the GNU GENERAL PUBLIC LICENSE version 2
    ! See file "GPL.txt" for conditions of use.
    !.........................................................................
    !  subroutine body starts at line  75
    !
    !  FUNCTION:
    !       Produce statistics report to RDEV
    !
    !  PRECONDITIONS REQUIRED:
    !       Stack-allocation operating environment (such as CRAY)
    !       number of columns, rows, and levels at most 99
    !
    !  SUBROUTINES AND FUNCTIONS CALLED:
    !       Models-3 I/O:  M3ERR(), READ3(), WRITE3()
    !
    !  REVISION  HISTORY:
    !       Prototype 3/1993 by CJC
    !       Version   3/2007 by CJC: REAL*8 accumulators
    !       Version  12/2013 by CJC: INTENT for arguments
    !       Version  10/2021 by CJC:  free ".f90" source format for IOAPI-4.0
    !***********************************************************************

    IMPLICIT NONE


    !...........   ARGUMENTS and their descriptions:

    INTEGER      , INTENT(IN) :: NCOLS      !  grid dimensions, from INNAME header
    INTEGER      , INTENT(IN) :: NLAYS      !  grid dimensions, from INNAME header
    REAL         , INTENT(IN) :: GRID( NCOLS, NLAYS )   !  the grid.
    INTEGER      , INTENT(IN) :: RDEV       !  unit number for stats report
    INTEGER      , INTENT(IN) :: NEPS       !  number of thresholds
    REAL         , INTENT(IN) :: EPS( * )   !  thresholds for threshold-fraction reports
    CHARACTER*(*), INTENT(IN) :: LABEL      !  legend text


    !...........   EXTERNAL FUNCTION:  number of leading blanks

    INTEGER, EXTERNAL :: LEN2


    !...........   SCRATCH LOCAL VARIABLES and their descriptions:

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

    !***********************************************************************
    !   begin body of subroutine  STATC

    !...........   Construct 3-D CUSTOM stats: max, min and their locations,
    !...........   mean, and sigma

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
111 CONTINUE
133 CONTINUE

    DNOM = 1.0 / DBLE( NCOLS * NLAYS )
    ASUM = DNOM * ASUM
    ASSQ = SQRT( MAX( ASSQ * DNOM - ASUM * ASUM , 0.0d0 ) )

    WRITE( MCBUF,94010 ) MC
    WRITE( MLBUF,94010 ) ML
    WRITE( NCBUF,94010 ) NC
    WRITE( NLBUF,94010 ) NL

    WRITE( RDEV,92010 )                                 &
            LABEL, ' Array statistics' ,                &
            'Max   ', AMAX, ' @(c,l)=(',                &
            MCBUF( LEN2( 1,20,MCBUF )+1 : 20 ),         &
            MLBUF( LEN2( 1,20,MLBUF )+1 : 20 ), ')',    &
            'Min   ', AMIN, ' @(c,l)=(',                &
            NCBUF( LEN2( 1,20,NCBUF )+1 : 20 ),         &
            NLBUF( LEN2( 1,20,NLBUF )+1 : 20 ), ')',    &
            'Mean  ', ASUM,                             &
            'Sigma ', ASSQ


    !...........   For each threshold level, count the number of times the
    !...........   grid value exceeds the threshold, and report it:

    DO  199  V = 1, NEPS    !  count threshold excesses:
        ECNT = 0
        T    = EPS( V )
        DO  188  L = 1, NLAYS
            DO  166  C = 1, NCOLS
                IF ( GRID( C,L ) .GE. T )  ECNT = ECNT + 1
166         CONTINUE
188     CONTINUE
        WRITE( RDEV,92020 ) T, ECNT, DNOM * DBLE( ECNT )
199 CONTINUE

    RETURN

    !******************  FORMAT  STATEMENTS   ******************************

    !...........   Informational (LOG) message formats... 92xxx

92010 FORMAT ( / , 'Variable:  ', A, A,                 &
             2( /9X, A, 1PE12.5, A, A, ',', A, A ),     &
             2( /9X, A, 1PE12.5 ) )

92020 FORMAT ( 9X , 'Number of times ', 1PE12.5,        &
             2X, 'exceeded:', I8,                       &
             2X, 'fraction:', F10.8 )

    !...........   Internal buffering formats............ 94xxx

94010 FORMAT( I20 )

END SUBROUTINE  STATC

