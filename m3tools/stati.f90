
SUBROUTINE  STATI( NROWS, NLAYS, N, ID, VV,     &
                   NEPS, EPS, LABEL, LOGDEV )

    !***********************************************************************
    ! Version "@(#)$Id: stati.f90 203 2021-10-14 18:02:11Z coats $"
    ! EDSS/Models-3 M3TOOLS.
    ! Copyright (C) 1992-2002 MCNC,
    ! (C) 1995-2002,2005-2013,2021 Carlie J. Coats, Jr.,
    ! (C) 2002-2010 Baron Advanced Meteorological Systems. LLC.
    ! Distributed under the GNU GENERAL PUBLIC LICENSE version 2
    ! See file "GPL.txt" for conditions of use.
    !.........................................................................
    !  subroutine body starts at line  76
    !
    !  FUNCTION:
    !       Produce statistics report to LOGDEV
    !
    !  PRECONDITIONS REQUIRED:
    !       None
    !
    !  SUBROUTINES AND FUNCTIONS CALLED:
    !       Models-3 I/O:  M3ERR(), READ3(), WRITE3()
    !
    !  REVISION  HISTORY:
    !       Prototype 3/1993 by CJC
    !       Version   3/2007 by CJC: REAL*8 accumulators
    !       Version  10/2021 by CJC:  free ".f90" source format for IOAPI-4.0
    !***********************************************************************

    IMPLICIT NONE

    !...........   ARGUMENTS and their descriptions:

    INTEGER, INTENT(IN) :: NROWS    !  grid dimensions, from INNAME header
    INTEGER, INTENT(IN) :: NLAYS    !  grid dimensions, from INNAME header
    INTEGER, INTENT(IN) :: N        !  actual number of data
    INTEGER, INTENT(IN) :: ID( NROWS )          !  ID-list
    REAL   , INTENT(IN) :: VV( NROWS, NLAYS )   !  the data values
    INTEGER, INTENT(IN) :: LOGDEV   !  unit number for stats report
    INTEGER, INTENT(IN) :: NEPS     !  number of thresholds
    REAL   , INTENT(IN) :: EPS( * ) !  thresholds for threshold-fraction reports

    CHARACTER*(*), INTENT(IN) :: LABEL   !  legend text


    !...........   EXTERNAL FUNCTION:  number of leading blanks

    INTEGER, EXTERNAL :: LEN2


    !...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER         R, L, V         !  row, level, variable, counters
    INTEGER         MI, MR, ML      !  indexes for maximum
    INTEGER         NI, NR, NL      !  indexes for minimum
    REAL            T
    REAL            AMAX
    REAL            AMIN
    REAL*8          ASUM
    REAL*8          ASSQ
    REAL*8          DNOM
    INTEGER         ECNT

    CHARACTER*20    MIBUF
    CHARACTER*20    MRBUF
    CHARACTER*20    MLBUF
    CHARACTER*20    NIBUF
    CHARACTER*20    NRBUF
    CHARACTER*20    NLBUF

    !***********************************************************************
    !   begin body of subroutine  STATI

    !...........   Construct 3-D VV stats: max, min and their locations,
    !...........   mean, and sigma

    MR   = ID( 1 )
    ML   = 1
    NR   = 1
    NL   = 1
    T    = VV( 1,1 )
    AMAX = T
    AMIN = T
    ASUM = 0.0
    ASSQ = 0.0

    DO  133  L = 1, NLAYS   !  3-D traversal:  all other layers
        DO  122  R = 1, N
            T    = VV( R,L )
            ASUM = ASUM + T
            ASSQ = ASSQ + T*T
            IF ( T .GT. AMAX ) THEN
                AMAX = T
                MR   = R
                MI   = ID( R )
                ML   = L
            ELSE IF ( T .LT. AMIN ) THEN
                AMIN = T
                NR   = R
                NI   = ID( R )
                NL   = L
            END IF
122     CONTINUE
133 CONTINUE

    DNOM = 1.0 / DBLE( N * NLAYS )
    ASUM = DNOM * ASUM
    ASSQ = SQRT( MAX( ASSQ * DNOM - ASUM * ASUM , 0.0d0 ) )

    WRITE( MIBUF,94010 ) MI
    WRITE( MRBUF,94010 ) MR
    WRITE( MRBUF,94010 ) ML
    WRITE( NIBUF,94010 ) NI
    WRITE( NRBUF,94010 ) NR
    WRITE( NRBUF,94010 ) NL

    WRITE( LOGDEV,92010 )                               &
            LABEL, ' array statistics' ,                &
            'Max   ', AMAX, ' at site ',                &
            MIBUF( LEN2( 1,20,MIBUF )+1 : 20 ),         &
            ' @(r,l)=',                                 &
            MRBUF( LEN2( 1,20,MRBUF )+1 : 20 ), ',',    &
            MLBUF( LEN2( 1,20,MLBUF )+1 : 20 ), ')',    &
            'Min   ', AMIN, ' at site ',                &
            NIBUF( LEN2( 1,20,NIBUF )+1 : 20 ),         &
            ' @(r,l)=',                                 &
            NRBUF( LEN2( 1,20,NRBUF )+1 : 20 ), ',',    &
            NLBUF( LEN2( 1,20,NLBUF )+1 : 20 ), ')',    &
            'Mean  ', ASUM,                             &
            'Sigma ', ASSQ


    !...........   For each threshold level, count the number of times the
    !...........   grid value exceeds the threshold, and report it:

    DO  199  V = 1, NEPS    !  count threshold excesses:
        ECNT = 0
        T    = EPS( V )
        DO  188  L = 1, NLAYS
            DO  177  R = 1, NROWS
                IF ( VV( R,L ) .GE. T )  ECNT = ECNT + 1
177         CONTINUE
188     CONTINUE
        WRITE( LOGDEV,92020 ) T, ECNT, DNOM * FLOAT( ECNT )
199 CONTINUE

    RETURN

    !******************  FORMAT  STATEMENTS   ******************************

    !...........   Informational (LOG) message formats... 92xxx

92010 FORMAT ( /5X , 'Variable:  ', A, A,           &
             2( /9X, A, 1PE12.5, 7A ),              &
             2( /9X, A, 1PE12.5 ) )

92020 FORMAT ( 9X , 'Number of times ', 1PE12.5,    &
             2X, 'exceeded:', I10,                  &
             2X, 'fraction:', F12.8 )

    !...........   Internal buffering formats............ 94xxx

94010 FORMAT( I20 )

END SUBROUTINE  STATI

