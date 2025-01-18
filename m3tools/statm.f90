
SUBROUTINE  STATM( NCOFS, NROWS, NTHIK,         &
                   NACT, INDX, COEF, CSUM,      &
                   LABEL, LOGDEV )

    !***********************************************************************
    ! Version "$Id: statm.f90 203 2021-10-14 18:02:11Z coats $"
    ! EDSS/Models-3 M3TOOLS.
    ! Copyright (C) 1992-2002 MCNC,
    ! (C) 1995-2002,2005-2013,2021 Carlie J. Coats, Jr.,
    ! and (C) 2002-2010 Baron Advanced Meteorological Systems. LLC.
    ! Distributed under the GNU GENERAL PUBLIC LICENSE version 2
    ! See file "GPL.txt" for conditions of use.
    !.........................................................................
    !  subroutine body starts at line  81
    !
    !  FUNCTION:
    !       Produce statistics report to LOGDEV
    !
    !  PRECONDITIONS REQUIRED: none
    !
    !  SUBROUTINES AND FUNCTIONS CALLED:  none
    !
    !  REVISION  HISTORY:
    !       Prototype 5/1995 by CJC
    !       Version   3/2007 by CJC: REAL*8 accumulators
    !       Version  12/2013 by CJC: INTENT for arguments
    !       Version  10/2021 by CJC:  free ".f90" source format for IOAPI-4.0
    !***********************************************************************

    IMPLICIT NONE

    !...........   ARGUMENTS and their descriptions:

    INTEGER, INTENT(IN   ) :: NCOFS   !  number of matrix coefficients
    INTEGER, INTENT(IN   ) :: NROWS   !  number of matrix rows
    INTEGER, INTENT(IN   ) :: NTHIK   !  number of (full-)matrix columns
    INTEGER, INTENT(IN   ) :: NACT(   NROWS ) !  no of actual cols, this row
    INTEGER, INTENT(IN   ) :: INDX(   NCOFS ) !  col-subscripts
    REAL   , INTENT(IN   ) :: COEF(   NCOFS ) !  col coeffs
    REAL   , INTENT(INOUT) :: CSUM( 2*NROWS ) !  matrix-column sums
    INTEGER, INTENT(IN   ) :: LOGDEV  !  unit number for stats report

    CHARACTER*(*), INTENT(IN) :: LABEL   !  legend text


    !...........   EXTERNAL FUNCTION:  number of leading blanks

    INTEGER, EXTERNAL :: LEN2


    !...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER         I, R, K, L, M  !  row, coef traversal counter
    INTEGER         MC, MR, MRR    !  indexes for maximum
    INTEGER         NC, NR, NRR    !  indexes for minimum
    REAL            S, T
    REAL            AMAX
    REAL            AMIN
    REAL*8          ASUM
    REAL*8          ASSQ
    REAL            RMAX
    REAL            RMIN
    REAL*8          RSUM
    REAL*8          RSSQ
    REAL*8          DNOM

    CHARACTER*20    MCBUF
    CHARACTER*20    MRBUF
    CHARACTER*20    NCBUF
    CHARACTER*20    NRBUF


    !***********************************************************************
    !   begin body of subroutine  STATM

    !...........   Construct 3-D GRID stats: max, min and their locations,
    !...........   mean, and sigma

    DO  11  R = 1, NTHIK
        CSUM( R ) = 0.0
11  CONTINUE

    S = 0.0
    DO  22 L = 1, NACT( 1 )
        S = S + COEF( L )
22  CONTINUE
    RMAX = S
    RMIN = S
    RSUM = 0.0
    RSSQ = 0.0

    M    = 0        !  high-water-mark for columns encountered

    MC   = INDX( 1 )
    MR   = 1
    MRR  = 1
    NC   = MC
    NR   = 1
    NRR  = 1
    T    = COEF( 1 )
    AMAX = T
    AMIN = T
    ASUM = 0.0
    ASSQ = 0.0

    K = 0
    DO  122  R = 1, NROWS

        S = 0.0

        DO  111  L = 1, NACT( R )
            K    = K + 1
            T    = COEF( K )
            I    = INDX( K )
            M    = MAX( M, I )
            CSUM( I ) = CSUM( I ) +  T
            S         = S         +  T
            ASUM      = ASUM      +  T
            ASSQ      = ASSQ      +  T * T
            IF ( T .GT. AMAX ) THEN
                AMAX = T
                MC   = I
                MR   = R
            ELSE IF ( T .LT. AMIN ) THEN
                AMIN = T
                NC   = I
                NR   = R
            END IF
111     CONTINUE

        RSUM = RSUM  +  S
        RSSQ = RSSQ  +  S * S
        IF (S  .GT. RMAX ) THEN
            RMAX = S
            MRR  = R
        ELSE IF ( S .LT. RMIN ) THEN
            RMIN = S
            NRR  = R
        END IF

122 CONTINUE

    DNOM = 1.0 / DBLE( K )
    ASUM = DNOM * ASUM
    ASSQ = SQRT( MAX( ASSQ * DNOM - ASUM * ASUM , 0.0d0 ) )

    WRITE( MCBUF,94010 ) MC
    WRITE( MRBUF,94010 ) MR
    WRITE( NCBUF,94010 ) NC
    WRITE( NRBUF,94010 ) NR

    WRITE( LOGDEV,92010 )                               &
        LABEL,                                          &
        'Matrix coefficient statistics' ,               &
        'Max   ', AMAX, ' @(c,r)=(',                    &
            MCBUF( LEN2( 1,20,MCBUF )+1 : 20 ),         &
            MRBUF( LEN2( 1,20,MRBUF )+1 : 20 ), ')',    &
        'Min   ', AMIN, ' @(c,r)=(',                    &
            NCBUF( LEN2( 1,20,NCBUF )+1 : 20 ),         &
            NRBUF( LEN2( 1,20,NRBUF )+1 : 20 ), ')',    &
        'Mean  ', ASUM,                                 &
        'Sigma ', ASSQ

    DNOM = 1.0 / FLOAT( NROWS )
    RSUM = DNOM * RSUM
    RSSQ = SQRT( MAX( RSSQ * DNOM - RSUM * RSUM , 0.0D0 ) )

    WRITE( MRBUF,94010 ) MRR
    WRITE( NRBUF,94010 ) NRR

    WRITE( LOGDEV,92020 )                               &
        'Matrix row-sum statistics' ,                   &
        'Max   ', RMAX, ' @ r=',                        &
            MRBUF( LEN2( 1,20,MRBUF )+1 : 20 ),         &
        'Min   ', RMIN, ' @ r=',                        &
            NRBUF( LEN2( 1,20,NRBUF )+1 : 20 ),         &
        'Mean  ', RSUM,                                 &
        'Sigma ', RSSQ

    !.......   Now compute statistics for column-sums:

    K    = M
    MC   = 1
    NC   = 1
    T    = CSUM( 1 )
    AMAX = T
    AMIN = T
    ASUM = 0.0
    ASSQ = 0.0

    DO  133  R = 1, K
        T    = CSUM( R )
        ASUM = ASUM + T
        ASSQ = ASSQ + T*T
        IF ( T .GT. AMAX ) THEN
            AMAX = T
            MC   = R
        ELSE IF ( T .LT. AMIN ) THEN
            AMIN = T
            NC   = R
        END IF
133 CONTINUE

    DNOM = 1.0 / DBLE( K )
    ASUM = DNOM * ASUM
    ASSQ = SQRT( MAX( ASSQ * DNOM - ASUM * ASUM , 0.0D0 ) )

    WRITE( MCBUF,94010 ) MC
    WRITE( NCBUF,94010 ) NC

    WRITE( LOGDEV,92020 )                           &
        'Matrix column-sum statistics' ,            &
        'Max   ', AMAX, ' @ c=',                    &
            MCBUF( LEN2( 1,20,MCBUF )+1 : 20 ),     &
        'Min   ', AMIN, ' @ c=',                    &
            NCBUF( LEN2( 1,20,NCBUF )+1 : 20 ),     &
        'Mean  ', ASUM,                             &
        'Sigma ', ASSQ


    RETURN

    !******************  FORMAT  STATEMENTS   ******************************

    !...........   Informational (LOG) message formats... 92xxx

92010 FORMAT ( /5X , 'Variable:  ', A,                  &
               /9X, A,                                  &
               2( /9X, A, 1PE12.5, A, A, ',', A, A ),   &
               2( /9X, A, 1PE12.5 ) )

92020 FORMAT ( 9X , A,                                  &
               2( /9X, A, 1PE12.5, A, A ),              &
               2( /9X, A, 1PE12.5 ) )



    !...........   Internal buffering formats............ 94xxx

94010 FORMAT( I20 )

END SUBROUTINE  STATM

