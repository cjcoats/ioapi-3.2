
        SUBROUTINE  STATM( NCOFS, NROWS, NTHIK,
     &                     NACT, INDX, COEF, CSUM,
     &                     LABEL, LOGDEV )

C***********************************************************************
C Version "$Id: statm.f 101 2015-01-16 16:52:50Z coats $"
C EDSS/Models-3 M3TOOLS.
C Copyright (C) 1992-2002 MCNC, (C) 1995-2002,2005-2013 Carlie J. Coats, Jr.,
C and (C) 2002-2010 Baron Advanced Meteorological Systems. LLC.
C Distributed under the GNU GENERAL PUBLIC LICENSE version 2
C See file "GPL.txt" for conditions of use.
C.........................................................................
C  subroutine body starts at line  77
C
C  FUNCTION:
C       Produce statistics report to LOGDEV
C
C  PRECONDITIONS REQUIRED: none
C
C  SUBROUTINES AND FUNCTIONS CALLED:  none
C
C  REVISION  HISTORY:
C       Prototype 5/1995 by CJC
C       Version   3/2007 by CJC: REAL*8 accumulators
C       Version  12/2013 by CJC: INTENT for arguments
C***********************************************************************

      IMPLICIT NONE

C...........   ARGUMENTS and their descriptions:

        INTEGER, INTENT(IN   ) :: NCOFS   !  number of matrix coefficients
        INTEGER, INTENT(IN   ) :: NROWS   !  number of matrix rows
        INTEGER, INTENT(IN   ) :: NTHIK   !  number of (full-)matrix columns
        INTEGER, INTENT(IN   ) :: NACT(   NROWS ) !  no of actual cols, this row
        INTEGER, INTENT(IN   ) :: INDX(   NCOFS ) !  col-subscripts
        REAL   , INTENT(IN   ) :: COEF(   NCOFS ) !  col coeffs
        REAL   , INTENT(INOUT) :: CSUM( 2*NROWS ) !  matrix-column sums
        INTEGER, INTENT(IN   ) :: LOGDEV  !  unit number for stats report

        CHARACTER*(*), INTENT(IN) :: LABEL   !  legend text


C...........   EXTERNAL FUNCTION:  number of leading blanks

        INTEGER, EXTERNAL :: LEN2


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

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


C***********************************************************************
C   begin body of subroutine  STATM

C...........   Construct 3-D GRID stats: max, min and their locations,
C...........   mean, and sigma

        DO  11  R = 1, NTHIK
            CSUM( R ) = 0.0
11      CONTINUE

        S = 0.0
        DO  22 L = 1, NACT( 1 )
            S = S + COEF( L )
22      CONTINUE
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
111         CONTINUE

            RSUM = RSUM  +  S
            RSSQ = RSSQ  +  S * S
            IF (S  .GT. RMAX ) THEN
                RMAX = S
                MRR  = R
            ELSE IF ( S .LT. RMIN ) THEN
                RMIN = S
                NRR  = R
            END IF

122     CONTINUE

        DNOM = 1.0 / DBLE( K )
        ASUM = DNOM * ASUM
        ASSQ = SQRT( MAX( ASSQ * DNOM - ASUM * ASUM , 0.0d0 ) )

        WRITE( MCBUF,94010 ) MC
        WRITE( MRBUF,94010 ) MR
        WRITE( NCBUF,94010 ) NC
        WRITE( NRBUF,94010 ) NR

        WRITE( LOGDEV,92010 )
     &      LABEL,
     &      'Matrix coefficient statistics' ,
     &      'Max   ', AMAX, ' @(c,r)=(',
     &          MCBUF( LEN2( 1,20,MCBUF )+1 : 20 ),
     &          MRBUF( LEN2( 1,20,MRBUF )+1 : 20 ), ')',
     &      'Min   ', AMIN, ' @(c,r)=(',
     &          NCBUF( LEN2( 1,20,NCBUF )+1 : 20 ),
     &          NRBUF( LEN2( 1,20,NRBUF )+1 : 20 ), ')',
     &      'Mean  ', ASUM,
     &      'Sigma ', ASSQ

        DNOM = 1.0 / FLOAT( NROWS )
        RSUM = DNOM * RSUM
        RSSQ = SQRT( MAX( RSSQ * DNOM - RSUM * RSUM , 0.0D0 ) )

        WRITE( MRBUF,94010 ) MRR
        WRITE( NRBUF,94010 ) NRR

        WRITE( LOGDEV,92020 )
     &      'Matrix row-sum statistics' ,
     &      'Max   ', RMAX, ' @ r=',
     &          MRBUF( LEN2( 1,20,MRBUF )+1 : 20 ),
     &      'Min   ', RMIN, ' @ r=',
     &          NRBUF( LEN2( 1,20,NRBUF )+1 : 20 ),
     &      'Mean  ', RSUM,
     &      'Sigma ', RSSQ

C.......   Now compute statistics for column-sums:

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
133     CONTINUE

        DNOM = 1.0 / DBLE( K )
        ASUM = DNOM * ASUM
        ASSQ = SQRT( MAX( ASSQ * DNOM - ASUM * ASUM , 0.0D0 ) )

        WRITE( MCBUF,94010 ) MC
        WRITE( NCBUF,94010 ) NC

        WRITE( LOGDEV,92020 )
     &      'Matrix column-sum statistics' ,
     &      'Max   ', AMAX, ' @ c=',
     &          MCBUF( LEN2( 1,20,MCBUF )+1 : 20 ),
     &      'Min   ', AMIN, ' @ c=',
     &          NCBUF( LEN2( 1,20,NCBUF )+1 : 20 ),
     &      'Mean  ', ASUM,
     &      'Sigma ', ASSQ


        RETURN

C******************  FORMAT  STATEMENTS   ******************************

C...........   Informational (LOG) message formats... 92xxx

92010   FORMAT ( /5X , 'Variable:  ', A,
     &           /9X, A,
     &           2( /9X, A, 1PE12.5, A, A, ',', A, A ),
     &           2( /9X, A, 1PE12.5 ) )

92020   FORMAT ( 9X , A,
     &           2( /9X, A, 1PE12.5, A, A ),
     &           2( /9X, A, 1PE12.5 ) )



C...........   Internal buffering formats............ 94xxx

94010   FORMAT( I20 )

        END SUBROUTINE  STATM

