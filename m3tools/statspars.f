
        SUBROUTINE  STATSPARS( NCOLS, NROWS, NTHIK, NVARS,
     &                         JDATE, JTIME,
     &                         INNAME, VNAMES, VTYPES, RDEV )

C***********************************************************************
C Version "$Id: statspars.f 163 2015-02-24 06:48:57Z coats $"
C EDSS/Models-3 M3TOOLS.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (C) 2003-2013 Baron Advanced Meteorological Systems,
C (C) 2007-2013 Carlie J. Coats, Jr., and
C (C) 2014 UNC Institute for the Environment.
C Distributed under the GNU GENERAL PUBLIC LICENSE version 2
C See file "GPL.txt" for conditions of use.
C.........................................................................
C  subroutine body starts at line  68
C
C  FUNCTION:
C       Statistics report to  on variables VNAMES  from file
C       INNAME and on the results of using GRIDOPS to apply the 
C       operations OPNAME( * ) to them.
C
C  PRECONDITIONS REQUIRED:  none
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       Models-3 I/O:  M3EXIT(), READ3(), WRITE3()
C
C  REVISION  HISTORY:
C       Prototype 3/93 by CJC
C       Modified  9/99 by CJC for enhanced portability
C
C       Version 02/2010 by CJC for I/O API v3.1:  Fortran-90 only;
C       USE M3UTILIO, and related changes.
C       Version  12/2013 by CJC: INTENT for arguments
C       Version  02/2015 by CJC: Support for M3INT8 variables.
C       Fix indexing bug
C***********************************************************************

      USE M3UTILIO
      IMPLICIT NONE


C...........   ARGUMENTS and their descriptions:

        INTEGER     , INTENT(IN) :: NCOLS   ! grid dim:  number of active coeffs
        INTEGER     , INTENT(IN) :: NROWS   ! grid dim:  number of matrix rows
        INTEGER     , INTENT(IN) :: NTHIK   ! grid dim:  number of matrix cols
        INTEGER     , INTENT(IN) :: NVARS   !  number of vbles to be totaled
        INTEGER     , INTENT(IN) :: JDATE   ! current model date
        INTEGER     , INTENT(IN) :: JTIME   ! current model time
        CHARACTER*16, INTENT(IN) :: INNAME                  !  input file logical name
        CHARACTER*16, INTENT(IN) :: VNAMES( NVARS )         !  list of vble names
        INTEGER     , INTENT(IN) :: VTYPES( NVARS )         !  list of types for variables
        INTEGER     , INTENT(IN) :: RDEV                    !  unit number for output


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         CSUM( 2 * NROWS )
        INTEGER         NACT( NROWS + (2*NVARS+1)*NCOLS )   !! NACT(NROWS) // INDX(NCOLS) // COEF(NCOLS,NVARS)
        REAL            GRID( NCOLS )                       !!  scratch array
        INTEGER         V, W, K


C***********************************************************************
C   begin body of subroutine  STATSPARS

        IF ( JDATE .NE. 0 .OR. JTIME .NE. 0 ) THEN
            WRITE( RDEV,92010 )
     &          INNAME, JDATE, JTIME, DT2STR( JDATE, JTIME )
        ELSE
            WRITE( RDEV,92010 ) INNAME
        END IF

        IF ( .NOT.READ3( INNAME, ALLVAR3, ALLAYS3,
     &                   JDATE, JTIME, NACT ) ) THEN

            CALL M3EXIT( 'M3STAT:STATSPARS', JDATE, JTIME,
     &                   'Read failure:  file ' // INNAME, 2 )

        END IF              !  if read3() failed

        K = NROWS + 1   !!  start of "INDX(NCOLS")
        W = K + NCOLS   !!  start of "COEF(NCOLS,NVARS")

        DO  V = 1, NVARS

            IF ( VTYPES( V ) .EQ. M3REAL ) THEN

                CALL STATM( NCOLS, NROWS, NTHIK,
     &                      NACT, NACT(K), NACT(W), CSUM,
     &                      VNAMES( V ), RDEV )
                W = W + NCOLS

            ELSE IF ( VTYPES( V ) .EQ. M3INT ) THEN

                CALL INTG2REAL( NCOLS, NACT(W), GRID )
                CALL STATM( NCOLS, NROWS, NTHIK,
     &                      NACT, NACT(K), GRID, CSUM,
     &                      VNAMES( V ), RDEV )
                W = W + NCOLS

            ELSE IF ( VTYPES( V ) .EQ. M3INT8 ) THEN

                CALL INT82REAL( NCOLS, NACT(W), GRID )
                CALL STATM( NCOLS, NROWS, NTHIK,
     &                      NACT, NACT(K), GRID, CSUM,
     &                      VNAMES( V ), RDEV )
                W = W + 2 * NCOLS

            ELSE IF ( VTYPES( V ) .EQ. M3DBLE ) THEN

                CALL DBLE2REAL( NCOLS, NACT(W), GRID )
                CALL STATM( NCOLS, NROWS, NTHIK,
     &                      NACT, NACT(K), GRID, CSUM,
     &                      VNAMES( V ), RDEV )
                W = W + 2 * NCOLS

            END IF

        END DO


        RETURN

C******************  FORMAT  STATEMENTS   ******************************

C...........   Informational (LOG) message formats... 92xxx

92010   FORMAT ( //5X, 'File:  ', A, :,
     &            /5X, 'Date and time:', I7.7, ':', I6.6, 2X, A )

        END SUBROUTINE  STATSPARS

