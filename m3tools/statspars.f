
        SUBROUTINE  STATSPARS( NCOLS, NROWS, NTHIK, NVARS,
     &                         JDATE, JTIME,
     &                         INNAME, VNAMES, VTYPES, RDEV )

C***********************************************************************
C Version "$Id: statspars.f 101 2015-01-16 16:52:50Z coats $"
C EDSS/Models-3 M3TOOLS.
C Copyright (C) 1992-2002 MCNC, (C) 1995-2002,2005-2013 Carlie J. Coats, Jr.,
C and (C) 2002-2010 Baron Advanced Meteorological Systems. LLC.
C Distributed under the GNU GENERAL PUBLIC LICENSE version 2
C See file "GPL.txt" for conditions of use.
C.........................................................................
C  subroutine body starts at line  105
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
        INTEGER         NACT( NROWS )
        INTEGER         INDX( NCOLS )
        REAL            COEF( NCOLS, NVARS )
        REAL            GRID( NCOLS )           !  scratch array
        INTEGER         V, W


C***********************************************************************
C   begin body of subroutine  STATSPARS

        IF ( JDATE .NE. 0 .OR. JTIME .NE. 0 ) THEN
            WRITE( RDEV,92010 )
     &          INNAME, JDATE, JTIME, DT2STR( JDATE, JTIME )
        ELSE
            WRITE( RDEV,92010 ) INNAME
        END IF

        IF ( READ3( INNAME, ALLVAR3, ALLAYS3,
     &                 JDATE, JTIME, NACT ) ) THEN

            W = 1

            DO  111  V = 1, NVARS

                IF ( VTYPES( V ) .EQ. M3REAL ) THEN

                    CALL STATM( NCOLS, NROWS, NTHIK,
     &                          NACT, INDX, COEF( 1,W ), CSUM,
     &                          VNAMES( V ), RDEV )
                    W = W + NCOLS

                ELSE IF ( VTYPES( V ) .EQ. M3INT ) THEN

                    CALL INTG2REAL( NCOLS, COEF( 1,W ), GRID )
                    CALL STATM( NCOLS, NROWS, NTHIK,
     &                          NACT, INDX, GRID, CSUM,
     &                          VNAMES( V ), RDEV )
                    W = W + NCOLS

                ELSE IF ( VTYPES( V ) .EQ. M3DBLE ) THEN

                    CALL DBLE2REAL( NCOLS, COEF( 1,W ), GRID )
                    CALL STATM( NCOLS, NROWS, NTHIK,
     &                          NACT, INDX, GRID, CSUM,
     &                          VNAMES( V ), RDEV )
                    W = W + 2 * NCOLS

                ELSE

                    CALL M3EXIT( 'M3STAT:STATSPARS', JDATE, JTIME,
     &                           'Bad type for variable ' //
     &                           VNAMES( V ), 2 )

                END IF

111         CONTINUE

        ELSE                !  read3() failed:

            CALL M3EXIT( 'M3STAT:STATSPARS', JDATE, JTIME,
     &                   'Read failure:  file ' // INNAME, 2 )

        END IF              !  if read3() worked, or not


        RETURN

C******************  FORMAT  STATEMENTS   ******************************

C...........   Informational (LOG) message formats... 92xxx

92010   FORMAT ( //5X, 'File:  ', A, :,
     &            /5X, 'Date and time:', I7.7, ':', I6.6, 2X, A )

        END SUBROUTINE  STATSPARS

