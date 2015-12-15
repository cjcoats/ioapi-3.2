
        SUBROUTINE AGGVAR( IFILE, INAME, VTYPE, VSIZE, ATYPE,
     &                     JDATE, JTIME, TSTEP, ASTEPS,
     &                     OFILE, ONAME )

C***********************************************************************
C Version "$Id: aggvars.f 163 2015-02-24 06:48:57Z coats $"
C EDSS/Models-3 M3TOOLS.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (C) 2003-2013 Baron Advanced Meteorological Systems,
C (C) 2007-2013 Carlie J. Coats, Jr., and
C (C) 2014 UNC Institute for the Environment.
C Distributed under the GNU GENERAL PUBLIC LICENSE version 2
C See file "GPL.txt" for conditions of use.
C.........................................................................
C       subroutine body starts at line  107
C
C  FUNCTION:
C       Aggregate time steps as either sum, average, max, or min.
C
C  PRECONDITIONS REQUIRED:
C       Valid date&times JDATE:JTIME, time-step TSTEP, duration ASTEPS.
C       PARAMETERs M3SUM, M3AVG, M3MAX match corresponding values in caller.
C       ATYPE is one of M3SUM, M3AVG, M3MAX.
C       Files IFILE, OFILE already OPEN3()'ed and contain INAME, ONAME
C       respectively.
C       valid VTYPE and correctly-set VSIZE
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       Models-3 I/O:  M3ERR(), READ3(), WRITE3()
C
C  REVISION  HISTORY:
C       Prototype subroutine TAGGREG 5/1997 by M Houyoux
C
C       TAGGREG modified  9/1999 by CJC for enhanced portability
C
C       New subroutine AGGVARS() 11/2002 for I/O API v2.2 adapted by CJC
C       from previous TAGGREG(): Multiple data types for the variables,
C       Fortran-90 only, self-contained operation.
C
C       New version with simplified memory management:  eliminate
C       allocation; use on-the-stack F90 "Auto" work variables.
C
C       Version  11/2004 by CJC:  add MIN as an aggregation option
C
C       Version  11/2005 by CJC:  eliminate unused vbles.
C
C       Version 02/2010 by CJC for I/O API v3.1:  Fortran-90 only;
C       USE M3UTILIO, and related changes.
C
C       Version 11/2013 by CJC:  OpenMP parallel
C
C       Version  02/2015 by CJC: Support for M3INT8 variables.
C***********************************************************************

      USE M3UTILIO

      IMPLICIT NONE

C...........   PARAMETERS

        INTEGER, PARAMETER ::  M3SUM = 1
        INTEGER, PARAMETER ::  M3AVG = 2
        INTEGER, PARAMETER ::  M3MAX = 3
        INTEGER, PARAMETER ::  M3MIN = 4

        CHARACTER(LEN= 32), PARAMETER ::  ANAME = 'M3TPROC/AGGVARS'

C...........   ARGUMENTS and their descriptions:

        CHARACTER*(*),INTENT( IN )::  IFILE   ! input file name
        CHARACTER*(*),INTENT( IN )::  INAME   ! input vble name
        INTEGER,      INTENT( IN )::  VTYPE   ! type M3INT|M3REAL|M3DBLE
        INTEGER,      INTENT( IN )::  VSIZE   ! total array dimensions
        INTEGER,      INTENT( IN )::  JDATE   ! current model date
        INTEGER,      INTENT( IN )::  JTIME   ! current model time
        INTEGER,      INTENT( IN )::  ATYPE   ! type of aggregation to perform
        INTEGER,      INTENT( IN )::  TSTEP   ! input-file time step
        INTEGER,      INTENT( IN )::  ASTEPS  ! number of timesteps to aggregate
        CHARACTER*(*),INTENT( IN )::  OFILE   ! output file name
        CHARACTER*(*),INTENT( IN )::  ONAME   ! output vble name


C...........   LOCAL VARIABLES and their descriptions:

        INTEGER         I, N
        INTEGER         ISTEP
        INTEGER         IDATE, ITIME
        REAL            RDIV
        REAL*8          DDIV

        CHARACTER(LEN=256)::    MESG

        INTEGER         IGRD ( VSIZE )
        INTEGER         ISCR ( VSIZE )
        INTEGER         LGRD ( VSIZE )
        INTEGER         LSCR ( VSIZE )
        REAL            RGRD ( VSIZE )
        REAL            RSCR ( VSIZE )
        REAL*8          DGRD ( VSIZE )
        REAL*8          DSCR ( VSIZE )


C***********************************************************************
C   begin body of VINITAGG entry:

        IDATE  = JDATE
        ITIME  = JTIME
        N = 0

        IF ( VTYPE .EQ. M3INT ) THEN

            IF( ATYPE .EQ. M3MAX ) THEN
!$OMP           PARALLEL DO DEFAULT( NONE ), SHARED( VSIZE, IGRD ),
!$OMP&                      PRIVATE( I )
                DO I = 1, VSIZE
                    IGRD( I ) = -1999999999
                END DO
            ELSE IF( ATYPE .EQ. M3MIN ) THEN
!$OMP           PARALLEL DO DEFAULT( NONE ), SHARED( VSIZE, IGRD ),
!$OMP&                      PRIVATE( I )
                DO I = 1, VSIZE
                    IGRD( I ) = 1999999999
                END DO
            ELSE
                WRITE( MESG, '( A, I12, 2X, A )' )
     &               'Aggregation type', ATYPE, 'not supported for INT'
                CALL M3WARN( ANAME, JDATE, JTIME, MESG )
                RETURN
            END IF

            DO ISTEP = 1, ASTEPS

                N = N + 1

                IF ( .NOT. READ3( IFILE, INAME, ALLAYS3,
     &                            IDATE, ITIME, ISCR ) ) THEN

                    MESG = 'Read failure:  file "' // TRIM( IFILE ) //
     &                     '" variable "' // TRIM( INAME ) // '"'
                    CALL M3WARN( ANAME, JDATE, JTIME, MESG )

                ELSE IF( ATYPE .EQ. M3MAX ) THEN

!$OMP               PARALLEL DO
!$OMP&                   DEFAULT( NONE ),
!$OMP&                    SHARED( VSIZE, IGRD, ISCR ),
!$OMP&                   PRIVATE( I )
                    DO I = 1, VSIZE
                        IGRD( I ) = MAX( IGRD( I ), ISCR( I ) )
                    END DO

                ELSE IF( ATYPE .EQ. M3MIN ) THEN

!$OMP               PARALLEL DO
!$OMP&                   DEFAULT( NONE ),
!$OMP&                    SHARED( VSIZE, IGRD, ISCR ),
!$OMP&                   PRIVATE( I )
                    DO I = 1, VSIZE
                        IGRD( I ) = MIN( IGRD( I ), ISCR( I ) )
                    END DO

                END IF              !  if read3() worked, or atype=max or min

                CALL NEXTIME( IDATE, ITIME, TSTEP )

            END DO              !  end loop on time steps

            IF ( N .EQ. 0 ) THEN
                WRITE( MESG, '( A, I9.7, A, I6.6 )' )
     &                 'No data starting at', JDATE, ':', JTIME
                CALL M3WARN( ANAME, JDATE, JTIME, MESG )
                RETURN
            END IF

            IF ( .NOT.WRITE3( OFILE, ONAME, JDATE, JTIME, IGRD ) ) THEN
                MESG = 'Write failure:  file "' // TRIM( OFILE ) //
     &                 '" and variable "'       // TRIM( ONAME ) // '"'
                CALL M3WARN( ANAME, JDATE, JTIME, MESG )
            END IF

        ELSE IF ( VTYPE .EQ. M3INT8 ) THEN

            IF( ATYPE .EQ. M3MAX ) THEN
!$OMP           PARALLEL DO DEFAULT( NONE ), SHARED( VSIZE, LGRD ),
!$OMP&                      PRIVATE( I )
                DO I = 1, VSIZE
                    LGRD( I ) = -1999999999
                END DO
            ELSE IF( ATYPE .EQ. M3MIN ) THEN
!$OMP           PARALLEL DO DEFAULT( NONE ), SHARED( VSIZE, LGRD ),
!$OMP&                      PRIVATE( I )
                DO I = 1, VSIZE
                    LGRD( I ) = 1999999999
                END DO
            ELSE
                WRITE( MESG, '( A, I12, 2X, A )' )
     &               'Aggregation type', ATYPE, 'not supported for INT'
                CALL M3WARN( ANAME, JDATE, JTIME, MESG )
                RETURN
            END IF

            DO ISTEP = 1, ASTEPS

                N = N + 1

                IF ( .NOT. READ3( IFILE, INAME, ALLAYS3,
     &                            IDATE, ITIME, LSCR ) ) THEN

                    MESG = 'Read failure:  file "' // TRIM( IFILE ) //
     &                     '" variable "' // TRIM( INAME ) // '"'
                    CALL M3WARN( ANAME, JDATE, JTIME, MESG )

                ELSE IF( ATYPE .EQ. M3MAX ) THEN

!$OMP               PARALLEL DO
!$OMP&                   DEFAULT( NONE ),
!$OMP&                    SHARED( VSIZE, LGRD, LSCR ),
!$OMP&                   PRIVATE( I )
                    DO I = 1, VSIZE
                        LGRD( I ) = MAX( LGRD( I ), LSCR( I ) )
                    END DO

                ELSE IF( ATYPE .EQ. M3MIN ) THEN

!$OMP               PARALLEL DO
!$OMP&                   DEFAULT( NONE ),
!$OMP&                    SHARED( VSIZE, LGRD, LSCR ),
!$OMP&                   PRIVATE( I )
                    DO I = 1, VSIZE
                        LGRD( I ) = MIN( LGRD( I ), LSCR( I ) )
                    END DO

                END IF              !  if read3() worked, or atype=max or min

                CALL NEXTIME( IDATE, ITIME, TSTEP )

            END DO              !  end loop on time steps

            IF ( N .EQ. 0 ) THEN
                WRITE( MESG, '( A, I9.7, A, I6.6 )' )
     &                 'No data starting at', JDATE, ':', JTIME
                CALL M3WARN( ANAME, JDATE, JTIME, MESG )
                RETURN
            END IF

            IF ( .NOT.WRITE3( OFILE, ONAME, JDATE, JTIME, LGRD ) ) THEN
                MESG = 'Write failure:  file "' // TRIM( OFILE ) //
     &                 '" and variable "'       // TRIM( ONAME ) // '"'
                CALL M3WARN( ANAME, JDATE, JTIME, MESG )
            END IF

        ELSE IF ( VTYPE .EQ. M3REAL ) THEN

            IF( ATYPE .EQ. M3MAX ) THEN
!$OMP           PARALLEL DO
!$OMP&               DEFAULT( NONE ),
!$OMP&                SHARED( VSIZE, RGRD ),
!$OMP&               PRIVATE( I )
                DO I = 1, VSIZE
                    RGRD( I ) = BADVAL3         !  -9.999E36 is VERY < 0
                END DO
            ELSE IF( ATYPE .EQ. M3MIN ) THEN
!$OMP           PARALLEL DO
!$OMP&               DEFAULT( NONE ),
!$OMP&                SHARED( VSIZE, RGRD ),
!$OMP&               PRIVATE( I )
                DO I = 1, VSIZE
                    RGRD( I ) = -BADVAL3         !  9.999E36 is HUGE
                END DO
            ELSE IF( ATYPE .EQ. M3SUM .OR.
     &               ATYPE .EQ. M3AVG ) THEN
!$OMP           PARALLEL DO
!$OMP&               DEFAULT( NONE ),
!$OMP&                SHARED( VSIZE, RGRD ),
!$OMP&               PRIVATE( I )
                 DO I = 1, VSIZE
                    RGRD( I ) = 0.0
                END DO
            ELSE
                WRITE( MESG, '( A, I12, 2X, A )' )
     &                'Aggregation type', ATYPE, 'not supported'
                CALL M3WARN( ANAME, JDATE, JTIME, MESG )
                RETURN
            END IF

            DO ISTEP = 1, ASTEPS

                N = N + 1

                IF ( .NOT. READ3( IFILE, INAME, ALLAYS3,
     &                            IDATE, ITIME, RSCR ) ) THEN

                    MESG = 'Read failure:  file "' // TRIM( IFILE ) //
     &                     '" variable "' // TRIM( INAME ) // '"'
                    CALL M3WARN( ANAME, JDATE, JTIME, MESG )

                ELSE IF( ATYPE .EQ. M3SUM .OR.
     &                   ATYPE .EQ. M3AVG ) THEN

!$OMP               PARALLEL DO
!$OMP&                   DEFAULT( NONE ),
!$OMP&                    SHARED( VSIZE, RGRD, RSCR ),
!$OMP&                   PRIVATE( I )
                    DO I = 1, VSIZE
                        RGRD( I ) = RGRD( I ) + RSCR( I )
                    END DO

                ELSE IF( ATYPE .EQ. M3MAX ) THEN

!$OMP               PARALLEL DO
!$OMP&                   DEFAULT( NONE ),
!$OMP&                    SHARED( VSIZE, RGRD, RSCR ),
!$OMP&                   PRIVATE( I )
                    DO I = 1, VSIZE
                        RGRD( I ) = MAX( RGRD( I ), RSCR( I ) )
                    END DO

                ELSE IF( ATYPE .EQ. M3MIN ) THEN

!$OMP               PARALLEL DO
!$OMP&                   DEFAULT( NONE ),
!$OMP&                    SHARED( VSIZE, RGRD, RSCR ),
!$OMP&                   PRIVATE( I )
                    DO I = 1, VSIZE
                        RGRD( I ) = MIN( RGRD( I ), RSCR( I ) )
                    END DO

                END IF              !  if read3() worked, or atype=..., or not

                CALL NEXTIME( IDATE, ITIME, TSTEP )

            END DO              !  end loop on time steps

            IF ( N .EQ. 0 ) THEN
                WRITE( MESG, '( A, I9.7, A, I6.6 )' )
     &                 'No data starting at', JDATE, ':', JTIME
                CALL M3WARN( ANAME, JDATE, JTIME, MESG )
                RETURN
            END IF

            IF( ATYPE .EQ. M3AVG ) THEN

                RDIV = 1.0 /FLOAT( N )
!$OMP           PARALLEL DO
!$OMP&               DEFAULT( NONE ),
!$OMP&                SHARED( VSIZE, RGRD, RDIV ),
!$OMP&               PRIVATE( I )
                DO  I = 1, VSIZE
                    RGRD( I ) = RGRD( I ) * RDIV
                END DO

            END IF

            IF ( .NOT.WRITE3( OFILE, ONAME, JDATE, JTIME, RGRD ) ) THEN
                MESG = 'Write failure:  file "' // TRIM( OFILE ) //
     &                 '" variable "' // TRIM( ONAME ) // '"'
                CALL M3EXIT ( ANAME, JDATE, JTIME, MESG, 2 )
            END IF

        ELSE IF ( VTYPE .EQ. M3DBLE ) THEN

            IF( ATYPE .EQ. M3MAX ) THEN
!$OMP           PARALLEL DO
!$OMP&               DEFAULT( NONE ),
!$OMP&                SHARED( VSIZE, DGRD ),
!$OMP&               PRIVATE( I )
                DO I = 1, VSIZE
                    DGRD( I ) = BADVAL3
                END DO
            ELSE IF( ATYPE .EQ. M3MIN ) THEN
!$OMP           PARALLEL DO
!$OMP&               DEFAULT( NONE ),
!$OMP&                SHARED( VSIZE, DGRD ),
!$OMP&               PRIVATE( I )
                DO I = 1, VSIZE
                    DGRD( I ) = -BADVAL3
                END DO
            ELSE IF( ATYPE .EQ. M3SUM .OR.
     &               ATYPE .EQ. M3AVG ) THEN
!$OMP           PARALLEL DO
!$OMP&               DEFAULT( NONE ),
!$OMP&                SHARED( VSIZE, DGRD ),
!$OMP&               PRIVATE( I )
                DO I = 1, VSIZE
                    DGRD( I ) = 0.0
                END DO
            ELSE
                WRITE( MESG, '( A, I12, 2X, A )' )
     &                'Aggregation type', ATYPE, 'not supported'
                    CALL M3WARN( ANAME, JDATE, JTIME, MESG )
                    RETURN
            END IF


            DO ISTEP = 1, ASTEPS

                N = N + 1

                IF ( .NOT.READ3( IFILE, INAME, ALLAYS3,
     &                           IDATE, ITIME, DSCR ) ) THEN

                    MESG = 'Read failure:  file "' // TRIM( IFILE ) //
     &                     '" variable "' // TRIM( INAME ) // '"'
                    CALL M3WARN( ANAME, JDATE, JTIME, MESG )

                ELSE IF( ATYPE .EQ. M3SUM .OR.
     &                   ATYPE .EQ. M3AVG ) THEN

!$OMP               PARALLEL DO
!$OMP&                  DEFAULT( NONE ),
!$OMP&                   SHARED( VSIZE, DGRD, DSCR ),
!$OMP&                  PRIVATE( I )

                    DO I = 1, VSIZE
                        DGRD( I ) = DGRD( I ) + DSCR( I )
                    END DO

                ELSE IF( ATYPE .EQ. M3MAX ) THEN

!$OMP               PARALLEL DO
!$OMP&                  DEFAULT( NONE ),
!$OMP&                   SHARED( VSIZE, DGRD, DSCR ),
!$OMP&                  PRIVATE( I )

                    DO I = 1, VSIZE
                        DGRD( I ) = MAX( DGRD( I ), DSCR( I ) )
                    END DO

                ELSE IF( ATYPE .EQ. M3MIN ) THEN

!$OMP               PARALLEL DO
!$OMP&                  DEFAULT( NONE ),
!$OMP&                   SHARED( VSIZE, DGRD, DSCR ),
!$OMP&                  PRIVATE( I )

                    DO I = 1, VSIZE
                        DGRD( I ) = MIN( DGRD( I ), DSCR( I ) )
                    END DO

                END IF              !  if read3() worked, or not

                CALL NEXTIME( IDATE, ITIME, TSTEP )

            END DO              !  end loop on time steps

            IF ( N .EQ. 0 ) THEN
                WRITE( MESG, '( A, I9.7, A, I6.6 )' )
     &                 'No data starting at', JDATE, ':', JTIME
                CALL M3WARN( ANAME, JDATE, JTIME, MESG )
                RETURN
            END IF

            IF( ATYPE .EQ. M3AVG ) THEN

                DDIV = 1.0D0 / DBLE( N )

!$OMP           PARALLEL DO
!$OMP&               DEFAULT( NONE ),
!$OMP&                SHARED( VSIZE, DGRD, DDIV ),
!$OMP&               PRIVATE( I )

                DO  I = 1, VSIZE
                    DGRD( I ) = DGRD( I ) * DDIV
                END DO

            END IF

            IF ( .NOT.WRITE3( OFILE, ONAME, JDATE, JTIME, DGRD ) ) THEN
                MESG = 'Write failure:  file "' // TRIM( OFILE ) //
     &                 '" variable "' // TRIM( ONAME ) // '"'
                CALL M3EXIT ( ANAME, JDATE, JTIME, MESG, 2 )
            END IF

        ELSE            !  vtype not m3int,m3real,m3dble

            WRITE( MESG, '( A, I10, 2X, 2A )' )
     &           'Unknown type', VTYPE, 'for variable', INAME
            CALL M3EXIT( ANAME, JDATE, JTIME, MESG, 2 )

        END IF          !  if vtype = m3int,m3real,m3dble, or not

        RETURN

        END  SUBROUTINE AGGVAR

