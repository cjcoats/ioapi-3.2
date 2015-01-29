
        SUBROUTINE  MAXREG

C***********************************************************************
C Version "$Id: agmask.f 101 2015-01-16 16:52:50Z coats $"
C EDSS/Models-3 M3TOOLS.
C Copyright (C) 1992-2002 MCNC, (C) 1995-2002,2005-2013 Carlie J. Coats, Jr.,
C and (C) 2002-2010 Baron Advanced Meteorological Systems. LLC.
C Distributed under the GNU GENERAL PUBLIC LICENSE version 2
C See file "GPL.txt" for conditions of use.
C.........................................................................
C  entry INITTAG body starts at line  110
C  entry AGGREG  body starts at line  195
C  entry OUTAGG  body starts at line  285
C  entry MAXAGG  body starts at line  522
C
C  FUNCTION:
C       Aggregate time steps as either sum, average, or max.
C       INITAGG: allocates memory and initializes array with first time
C                step of data
C       AGGREG:  Modifies gridded storage array based on current time step
C                of data
C       OUTAGG:  Completes gridded array values and writes to output file
C
C  PRECONDITIONS REQUIRED:
C       Valid dates and times JDATE:JTIME
C       f77 MALLOC()-allocation operating environment (such as Sun, SGI)
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       Models-3 I/O:  M3ERR(), READ3(), WRITE3()
C
C  REVISION  HISTORY:
C       Prototype 5/97 by M Houyoux
C
C       Modified  9/99 by CJC for enhanced portability
C
C       Modified 11/99 by CJC to keep hour-maxes of the aggregated result.
C       F90 only.
C
C       Modified 11/2005 by CJC:  eliminate unused vbles.
C
C       Version 02/2010 by CJC for I/O API v3.1:  Fortran-90 only;
C       USE M3UTILIO, and related changes.
C
C       Version 11/2013 by CJC:  OpenMP parallel
C***********************************************************************

      USE M3UTILIO

      IMPLICIT NONE

C...........   PARAMETERS:

        INTEGER, PARAMETER :: M3SUM = 1
        INTEGER, PARAMETER :: M3AVE = 2
        INTEGER, PARAMETER :: M3MAX = 3

        CHARACTER*2, PARAMETER :: SUFFIX( 6 ) =
     &     (/ '_1', '_2', '_3', '_4', '_5', '_6' /)

C...........   ARGUMENTS and their descriptions:

        INTEGER         NCOLS   ! column dimension for this variable
        INTEGER         NROWS   ! row    dimension for this variable
        INTEGER         NLAYS   ! layer  dimension for this variable
        INTEGER         RUNLEN  ! run-length dimension
        INTEGER         NMASKS  ! mask dimension
        INTEGER         JDATE   ! current model date
        INTEGER         JTIME   ! current model time
        CHARACTER*16    INAME   !  logical name of the  input file
        CHARACTER*16    ANAME   !  logical name of the AGG output file
        CHARACTER*16    MNAME   !  logical name of the MAX output file
        CHARACTER*16    VNAMEI  !  vble name (input)
        CHARACTER*16    VNAMEO  !  vble name (output)
        CHARACTER*16    MASKF( NMASKS )  !  mask-file names
        CHARACTER*16    MASKV( NMASKS )  !  mask-vble names
        INTEGER         T       ! time step within the aggregation
        INTEGER         TYPE    ! type of aggregation to perform
        INTEGER         NSTEPS  ! number of time steps - used for average only
        INTEGER         LOGDEV  ! unit number for output
        REAL            CMAX( RUNLEN, NMASKS )    ! max( grid ) and its
        INTEGER         CCOL( RUNLEN, NMASKS )    ! (c,r,l) location...
        INTEGER         CROW( RUNLEN, NMASKS )
        INTEGER         CLAY( RUNLEN, NMASKS )


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         J, K, C, R, L, M, JTOP
        REAL            X, DENOM
        CHARACTER*16    VNAME

        REAL,    ALLOCATABLE, SAVE::    GRID( :, :, : )
        REAL,    ALLOCATABLE, SAVE::    AMAX( :, :, :, : )
        REAL,    ALLOCATABLE, SAVE::    ABAR( :, :, : )
        REAL,    ALLOCATABLE, SAVE::    AGRD( :, :, : )
        INTEGER, ALLOCATABLE, SAVE::    EXC1( :, :, : )
        INTEGER, ALLOCATABLE, SAVE::    EXC8( :, :, : )
        INTEGER, ALLOCATABLE, SAVE::    MASK( :, :, : )
        LOGICAL, SAVE::              FIRSTIME = .TRUE.

        CHARACTER*256   MESG

C***********************************************************************
C   begin body of subroutine  TAGGREG

C***********************************************************************
C   begin body of entry point INITAGG

        ENTRY INITAGG( NCOLS, NROWS, NLAYS, T, JDATE, JTIME,
     &                 INAME, VNAMEI, NMASKS, MASKF, MASKV, LOGDEV )


        IF ( FIRSTIME ) THEN
            ALLOCATE( GRID( NCOLS, NROWS, NLAYS ),
     &                AMAX( NCOLS, NROWS, NLAYS, 6 ),
     &                ABAR( NCOLS, NROWS, NLAYS ),
     &                AGRD( NCOLS, NROWS, NLAYS ),
     &                EXC1( NCOLS, NROWS, NLAYS ),
     &                EXC8( NCOLS, NROWS, NLAYS ),
     &                MASK( NCOLS, NROWS, NMASKS ),
     &                STAT=K )
            IF ( K .NE. 0 ) THEN
                 CALL M3EXIT( 'TAGGREG', JDATE, JTIME,
     &                        'Memory allocation error', 2 )
            END IF
            FIRSTIME = .FALSE.

            DO  M = 1, NMASKS
                IF ( .NOT. READ3( MASKF(M), MASKV(M), ALLAYS3,
     &                            JDATE, JTIME, MASK(1,1,M) ) ) THEN
                    MESG = 'Read failure:  file "' //
     &                     TRIM( MASKF(M) ) //
     &                     '" variable "' // MASKV(M) // '"'
                    CALL M3EXIT ( 'M3AGMASK:AGGREG', JDATE, JTIME,
     &                            MESG, 2 )
                END IF              !  if read3() worked, or not
            END DO

        END IF          ! if firstime

        IF ( .NOT. READ3( INAME, VNAMEI, ALLAYS3,
     &                    JDATE, JTIME, GRID ) ) THEN
            MESG = 'Read failure:  file "' // TRIM( INAME ) //
     &            '" variable "' // TRIM( VNAMEI ) // '"'
            CALL M3EXIT ( 'M3AGMASK:AGGREG', JDATE, JTIME, MESG, 2 )
        END IF              !  if read3() worked, or not

        IF ( T .EQ. 1 ) THEN
            IF ( NLAYS .EQ. 1 ) THEN        !!  for parallelization
!$OMP           PARALLEL DO
!$OMP&                  DEFAULT( NONE ),
!$OMP&                  SHARED( NCOLS, NROWS, GRID, EXC1, ABAR ),
!$OMP&                  PRIVATE( C, R, X )
                DO  R = 1, NROWS
                DO  C = 1, NCOLS
                    X = GRID( C,R,1 )
                    IF ( X .GT. 0.125 ) THEN
                        EXC1( C,R,1 ) = 1
                    ELSE
                        EXC1( C,R,1 ) = 0
                    END IF
                    EXC8( C,R,1 ) = 0
                    ABAR( C,R,1 ) = 0
                END DO
                END DO
            ELSE
!$OMP           PARALLEL DO
!$OMP&                  DEFAULT( NONE ),
!$OMP&                  SHARED( NCOLS, NROWS, NLAYS, GRID, EXC1, ABAR ),
!$OMP&                  PRIVATE( C, R, L, X )
                DO  L = 1, NLAYS
                DO  R = 1, NROWS
                DO  C = 1, NCOLS
                    X = GRID( C,R,L )
                    IF ( X .GT. 0.125 ) THEN
                        EXC1( C,R,L ) = 1
                    ELSE
                        EXC1( C,R,L ) = 0
                    END IF
                    EXC8( C,R,L ) = 0
                    ABAR( C,R,L ) = 0
                END DO
                END DO
                END DO
            END IF
        END IF          !  if first time step for this variable

        RETURN


C***********************************************************************
C   begin body of entry point AGGREG

        ENTRY AGGREG( NCOLS, NROWS, NLAYS, JDATE, JTIME,
     &                INAME, VNAMEI, TYPE , LOGDEV   )

        IF ( .NOT. READ3( INAME, VNAMEI, ALLAYS3,
     &                        JDATE, JTIME, AGRD ) ) THEN
            MESG = 'Read failure:  file "' // TRIM( INAME ) //
     &            '" variable "' // TRIM( VNAMEI ) // '"'
            CALL M3EXIT ( 'M3AGMASK:AGGREG', JDATE, JTIME, MESG, 2 )
        ELSE IF( TYPE .EQ. M3SUM .OR. TYPE .EQ. M3AVE ) THEN

            IF ( NLAYS .EQ. 1 ) THEN        !!  for parallelization
!$OMP           PARALLEL DO
!$OMP&                  DEFAULT( NONE ),
!$OMP&                  SHARED( NCOLS, NROWS, AGRD, GRID, EXC1 ),
!$OMP&                  PRIVATE( C, R, X )
                DO  R = 1, NROWS
                DO  C = 1, NCOLS
                    X = AGRD( C,R,1 )
                    GRID( C,R,1 ) = GRID( C,R,1 ) + X
                    IF ( X .GT. 0.125 ) THEN
                        EXC1( C,R,1 ) = EXC1( C,R,1 ) + 1
                    END IF
                END DO
                END DO
            ELSE
!$OMP           PARALLEL DO
!$OMP&                  DEFAULT( NONE ),
!$OMP&                  SHARED( NCOLS, NROWS, NLAYS, AGRD, GRID, EXC1 ),
!$OMP&                  PRIVATE( C, R, L, X )
                DO  L = 1, NLAYS
                DO  R = 1, NROWS
                DO  C = 1, NCOLS
                    X = AGRD( C,R,L )
                    GRID( C,R,L ) = GRID( C,R,L ) + X
                    IF ( X .GT. 0.125 ) THEN
                        EXC1( C,R,L ) = EXC1( C,R,L ) + 1
                    END IF
                END DO
                END DO
                END DO
            END IF

        ELSE IF( TYPE .EQ. M3MAX ) THEN

            IF ( NLAYS .EQ. 1 ) THEN        !!  for parallelization
!$OMP           PARALLEL DO
!$OMP&                  DEFAULT( NONE ),
!$OMP&                  SHARED( NCOLS, NROWS, AGRD, GRID, EXC1 ),
!$OMP&                  PRIVATE( C, R, X )
                DO  R = 1, NROWS
                DO  C = 1, NCOLS
                    X = AGRD( C,R,L )
                    GRID( C,R,L ) = MAX( GRID( C,R,L ), X )
                    IF ( X .GT. 0.125 ) THEN
                        EXC1( C,R,L ) = EXC1( C,R,L ) + 1
                    END IF
                END DO
                END DO
            ELSE
!$OMP           PARALLEL DO
!$OMP&                  DEFAULT( NONE ),
!$OMP&                  SHARED( NCOLS, NROWS, NLAYS, AGRD, GRID, EXC1 ),
!$OMP&                  PRIVATE( C, R, L, X )
                DO  L = 1, NLAYS
                DO  R = 1, NROWS
                DO  C = 1, NCOLS
                    X = AGRD( C,R,L )
                    GRID( C,R,L ) = MAX( GRID( C,R,L ), X )
                    IF ( X .GT. 0.125 ) THEN
                        EXC1( C,R,L ) = EXC1( C,R,L ) + 1
                    END IF
                END DO
                END DO
                END DO
            END IF

        ELSE

            WRITE( MESG, '( A, I8, 2X, A)' )
     &          'Aggregation type ', TYPE, 'not supported'
            CALL M3EXIT ( 'M3AGMASK:AGGREG', JDATE, JTIME, MESG, 2 )

        END IF              !  if read3() worked, or not

        RETURN


C***********************************************************************
C   begin body of entry point OUTAGG

        ENTRY OUTAGG( NCOLS, NROWS,  NLAYS, RUNLEN, NMASKS,
     &                T, JDATE, JTIME, ANAME, VNAMEO, TYPE , NSTEPS,
     &                CMAX, CCOL, CROW, CLAY, LOGDEV )

        JTOP = MIN( 6, T-1 )    !  number of AMAX(:,:,:,*) already set

        IF( TYPE .EQ. M3AVE ) THEN

            DENOM = 1.0 / REAL( NSTEPS )
            DO M = 1, NMASKS
                CMAX( T,M ) = -9.999E37
                CCOL( T,M ) = 0
                CROW( T,M ) = 0
                CLAY( T,M ) = 0
            END DO

            IF ( NLAYS .EQ. 1 ) THEN        !!  for parallelization

!$OMP           PARALLEL DO
!$OMP&                 DEFAULT( NONE ),
!$OMP&                 SHARED( NCOLS, NROWS, NMASKS, JTOP, DENOM,
!$OMP&                         T, GRID, ABAR, MASK, CMAX, CCOL, CROW,
!$OMP&                         CLAY, EXC8, AMAX ),
!$OMP&                PRIVATE( C, R, M, J, X )

                DO  R = 1, NROWS
                DO  C = 1, NCOLS

                    X = DENOM * GRID( C,R,L )
                    GRID( C,R,L ) = X
                    ABAR( C,R,L ) = ABAR( C,R,L ) + X

                    DO M = 1, NMASKS
                        IF ( MASK( C,R,M ) .GT. 0 ) THEN
                            IF ( X .GT. CMAX( T,M ) ) THEN
                                CMAX( T,M ) = X
                                CCOL( T,M ) = C
                                CROW( T,M ) = R
                                CLAY( T,M ) = L
                            END IF
                        END IF
                    END DO

                    IF ( X .GT. 0.085 ) THEN
                        EXC8( C,R,L ) = EXC8( C,R,L ) + 1
                    END IF

                    DO  J = 1, JTOP
                        IF ( X .GT. AMAX( C,R,L,J ) ) THEN
                            DO  K = JTOP, J+1, -1
                                AMAX( C,R,L,K ) =  AMAX( C,R,L,K-1 )
                            END DO
                            AMAX( C,R,L,J ) = X
                            GO TO  11
                        END IF
                    END DO
                    IF ( JTOP .LT. 6 ) THEN
                        AMAX( C,R,L,JTOP+1 ) = X
                    END IF          !  if jtop<6
11                  CONTINUE

                END DO
                END DO

            ELSE

!$OMP           PARALLEL DO
!$OMP&                 DEFAULT( NONE ),
!$OMP&                 SHARED( NCOLS, NROWS, NLAYS, NMASKS, JTOP, DENOM,
!$OMP&                         T, GRID, ABAR, MASK, CMAX, CCOL, CROW,
!$OMP&                         CLAY, EXC8, AMAX ),
!$OMP&                PRIVATE( C, R, L, M, J, X )

                DO  L = 1, NLAYS
                DO  R = 1, NROWS
                DO  C = 1, NCOLS

                    X = DENOM * GRID( C,R,L )
                    GRID( C,R,L ) = X
                    ABAR( C,R,L ) = ABAR( C,R,L ) + X

                    DO M = 1, NMASKS
                        IF ( MASK( C,R,M ) .GT. 0 ) THEN
                            IF ( X .GT. CMAX( T,M ) ) THEN
                                CMAX( T,M ) = X
                                CCOL( T,M ) = C
                                CROW( T,M ) = R
                                CLAY( T,M ) = L
                            END IF
                        END IF
                    END DO

                    IF ( X .GT. 0.085 ) THEN
                        EXC8( C,R,L ) = EXC8( C,R,L ) + 1
                    END IF

                    DO  J = 1, JTOP
                        IF ( X .GT. AMAX( C,R,L,J ) ) THEN
                            DO  K = JTOP, J+1, -1
                                AMAX( C,R,L,K ) =  AMAX( C,R,L,K-1 )
                            END DO
                            AMAX( C,R,L,J ) = X
                            GO TO  12
                        END IF
                    END DO
                    IF ( JTOP .LT. 6 ) THEN
                        AMAX( C,R,L,JTOP+1 ) = X
                    END IF          !  if jtop<6
12                  CONTINUE

                END DO
                END DO
                END DO

            END IF

        ELSE    !  not average:  sum or max

            DO M = 1, NMASKS
                CMAX( T,M ) = -9.99E-37
                CCOL( T,M ) = 0
                CROW( T,M ) = 0
                CLAY( T,M ) = 0
            END DO

            IF ( NLAYS .EQ. 1 ) THEN        !!  for parallelization

!$OMP           PARALLEL DO
!$OMP&                 DEFAULT( NONE ),
!$OMP&                 SHARED( NCOLS, NROWS, NMASKS, JTOP, DENOM,
!$OMP&                         T, GRID, ABAR, MASK, CMAX, CCOL, CROW,
!$OMP&                         CLAY, EXC8, AMAX ),
!$OMP&                PRIVATE( C, R, M, J, X )

                DO  R = 1, NROWS
                DO  C = 1, NCOLS

                    X = GRID( C,R,L )
                    ABAR( C,R,L ) = ABAR( C,R,L ) + X

                    DO M = 1, NMASKS
                        IF ( MASK( C,R,M ) .GT. 0 ) THEN
                            IF ( X .GT. CMAX( T,M ) ) THEN
                                CMAX( T,M ) = X
                                CCOL( T,M ) = C
                                CROW( T,M ) = R
                                CLAY( T,M ) = L
                            END IF
                        END IF
                    END DO

                    IF ( X .GT. 0.085 ) THEN
                        EXC8( C,R,L ) = EXC8( C,R,L ) + 1
                    END IF

                    DO  J = 1, JTOP
                        IF ( X .GT. AMAX( C,R,L,J ) ) THEN
                            DO  K = JTOP, J+1, -1
                                AMAX( C,R,L,K ) =  AMAX( C,R,L,K-1 )
                            END DO
                            AMAX( C,R,L,J ) = X
                            GO TO  21
                        END IF
                    END DO
                    IF ( JTOP .LT. 6 ) THEN
                        AMAX( C,R,L,JTOP+1 ) = X
                    END IF          !  if jtop<6
21                  CONTINUE

                END DO
                END DO

            ELSE

!$OMP           PARALLEL DO
!$OMP&                 DEFAULT( NONE ),
!$OMP&                 SHARED( NCOLS, NROWS, NLAYS, NMASKS, JTOP, DENOM,
!$OMP&                         T, GRID, ABAR, MASK, CMAX, CCOL, CROW,
!$OMP&                         CLAY, EXC8, AMAX ),
!$OMP&                PRIVATE( C, R, L, M, J, X )

                DO  L = 1, NLAYS
                DO  R = 1, NROWS
                DO  C = 1, NCOLS

                    X = GRID( C,R,L )
                    ABAR( C,R,L ) = ABAR( C,R,L ) + X

                    DO M = 1, NMASKS
                        IF ( MASK( C,R,M ) .GT. 0 ) THEN
                            IF ( X .GT. CMAX( T,M ) ) THEN
                                CMAX( T,M ) = X
                                CCOL( T,M ) = C
                                CROW( T,M ) = R
                                CLAY( T,M ) = L
                            END IF
                        END IF
                    END DO

                    IF ( X .GT. 0.085 ) THEN
                        EXC8( C,R,L ) = EXC8( C,R,L ) + 1
                    END IF

                    DO  J = 1, JTOP
                        IF ( X .GT. AMAX( C,R,L,J ) ) THEN
                            DO  K = JTOP, J+1, -1
                                AMAX( C,R,L,K ) =  AMAX( C,R,L,K-1 )
                            END DO
                            AMAX( C,R,L,J ) = X
                            GO TO  22
                        END IF
                    END DO
                    IF ( JTOP .LT. 6 ) THEN
                        AMAX( C,R,L,JTOP+1 ) = X
                    END IF          !  if jtop<6
22                  CONTINUE

                END DO
                END DO
                END DO

            END IF

        END IF          !  if averaging; else (sum or max)

        IF ( .NOT. WRITE3( ANAME, VNAMEO,
     &                     JDATE, JTIME, GRID ) ) THEN
            MESG = 'Write failure:  file "' // TRIM( ANAME ) //
     &                    '" variable "' // TRIM( VNAMEO ) // '"'
            CALL M3EXIT ( 'M3AGMASK:AGGREG', JDATE, JTIME, MESG, 2 )
        END IF

        RETURN

C***********************************************************************
C   begin body of entry point MAXAGG

        ENTRY MAXAGG( JDATE, JTIME, MNAME, RUNLEN, VNAMEO )

        DO  J = 1, 6

            VNAME = TRIM( VNAMEO ) // SUFFIX( J )

            GRID = AMAX( :, :, :, J )

            IF ( .NOT. WRITE3( MNAME, VNAME,
     &                         JDATE, JTIME, GRID ) ) THEN
                MESG = 'Write failure:  file "' // TRIM( MNAME ) //
     &                        '" variable "' // TRIM( VNAME ) // '"'
                CALL M3EXIT ( 'M3AGMASK:AGGREG', JDATE, JTIME, MESG, 2 )
            END IF

        END DO

        X     = 1.0 / FLOAT( RUNLEN )
        ABAR  = X * ABAR
        VNAME = TRIM( VNAMEO ) // '_BAR'

        IF ( .NOT. WRITE3( MNAME, VNAME,
     &                     JDATE, JTIME, ABAR ) ) THEN
            MESG = 'Write failure:  file "' // TRIM( MNAME ) //
     &                    '" variable "' // TRIM( VNAME ) // '"'
            CALL M3EXIT ( 'M3AGMASK:AGGREG', JDATE, JTIME, MESG, 2 )
        END IF

        VNAME = TRIM( VNAMEO ) // '_1_125'

        IF ( .NOT. WRITE3( MNAME, VNAME,
     &                     JDATE, JTIME, EXC1 ) ) THEN
            MESG = 'Write failure:  file "' // TRIM( MNAME ) //
     &                    '" variable "' // TRIM( VNAME ) // '"'
            CALL M3EXIT ( 'M3AGMASK:AGGREG', JDATE, JTIME, MESG, 2 )
        END IF

        VNAME = TRIM( VNAMEO ) // '_8_85'

        IF ( .NOT. WRITE3( MNAME, VNAME,
     &                     JDATE, JTIME, EXC8 ) ) THEN
            MESG = 'Write failure:  file "' // TRIM( MNAME ) //
     &                    '" variable "' // TRIM( VNAME ) // '"'
            CALL M3EXIT ( 'M3AGMASK:AGGREG', JDATE, JTIME, MESG, 2 )
        END IF

        END SUBROUTINE  MAXREG

