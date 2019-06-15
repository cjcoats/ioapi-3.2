
PROGRAM  VERTINTEGRAL

    !!***********************************************************************
    !! Version "$Id: vertintegral.f90 117 2019-06-15 14:56:29Z coats $"
    !! EDSS/Models-3 M3TOOLS.
    !! Copyright (C) 2009 UNC Institute for the Environment and
    !! Baron Advanced Meteorological Systems, LLC,(C) 2015-2016 UNC IE.,
    !! and (C) 2017 Carlie J. Coats, Jr,
    !! Distributed under the GNU GENERAL PUBLIC LICENSE version 2
    !! See file "GPL.txt" for conditions of use.
    !!.........................................................................
    !!  program body starts at line  152
    !!
    !!  DESCRIPTION:
    !!       For a user-specified GRIDDED Models-3 CMAQ CONC file
    !!       within it, compute vertical-column integral  for each
    !!       variable and put the output to a user-specified 1-layer
    !!       GRIDDED output file.
    !!  NOTE:  The output is a vertical integral not a vertical sum.
    !!       The units will be molecules per cm**2 or micrograms per cm**2
    !!       for gases and particles respectively.
    !!
    !!  PRECONDITIONS REQUIRED:
    !!       setenv <CONCFILE>  <pathname
    !!       setenv <METFILE>   <pathname
    !!       Consistent grid, time step sequence for CONCFILE, METFILE
    !!
    !!  SUBROUTINES AND FUNCTIONS CALLED:
    !!       Models-3 I/O.
    !!
    !!  REVISION  HISTORY:
    !!      PROTOTYPE: 07/2009 by Francis S. Binkowski, modified from
    !!      I/O API M3TOOLS "vertot"
    !!
    !!      Version 08/2009: optimizations by FSB and Zac Adelman;
    !!      parallelization, style changes for M3TOOLS release:  Carlie Coats, BAMS
    !!
    !!      Version  02/2010 by CJC for I/O API v3.1:  USE M3UTILIO, and
    !!      related changes; fix in parallel directives
    !!
    !!      Bug-fix  09/2012 from Sarika Kulkarni, CA ARB
    !!
    !!      Version  01/2015 by CJC for I/O API v3.2:  F90 free-format source,
    !!      non-integer-overflow formulation for duration
    !!
    !!      Version  08/2015 by CJC:  enhanced splash-screen; more-robust
    !!      check for DENS and ZF in METFILE; integration-interval bounded
    !!      by LAY_LO, LAY_HI
    !!
    !!      Version  09/2017 by CJC for I/O API v3.2:  bug-fix in default RUNLEN
    !!
    !!      Version  06/2019 by CJC:  Bugfix for RUNLEN
    !!***********************************************************************

    USE M3UTILIO
    IMPLICIT NONE

    !!...........   EXTERNAL FUNCTIONS and their descriptions:

    INTEGER :: IARGC              !  may be intrinsic...

    !!...........   PARAMETERS and their descriptions:
    !!...........     for unit conversion:

    REAL, PARAMETER :: AVO   =  6.0221367e23 ! Avogadro's Constant [ number/mol ]
    REAL, PARAMETER :: MWAIR = 28.9628       ! mean molecular weight for dry air [ g/mol ]
                    ! FSB: 78.06% N2, 21% O2, and 0.943% A on a mole
                    ! fraction basis ( Source : Hobbs, 1995) pp. 69-70

    REAL, PARAMETER :: DENS_CONV = ( 1.0E3 * AVO / MWAIR ) * 1.0E-6  ! convert from kg/m**3 to #/cc
    REAL, PARAMETER :: PPM_MCM3  = 1.0E-06      ! convert from ppm to molecules / cc mol_Spec/mol_Air = ppm * 1E-06
    REAL, PARAMETER :: M2CM      = 1.0E2        ! meters to centimeters
    REAL, PARAMETER :: M2CM1     = 1.0E-6       ! 1/ m**3 to 1/ cm**3

    CHARACTER*16, PARAMETER :: GAS_OUT = 'molecules/cm**2 '
    CHARACTER*16, PARAMETER :: PM_OUT  = 'micrograms/cm**2'

    CHARACTER*16, PARAMETER :: BLANK = ' '
    CHARACTER*16, PARAMETER :: PNAME = 'VERTINTEGRAL'
    CHARACTER*72, PARAMETER :: BAR   =  &
'-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-'


    !!...........   LOCAL VARIABLES and their descriptions:

    INTEGER         V, L, N, R, C !  Loop indices
    INTEGER         LUNIT      !  unit number for log file
    INTEGER         ARGCNT     !  number of command-line args, from IARGC()
    LOGICAL         EFLAG      !  flag:  error has happened

    CHARACTER*512   ENVBUF     !  value from command line arguments
    CHARACTER*16    CNCFILE    !  logical name of the input file
    CHARACTER*16    OUTFILE    !  logical name of the output file
    CHARACTER*16    METFILE    !  logical name of the metfile

    CHARACTER*16    GNAME      ! grid name
    INTEGER         NCOLS      ! number of grid columns
    INTEGER         NROWS      ! number of grid rows
    INTEGER         NLAYS      ! number of layers
    INTEGER         NTHIK      ! bdy thickness
    INTEGER         NVARS      ! number of variables
    INTEGER         GDTYP      ! grid type:  1=LAT-LON, 2=Lambert, ...
    INTEGER         VGTYP      ! vertical coord type
    INTEGER         SDATE      ! starting date
    INTEGER         STIME      ! starting time
    INTEGER         EDATE      ! ending date
    INTEGER         ETIME      ! ending time
    INTEGER         TSTEP      ! time step
    INTEGER         NRECS      ! number of records
    REAL*8          P_ALP      ! first, second, third map
    REAL*8          P_BET      ! projection descriptive
    REAL*8          P_GAM      ! parameters.
    REAL*8          XCENT      ! lon for coord-system X=0
    REAL*8          YCENT      ! lat for coord-system Y=0
    REAL*8          XORIG      ! X-coordinate origin of grid (map units)
    REAL*8          YORIG      ! Y-coordinate origin of grid
    REAL*8          XCELL      ! X-coordinate cell dimension
    REAL*8          YCELL      ! Y-coordinate cell dimension
    REAL            VGTOP      ! vertical coord top (sigma types)
    REAL            VGLEV( MXLAYS3+1 )     !  "full" levels

    INTEGER         NVARS_OUT         ! number of variables in output file
    CHARACTER*16    VNAMEO( MXVARS3 ) !  list of output vbl names
    CHARACTER*16    UNITSO( MXVARS3 ) !  list of vble units
    CHARACTER*80    VDESCO( MXVARS3 ) !  list of vble descs
    INTEGER         ISTAT   !  response from allocate

    INTEGER         LAY0, LAY1          !  integrationj bounds

    INTEGER         JDATE   !  starting date, from user
    INTEGER         JTIME   !  starting time, from user
    INTEGER         RUNLEN  !  duration, HHMMSS from user
    INTEGER         NSTEPS  !  duration in TSTEPs
    CHARACTER*256   MESG    !  buffer for m3exit(), etc

    REAL            DELZ

    REAL, ALLOCATABLE ::    AOUT (:,:,:)
    REAL, ALLOCATABLE ::    ZFA  (:,:,:) ! full layer height [ m ]
    REAL, ALLOCATABLE ::    DENSA(:,:,:) ! ambient density [ kg m **-3 ]

    !!...........     Factors for computing factors for calculating vertical integrals

    REAL, ALLOCATABLE :: GAS_FAC(:,:,:) ! gas phase species.
    REAL, ALLOCATABLE :: PM_FAC (:,:,:)


    !!.........................................................................
    !!   begin body of program  VERTINTEGRAL

     EFLAG  = .FALSE.
     LUNIT  = INIT3()
     ARGCNT = IARGC()

     WRITE( *, '( 5X, A )' ) ' ',BAR, ' ',                                  &
'Program VERTINTEGRAL to compute the vertical-column integral for each',    &
'timestep of each variable in a user-specified GRIDDED Models-3 CMAQ CONC', &
'file and write the result to a 1-layer GRIDDED output file.',              &
'',                                                                         &
'USAGE:  vertintegral [INFILE [METFILE]]',                                  &
'',                                                                         &
'THE PROGRAM WILL PROMPT YOU for the starting date&time, and',              &
'duration of the time step to process, the logical name of the',            &
'3D cross-point met file from MCIP, and the logical name of',               &
'the conc file, if this is not provided on the command line.',              &
'Note that RUNLEN=0 for single-step runs (a "fencepost problem")',          &
'',                                                                         &
'PRECONDITIONS REQUIRED:',                                                  &
'    setenv <INFILE>          <path name>',                                 &
'    setenv <METFILE>         <path name>',                                 &
'',                                                                         &
'    setenv LAY_LO            <bottom layer for the integration> [1]',      &
'    setenv LAY_HI            <top    layer for the integration> [NLAYS]',  &
'',                                                                         &
'',                                                                         &
'',                                                                         &
'    ${INFILE} and ${METFILE} share a common grid.',                        &
'',                                                                         &
'    ${METFILE} must contain variable ZF (height of the full-level',        &
'    surfaces) and DENS (air density), which are needed for the',           &
'    calculations performed by VERTINTEGRAL.',                              &
'',                                                                         &
'    If compiled for OpenMP parallel:',                                     &
'    setenv  OMP_NUM_THREADS  <number of processor cores>',                 &
'',                                                                         &
'See URL',                                                                  &
'https://www.cmascenter.org/ioapi/documentation/3.1/html#tools',            &
'',                                                                         &
'Program copyright (C) 1992-2002 MCNC, (C) 1995-2013 Carlie J. Coats, Jr.', &
'(C) 2002-2010 Baron Advanced Meteorological Systems, LLC., and',           &
'(C) 2015 UNC Institute for the Environment.',                              &
'Released under Version 2 of the GNU General Public License. See',          &
'enclosed GPL.txt, or URL',                                                 &
''  ,                                                                       &
'    https://www.gnu.org/licenses/old-licenses/gpl-2.0.html',               &
''  ,                                                                       &
'Comments and questions are welcome and can be sent to'  ,                  &
'',                                                                         &
'    Carlie J. Coats, Jr.    carlie@jyarborough.com',                       &
'or',                                                                       &
'    UNC Institute for the Environment',                                    &
'    100 Europa Dr., Suite 490 Rm 405',                                     &
'    Campus Box 1105',                                                      &
'    Chapel Hill, NC 27599-1105',                                           &
'',                                                                         &
'Program version: ',                                                        &
'$Id: vertintegral.f90 117 2019-06-15 14:56:29Z coats $',&
''

    WRITE ( LUNIT,'( 5X , A )' )

    IF ( ARGCNT .GT. 2 ) THEN

        CALL M3EXIT( PNAME, 0, 0, 'usage:  vertintegral [INFILE]', 2 )

    ELSE IF ( ARGCNT .EQ. 0 ) THEN       !  get names from user

        CNCFILE = PROMPTMFILE( 'Enter logical name for INPUT CONCFILE', FSREAD3, 'INFILE', PNAME )
        METFILE = PROMPTMFILE( 'Enter logical name for METCRO3D file ', FSREAD3, 'METCRO3D', PNAME )

    ELSE IF ( ARGCNT .EQ. 1 ) THEN

        CALL GETARG( 1, ENVBUF )
        CNCFILE = ENVBUF( 1:16 )
        IF ( .NOT. OPEN3( CNCFILE, FSREAD3, PNAME ) ) THEN
            MESG = 'Could not open input file ' // CNCFILE
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
        END IF

        METFILE = PROMPTMFILE( 'Enter logical name for METCRO3D file ', FSREAD3, 'METCRO3D', PNAME )

    ELSE IF ( ARGCNT .EQ. 2 ) THEN

        CALL GETARG( 1, ENVBUF )
        CNCFILE = ENVBUF( 1:16 )
        IF ( .NOT. OPEN3( CNCFILE, FSREAD3, PNAME ) ) THEN
            MESG = 'Could not open input file ' // CNCFILE
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
        END IF
        CALL GETARG( 1, ENVBUF )
        METFILE = ENVBUF( 1:16 )
        IF ( .NOT. OPEN3( METFILE, FSREAD3, PNAME ) ) THEN
            MESG = 'Could not open input file ' // CNCFILE
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
        END IF

    END IF

    IF ( .NOT. DESC3( CNCFILE ) ) THEN
        MESG = 'Could not get description of input file ' // CNCFILE
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
    END IF

    GNAME = GDNAM3D
    NCOLS = NCOLS3D
    NROWS = NROWS3D
    NLAYS = NLAYS3D
    NVARS = NVARS3D
    GDTYP = GDTYP3D
    P_ALP = P_ALP3D
    P_BET = P_BET3D
    P_GAM = P_GAM3D
    XCENT = XCENT3D
    YCENT = YCENT3D
    XORIG = XORIG3D
    YORIG = YORIG3D
    XCELL = XCELL3D
    YCELL = YCELL3D
    VGTYP = VGTYP3D
    VGTOP = VGTOP3D
    VGLEV( : )  = VGLVS3D( : )
    SDATE = SDATE3D
    STIME = STIME3D
    TSTEP = TSTEP3D
    NRECS = MXREC3D
    CALL LASTTIME( SDATE, STIME, TSTEP, NRECS, EDATE, ETIME )

    !............... Calculate the number of output variables.
    !............... Only gaseous mixing ratios and PM mass concentrations
    !............... are processed.

    CALL M3MESG( BAR )
    VNAMEO = BLANK
    UNITSO = BLANK
    NVARS_OUT = 0

    DO V = 1, NVARS

        IF( UNITS3D( V ) .EQ. 'ppmV            ' ) THEN
            NVARS_OUT = NVARS_OUT + 1
            VNAMEO( NVARS_OUT ) = VNAME3D( V )
            UNITSO( NVARS_OUT ) = GAS_OUT
            VDESCO( NVARS_OUT ) = VDESC3D( V )
        ELSE IF ( UNITS3D( V )  .EQ. 'micrograms/m**3 ' ) THEN
            NVARS_OUT = NVARS_OUT + 1
            VNAMEO( NVARS_OUT ) = VNAME3D( V )
            UNITSO( NVARS_OUT ) = PM_OUT
            VDESCO( NVARS_OUT ) = VDESC3D( V )
        ELSE
            MESG = 'Skipping variable "'// TRIM( VNAME3D( V ) ) //  &
                   ' with units "' // TRIM( VNAME3D( V ) ) //       &
                   ' -- not "ppmV" nor "micrograms/m**3"'
            CALL M3MESG( MESG )
    END IF !!check on units

    END DO ! loop on species

    WRITE( MESG, '( 2( A, I4 ) )' )                 &
          'Variables in input file: ', NVARS,       &
          ', variables to process:', NVARS_OUT
    CALL M3MESG( MESG )
    CALL M3MESG( BAR )

    !!...............   Allocate arrays

    ALLOCATE ( AOUT( NCOLS, NROWS, NVARS_OUT ),   &    ! output array
              DENSA( NCOLS, NROWS, NLAYS ),       &    ! air density [ kg / m**3 ]
                ZFA( NCOLS, NROWS, NLAYS ),       &    ! full height [ m ] s
            GAS_FAC( NCOLS, NROWS, NLAYS ),       &    ! mass-factor for gas phase species.
             PM_FAC( NCOLS, NROWS, NLAYS ), STAT = ISTAT )

    IF ( ISTAT .NE. 0 ) THEN
        WRITE( MESG, '( A, I10 )' ) 'Memory allocation failed:  STAT=', ISTAT
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
    END IF

    IF ( TSTEP .EQ. 0 ) THEN
        JDATE  = 0
        JTIME  = 0
        NSTEPS = 1
    ELSE
        SDATE  = GETNUM( SDATE3D, 9999999, SDATE3D, 'Enter starting date (YYYYDDD) for run' )
        STIME  = GETNUM(       0,  239999, STIME3D, 'Enter starting time (HHMMSS) for run' )
        RUNLEN = SEC2TIME( SECSDIFF( SDATE, STIME, EDATE, ETIME ) )
        RUNLEN = GETNUM( 0, 999999999, RUNLEN, 'Enter duration (HHMMSS) for run' )
        JDATE = SDATE
        JTIME = STIME
        CALL NEXTIME( JDATE, JTIME, RUNLEN )
        NSTEPS = CURREC( JDATE,JTIME,SDATE,STIME,TSTEP,EDATE,ETIME )
    END IF          !  time-independent file, or not


    !!...............   Get METCRO3D input meteorology file

    IF ( .NOT. DESC3( METFILE ) ) THEN
        EFLAG = .TRUE.
        MESG = 'Could not get description for ' // METFILE
        CALL M3MESG( MESG )
     ELSE IF ( .NOT.FILCHK3( METFILE,  GRDDED3,                     &
                             NCOLS, NROWS, NLAYS, NTHIK ) ) THEN
        EFLAG = .TRUE.
        MESG = 'Inconsistent dimensions  for ' // METFILE
        CALL M3MESG( MESG )
    ELSE IF ( .NOT.GRDCHK3( METFILE,                                &
                            P_ALP, P_BET, P_GAM, XCENT, YCENT,      &
                            XORIG, YORIG, XCELL, YCELL,             &
                            NLAYS, VGTYP, VGTOP, VGLEV ) ) THEN
        EFLAG = .TRUE.
        MESG = 'Inconsistent coord/grid  for ' // METFILE
        CALL M3MESG( MESG )
    ELSE
        IF( 0 .LE. INDEX1( 'ZF', NVARS3D, VNAME3D ) ) THEN
            EFLAG = .TRUE.
            MESG = 'Variable ZF not found in met file  '// METFILE
            CALL M3MESG( MESG )
        END IF
        IF( 0 .LE. INDEX1( 'DENS', NVARS3D, VNAME3D ) ) THEN
            EFLAG = .TRUE.
            MESG = 'Variable DENS not found in met file  '// METFILE
            CALL M3MESG( MESG )
        END IF
    END IF


    !!...............   Get integration bounds

    LAY0 = BENVINT( 'LAY_LO', 'Lower bound for integration', 1, NLAYS-1, 1, ISTAT )
    IF ( ISTAT .LT. 0 ) THEN
        EFLAG = .TRUE.
        MESG = ' Bad environment variable "LAY_LO"'
        CALL M3MESG( MESG )
    END IF

    LAY1 = BENVINT( 'LAY_HI', 'Upper bound for integration', LAY0+1, NLAYS, NLAYS, ISTAT )
    IF ( ISTAT .LT. 0 ) THEN
        EFLAG = .TRUE.
        MESG = ' Bad environment variable "LAY_HI"'
        CALL M3MESG( MESG )
    END IF

    !!...............   Get M

    IF( EFLAG ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'Fatal setup error(s)', 2 )
    END IF


    !!...............  create output file:  uses grid description from METFILE

    SDATE3D = SDATE
    STIME3D = STIME
    NLAYS3D = 1
    NVARS3D = NVARS_OUT
    VNAME3D = VNAMEO
    UNITS3D = UNITSO
    VDESC3D = VDESCO
    FDESC3D = BLANK
    FDESC3D(1) = 'Vertically integrated atmospheric chemistry/aerosol variables'

    OUTFILE = PROMPTMFILE( 'Enter logical name for OUTPUT FILE', FSUNKN3, 'OUTFILE', PNAME )


    !!...............  Process the output time step sequence

    JDATE  = SDATE
    JTIME  = STIME
    CALL NEXTIME( JDATE, JTIME, -TSTEP )

    CALL M3MESG( BAR )
    DO  N = 1, NSTEPS

        CALL NEXTIME( JDATE, JTIME, TSTEP )

        WRITE( MESG, '( A, I9.7, A, I6.6 )' ) 'Processing', JDATE, ':', JTIME
        CALL M3MESG( BLANK )
        CALL M3MESG( MESG )

        !............ get full layer height ZF [ m ],
        !............ air density DENS [kg/m**3]

       IF ( .NOT. READ3( METFILE, 'ZF', ALLAYS3, JDATE, JTIME, ZFA ) ) THEN
            MESG  = 'Could not read ZF from ' // METFILE
            EFLAG = .TRUE.
            CALL M3MESG( MESG )
            CYCLE
        END IF

        IF ( .NOT. READ3( METFILE, 'DENS' , ALLAYS3, JDATE, JTIME, DENSA ) ) THEN
            MESG = 'Could not read DENS from ' // METFILE
            EFLAG = .TRUE.
            CALL M3MESG( MESG )
            CYCLE
        END IF

        !............    Calculate factors for computing vertical integrals

!$OMP   PARALLEL DO                                                         &
!$OMP&      DEFAULT( NONE ),                                                &
!$OMP&       SHARED( NLAYS, NROWS, NCOLS, ZFA, GAS_FAC, PM_FAC, DENSA ),    &
!$OMP&      PRIVATE( C, R, L, DELZ )

        DO L  = LAY0, LAY1

            IF ( L .EQ. LAY0 ) THEN

                DO R = 1, NROWS
                DO C = 1, NCOLS
                    DELZ = ZFA(C,R,L) * M2CM
                    GAS_FAC(C,R,L) = PPM_MCM3 * DENSA(C,R,L) * DENS_CONV * DELZ
                    PM_FAC (C,R,L) = M2CM1 * DELZ
                END DO ! loop on columns
                END DO  ! loop on rows

            ELSE

                DO R = 1, NROWS
                DO C = 1, NCOLS
                    DELZ = ( ZFA(C,R,L) - ZFA(C,R,L-1) ) * M2CM
                    GAS_FAC(C,R,L) = PPM_MCM3 * DENSA(C,R,L) * DENS_CONV * DELZ
                    PM_FAC (C,R,L) = M2CM1 * DELZ
                END DO ! loop on columns
                END DO  ! loop on rows

            END IF

        END DO  ! loop on layers


        !............  process for integral

!$OMP    PARALLEL DO                                                        &
!$OMP&       DEFAULT( NONE ),                                               &
!$OMP&        SHARED( NVARS_OUT, NCOLS, NROWS, NLAYS, JDATE, JTIME,         &
!$OMP&                LAY0, LAY1, VNAMEO, UNITSO, GAS_FAC, PM_FAC, AOUT ),  &
!$OMP&       PRIVATE( V, MESG )

        DO V = 1, NVARS_OUT  ! loop over output variables

            !! check units of variables.
            !! only process gas phase mixing ratio and aerosol mass concentration.

            IF( UNITSO( V ) .EQ. GAS_OUT ) THEN

                CALL VERTOT( CNCFILE, VNAMEO(V), JDATE, JTIME,  &
                             NCOLS, NROWS, NLAYS, LAY0, LAY1,   &
                             GAS_FAC, AOUT( 1,1,V ) )

            ELSE IF( UNITSO( V ) .EQ. PM_OUT ) THEN

                CALL VERTOT( CNCFILE, VNAMEO(V), JDATE, JTIME,  &
                             NCOLS, NROWS, NLAYS, LAY0, LAY1,   &
                             PM_FAC, AOUT( 1,1,V ) )

            END IF  ! check on species

        END DO ! loop on species (NVARS)


        !!  write output file:  do this as a serial loop to avoid
        !!  out-of-order writes that cause netCDF-padding overheads

        DO V = 1, NVARS_OUT  ! loop over output variables

            IF( .NOT. WRITE3( OUTFILE, VNAMEO(V), JDATE, JTIME, AOUT(1,1,V) ) )  THEN
                MESG  = ' ERROR writing "'// TRIM( VNAMEO(V) )//'"'
                EFLAG = .TRUE.
                CALL M3MESG( MESG )
            END IF

        END DO ! loop on species (NVARS)

    END DO         !  end loop on time steps



    IF ( EFLAG ) THEN
        MESG  = 'Failure in program'
        ISTAT = 2
    ELSE
        MESG  = 'Success in program'
        ISTAT = 0
    END IF

    CALL M3EXIT( PNAME, 0, 0, MESG, ISTAT )


CONTAINS    !!==========================================================================


    SUBROUTINE  VERTOT( FNAME, VNAME, JDATE, JTIME,         &
                        NCOLS, NROWS, NLAYS, LAY0, LAY1,    &
                        VFAC, VTOT )

        USE M3UTILIO

        IMPLICIT NONE

        CHARACTER*(*), INTENT(IN   ) :: FNAME, VNAME
        INTEGER,       INTENT(IN   ) :: JDATE, JTIME
        INTEGER,       INTENT(IN   ) :: NCOLS, NROWS, NLAYS, LAY0, LAY1
        REAL   ,       INTENT(IN   ) :: VFAC( NCOLS, NROWS, NLAYS )
        REAL   ,       INTENT(INOUT) :: VTOT( NCOLS, NROWS )

        REAL        VINP( NCOLS, NROWS, NLAYS )
        INTEGER     C, R, L
        CHARACTER*256       MESG

        !!...............   subroutine body  .......................
        !!   get variable from CMAQ CONC file
        !!   NOTE:  called inside a parallel-loop construct.
        !!   Uses fact that READ3() is thread-safe

        IF ( .NOT. READ3( FNAME, VNAME, ALLAYS3, JDATE, JTIME, VINP ) ) THEN
            MESG = 'Could not read variable '// VNAME
            CALL M3EXIT( 'VERTINTEGRAL/VERTOT',JDATE,JTIME,MESG,1 )
        END IF

        DO R = 1, NROWS     !!  loop-nest order for cache-efficiency:

            DO C = 1, NCOLS
                VTOT( C,R ) = VINP( C,R,LAY0 )*VFAC( C,R,LAY0 )
            END DO

            DO L = LAY0+1, LAY1
            DO C = 1, NCOLS
                VTOT( C,R ) = VTOT( C,R ) + VINP( C,R,L )*VFAC( C,R,L )
            END DO
            END DO

        END DO

        RETURN

    END SUBROUTINE VERTOT


END PROGRAM  VERTINTEGRAL

