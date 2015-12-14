
LOGICAL FUNCTION RDTFLAG( FID,VID, JDATE,JTIME, STEP, VERBOSE ) RESULT( RDFLAG )

    !!***********************************************************************
    !!EDSS/Models-3 I/O API.
    !!Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
    !!(C) 2003-2013 Baron Advanced Meteorological Systems,
    !!(C) 2007-2013 Carlie J. Coats, Jr., and
    !!(C) 2014-2015 UNC Institute for the Environment.
    !!Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    !!See file "LGPL.txt" for conditions of use.
    !!.........................................................................
    !! function body starts at line  121
    !!
    !! FUNCTION:
    !!      returns TRUE with STEP = record number for this time step
    !!      iff time step for (JDATE,JTIME) and variable with ID=VID is
    !!      available in file with ID=FID.
    !!
    !!      If FID is time independent, only VID is significant (but
    !!      not JDATE:JTIME).
    !!
    !!      If FID is a "list file-set" upon entry, returns the FID of the
    !!      appropriate "list" entry.
    !!
    !!      If VERBOSE, writes warning message when data not available.
    !!
    !! PRECONDITIONS REQUIRED:
    !!      FID is the file ID of either a "real" netCDF file or of a
    !!      "list" file; in either case, for a file already opened by
    !!      OPEN3().
    !!
    !!      VID is the ID for a valid variable in FID, or else
    !!      is -1 for "all variables".
    !!
    !!      For list-files, also returns the FID which contains the actual
    !!      time step.
    !!
    !! SUBROUTINES AND FUNCTIONS CALLED:
    !!      JSTEP3
    !!
    !! REVISION  HISTORY:
    !!      Adapted  3/2002 by CJC from READ3, READ4D, INTERP3, and CHECK3
    !!
    !!      Modified 8/2002 by CJC:  fixed JSTEP3 RESTART-file bug
    !!
    !!      Modified 7/2003 by CJC:  improved error-handling
    !!
    !!      Modified 10/2003 by CJC for I/O API version 3:  support for
    !!      native-binary BINFIL3 file type
    !!
    !!      Bug-Fix 11/2004 by CJC:  correct "timestep not available"
    !!      test & message for case that VID > 0.
    !!
    !!      Modified 11/2004 by CJC:  new "verbose-flag" argument
    !!
    !!      Modified 1/2007 by CJC:  improved error-messages; logic simplification
    !!
    !!      Modified 03/2010 by CJC: F9x changes for I/O API v3.1
    !!
    !!      Modified 02/2015 by CJC for I/O API 3.2: USE M3UTILIO, MODNCFIO
    !!      support for MPI/PnetCDF MPIGRD3 files; F90 free format
    !!
    !!      Version  11/2015 by CJC: replace MPI_OFFSET_KIND by hard-coded INTEGER(8)
    !!      because OpenMPI-1.4.x does not follow the MPOI "standard" competently.
    !!***********************************************************************

    USE M3UTILIO
    USE MODNCFIO

    IMPLICIT NONE

    !!...........   INCLUDES:

    INCLUDE 'STATE3.EXT'


    !!...........   ARGUMENTS and their descriptions:

    INTEGER, INTENT(INOUT) :: FID             !  subscript for file in STATE3 arrays
    INTEGER, INTENT(IN   ) :: VID             !  subscript for vble in STATE3 arrays
    INTEGER, INTENT(IN   ) :: JDATE           !  date (YYYYDDD) for query
    INTEGER, INTENT(IN   ) :: JTIME           !  time (HHMMSS) for query
    INTEGER, INTENT(  OUT) :: STEP            !  time step record number
    LOGICAL, INTENT(IN   ) :: VERBOSE


    !!...........   EXTERNAL FUNCTIONS and their descriptions:

    INTEGER, EXTERNAL :: RDBFLAG         !  for BINFIL3 files
    LOGICAL, EXTERNAL :: SYNCFID


    !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER         F, I, V         !  loop counters over files, variables
    INTEGER         FLAG1, FLAG2    !  date:time scratch vbles
    INTEGER         IERR            !  netCDF error status return
    INTEGER         DIMT( 5 )       !  corner   for NF_GET_VARA_*()
    INTEGER         DELT( 5 )       !  diagonal for NF_GET_VARA_*()
    INTEGER         FLAGS( 2,MXVARS3 )!  flags from NF_GET_VARA_*()
    LOGICAL         EFLAG
    CHARACTER*16    FNAME, VNAME
    CHARACTER*256   MESG            !  buffer for building error messages         !  netCDF error status return

#ifdef IOAPI_PNCF
    INCLUDE 'mpif.h'

    INTEGER( 8 ) :: DIMP( 5 )       !  corner:    FLAGS
    INTEGER( 8 ) :: DELP( 5 )       !  diagonal:  FLAGS

!!  INTEGER( MPI_OFFSET_KIND ) :: DIMP( 5 )       !  corner:    FLAGS
!!  INTEGER( MPI_OFFSET_KIND ) :: DELP( 5 )       !  diagonal:  FLAGS
#endif

    !!***********************************************************************
    !!  begin body of function  RDTFLAG

    !!.......   If list file-set, find which actual file contains this time step:

    FNAME = FLIST3( FID )

    IF ( CDFID3( FID ) .EQ. LSTFIL3 ) THEN     !  list "file set"

        F = IFRST3( MXFILE3 ) - 1
        DO  I = 1, NLIST3( FID )
            F = F + 1
            IF ( BEGRC3( FID ) .LE. STEP .AND.      &
                 ENDRC3( FID ) .GE. STEP ) THEN
                FID   = ILIST3( F )
                GO TO  11
            END IF
        END DO

        !!  if you get to here:  data not available in this file-set

        WRITE( MESG,91020 ) 'Requested date & time:', JDATE, JTIME
        CALL M3MSG2( MESG )
        MESG = 'Time step not available in file-set ' // FNAME
        CALL M3WARN( 'RDTFLAG', JDATE, JTIME, MESG )
        RDFLAG = .FALSE.
        RETURN

11      CONTINUE

    ELSE IF ( CDFID3( FID ) .EQ. VIRFIL3 ) THEN     !  virtual "file"

        CALL M3WARN( 'RDTFLAG', JDATE, JTIME, 'Bad call to RDTFLAG' )
        RDFLAG = .FALSE.
        RETURN        

    END IF                  !  if cdfid3(fid) = lstfil3

    !!...........   Compute record number, and check availability:


    IF ( FTYPE3( FID ) .EQ. DCTNRY3 ) THEN

        STEP = VID
        IF ( STEP .GT. NVARS3( FID ) ) THEN
            WRITE( MESG, '(3A,I9)' ) 'Dictionary-file ', FNAME, ' does not contain variable-index', VID
            CALL M3WARN( 'RDTFLAG', JDATE, JTIME, MESG )
            RDFLAG = .FALSE.
            RETURN
        END IF

    ELSE

        STEP = JSTEP3( JDATE, JTIME, SDATE3( FID ), STIME3( FID ), ABS( TSTEP3( FID ) ) )

        IF ( STEP .LT. 0 ) THEN

            IF ( VERBOSE ) THEN
                WRITE( MESG,91020 ) 'Requested date & time:    ', JDATE, JTIME
                CALL M3MSG2( MESG )
                WRITE( MESG,91020 ) 'File starting date & time:', SDATE3( FID ), STIME3( FID )
                CALL M3MSG2( MESG )
                WRITE( MESG,91030 ) 'File time step:           ', TSTEP3( FID )
                CALL M3MSG2( MESG )
                MESG = 'Time step error for file:  '//FLIST3(FID)
                CALL M3WARN( 'RDTFLAG', JDATE, JTIME, MESG )
            END IF          !  if verbose
            RDFLAG = .FALSE.
            RETURN

        END IF          !  check on step number

        IF ( TSTEP3( FID ) .LT. 0 ) THEN
            STEP  = 1 + MOD( STEP - 1, 2 )
            FLAG1 = JDATE
            FLAG2 = JTIME
        ELSE IF ( TSTEP3( FID ) .GT. 0 ) THEN
            STEP  = STEP
            FLAG1 = JDATE
            FLAG2 = JTIME
        ELSE    ! tstep3( fid ) = 0
            FLAG1 = 0
            FLAG2 = 0
        END IF

    END IF          ! if dictionary-file, or not


    IF ( VOLAT3( FID ) ) THEN      !  volatile file:  synch with disk

        IF ( .NOT. SYNCFID( FID ) ) THEN

            MESG = 'Error with disk synchronization for file:  ' // FNAME
            CALL M3WARN( 'RDTFLAG', JDATE, JTIME, MESG )
            RDFLAG = .FALSE.
            RETURN

        END IF              !  if synch failed

    END IF                  !  if file is volatile

    !!.......   Deal with netCDF, native-binary-layer BINFIL3 files:

    IF ( VID .GT. 0 ) THEN  !  reading just one variable

        DIMT( 2 ) = VID     !  variable-number
        DELT( 2 ) = 1       !  extent:  one variable (also loop count)

    ELSE            !  reading all variables

        DIMT( 2 ) = 1               !  initial variable-number
        DELT( 2 ) = MAX( 1, NVARS3( FID ) )   !  extent:  all variables

    END IF

    EFLAG = .FALSE.         !  no errors yet...


    IF ( CDFID3( FID ) .EQ. BINFIL3 ) THEN       ! BINFIL3 file:

!$OMP CRITICAL( S_NC )
        IERR = RDBFLAG( FID, VID, STEP, FLAGS )
!$OMP END CRITICAL( S_NC )

        IF ( IERR .EQ. 0 ) THEN
            MESG = 'Error reading time-flags for BINIO3 file ' // FNAME
            EFLAG = .TRUE.
        END IF          !  if rdbflag() failed

    ELSE IF ( FTYPE3( FID ) .EQ. MPIGRD3 ) THEN     !  PnetCDF file:

#ifdef  IOAPI_PNCF
        DIMP( 1 ) = 1           !  field:  date or time
        DELP( 1 ) = 2           !  extent:  entire field
        DIMP( 3 ) = STEP        !  timestep dimension
        DELP( 3 ) = 1           !  extent in timestep dimension

!$OMP CRITICAL( S_NC )
        IERR = NFMPI_GET_VARA_INT( CDFID3( FID ), TINDX3( FID ), DIMT, DELT, FLAGS )
!$OMP END CRITICAL( S_NC )
        IF ( IERR .EQ. 8 ) THEN     !  timestep flag not yet written

            EFLAG = .TRUE.
            MESG  = 'Time step not yet written in file ' // FNAME

        ELSE IF ( IERR .NE. 0 ) THEN

            WRITE( MESG,91010 ) 'netCDF error number', IERR
            CALL M3MSG2( MESG )
            EFLAG = .TRUE.
            MESG  = 'Error reading netCDF time step flag for '// FNAME

        END IF
#endif

#ifndef  IOAPI_PNCF
        EFLAG = .TRUE.
        MESG  = 'MPI/PnetCDF I/O not enabled in this build'
#endif

    ELSE IF ( CDFID3( FID ) .GE. 0 ) THEN           !  netCDF file:

        DIMT( 1 ) = 1           !  field:  date or time
        DELT( 1 ) = 2           !  extent:  entire field
        DIMT( 3 ) = STEP        !  timestep dimension
        DELT( 3 ) = 1           !  extent in timestep dimension

!$OMP CRITICAL( S_NC )
        IERR = NF_GET_VARA_INT( CDFID3( FID ), TINDX3( FID ), DIMT, DELT, FLAGS )
!$OMP END CRITICAL( S_NC )
        IF ( IERR .EQ. 8 ) THEN     !  timestep flag not yet written

            EFLAG = .TRUE.
            MESG  = 'Time step not yet written in file ' // FNAME

        ELSE IF ( IERR .NE. 0 ) THEN

            WRITE( MESG,91010 ) 'netCDF error number', IERR
            CALL M3MSG2( MESG )
            EFLAG = .TRUE.
            MESG  = 'Error reading netCDF time step flag for '// FNAME

        END IF

    END IF          !  if netCDF file; else if BINIO3 file

    IF ( EFLAG ) THEN       !  errors

        IF ( VERBOSE ) THEN
            CALL M3WARN( 'RDTFLAG', JDATE, JTIME, MESG )
        END IF              !  if verbose

        RDFLAG = .FALSE.
        RETURN

    END IF                  !  if eflag


!!...........   Check time step flags for all variables:

    IF ( VID .GT. 0 ) THEN

        IF ( FLAGS( 1,1 ) .NE. FLAG1  .OR.          &
             FLAGS( 2,1 ) .NE. FLAG2  ) THEN


            VNAME = VLIST3( VID,FID )
            MESG  = 'Time step not available in file ' // FNAME // ' for variable ' // VNAME
            EFLAG = .TRUE.
            CALL M3WARN( 'RDTFLAG', JDATE, JTIME, MESG )

        END IF          !  if bad flag value

    ELSE

        DO  V = 1, DELT( 2 )

            IF ( FLAGS( 1,V ) .NE. FLAG1  .OR.      &
                 FLAGS( 2,V ) .NE. FLAG2  ) THEN

                VNAME =VLIST3( V,FID )
                MESG  = 'Time step not available in file ' // FNAME // ' for variable ' // VNAME
                EFLAG = .TRUE.
                CALL M3WARN( 'RDTFLAG', JDATE, JTIME, MESG )

            END IF          !  if bad flag value

        END DO

    END IF          !  if vid > 0, or not

    RDFLAG = ( .NOT.EFLAG )

    RETURN

    !!******************  FORMAT  STATEMENTS   ******************************

    !!...........   Error and warning message formats..... 91xxx

91010   FORMAT ( 3 ( A , :, I5, :, 2X ) )

91020   FORMAT ( A , I9, ':' , I6.6, :, A )

91030   FORMAT ( A , I6.6 )

END FUNCTION RDTFLAG

