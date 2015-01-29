
        LOGICAL FUNCTION RDTFLAG( FID,VID, JDATE,JTIME, STEP, VERBOSE )

C***********************************************************************
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
C (C) 2003-2010 by Baron Advanced Meteorological Systems.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  function body starts at line  102
C
C  FUNCTION:
C       returns TRUE with STEP = record number for this time step
C       iff time step for (JDATE,JTIME) and variable with ID=VID is
C       available in file with ID=FID.
C
C       If FID is time independent, only VID is significant (but
C       not JDATE:JTIME).
C
C       If FID is a "list file-set" upon entry, returns the FID of the
C       appropriate "list" entry.
C
C       If VERBOSE, writes warning message when data not available.
C
C  PRECONDITIONS REQUIRED:
C       FID is the file ID of either a "real" netCDF file or of a
C       "list" file; in either case, for a file already opened by
C       OPEN3().
C
C       VID is the ID for a valid variable in FID, or else
C       is -1 for "all variables".
C
C       For list-files, also returns the FID which contains the actual
C       time step.
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       JSTEP3
C
C  REVISION  HISTORY:
C       Adapted  3/2002 by CJC from READ3, READ4D, INTERP3, and CHECK3
C
C       Modified 8/2002 by CJC:  fixed JSTEP3 RESTART-file bug
C
C       Modified 7/2003 by CJC:  improved error-handling
C
C       Modified 10/2003 by CJC for I/O API version 3:  support for
C       native-binary BINFIL3 file type
C
C       Bug-Fix 11/2004 by CJC:  correct "timestep not available"
C       test & message for case that VID > 0.
C
C       Modified 11/2004 by CJC:  new "verbose-flag" argument
C
C       Modified 1/2007 by CJC:  improved error-messages; logic simplification
C
C       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
C***********************************************************************

      IMPLICIT NONE

C...........   INCLUDES:

        INCLUDE 'PARMS3.EXT'
        INCLUDE 'STATE3.EXT'
        INCLUDE 'NETCDF.EXT'


C...........   ARGUMENTS and their descriptions:

        INTEGER, INTENT(INOUT) :: FID             !  subscript for file in STATE3 arrays
        INTEGER, INTENT(IN   ) :: VID             !  subscript for vble in STATE3 arrays
        INTEGER, INTENT(IN   ) :: JDATE           !  date (YYYYDDD) for query
        INTEGER, INTENT(IN   ) :: JTIME           !  time (HHMMSS) for query
        INTEGER, INTENT(  OUT) :: STEP            !  time step record number
        LOGICAL, INTENT(IN   ) :: VERBOSE


C...........   EXTERNAL FUNCTIONS and their descriptions:

        INTEGER, EXTERNAL :: JSTEP3          !  compute time step record numbers
        INTEGER, EXTERNAL :: RDBFLAG         !  for BINFIL3 files
        LOGICAL, EXTERNAL :: SYNCFID


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         F, I, V         !  loop counters over files, variables
        INTEGER         FLAG1, FLAG2    !  date:time scratch vbles
        INTEGER         IERR            !  netCDF error status return
        INTEGER         DIMT( 5 )       !  corner   for NCVGT()
        INTEGER         DELT( 5 )       !  diagonal for NCVGT()
        INTEGER         FLAGS( 2,MXVARS3 )!  flags from NCVGT()
        LOGICAL         EFLAG
        CHARACTER*256   MESG            !  buffer for building error messages

C***********************************************************************
C   begin body of function  RDTFLAG

C.......   If list file-set, find which actual file contains this time step:

        IF ( CDFID3( FID ) .EQ. LSTFIL3 ) THEN     !  list "file set"

            F = IFRST3( MXFILE3 ) - 1
            DO  I = 1, NLIST3( FID )
                F = F + 1
                IF ( BEGRC3( FID ) .LE. STEP .AND.
     &               ENDRC3( FID ) .GE. STEP ) THEN
                    FID   = ILIST3( F )
                    GO TO  11
                END IF
            END DO

            !!  if you get to here:  data not available in this file-set

            WRITE( MESG,91020 )
     &              'Requested date & time:', JDATE, JTIME
            CALL M3MSG2( MESG )
            MESG = 'Variable ' // VLIST3( VID,FID ) //
     &             ' not available in file-set ' // FLIST3( FID )
            CALL M3WARN( 'RDTFLAG', JDATE, JTIME, MESG )
            RDTFLAG = .FALSE.
            RETURN

11          CONTINUE

        END IF                  !  if cdfid3(fid) = lstfil3

C...........   Compute record number, and check availability:


        IF ( FTYPE3( FID ) .EQ. DCTNRY3 ) THEN

            STEP = VID

        ELSE

            STEP = JSTEP3( JDATE, JTIME,
     &                     SDATE3( FID ),
     &                     STIME3( FID ),
     &                     ABS( TSTEP3( FID ) ) )

            IF ( STEP .LT. 0 ) THEN

                IF ( VERBOSE ) THEN
                    WRITE( MESG,91020 )
     &                 'Requested date & time:    ', JDATE, JTIME
                    CALL M3MSG2( MESG )
                    WRITE( MESG,91020 )
     &                 'File starting date & time:',
     &                 SDATE3( FID ), STIME3( FID )
                    CALL M3MSG2( MESG )
                    WRITE( MESG,91030 )
     &                 'File time step:           ', TSTEP3( FID )
                    CALL M3MSG2( MESG )
                    MESG = 'Time step error for file:  '//FLIST3(FID)
                    CALL M3WARN( 'RDTFLAG', JDATE, JTIME, MESG )
                END IF          !  if verbose
                RDTFLAG = .FALSE.
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

                MESG = 'Error with disk synchronization for file:  '
     &                 // FLIST3( FID )
                CALL M3WARN( 'RDTFLAG', JDATE, JTIME, MESG )
                RDTFLAG = .FALSE.
                RETURN

            END IF              !  if synch failed

        END IF                  !  if file is volatile

C.......   Deal with netCDF, native-binary-layer BINFIL3 files:

        IF ( VID .GT. 0 ) THEN  !  reading just one variable

            DIMT( 2 ) = VID     !  variable-number
            DELT( 2 ) = 1       !  extent:  one variable (also loop count)

        ELSE            !  reading all variables

            DIMT( 2 ) = 1               !  initial variable-number
            DELT( 2 ) = MAX( 1, NVARS3( FID ) )   !  extent:  all variables

        END IF

        EFLAG = .FALSE.         !  no errors yet...

        IF ( CDFID3( FID ) .GE. 0 ) THEN          !  netCDF file:

            DIMT( 1 ) = 1           !  field:  date or time
            DELT( 1 ) = 2           !  extent:  entire field
            DIMT( 3 ) = STEP        !  timestep dimension
            DELT( 3 ) = 1           !  extent in timestep dimension

!$OMP CRITICAL( S_NC )
            CALL NCVGT( CDFID3( FID ), TINDX3( FID ),
     &                  DIMT, DELT, FLAGS, IERR )
!$OMP END CRITICAL( S_NC )
            IF ( IERR .EQ. 8 ) THEN     !  timestep flag not yet written

                EFLAG = .TRUE.
                MESG  = 'Time step not yet written in file ' //
     &                  FLIST3( FID )

            ELSE IF ( IERR .NE. 0 ) THEN

                WRITE( MESG,91010 ) 'netCDF error number', IERR
                CALL M3MSG2( MESG )
                EFLAG = .TRUE.
                MESG  = 'Error reading netCDF time step flag for '//
     &                 FLIST3( FID )

            END IF

        ELSE IF ( CDFID3( FID ) .EQ. BINFIL3 ) THEN       ! BINFIL3 file:

!$OMP CRITICAL( S_NC )
            IERR = RDBFLAG( FID, VID, STEP, FLAGS )
!$OMP END CRITICAL( S_NC )

            IF ( IERR .EQ. 0 ) THEN

                MESG = 'Error reading time-flags for BINIO3 file '
     &              // FLIST3( FID )
                EFLAG = .TRUE.

            END IF          !  if ierr nonzero or not for NCVGT1()

        END IF          !  if netCDF file; else if BINIO3 file

        IF ( EFLAG ) THEN       !  errors

            IF ( VERBOSE ) THEN
                CALL M3WARN( 'RDTFLAG', JDATE, JTIME, MESG )
            END IF              !  if verbose

            RDTFLAG = .FALSE.
            RETURN

        END IF                  !  if eflag


C...........   Check time step flags for all variables:

        IF ( VID .GT. 0 ) THEN

            IF ( FLAGS( 1,1 ) .NE. FLAG1  .OR.
     &           FLAGS( 2,1 ) .NE. FLAG2  ) THEN

                MESG = 'Time step not available in file ' //
     &                  FLIST3( FID ) // ' for variable ' // 
     &                  VLIST3( VID,FID )
                EFLAG = .TRUE.
                CALL M3WARN( 'RDTFLAG', JDATE, JTIME, MESG )

            END IF          !  if bad flag value

        ELSE

            DO  V = 1, DELT( 2 )

                IF ( FLAGS( 1,V ) .NE. FLAG1  .OR.
     &               FLAGS( 2,V ) .NE. FLAG2  ) THEN

                    MESG = 'Time step not available in file ' //
     &                      FLIST3( FID ) // ' for variable ' // 
     &                      VLIST3( V,FID )
                    EFLAG = .TRUE.
                    CALL M3WARN( 'RDTFLAG', JDATE, JTIME, MESG )

                END IF          !  if bad flag value

            END DO

        END IF          !  if vid > 0, or not

        RDTFLAG = ( .NOT.EFLAG )

        RETURN

C******************  FORMAT  STATEMENTS   ******************************

C...........   Error and warning message formats..... 91xxx

91010   FORMAT ( 3 ( A , :, I5, :, 2X ) )

91020   FORMAT ( A , I9, ':' , I6.6, :, A )

91030   FORMAT ( A , I6.6 )

        END FUNCTION RDTFLAG

