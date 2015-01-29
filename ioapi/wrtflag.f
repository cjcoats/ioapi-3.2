
        LOGICAL FUNCTION WRTFLAG( FID, VID, FLAGS, STEP2 )

C***********************************************************************
C Version "$Id: wrtflag.f 100 2015-01-16 16:52:16Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
C (C) 2003-2010 by Baron Advanced Meteorological Systems.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  function body starts at line  73
C
C  FUNCTION:
C       writes "time-step flag" part of time step records from Models-3
C       files with index FID, for all layers and variables for routine
C       WRVARS and PWRITE3
C
C  RETURN VALUE:  TRUE iff the operation succeeds
C
C  PRECONDITIONS REQUIRED:
C       Should only be called by the above routines after OPEN3() has
C       checked for file and time step availability.
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       (netCDF)
C
C  REVISION  HISTORY:  
C       Adapted 3/2002 by Carlie J. Coats, Jr., MCNC-EMC from
C       timestep-flag parts of WRVARS()
C
C       Modified 5/2003 by CJC:  additional error-logging messages
C
C       Modified 10/2003 by CJC for I/O API version 3:  support for
C       native-binary BINFILE3 file type
C
C       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
C***********************************************************************

      IMPLICIT NONE

C...........   INCLUDES:

        INCLUDE 'PARMS3.EXT'
        INCLUDE 'STATE3.EXT'
        INCLUDE 'NETCDF.EXT'


C...........   ARGUMENTS and their descriptions:

        INTEGER, INTENT(IN   ) :: FID             !  file index within the STATE3 commons
        INTEGER, INTENT(IN   ) :: VID             !  vble index within the STATE3 commons
        INTEGER, INTENT(IN   ) :: FLAGS( 2 )      !  ( jdate yyyyddd, jtime hhmmss )
        INTEGER, INTENT(IN   ) :: STEP2           !  physical time step number (maybe mod 2)


C...........   EXTERNAL FUNCTIONS and their descriptions:

        INTEGER, EXTERNAL :: WRBFLAG         !  for BINFIL3 files


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         IERR            !  netCDF error status return
        INTEGER         DIMT( 5 )       !  corner:    FLAGS
        INTEGER         DELT( 5 )       !  diagonal:  FLAGS
        CHARACTER*256   MESG


C***********************************************************************
C   begin body of function  WRTFLAG

        IF ( VID .GT. 0 ) THEN          !  write time flag for just one variable

            DIMT( 2 ) = VID    !  starting variable
            
        ELSE IF ( NVARS3( FID ) .EQ. 0 ) THEN	!  write all-vars timestep flag

            DIMT( 2 ) = 1           !  variable-number

        ELSE

            MESG = 'Invalid call to WRTFLAG() for file ' // FLIST3(FID)
            CALL M3MSG2( MESG )
            WRTFLAG = .FALSE.
            RETURN

        END IF                  !  if writing just one vble, or all vbles

C...........   Deal with case of BINIO3 file:

        IF ( CDFID3( FID ) .EQ. BINFIL3 ) THEN

!$OMP CRITICAL( S_NC )

            IERR = WRBFLAG( FID, VID, STEP2, FLAGS )

!$OMP END CRITICAL( S_NC )

            IF ( IERR .EQ. 0 ) THEN
                WRTFLAG = .FALSE.
                MESG    = 'WRTFLAG:  Error writing time-flag for vble '
     &              // VLIST3( VID,FID ) //
     &              'from file ' // FLIST3( FID )
                CALL M3MSG2( MESG )
            ELSE
                WRTFLAG = .TRUE.
            END IF

            RETURN

       END IF   !  if BINFIL3

C...........   Deal with netCDF file:  set up hyperslab for TFLAG

        DIMT( 1 ) = 1           !  field:  date or time
        DELT( 1 ) = 2           !  extent:  both date and time
        DIMT( 3 ) = STEP2       !  time start
        DELT( 3 ) = 1           !  time extent:  1

        DELT( 2 ) = 1           !  extent:  1


        IF ( CDFID3( FID ) .GE. 0 ) THEN

!$OMP CRITICAL( S_NC )
            CALL NCVPT( CDFID3( FID ), TINDX3( FID ),
     &                  DIMT, DELT, FLAGS, IERR )
!$OMP END CRITICAL( S_NC )

            IF ( IERR .NE. 0 ) THEN

!$OMP CRITICAL( S_LOGOUT )
                WRITE( LOGDEV,* ) 
     &              'WRTFLAG:  Error writing time-flag for vble '
     &              // VLIST3( VID,FID ) //
     &              'from file ' // FLIST3( FID )
                WRITE( LOGDEV,* ) 'netCDF error number', IERR
                WRITE( LOGDEV,* ) 'IOAPI file ID ', FID
                WRITE( LOGDEV,* ) 'netCDF ID     ', CDFID3( FID )
                WRITE( LOGDEV,* ) 'variable      ', VINDX3( VID,FID )
                WRITE( LOGDEV,* ) 'dims array    ', DIMT
                WRITE( LOGDEV,* ) 'delts array   ', DELT
                WRITE( LOGDEV,* )
!$OMP END CRITICAL( S_LOGOUT )

                WRTFLAG = .FALSE.

            ELSE

                WRTFLAG = .TRUE.         ! (if you get to here)

            END IF                      !  if ierr nonzero:  NCVPT() failed

        ELSE

            WRTFLAG = .FALSE.
            MESG = 'WRTFLAG:  invalid file type for '// FLIST3( FID )
            CALL M3MSG2( MESG )

        END IF

        RETURN

        END FUNCTION WRTFLAG

