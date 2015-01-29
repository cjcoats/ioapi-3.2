
        LOGICAL FUNCTION WRVARS( FID, VID, FLAGS, STEP2, 
     &                           DIMS, DELS, DELTA, BUFFER )

C***********************************************************************
C Version "$Id: wrvars.F 100 2015-01-16 16:52:16Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (C) 2003-2010 by Baron Advanced Meteorological Systems.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  function body starts at line  99
C
C  FUNCTION:  writes "variables" part of time step records from Models-3
C             files with index FID, for all layers and variables for
C             routines WRCUSTOM, WRGWRDED, WRBNDARY, WRIDDATA, WRPROFIL,
C             and WRGRNEST.
C
C  MACHINE DEPENDENCY:
C       TYPSIZE( 6 ) must be sizeof( double )/sizeof( real )
C
C  RETURN VALUE:  TRUE iff the operation succeeds
C
C  PRECONDITIONS REQUIRED:  Should only be called by the above routines,
C             after OPEN3() has checked for file and time step availability,
C             and after the above routines have set up DIMS, , and DELTA.
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       WRTFLAG, (netCDF)
C
C  REVISION  HISTORY:  
C       prototype 3/1992 by CJC
C
C       modified 10/1994 by CJC:  writes STEP number instead of OKFLAG3;
C       supports WRITE3() operations with single-variable granularity
C
C       Modified 2/1997  by CJC for OpenMP:  make it thread-safe to
C       call WRITE3 on distinct files in task-parallel.
C
C       Modified  9/1999 by CJC for new portability conventions
C       Modified  3/2002 by CJC for new CMAQ functionality:  uses WRTFLAG()
C
C       Modified 10/2003 by CJC for I/O API version 3:
C       Structure in terms of LOGICAL SYNCFID
C       support for native-binary BINFILE3 and LISTFIL3 file types
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
        INTEGER, INTENT(IN   ) :: DIMS( 5 )       !  corner arg array for NCVGT()
        INTEGER, INTENT(IN   ) :: DELS( 5 )       !  diag   arg array for NCVGT()
        INTEGER, INTENT(IN   ) :: DELTA           !  d(INDX) / d(NCVGTcall)
        REAL   , INTENT(IN   ) :: BUFFER(*)       !  buffer array for input


C...........   EXTERNAL FUNCTIONs:

        INTEGER, EXTERNAL :: WRBFLAG, WRBVARS
        LOGICAL, EXTERNAL :: WRTFLAG, SYNCFID


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER       IERR            !  netCDF error status return
        INTEGER       VAR             !  loop counter for file variables
        INTEGER       INDX            !  subscript location in BUFFER(*)
        LOGICAL       AFLAG
        CHARACTER*256 MESG

#if _CRAY || REAL8
        INTEGER, PARAMETER :: TYPSIZE( 6 ) = (/ 1, 1, 1, 1, 1, 1 /)
#endif
#if ! ( _CRAY || REAL8 )
        INTEGER, PARAMETER :: TYPSIZE( 6 ) = (/ 1, 1, 1, 1, 1, 2 /)
#endif


C***********************************************************************
C   begin body of function  WRVARS

C...............   Process native-binary-layer files:

        IF ( CDFID3( FID )  .EQ. BINFIL3 ) THEN    ! native-binary file

!$OMP CRITICAL( S_NC )
            IERR = WRBVARS( FID, VID, STEP2, BUFFER )
            IF ( 0 .EQ. IERR ) THEN     !  error
                AFLAG = .TRUE.
            ELSE IF ( 0 .EQ. WRBFLAG( FID, VID, STEP2, FLAGS ) ) THEN
                AFLAG = .TRUE.
            ELSE
                AFLAG = .FALSE.
            END IF
!$OMP END CRITICAL( S_NC )

            IF ( AFLAG ) THEN
!$OMP CRITICAL( S_LOGOUT )
                WRITE( LOGDEV,'( 5X, A )' )
     &                  '>>> WARNING in subroutine WRVARS <<<'
                IF ( VID .GT. 0 ) THEN
                    WRITE( LOGDEV, '( 5X, 5 A )' )
     &                  'Error writing variable ', VLIST3( VID,FID ),
     &                  'to BINIO file ', FLIST3( FID )
                    WRITE( LOGDEV,'( 5X, A, I6 )' ) 
     &                  'variable  ID  ', VINDX3( VID,FID )
                ELSE
                    WRITE( LOGDEV,'( 5X, 2 A )' ) 
     &                  'Error writing ALL-VARIABLES to BINIO file ', 
     &                  FLIST3( FID )
                END IF
                WRITE( LOGDEV,'( 5X, A, I6 )' ) 'IOAPI file ID ', FID
                WRITE( LOGDEV,'( 5X, A, 5 I6 )' ) 'dims array  ', DIMS
                WRITE( LOGDEV,'( 5X, A, 5 I6 )' ) 'delts array ', DELS
                WRITE( LOGDEV,* )
!$OMP END CRITICAL( S_LOGOUT )
                WRVARS = .FALSE.
            ELSE
                WRVARS = .TRUE.
            END IF

            RETURN

        END IF                          !  if native-binary file

        IF ( VID .GT. 0 ) THEN          !  write just one variable

C...............   Write contents of the buffer for this variable

!$OMP CRITICAL( S_NC )
            CALL NCVPT( CDFID3( FID ), VINDX3( VID,FID ),
     &                  DIMS, DELS, BUFFER, IERR )
!$OMP END CRITICAL( S_NC )

            IF ( IERR .NE. 0 ) THEN
!$OMP CRITICAL( S_LOGOUT )
                WRITE( LOGDEV,91010 )
     &                  'Error writing variable '
     &                  // VLIST3( VID,FID ) //
     &                  'to file ' // FLIST3( FID ) ,
     &                  'netCDF error number', IERR
                WRITE( LOGDEV,* ) 'IOAPI file ID ', FID
                WRITE( LOGDEV,* ) 'netCDF ID     ', CDFID3( FID )
                WRITE( LOGDEV,* ) 'variable      ', VINDX3( VID,FID )
                WRITE( LOGDEV,* ) 'dims array    ', DIMS
                WRITE( LOGDEV,* ) 'delts array   ', DELS
                WRITE( LOGDEV,* )
!$OMP END CRITICAL( S_LOGOUT )
        
                WRVARS = .FALSE.
                RETURN
        
            END IF          !  ierr nonzero:  NCVPT() failed

C...............   Write time step flag:

            WRVARS = WRTFLAG( FID, VID, FLAGS, STEP2 )
            RETURN

        ELSE IF ( NVARS3( FID ) .EQ. 0 ) THEN	!  write all-vars timestep flag

            WRVARS = WRTFLAG( FID, VID, FLAGS, STEP2 )
            RETURN

        ELSE                            !  write all variables:
        
C...............   Write contents of the buffer for each variable

            INDX  = 1                   !  starting subscript for BUFFER(*)
            AFLAG = .TRUE.
            
            DO  12  VAR = 1 , NVARS3( FID )
                   
!$OMP CRITICAL( S_NC )
                CALL NCVPT( CDFID3( FID ), VINDX3( VAR,FID ),
     &                      DIMS, DELS, BUFFER( INDX ), IERR )
!$OMP END CRITICAL( S_NC )
                IF ( IERR .NE. 0 ) THEN
!$OMP CRITICAL( S_LOGOUT )
                    MESG = 'Error writing variable '
     &                  // VLIST3( VAR,FID ) //
     &                  'to file ' // FLIST3( FID )
                    WRITE( LOGDEV,* ) MESG( 1:72 )
                    WRITE( LOGDEV,* ) 'netCDF error number', IERR
                    WRITE( LOGDEV,* ) 'IOAPI ID    ', FID
                    WRITE( LOGDEV,* ) 'netCDF ID   ', CDFID3( FID )
                    WRITE( LOGDEV,* ) 'vble        ', VINDX3( VAR,FID )
                    WRITE( LOGDEV,* ) 'dims array  ', DIMS
                    WRITE( LOGDEV,* ) 'delts array ', DELS
                    WRITE( LOGDEV,* ) 'offset      ', INDX
                    WRITE( LOGDEV,* ) 'WRITE3 request: ALL VARIABLES'
                    WRITE( LOGDEV,* )
!$OMP END CRITICAL( S_LOGOUT )
        
                    WRVARS = .FALSE.
                    RETURN
        
                END IF                  !  ierr nonzero:  NCVPT() failed
                   
                INDX = INDX  +  DELTA * TYPSIZE( VTYPE3( VAR,FID ) )
                   
C...............   Write time step flags:
      
                IF ( .NOT.WRTFLAG( FID, VAR, FLAGS, STEP2 ) ) THEN
                    MESG = 'Error writing time-flags for file '
     &                     // FLIST3( FID )
                    CALL M3MSG2( MESG )
                    WRVARS = .FALSE.
                    RETURN
                END IF

12          CONTINUE                    !  end loop on variables VAR

        END IF                  !  if writing just one vble, or all vbles
        
        IF ( VOLAT3( FID ) ) THEN     !  volatile file:  synch with disk
            
            IF ( .NOT. SYNCFID( FID ) ) THEN

                MESG = 'Error with disk synchronization for file:  '
     &                 // FLIST3( FID )
                CALL M3MSG2( MESG )
                WRVARS = .FALSE.
                RETURN

            END IF              !  if synch failed

        END IF                  !  if file volatile

        WRVARS = .TRUE.         ! (if you get to here)
        RETURN

C******************  FORMAT  STATEMENTS   ******************************

C...........   Error and warning message formats..... 91xxx
 
91010   FORMAT ( //5X , '>>> WARNING in subroutine WRVARS <<<',
     &            2 ( /5X , A , : ) , I5, // )
 
91030   FORMAT ( //5X , '>>> WARNING in subroutine WRITE3 <<<',
     &            /5X , A , I5 , : ,
     &            /5X , A ) 

        END FUNCTION WRVARS

