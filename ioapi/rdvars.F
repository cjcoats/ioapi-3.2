
        LOGICAL FUNCTION RDVARS( FID, VID, DIMS, DELS, DELTA, BUFFER )

C***********************************************************************
C Version "$Id: rdvars.F 100 2015-01-16 16:52:16Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (C) 2003-2010 by Baron Advanced Meteorological Systems.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  function body starts at line  121
C
C  FUNCTION:
C       reads "variables" part of time step records from Models-3 file
C       with index FID, for variable with index VID, for routines
C       RDCUSTOM, RDGRDDED, RDBNDARY, RDIDDATA, RDPROFIL, and RDGRNEST.
C
C       If VID is -1 = ALLAYS3 reads all variables; if LAYER is -1,
C       reads all layers.
C
C  MACHINE DEPENDENCY:
C	TYPSIZE( 6 ) must be sizeof( double )/sizeof( real )
C
C  RETURN VALUE:  TRUE iff the operation succeeds (and the data is available)
C
C  PRECONDITIONS REQUIRED:
C       DIMS and DELS must have the time dimension entries as the last
C       nonzero entries, and must be padded with zeros beyond that point.
C
C       Should only be called by the above routines, after OPEN3() has
C       checked for file and time step availability, and after the above
C       routines have set up DIMS, DELS, and DELTA.
C
C  SUBROUTINES AND FUNCTIONS CALLED:  INDEX1
C
C  REVISION  HISTORY:  
C	prototype 3/1992 by CJC
C
C	Modified  9/1994 by CJC:  argument VID instead of VNAME
C
C       Modified  5/1998 by CJC for OpenMP thread-safety
C
C       Modified 10/2003 by CJC for I/O API version 3:  support for
C       native-binary BINFIL3 file type; uses INTEGER NAME2FID
C
C       Modified  4/2004 by CJC for I/O API version 3:  fix bug in
C       ALLAYS-ALVARS case; correct ALLVARS warning messages.
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
        INTEGER, INTENT(IN   ) :: VID             !  variable index or -1 == ALL
        INTEGER, INTENT(IN   ) :: DIMS( 5 )       !  corner arg array for NCVGT()
        INTEGER, INTENT(IN   ) :: DELS( 5 )       !  corner arg array for NCVGT()
        INTEGER, INTENT(IN   ) :: DELTA           !  d(INDX) / d(NCVGTcall)
        REAL   , INTENT(  OUT) :: BUFFER(*)       !  buffer array for input


C...........   EXTERNAL FUNCTIONs:

        INTEGER, EXTERNAL :: RDBVARS


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         IERR            !  netCDF error status return
        INTEGER         VAR             !  loop counter for file variables
        INTEGER         INDX            !  subscript location in BUFFER(*)
        INTEGER         I               !  counter
        INTEGER         STEP, NREC      !  time subscript location in BUFFER(*)
        INTEGER         LAYR            !  layer subscript location in BUFFER(*)
        INTEGER         RSIZE
        LOGICAL         EFLAG
        CHARACTER*256   MESG
        
        INTEGER, PARAMETER :: STEPDEX( -2:9 ) =
     &       (/
     &        IMISS3,   !  out-of-range
     &        3,        !  CUSTOM3
     &        IMISS3,   !  DCTNRY3:  should not go through RDVARS()
     &        4,        !  GRDDED3
     &        3,        !  BNDARY3
     &        3,        !  IDDATA3
     &        4,        !  PROFIL3
     &        4,        !  GRNEST3
     &        2,        !  SMATRX3
     &        4,        !  TSRIES3
     &        4,        !  PTRFLY3
     &        IMISS3    !  out-of-range
     &        /)

#if _CRAY || REAL8
        INTEGER, PARAMETER :: TYPSIZE( 6 ) = (/ 1, 1, 1, 1, 1, 1 /)
#endif
#if ! ( _CRAY || REAL8 )
        INTEGER, PARAMETER :: TYPSIZE( 6 ) = (/ 1, 1, 1, 1, 1, 2 /)
#endif


C***********************************************************************
C   begin body of function  RDVARS

C...........   Perform the reads, according to VID

        IF ( CDFID3( FID ) .EQ. BINFIL3 ) THEN    ! native-binary file

            !!  the highest-subscripted nonzero entry of DELS()
            !!  determines STEP:  this may be 1 or 2 (the latter
            !!  for calls from INTERP3() or DDTVAR3())

            I = STEPDEX( MIN( 9, MAX( -2, FTYPE3( FID ) ) ) )

            IF ( I .LT.0 ) THEN
                IF ( VID .GT. 0 ) THEN
                    MESG = 'RDVARS:  bad file type for variable ' //
     &                 VLIST3( VAR,FID )// 'from file ' // FLIST3( FID )
                ELSE
                    MESG = 'RDVARS:  bad file type for file ' // 
     &                 FLIST3( FID )
                END IF
                CALL M3MSG2( MESG )
                RDVARS = .FALSE.
                RETURN
            ELSE
                STEP = DIMS( I )
                NREC = DELS( I )
                LAYR = DIMS( I-1 )
            END IF

            IF ( LAYR .EQ. 1 .AND. DELS(I-1) .EQ. NLAYS3( FID ) ) THEN
                LAYR = ALLAYS3
            END IF

            IF ( VID .GT. 0 ) THEN
                RSIZE = DELTA*TYPSIZE( VTYPE3( VID,FID ) )
            ELSE
                RSIZE = 0
                DO VAR = 1, MAX( 1, NVARS3( FID ) )
                    RSIZE = RSIZE + DELTA*TYPSIZE( VTYPE3( VAR,FID ) )
                END DO
            END IF

            EFLAG = .FALSE.
            INDX  = 1

!$OMP CRITICAL( S_NC )
            DO  I = STEP, STEP+NREC-1
                IF ( 0 .EQ. RDBVARS( FID, VID, LAYR, I, 
     &                               BUFFER( INDX ) ) ) THEN
                    EFLAG = .TRUE.
                END IF
                INDX = INDX + RSIZE
            END DO
!$OMP END CRITICAL( S_NC )

            IF ( EFLAG  ) THEN     !  error
                IF ( VID .GT. 0 ) THEN
                    MESG = 'Error reading variable ' //
     &                VLIST3( VID,FID ) // 'from file ' // FLIST3( FID )
                ELSE
                    MESG = 'Error reading ALL VARIABLES from file ' //
     &                FLIST3( FID )
                END IF
                CALL M3MSG2( MESG )
                RDVARS = .FALSE.
            ELSE
                RDVARS = .TRUE.
            END IF

            RETURN

        END IF          !  if native-binary file

        IF ( VID .EQ. ALLAYS3 ) THEN

            INDX = 1    !  starting subscript for BUFFER(*)

            DO  12  VAR = 1 , NVARS3( FID )

!$OMP           CRITICAL( S_NC )
                CALL NCVGT( CDFID3( FID ), VINDX3( VAR,FID ),
     &                      DIMS, DELS, BUFFER( INDX ), IERR )
!$OMP           END CRITICAL( S_NC )

                IF ( IERR .NE. 0 ) THEN
                    WRITE( MESG,'( A, I6, 2X, 5 A )' )
     &                   'netCDF error number', IERR,
     &                   'Error reading variable "', VLIST3( VID,FID ), 
     &                   '" from file "', FLIST3( FID ), '"'
                    CALL M3MSG2( MESG )
                    RDVARS = .FALSE.
                    RETURN
                END IF          !  ierr nonzero:  NCVGTC() failed

                INDX = INDX  +  DELTA * TYPSIZE( VTYPE3( VAR,FID ) )

12          CONTINUE    !  end loop on variables VAR

        ELSE    !  read a specific variable

!$OMP       CRITICAL( S_NC )
            CALL NCVGT( CDFID3( FID ), VINDX3( VID,FID ),
     &                  DIMS, DELS, BUFFER, IERR )
!$OMP       END CRITICAL( S_NC )

            IF ( IERR .NE. 0 ) THEN
                WRITE( MESG, '( A, I6, 2X, 5 A )' )
     &              'netCDF error number', IERR,
     &              'Error reading variable "', VLIST3( VID,FID ), 
     &              '" from file "', FLIST3( FID ), '"'
                    CALL M3MSG2( MESG )
                RDVARS = .FALSE.
                RETURN
            END IF          !  ierr nonzero:  NCVGTC() failed, or succeeded

        END IF  !  read all variables, or read a specific variable

        RDVARS = .TRUE.
        RETURN

C******************  FORMAT  STATEMENTS   ******************************

C...........   Error and warning message formats..... 91xxx

91010   FORMAT ( 3 ( A , :, 2X ) , I5, // )


        END FUNCTION RDVARS

