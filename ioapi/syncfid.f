
        LOGICAL FUNCTION SYNCFID( FID )

C***********************************************************************
C Version "$Id: syncfid.f 100 2015-01-16 16:52:16Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
C (C) 2003-2010 by Baron Advanced Meteorological Systems.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  function  SYNCFID  starts at line  63
C
C  FUNCTION:
C       Flushes/syncs I/O API file with STATE3-index FID
C
C  RETURN VALUE:
C       TRUE iff it succeeds.
C
C  PRECONDITIONS REQUIRED:
C       FNAME exists and has been opened
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       netCDF; FLUSHBIN3
C
C  REVISION  HISTORY:  
C       Prototype 10/2003 by CJC for I/O API version 3
C
C       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
C***********************************************************************

        IMPLICIT NONE

C...........   INCLUDES:

        INCLUDE 'PARMS3.EXT'
        INCLUDE 'STATE3.EXT'
        INCLUDE 'NETCDF.EXT'


C...........   ARGUMENTS and their descriptions:

        INTEGER, INTENT(IN   ) :: FID             !  STATE3-index for the file


C...........   EXTERNAL FUNCTIONS and their descriptions:

        INTEGER, EXTERNAL :: FLUSHBIN3  !  sync for BINIO3 files


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         IERR            !  netCDF error status return
        CHARACTER*80    MESG
        INTEGER         N, F            !  for list files
        LOGICAL         EFLAG
        CHARACTER*16    FNAME


C***********************************************************************
C   begin body of function  SYNCFID

        EFLAG = .FALSE.

        IF( CDFID3( FID ) .GE. 0 ) THEN

!$OMP       CRITICAL( S_NC )
            IERR = NF_SYNC( CDFID3( FID ) )
!$OMP       END CRITICAL( S_NC )
            IF ( IERR .NE. 0 ) THEN
                FNAME = FLIST3( FID )
                MESG  = 'Error flushing netCDF file "' //
     &                    TRIM( FNAME ) // '"'
                CALL M3MSG2( MESG )
                WRITE( MESG, '( A, I9 )' ) 
     &                  'netCDF error number', IERR
                CALL M3MSG2( MESG )
                SYNCFID = .FALSE.
            ELSE
                SYNCFID = .TRUE.
            END IF      !  if ierr nonzero

        ELSE IF ( CDFID3( FID ) .EQ. BINFIL3 ) THEN

!$OMP       CRITICAL( S_NC )
            IF ( 0 .EQ. FLUSHBIN3( FID ) ) THEN
                FNAME = FLIST3( FID )             
                MESG  = 'Error flushing BINIO3 file "' //
     &                    TRIM( FNAME ) // '"'
                CALL M3MSG2( MESG )
                SYNCFID = .FALSE.
            ELSE
                SYNCFID = .TRUE.
            END IF
!$OMP       END CRITICAL( S_NC )

        ELSE IF ( CDFID3( FID ) .EQ. LSTFIL3 ) THEN

            EFLAG = .FALSE.
!$OMP       CRITICAL( S_NC )
            DO  N = 1, NLIST3( FID )
                F = ILIST3( N )
                IF ( CDFID3( F ) .GE. 0 ) THEN
                    IERR = NF_SYNC( CDFID3( F ) )
                    EFLAG = EFLAG .OR. ( IERR .NE. 0 )
                ELSE IF ( CDFID3( F ) .EQ. BINFIL3 ) THEN
                    EFLAG = EFLAG .OR. ( 0 .EQ. FLUSHBIN3( F ) )
                END IF
            END DO
!$OMP       END CRITICAL( S_NC )

            IF ( EFLAG ) THEN
                FNAME = FLIST3( FID )
                MESG  = 'Error flushing list file "' //
     &                    TRIM( FNAME ) // '"'
                CALL M3MSG2( MESG )
                SYNCFID = .FALSE.
            END IF

        ELSE

            SYNCFID = .TRUE.

        END IF  ! if cdfid(FID) positive, or =buffil3, or listfil3, or not

        RETURN

        END FUNCTION SYNCFID
        
