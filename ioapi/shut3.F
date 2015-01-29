
        LOGICAL FUNCTION SHUT3 ( )

C***********************************************************************
C Version "$Id: shut3.F 100 2015-01-16 16:52:16Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (C) 2003-2010 by Baron Advanced Meteorological Systems.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  function body starts at line  69
C
C  FUNCTION:  Flushes and closes down all Models-3 files currently open.
C
C  RETURN VALUE:  TRUE iff it succeeds.
C
C  PRECONDITIONS REQUIRED:  none
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       netCDF2 interface to netCDF
C
C  REVISION  HISTORY:  
C       prototype 3/1992 by CJC
C
C       Modified  8/1995 by CJC to support CLOSE3()
C
C       Modified  5/1998 by CJC for OpenMP thread-safety
C
C       Modified  5/1999 by ALT for coupling-mode operation
C
C       Modified 10/2003 by CJC for I/O API version 3:  support for
C       native-binary BINFIL3 file type.
C
C       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
C***********************************************************************

      IMPLICIT NONE

C...........   INCLUDES:

        INCLUDE 'PARMS3.EXT'
        INCLUDE 'STATE3.EXT'
        INCLUDE 'NETCDF.EXT'
#ifdef IOAPICPL
        INCLUDE 'STATE3V.EXT'
#endif


C...........   EXTERNAL FUNCTIONS and their descriptions:

        INTEGER, EXTERNAL :: CLOSEBIN3


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         FILE            !  loop counter over files
        INTEGER         V               !  loop counter over vbles
        INTEGER         IERR            !  netCDF error status return

#ifdef IOAPICPL
        LOGICAL  SHUT3V
        EXTERNAL SHUT3V
#endif

C***********************************************************************
C   begin body of function  SHUT3

        SHUT3 = .TRUE.
        IF ( .NOT. FINIT3 ) THEN
            RETURN
        END IF

!$OMP CRITICAL( S_NC )
        DO  111  FILE = 1 , COUNT3

            IF( FLIST3( FILE ) .NE. CMISS3 ) THEN

                IF( CDFID3( FILE ) .GE. 0 ) THEN

                    CALL NCCLOS( CDFID3( FILE ), IERR )
                    IF ( IERR .NE. 0 ) THEN
                        WRITE( LOGDEV,91010 )
     &                  'Error closing netCDF file ',
     &                  'File name:  ' // FLIST3( FILE ) ,
     &                  'netCDF error number', IERR
                        SHUT3 = .FALSE.
                    END IF

                ELSE IF( CDFID3( FILE ) .EQ. BINFIL3 ) THEN

                    IERR = CLOSEBIN3( FILE )
                    IF ( IERR .EQ. 0 ) THEN
                        WRITE( LOGDEV,91010 )
     &                  'Error closing BINFIL3 file ',
     &                  'File name:  ' // FLIST3( FILE )
                        SHUT3 = .FALSE.
                    END IF
                END IF

            END IF              !  if flist not "missing"

            CALL BUFDEL3( FILE )
            FLIST3( FILE ) = CMISS3
            NLIST3( FILE ) = 0
            IFRST3( FILE ) = IMISS3
            DO  11 V = 1, NVARS3( FILE )
                VLIST3( V,FILE ) = CMISS3
                ILAST3( V,FILE ) = IMISS3
                LDATE3( V,FILE ) = IMISS3
                LTIME3( V,FILE ) = IMISS3
                NDATE3( V,FILE ) = IMISS3
                NTIME3( V,FILE ) = IMISS3
11          CONTINUE

111     CONTINUE        !  end loop on files
!$OMP END CRITICAL( S_NC )

        COUNT3 = IMISS3
        ILCNT3 = 0
        FINIT3 = .FALSE.

#ifdef  IOAPICPL
        SHUT3 = SHUT3V()
#endif

        RETURN

C******************  FORMAT  STATEMENTS   ******************************

C...........   Error and warning message formats..... 91xxx

91010   FORMAT ( //5X , '>>> WARNING in subroutine SHUT3 <<<',
     &            3 ( /5X , A , : ) , I5, // )

        END FUNCTION SHUT3

