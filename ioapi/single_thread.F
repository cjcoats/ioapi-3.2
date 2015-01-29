C================================================================
C @(#)$Header$
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (C) 2003-2010 by Baron Advanced Meteorological Systems.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C
C     The next 3 functions: read3v, write3v, and shut3v
C     need a OpenMP critical section around them to ensure
C     thread safe behavior
C     Note: open3v and desc3v are currently not thread-safe
C     but open3 is used itself in critical sections, while
C     desc3 should not be used in parallel regions because of the 
C     variables in the FDESC common block
C
C       Modified 9/2004 by CJC for virtual INTERP bug-fix: change to
C       READ3V and WRITE3V interfaces
C================================================================

      LOGICAL FUNCTION READ3V( FID, VID, SKIP, COUNT,
     $                         JDATE, JTIME, BUFFER )

      IMPLICIT NONE

      INCLUDE 'PARMS3.EXT'
      INCLUDE 'STATE3.EXT'
      INCLUDE 'STATE3V.EXT'

C.....Arguments

      INTEGER        FID, VID
      INTEGER        SKIP
      INTEGER        JDATE
      INTEGER        JTIME
      INTEGER        COUNT
      REAL           BUFFER(*)

C.....Function prototype

      LOGICAL, EXTERNAL :: READ3V_ST

!$OMP CRITICAL( S_PVM )
      READ3V = READ3V_ST( PLIST3(FID), VLIST3(VID,FID), SKIP,
     $                       JDATE, JTIME, BUFFER, COUNT,
     $                       VTYPE3( VID,FID ) )
!$OMP END CRITICAL( S_PVM )
      RETURN
      END FUNCTION READ3V

C================================================================

      LOGICAL FUNCTION WRITE3V( FID, VID,
     $                          JDATE, JTIME, BUFFER )

      IMPLICIT NONE

      INCLUDE 'PARMS3.EXT'
      INCLUDE 'STATE3.EXT'
      INCLUDE 'STATE3V.EXT'

C.....Arguments

      INTEGER, INTENT(IN   ) :: FID, VID
      INTEGER, INTENT(IN   ) :: JDATE
      INTEGER, INTENT(IN   ) :: JTIME
      REAL   , INTENT(IN   ) :: BUFFER(*)

C.....Function prototype

      LOGICAL, EXTERNAL :: WRITE3V_ST

!$OMP CRITICAL( S_PVM )
      WRITE3V = WRITE3V_ST( PLIST3(FID), VLIST3(VID,FID),
     &                      JDATE, JTIME, BUFFER,
     &                      BSIZE3( FID )*NLAYS3( FID ),
     &                      VTYPE3( VID,FID ) )
!$OMP END CRITICAL( S_PVM )
      RETURN
      END FUNCTION WRITE3V

C================================================================

      LOGICAL FUNCTION SHUT3V()

      IMPLICIT NONE

C.....Arguments

C.....Function prototype

      LOGICAL, EXTERNAL :: SHUT3V_ST

!$OMP CRITICAL( S_PVM )
      SHUT3V = SHUT3V_ST()
!$OMP END CRITICAL( S_PVM )
      RETURN
      END FUNCTION SHUT3V

C================================================================




