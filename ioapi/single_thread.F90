    !================================================================
    ! @(#)$Header$
    ! EDSS/Models-3 I/O API.
    ! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
    ! (C) 2003-2010 by Baron Advanced Meteorological Systems, and
    ! (C) 2016 UNC Institute for the Environment.
    ! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    ! See file "LGPL.txt" for conditions of use.
    !.........................................................................
    !
    !     The next 3 functions: read3v, write3v, and shut3v
    !     need a OpenMP critical section around them to ensure
    !     thread safe behavior
    !     Note: open3v and desc3v are currently not thread-safe
    !     but open3 is used itself in critical sections, while
    !     desc3 should not be used in parallel regions because of the
    !     variables in the FDESC common block
    !
    !       Modified 9/2004 by CJC for virtual INTERP bug-fix: change to
    !       READ3V and WRITE3V interfaces
    !
    !       Modified 6/2016 by CJC:  INTENT; conditionally compiles
    !       only  if  IOAPICPL is defined:  Edward Anderson (Lockheed Martin,
    !       supporting the U.S. EPA) points out this is troublesome
    !       for shared-library construction otherwise.
    !
    !       Version 10/2021 by CJC:  free ".f90" source format for IOAPI-4.0
    !================================================================

#ifdef IOAPICPL

LOGICAL FUNCTION READ3V( FID, VID, SKIP, COUNT, JDATE, JTIME, BUFFER )

    IMPLICIT NONE

    INCLUDE 'PARMS3.EXT'
    INCLUDE 'STATE3.EXT'
    INCLUDE 'STATE3V.EXT'

    !.....Arguments

    INTEGER, INTENT(IN   ) :: FID, VID
    INTEGER, INTENT(IN   ) :: SKIP
    INTEGER, INTENT(IN   ) :: JDATE
    INTEGER, INTENT(IN   ) :: JTIME
    INTEGER, INTENT(IN   ) :: COUNT
    REAL   , INTENT(  OUT) :: BUFFER(*)

    !.....Function prototype

    LOGICAL, EXTERNAL :: READ3V_ST

    !$OMP CRITICAL( S_PVM )
    READ3V = READ3V_ST( PLIST3(FID), VLIST3(VID,FID), SKIP,     &
                        JDATE, JTIME, BUFFER, COUNT,            &
                        VTYPE3( VID,FID ) )
    !$OMP END CRITICAL( S_PVM )
    RETURN
END FUNCTION READ3V

    !================================================================

LOGICAL FUNCTION WRITE3V( FID, VID, JDATE, JTIME, BUFFER )

    IMPLICIT NONE

    INCLUDE 'PARMS3.EXT'
    INCLUDE 'STATE3.EXT'
    INCLUDE 'STATE3V.EXT'

    !.....Arguments

    INTEGER, INTENT(IN   ) :: FID, VID
    INTEGER, INTENT(IN   ) :: JDATE
    INTEGER, INTENT(IN   ) :: JTIME
    REAL   , INTENT(IN   ) :: BUFFER(*)

    !.....Function prototype

    LOGICAL, EXTERNAL :: WRITE3V_ST

    !$OMP CRITICAL( S_PVM )
    WRITE3V = WRITE3V_ST( PLIST3(FID), VLIST3(VID,FID),     &
                          JDATE, JTIME, BUFFER,             &
                          BSIZE3( FID )*NLAYS3( FID ),      &
                          VTYPE3( VID,FID ) )
    !$OMP END CRITICAL( S_PVM )
    RETURN
END FUNCTION WRITE3V

    !================================================================

LOGICAL FUNCTION SHUT3V()

    IMPLICIT NONE

    !.....Arguments

    !.....Function prototype

    LOGICAL, EXTERNAL :: SHUT3V_ST

    !$OMP CRITICAL( S_PVM )
    SHUT3V = SHUT3V_ST()
    !$OMP END CRITICAL( S_PVM )
    RETURN
END FUNCTION SHUT3V

    !================================================================

#endif      /*  ifdef IOAPICPL   */




