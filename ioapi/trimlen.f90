
INTEGER  FUNCTION  TRIMLEN ( STRING )

    !***********************************************************************
    ! Version "$Id: trimlen.f90 203 2021-10-14 18:02:11Z coats $"
    ! EDSS/Models-3 I/O API.
    ! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
    ! (C) 2003-2010 Baron Advanced Meteorological Systems, and
    ! (C) 2014 UNC Institute for the Environment.
    ! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    ! See file "LGPL.txt" for conditions of use.
    !.........................................................................
    !  function body starts at line 40
    !
    !  FUNCTION:  return the effective length of argument CHARACTER*(*) STRING,
    !             after trailing blanks have been trimmed.
    !
    !  PRECONDITIONS REQUIRED:  none
    !
    !  SUBROUTINES AND FUNCTIONS CALLED:  none
    !
    !  REVISION  HISTORY:
    !       Prototype  8/1991 by CJC
    !       Version    2/1993 for CRAY by CJC
    !       Version    9/2014 by CJC:  Use F90 LEN_TRIM
    !       Version 10/2021 by CJC:  free ".f90" source format for IOAPI-4.0
    !***********************************************************************

    IMPLICIT NONE


    !...........   ARGUMENTS and their descriptions:

    CHARACTER*(*), INTENT( IN ) :: STRING


    !***********************************************************************
    !   begin body of function  TRIMLEN

    TRIMLEN = LEN_TRIM( STRING )

    RETURN

END FUNCTION  TRIMLEN

