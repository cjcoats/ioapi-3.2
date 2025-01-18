
BLOCK DATA  INITBLK3

    !***********************************************************************
    ! Version "$Id: initblk3.f90 203 2021-10-14 18:02:11Z coats $"
    ! EDSS/Models-3 I/O API.
    ! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
    ! (C) 2003-2011 by Baron Advanced Meteorological Systems,
    ! (C) 2021 Carlie J. Coats, Jr..
    ! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    ! See file "LGPL.txt" for conditions of use.
    !.........................................................................
    !
    !  FUNCTION:  initialize I/O state for STATE3 common, Models-3 I/O API
    !
    !  REVISION  HISTORY:
    !       prototype 3/1992 by CJC
    !       Version   4/2011 by CJC:  initialize VGTYP3(1) for CHKBUF3()
    !       Version 10/2021 by CJC:  free ".f90" source format for IOAPI-4.0
    !***********************************************************************

    IMPLICIT NONE

    INCLUDE  'PARMS3.EXT'
    INCLUDE  'STATE3.EXT'

    DATA  COUNT3 / 0 /
    DATA  LOGDEV / IMISS3  /
    DATA  FINIT3 / .FALSE. /
    DATA  VGTYP3(1) / 0 /

END BLOCKDATA INITBLK3

