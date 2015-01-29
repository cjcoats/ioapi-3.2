
        SUBROUTINE  M3ABORT( FNAME, FNUM, IERR, MESSAGE )

C***********************************************************************
C Version "$Id: m3abort.F 100 2015-01-16 16:52:16Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (C) 2003-2010 by Baron Advanced Meteorological Systems.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  subroutine body starts at line  57
C
C  FUNCTION:  
C       Generate complex netCDF-related messages for I/O API,
C       and call NCABOR() on the related file
C
C  PRECONDITIONS REQUIRED:  
C       message fits on one line
C       netCDF 3.x or later
C
C  SUBROUTINES AND FUNCTIONS CALLED:  INIT3, TRIMLEN, NF_STRERROR
C
C  REVISION  HISTORY:   
C       Adapted  9/1999 by CJC from M3MESG()
C       Modified 9/1999 by CJC for AIX "flush_"
C       Modified 9/1999 by CJC for AIX "flush_"
C       Modified 7/2003 by CJC:  factor through M3MSG2(); declare TRIMLEN()
C       Bug-fix  2/2005 by CJC:  start with M3MSG2(MESSAGE) not M3MSG2(MESG)
C       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
C***********************************************************************

      IMPLICIT NONE

C...........   INCLUDES:

        INCLUDE 'IODECL3.EXT'
        INCLUDE 'NETCDF.EXT'


C...........   ARGUMENT and its description:

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !  file name
        INTEGER      , INTENT(IN   ) :: FNUM            !  netCDF file ID
        INTEGER      , INTENT(IN   ) :: IERR            !  netCDF error number
        CHARACTER*(*), INTENT(IN   ) :: MESSAGE


C.......   Scratch LOCAL VARIABLES

        CHARACTER*256   MESG       !  fixed-length buffer


C***********************************************************************
C   begin body of subroutine  M3MSG2

        CALL M3MSG2( MESSAGE )
        WRITE( MESG, '( A, :, I5, :, 2X, 3A )' ) 
     &          'netCDF error number', IERR, 
     &          'processing file "', TRIM( FNAME ), '"'
        CALL M3MSG2( MESG  )
        MESG = NF_STRERROR( IERR )
        CALL M3MSG2( MESG  )

        RETURN

        END SUBROUTINE  M3ABORT

