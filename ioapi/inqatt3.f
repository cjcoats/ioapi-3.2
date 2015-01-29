
        LOGICAL FUNCTION INQATT3( FNAME, VNAME, MXATTS, 
     &                            NATTS, ANAMES, ATYPES, ASIZES )

C***********************************************************************
C Version "$Id: inqatt3.f 100 2015-01-16 16:52:16Z coats $"
C BAMS/MCNC/EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
C (C) 2003-2012 Baron Advanced Meteorological Systems
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  subroutine body starts at line  133
C
C  FUNCTION:
C       returns list of attributes, their types, and sizes for the
C       file FNAME and variable VNAME (or ALLVAR3 for "global" file
C       attributes).
C
C  PRECONDITIONS REQUIRED:
C       Number of attributes at most MXVARS3( = 120)
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       netCDF
C
C  REVISION  HISTORY:
C       prototype 1/2002 by Carlie J. Coats, Jr., MCNC-EMC for I/O API v2.2
C
C       Modified 7/2003 by CJC:  bugfix -- clean up critical sections
C       associated with INIT3()
C
C       Modified 7/2012 by CJC:  bugfix associated with attribute-table
C       overflow.  Exclude standard attributes
C***********************************************************************

      IMPLICIT NONE

C...........   INCLUDES:

      INCLUDE 'NETCDF.EXT'      ! netCDF  constants
      INCLUDE 'PARMS3.EXT'      ! I/O API constants
      INCLUDE 'STATE3.EXT'      ! I/O API internal state


C...........   ARGUMENTS and their descriptions:

        CHARACTER*(*), INTENT(IN   ) :: FNAME             !  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME             !  vble name, or ALLVARS3
        INTEGER      , INTENT(IN   ) :: MXATTS            !  max number of attributes
        INTEGER      , INTENT(  OUT) :: NATTS             !  number of actual attributes
        CHARACTER*(*), INTENT(  OUT) :: ANAMES( MXATTS )  !  attribute names
        INTEGER      , INTENT(  OUT) :: ATYPES( MXATTS )  !  " types (M3REAL, M3INT, M3DBLE)
        INTEGER      , INTENT(  OUT) :: ASIZES( MXATTS )  !  " size/length

C...........   EXTERNAL FUNCTIONS and their descriptions:

        INTEGER, EXTERNAL :: INIT3, INDEX1, NAME2FID


C...........   PARAMETERs:  "standard-M3IO-attributes" table

        INTEGER     , PARAMETER :: NSTDATTS = 37
        CHARACTER*32, PARAMETER ::  STDATTS( NSTDATTS ) = 
     &  (/  'CDATE                            ',
     &      'CTIME                            ',
     &      'EXEC_ID                          ',
     &      'FILEDESC                         ',
     &      'FTYPE                            ',
     &      'FTYPE                            ',
     &      'GDNAM                            ',
     &      'GDTYP                            ',
     &      'HISTORY                          ',
     &      'IOAPI_VERSION                    ',
     &      'NCOLS                            ',
     &      'NLAYS                            ',
     &      'NROWS                            ',
     &      'NTHIK                            ',
     &      'NVARS                            ',
     &      'P_ALP                            ',
     &      'P_BET                            ',
     &      'P_GAM                            ',
     &      'SDATE                            ',
     &      'STIME                            ',
     &      'TSTEP                            ',
     &      'UPNAM                            ',
     &      'VAR-LIST                         ',
     &      'VGLVLS                           ',
     &      'VGTOP                            ',
     &      'VGTYP                            ',
     &      'WDATE                            ',
     &      'WTIME                            ',
     &      'XCELL                            ',
     &      'XCENT                            ',
     &      'XORIG                            ',
     &      'YCELL                            ',
     &      'YCENT                            ',
     &      'YORIG                            ',
     &      'long_name                        ',
     &      'units                            ',
     &      'var_desc                         '  /)

C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         I, N, F, V      !  subscripts for STATE3 arrays
        INTEGER         FID, VID        !  netCDF ID's
        INTEGER         IERR            !  netCDF error status return
        LOGICAL         EFLAG
        CHARACTER*16    FIL16           !  scratch file-name buffer
        CHARACTER*16    VAR16           !  scratch vble-name buffer
        CHARACTER*32    ANAME           !  scratch vble-name buffer
        CHARACTER*256   MESG            !  message-buffer


C***********************************************************************
C   begin body of subroutine  INQATT3

C.......   Check that Models-3 I/O has been initialized:

        EFLAG = .FALSE.
!$OMP   CRITICAL( S_INIT )
        IF ( .NOT. FINIT3 ) THEN
            LOGDEV = INIT3()
            EFLAG  = .TRUE.
        END IF          !  if not FINIT3
!$OMP   END CRITICAL( S_INIT )
        IF ( EFLAG ) THEN
            CALL M3MSG2(  'INQATT3:  I/O API not yet initialized.' )
            INQATT3 = .FALSE.
            RETURN
        END IF
        
        IF ( EFLAG ) RETURN

        F   = NAME2FID( FNAME )
        EFLAG = ( F .LE. 0 )

        IF ( LEN_TRIM( VNAME ) .GT. NAMLEN3 ) THEN
            EFLAG = .TRUE.
            MESG  = 'File "'// FNAME// '" Variable "'// VNAME // '"'
            CALL M3MSG2( MESG )
            WRITE( MESG, '( A, I10 )'  )
     &          'Max vble name length 16; actual:', LEN_TRIM( VNAME )
            CALL M3MSG2( MESG )
        END IF          !  if len( vname ) > 16
        
        IF ( EFLAG ) THEN
            MESG = 'Invalid variable or file name arguments'
            CALL M3WARN( 'INQATT3', 0, 0, MESG )
            INQATT3 = .FALSE.
            RETURN
        END IF

        VAR16 = VNAME   !  fixed-length-16 scratch copy of name
        FIL16 = FNAME   !  fixed-length-16 scratch copy of name

        IF ( F .EQ. 0 ) THEN  !  file not available

            MESG = 'File "'// FIL16 // '" not yet opened.'
            CALL M3WARN( 'INQATT3', 0, 0, MESG )
            INQATT3 = .FALSE.
            RETURN

        ELSE IF ( CDFID3( F ) .LT. 0 ) THEN

            MESG = 'File:  "' // FIL16 // '" is NOT A NetCDF file.'
            CALL M3WARN( 'INQATT3', 0, 0, MESG )
            INQATT3 = .FALSE.
            RETURN

        ELSE

            FID = CDFID3( F )

        END IF          !  if file not opened, or if readonly, or if volatile

C...........   Get ID for variable(s) to be inquired.
            
        IF ( VAR16 .EQ. ALLVAR3 ) THEN

            VID = NCGLOBAL

        ELSE

            V = INDEX1( VAR16, NVARS3( F ) , VLIST3( 1,F ) )
            IF ( V .EQ. 0 ) THEN
                MESG = 'Variable "'      // VAR16 //
     &                 '" not in file "' // FIL16 // '"'
                CALL M3WARN( 'INQATT3', 0, 0, MESG )
                INQATT3 = .FALSE.
                RETURN
            ELSE
                VID = VINDX3( V, F )
            END IF

        END IF          !  if VAR16 is 'ALL', or not.

C...........   Inquire attributes for this file and variable:
C...........   how many; names; sizes and types:
C...........   Somewhat tortured logic-structure due to the fact that
C...........   one can't execute a RETURN within a critical section.
           
!$OMP   CRITICAL( S_NC )

        IERR = NF_INQ_VARNATTS( FID, VID, NATTS )

        IF ( IERR .NE. NF_NOERR ) THEN

            MESG = 'Error inquiring attribute count for file "' //
     &             FNAME // '" and vble "' // VNAME // '"'
            CALL M3WARN( 'INQATT3', 0, 0, MESG )
            EFLAG = .TRUE.

        ELSE IF ( NATTS .GT. MXATTS+51 ) THEN

            MESG = 'Too many attributes for file "' // FNAME // 
     &             '" and vble "' // VNAME // '"'
            CALL M3WARN( 'INQATT3', 0, 0, MESG )
            EFLAG = .TRUE.

        ELSE
        
            N = 0
            
            DO  I = 1, NATTS

                IERR = NF_INQ_ATTNAME( FID, VID, I, ANAME )

                IF ( IERR .NE. NF_NOERR ) THEN

                    EFLAG = .TRUE.
                    MESG = 'Error inquiring att-name for file "' //
     &                     FNAME // '" and vble "' // VNAME // '"'
                    CALL M3MSG2( MESG )

                ELSE IF ( INDEX1( ANAME,NSTDATTS,STDATTS ) .GT. 0 ) THEN
                
                    CONTINUE        !!  skip over "standard" M3IO attributes

                ELSE IF ( N .LT. MXATTS ) THEN

                    N = N + 1
                    ANAMES( N ) = ANAME
                    IERR = NF_INQ_ATT( FID, VID, ANAME,
     &                                 ATYPES( N ), ASIZES( N ) )

                    IF ( IERR .NE. NF_NOERR ) THEN

                        EFLAG = .TRUE.
                        MESG = 'Error inquiring type&size: att "' //
     &                          ANAMES( I ) // 
     &                          '" for file "' // FNAME //
     &                          '" and vble "' // VNAME // '"'
                        CALL M3MSG2( MESG )

                    END IF

                ELSE

                    EFLAG = .TRUE.
                    MESG = 'Array overflow inquiring att-names' //
     &                     ' for file "'  // FNAME // 
     &                     '" and vble "' // VNAME // '"'
                    CALL M3MSG2( MESG )

                END IF

            END DO      !  end loop on attributes for this vble
        
        END IF

!$OMP   END CRITICAL( S_NC )

        IF ( EFLAG ) THEN
            INQATT3 = .FALSE.
            MESG = 'INQATT3:  Error inquiring attributes for file "' //
     &             FNAME // '" and vble "' // VNAME // '"'
            CALL M3WARN( 'INQATT3', 0, 0, MESG )
        ELSE
            INQATT3 = .TRUE.
        END IF          !  ierr nonzero:  NCAPTC) failed, ro not

        RETURN

        END

