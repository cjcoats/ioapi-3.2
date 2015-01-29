
        LOGICAL FUNCTION  WRATT3( FNAME, VNAME, 
     &                            ANAME, ATYPE, AMAX, AVAL )
        IMPLICIT NONE
        LOGICAL WRATTC

C***********************************************************************
C Version "$Id: wratt3.f 100 2015-01-16 16:52:16Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (C) 2003-2010 by Baron Advanced Meteorological Systems and 
C (C) 2014 UNC Institute for the Environment
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  subroutine body starts at line   93
C   Entry  WRATTC  starts at line  110
C
C  FUNCTION:
C       Puts the attribute named ANAME with value AVAL( AMAX ) to the
C       variable VNAME in file FNAME (or to the global file attributes
C       if VNAME == ALLVAR3).  ATYPE should be one of M3REAL, M3INT, or
C       M3DBLE.
C       CHARACTER-string attributes use 
C               ENTRY   WRATTC( FNAME, VNAME, ANAME, CVAL )
C
C  PRECONDITIONS REQUIRED:
C       File must have been previously opened for output.
C       There are serious performance implications if the file has had
C       any output written to it (netCDF will do a behind-the-scenes
C       file-copy of the entire current contents.)
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
C       Modified 12/2004 by CJC:  bugfix for character attribute length; 
C       improved error messages; restructure NF_ENDDEF call.
C
C       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
C
C       Modified 12/2014 by CJC: logic cleanup; use NAME2FID()
C***********************************************************************

C...........   INCLUDES:

      INCLUDE 'NETCDF.EXT'      ! netCDF  constants
      INCLUDE 'PARMS3.EXT'      ! I/O API constants
      INCLUDE 'STATE3.EXT'      ! I/O API internal state


C...........   ARGUMENTS and their descriptions:

        CHARACTER*(*), INTENT(IN   ) :: FNAME         !  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME         !  variable name, or ALLVARS3
        CHARACTER*(*), INTENT(IN   ) :: ANAME         !  attribute name
        INTEGER      , INTENT(IN   ) :: ATYPE         !  attribute type (M3REAL, M3INT, M3DBLE)
        INTEGER      , INTENT(IN   ) :: AMAX          !  attribute dimensionality/size
        REAL         , INTENT(IN   ) :: AVAL( AMAX )  !  attribute value (numeric)
        CHARACTER*(*), INTENT(IN   ) :: CVAL          !  attribute value (character-string)


C...........   EXTERNAL FUNCTIONS and their descriptions:

        INTEGER, EXTERNAL :: INIT3      !  Initialize I/O API
        INTEGER, EXTERNAL :: INDEX1     !  look up names in name tables
        INTEGER, EXTERNAL :: NAME2FID   !  fname~~> fid lookup


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         F, V            !  subscripts for STATE3 arrays
        INTEGER         FID, VID        !  netCDF ID's
        INTEGER         FLEN, VLEN      !  name lengths for file, vble
        INTEGER         IERR            !  netCDF error status return
        INTEGER         ITYPE
        LOGICAL         EFLAG
        CHARACTER*16    FIL16           !  scratch file-name buffer
        CHARACTER*16    VAR16           !  scratch vble-name buffer
        CHARACTER*256   MESG            !  message-buffer


C***********************************************************************
C   begin body of subroutine  WRATT3

C...........   Check attribute type
            
        IF ( ( ATYPE .NE. NF_CHAR   ) .AND. 
     &       ( ATYPE .NE. NF_INT    ) .AND. 
     &       ( ATYPE .NE. NF_FLOAT  ) .AND. 
     &       ( ATYPE .NE. NF_DOUBLE ) ) THEN

            WRITE( MESG , '( 3 A, I10 )' ) 
     &           'WRATT3:  Attribute "'  , ANAME, 
     &           '" has unsupported type', ATYPE
            CALL M3WARN( 'WRATT3', 0, 0, MESG )
            WRATT3 = .FALSE.
            RETURN

        END IF

        ITYPE = ATYPE
        GO TO 111
        
        ENTRY WRATTC( FNAME, VNAME, ANAME, CVAL )
        ITYPE = NF_CHAR
        !! fall through to  111

111     CONTINUE

C.......   Preliminary checks:

        EFLAG = .FALSE.
        F     = NAME2FID( FNAME )
        VLEN  = LEN_TRIM( VNAME )

        IF ( F .LE. 0 ) THEN

            EFLAG = .TRUE.

        ELSE IF ( RONLY3( F ) ) THEN

            EFLAG = .TRUE.
            MESG = 'File:  "' // FIL16 // '" is READ-ONLY.'
            CALL M3MSG2( MESG )

        ELSE IF ( CDFID3( F ) .LT. 0 ) THEN

            EFLAG = .TRUE.
            MESG = 'File:  "' // FIL16 // '" is NOT A NetCDF file.'
            CALL M3MSG2( MESG )

        ELSE IF ( VLEN .GT. NAMLEN3 ) THEN

            EFLAG = .TRUE.
            MESG  = 'File "'// FNAME// '" Variable "'// VNAME//'"'
            CALL M3MSG2( MESG )
            WRITE( MESG, '( A, I10 )'  )
     &          'Max vble name length 16; actual:', VLEN
            CALL M3MSG2( MESG )

        ELSE

            FID = CDFID3( F )

        END IF          !  if len( vname ) > 16

C...........   Get ID for variable(s) to be written.

        VAR16 = VNAME   !  fixed-length-16 scratch copy of name
            
        IF ( VAR16 .EQ. ALLVAR3 ) THEN

            VID = NCGLOBAL

        ELSE

            V = INDEX1( VAR16, NVARS3( F ) , VLIST3( 1,F ) )
            IF ( V .EQ. 0 ) THEN
                EFLAG = .TRUE.
                MESG = 'Variable "'      // VAR16 //
     &                 '" not in file "' // FIL16 // '"'
                CALL M3MSG2( MESG )
            ELSE
                VID = VINDX3( V, F )
            END IF

        END IF          !  if VAR16 is 'ALL', or not.
        
        IF ( EFLAG ) THEN
            MESG = 'Invalid variable or file name arguments'
            CALL M3MESG( MESG )
            GO TO 999
        END IF


C...........   Put file into define mode; write the attribute; and
C...........   restore the file to data mode:
C...........   Somewhat tortured logic-structure due to the fact that
C...........   one can't execute a RETURN within a critical section.
            
!$OMP   CRITICAL( S_NC )

        IERR = NF_REDEF( FID )

        IF ( IERR .NE. NF_NOERR ) THEN

            WRITE( MESG, '( A, I10, 2X, 3 A )' )
     &      'Error', IERR, 'putting file "', FNAME, '" into define mode'
            CALL M3WARN( 'WRATT3', 0, 0, MESG )
            EFLAG = .TRUE.

        ELSE

            IF ( ITYPE .EQ. NF_CHAR ) THEN
                CALL NCAPTC( FID, VID, ANAME, NCCHAR,
     &                       LEN( CVAL ), CVAL, IERR )
            ELSE IF ( ITYPE .EQ. M3REAL ) THEN
                CALL NCAPT( FID, VID, ANAME, NF_FLOAT,
     &                      AMAX, AVAL, IERR )
            ELSE IF ( ITYPE .EQ. M3INT ) THEN
                CALL NCAPT( FID, VID, ANAME, NF_INT,
     &                      AMAX, AVAL, IERR )
            ELSE IF ( ITYPE .EQ. M3DBLE ) THEN
                CALL NCAPT( FID, VID, ANAME, NF_DOUBLE,
     &                      AMAX, AVAL, IERR )
            END IF

            IF ( IERR .NE. NF_NOERR ) THEN
                IF ( IERR .NE. NF_NOERR ) THEN
                    MESG = 'Error creating attribute "' // ANAME //
     &                     '" for file "' // FNAME //
     &                     '" and vble "' // VNAME // '"'
                    CALL M3WARN( 'WRATT3', 0, 0, MESG )
                    EFLAG = .TRUE.
                END IF
            END IF

            IERR = NF_ENDDEF( FID )
            IF ( IERR .NE. NF_NOERR ) THEN
                WRITE( MESG, '( A, I10, 2X, 3 A )' )
     &              'Error', IERR, 'putting file "', FNAME,
     &              '" back into data mode'
                CALL M3WARN( 'WRATT3', 0, 0, MESG )
                EFLAG = .TRUE.
            END IF

        END IF
        
!$OMP   END CRITICAL( S_NC )
        
        IF ( IERR .NE. NF_NOERR ) THEN
            EFLAG = .TRUE.
        END IF
        
999     CONTINUE

        IF ( EFLAG ) THEN
            MESG = 'Error writing attribute "' // ANAME //
     &             '" for file "' // FNAME //
     &             '" and vble "' // VNAME // '"'
            CALL M3WARN( 'WRATT3', 0, 0, MESG )
        END IF          !  ierr nonzero:  NCAPTC) failed
        IF ( ITYPE .EQ. NF_CHAR ) THEN
            WRATTC = ( .NOT. EFLAG )
        ELSE
            WRATT3 = ( .NOT. EFLAG )
        END IF

        RETURN

        END FUNCTION  WRATT3

