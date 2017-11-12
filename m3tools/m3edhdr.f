
        PROGRAM  M3EDHDR

C***********************************************************************
C Version "$Id: m3edhdr.f 58 2017-11-12 16:33:22Z coats $"
C EDSS/Models-3 M3TOOLS.
C Copyright (C) 1992-2002 MCNC, (C) 1995-2002,2005-2013 Carlie J. Coats, Jr.,
C (C) 2002-2010 Baron Advanced Meteorological Systems. LLC., and
C (C) 2015 UNC Institute for the Environment.
C Distributed under the GNU GENERAL PUBLIC LICENSE version 2
C See file "GPL.txt" for conditions of use.
C.........................................................................
C  program body starts at line  112
C
C  FUNCTION:
C       Edit file header attributes.
C
C  PRECONDITIONS REQUIRED:
C       Models-3 I/O API input file.
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       GETEFILE, GETNUM, GETREAL, GETDBLE,GETYN, NEXTIME,
C       Models-3 I/O.
C
C  REVISION  HISTORY:
C       Prototype 5/1996 by CJC
C       Modified  9/1999 by CJC for enhanced portability
C       Modified  7/2001 by CJC to support new file type KFFILE3
C       Version  11/2001 by CJC for I/O API Version 2.1
C       Version   2/2005 by CJC: add option for editing file description
C       Version   6/2005 by CJC:  use3 M3PROMPT() for file description
C       to get around AIX FLUSH() problem
C       Version 02/2010 by CJC for I/O API v3.1:  Fortran-90 only;
C       USE M3UTILIO, and related changes.
C       Version 10/2015 by CJC for I/O API v3.2:  use NF_* netCDF-3 calls
C       instead of NC*() netCDF-2 calls.
C***********************************************************************

      USE M3UTILIO

      IMPLICIT NONE

C...........   EXTERNAL FUNCTIONS and their descriptions:

         INTEGER :: IARGC

C...........   INCLUDES:

        INCLUDE 'NETCDF.EXT'  !  netCDF parameter definitions
        INCLUDE 'STATE3.EXT'  !  I/O API internal data structures

C...........   PARAMETERS and their descriptions:

        CHARACTER*80, PARAMETER :: BLANK = ' '
        CHARACTER*16, PARAMETER :: PNAME = 'M3EDHDR'

C...........   LOCAL VARIABLES and their descriptions:

        INTEGER         ARGCNT  !  number of command-line args, from IARGC()

        CHARACTER*256   ENVBUF  !  value from command line arguments
        CHARACTER*16    INAME   !  logical name of the first  input file
        INTEGER         FNUM    !  state3 file index
        INTEGER         FID     !  netCDF file ID
        INTEGER         IERR   !  netCDF return status

        INTEGER         SDATE   !  starting date
        INTEGER         STIME   !  starting time
        INTEGER         TSTEP   !  time step
        INTEGER         VGTYP   !  vertical grid type
        REAL*8          P_ALP   !  first map projection descriptive parameter
        REAL*8          P_BET   !  second map projection descriptive parameter
        REAL*8          P_GAM   !  third map projection descriptive parameter
        REAL*8          XCENT   !  longitude for coordinate system origin
        REAL*8          YCENT   !  latitude for coordinate system origin
        REAL*8          XORIG   !  X-coordinate for lower-left grid corner
        REAL*8          YORIG   !  Y-coordinate for lower-left grid corner
        REAL*8          XCELL   !  X-coordinate grid cell size
        REAL*8          YCELL   !  Y-coordinate grid cell size
        REAL            VGTOP   !  model-top, for sigma coord types
        REAL            VGLVS( MXLAYS3 + 1 )

        INTEGER         K, L, V    !  counters
        INTEGER         CHOICE  !  E-menu choice
        INTEGER         STATUS  !  I/O status
        LOGICAL         CFLAG   !  changed-attribute flag

        LOGICAL         DFLAG   !  file in define-mode?

        CHARACTER*16    NAMBUF
        CHARACTER*80    SCRBUF
        CHARACTER*256   MESG

        CHARACTER*80    EMENU( 10 )
        DATA            EMENU
     &  /
     &  'Edit horizontal coordinate system parameters,' ,       !  choice 1
     &  'Edit horizontal grid parameters,' ,                    !  choice 2
     &  'Edit vertical   grid parameters,' ,                    !  choice 3
     &  'Edit time step structure parameters,' ,                !  choice 4
     &  'Edit variable names and descriptions,' ,               !  choice 5
     &  'Transform KM-based grid description to M-based,',      !  choice 6
     &  'Transform grid description from centers to corners,',  !  choice 7
     &  'Change the grid name, or',                             !  choice 8
     &  'Replace the file description text',                    !  choice 9
     &  'Quit the M3EDHDR program'                              !  choice 10
     &  /


C.........................................................................
C   begin body of program  M3EDHDR

        LOGDEV = INIT3()
        WRITE ( LOGDEV, '( 5X, A )' )
     &      ' ',
     &  'Program M3EDHDR to edit Models-3 I/O API file headers.', ' ',
     &  'Note that only certain attributes -- the grid name GDNAM3D,',
     &  'coordinate description parameters P_ALP3D, P_BET3D, P_GAM3D',
     &  'XCENT3D, YCENT3D, horizontal grid description parameters ',
     &  'XORIG3D, YORIG3D, XCELL3D, YCELL3D, vertical grid description',
     &  'parameters VGTYP, VGTOP3D, and VGLVS3D, the time-structure',
     &  'parameters SDATE3D, STIME3D, TSTEP3D for time-stepped files,',
     &  'and the descriptions, and units VNAME3D, UNITS3D, VDESC3D for',
     &  'variables can be changed.',
     &  ' ',
     &  'You need to have assigned a logical name to the physical ',
     &  'file name of the input file, according to Models-3',
     &  'conventions, using the operation "setenv <lname> <pname>".',
     &  ' ',
     &  'USAGE:  m3edhdr [INFILE]',
     &  '(and then answer the prompts).',
     &' ',
     &'See URL',
     &'https://www.cmascenter.org/ioapi/documentation/3.1/html#tools',
     &' ',
     &'Program copyright (C) 1992-2002 MCNC, (C) 1995-2013',
     &'Carlie J. Coats, Jr., and (C) 2002-2010 Baron Advanced',
     &'Meteorological Systems, LLC.  Released under Version 2',
     &'of the GNU General Public License. See enclosed GPL.txt, or',
     &'URL http://www.gnu.org/copyleft/gpl.html',
     &' ',
     &'Comments and questions are welcome and can be sent to',
     &' ',
     &'    Carlie J. Coats, Jr.    carlie@jyarborough.com',
     &'or',
     &'    UNC Institute for the Environment',
     &'    137 E. Franklin St. Suite 602 Room 613-C',
     &'    Campus Box 1105',
     &'    Chapel Hill, NC 27599-1105',
     &' ',
     &'Program version: ',
     &'$Id:: m3edhdr.f 58 2017-11-12 16:33:22Z coats                 $',
     &' '

        ARGCNT = IARGC()

        IF ( ARGCNT .GT. 1 ) THEN
            CALL M3EXIT( PNAME, 0, 0,
     &                   'USAGE:  m3edhdr [INFILE]', 2 )
        END IF

        IF ( ARGCNT .EQ. 0 ) THEN       !  get names from user

            INAME = PROMPTMFILE( 'Enter logical name for INPUT FILE',
     &                           FSRDWR3, 'INFILE', PNAME )

        ELSE                    !  else argcnt = 1

            CALL GETARG( 1, ENVBUF )
            INAME = ENVBUF( 1:16 )
            IF ( .NOT. OPEN3( INAME, FSRDWR3, PNAME ) ) THEN
                MESG = 'Could not open input file "'
     &                  // TRIM( INAME ) // '"'
                CALL M3EXIT( PNAME, 0, 0, MESG, 3 )
            END IF

        END IF          !  if argcnt > 2, =0, or not


C...........   Get and save input file description:

        FNUM = INDEX1( INAME, COUNT3, FLIST3 )
        FID  = CDFID3( FNUM )
        IF ( FID .LT. 0 ) THEN
            MESG = 'Input file "' // TRIM( INAME ) //
     &             '" not a physical netCDF file'
            CALL M3EXIT( PNAME, 0, 0, MESG, 3 )
        ELSE IF ( FTYPE3( FNUM ) .LT. KFEVNT3 ) THEN
            MESG = 'Input file "' // TRIM( INAME ) //
     &             '" not a _data_ file'
            CALL M3EXIT( PNAME, 0, 0, MESG, 3 )
        ELSE IF ( FTYPE3( FNUM ) .EQ.DGRAPH3  ) THEN
            MESG = 'Input file "' // TRIM( INAME )//
     &             '" of type DIRECTED GRAPH; not a _data_ file'
            CALL M3EXIT( PNAME, 0, 0, MESG, 3 )
        END IF

        IF ( .NOT. DESC3( INAME ) ) THEN
            MESG = 'Could not get description of input file "'
     &             // TRIM( INAME ) // '"'
            CALL M3EXIT( PNAME, 0, 0, MESG, 3 )
        END IF


C.......   Head of loop:  choose next edit operation.

        CHOICE = 0
        DFLAG  = .FALSE.

11      CONTINUE

            CHOICE = GETMENU( 10, 1 + MOD( CHOICE, 8 ),
     &                        'What do you want to edit', EMENU )


            IF ( CHOICE .EQ. 10 ) THEN      !  quit

                IF ( DFLAG ) THEN
                    IERR = NF_ENDDEF( FID )
                    IF ( IERR .NE. 0 ) THEN
                         WRITE( MESG, 94000 )
     &                      'Error', IERR,
     &                      'leaving DEFINE mode for "' //
     &                      TRIM( INAME ) // '"'
                        CALL M3WARN( PNAME, 0, 0, MESG )
                    END IF      !  if nf_enddef() failed
                END IF
                CALL M3EXIT( PNAME, 0, 0,
     &               'Program  M3EDHDR  completed successfully', 0 )

            ELSE IF ( .NOT. DFLAG ) THEN

                IERR = NF_REDEF( FID )
                IF ( IERR .NE. 0 ) THEN
                     WRITE( MESG, 94000 )
     &                 'Error', IERR, 'starting DEFINE mode for "' //
     &                 TRIM( INAME ) // '"'
                    CALL M3WARN( PNAME, 0, 0, MESG )
                END IF      !  if nf_redef() failed
                DFLAG = .TRUE.

            END IF      !  if choice=quot; else if not yet in define-mode


            IF ( CHOICE .EQ. 1 ) THEN   !  horizontal coordinate system:

                IF ( GDTYP3D .EQ. LATGRD3 ) THEN

                    CALL M3MESG(
     &              'File has a Lat-Lon grid; no parameters to edit' )

                ELSE    !  else not a lat-lon grid

                    P_ALP = GETDBLE( DBLE( BADVAL3 ), -DBLE( BADVAL3 ),
     &                               P_ALP3D,
     &                      'Enter first  coordinate parameter P_ALP' )

                    IF ( P_ALP .NE. P_ALP3D ) THEN

                        IERR = NF_PUT_ATT_DOUBLE( FID, NCGLOBAL,
     &                             'P_ALP', NCDOUBLE, 1, P_ALP )

                        IF ( IERR .NE. 0 ) THEN
                            WRITE( MESG, 94000 )
     &                      'Error', IERR, 'redefining P_ALP in "' //
     &                      TRIM( INAME ) // '"'
                            CALL M3WARN( PNAME, 0, 0, MESG )
                        ELSE
                            P_ALP3D = P_ALP
                        END IF      !  if NF_PUT_ATT_DOUBLE() failed

                    END IF      !  if p_alp changed

                    IF ( FTYPE3D .NE. UTMGRD3 ) THEN

                        P_BET = GETDBLE( DBLE( BADVAL3 ),
     &                                   -DBLE( BADVAL3 ),
     &                                   P_BET3D,
     &                  'Enter second coordinate parameter P_BET' )

                        IF ( P_BET .NE. P_BET3D ) THEN

                            IERR = NF_PUT_ATT_DOUBLE( FID, NCGLOBAL,
     &                             'P_BET', NCDOUBLE, 1, P_BET )

                            IF ( IERR .NE. 0 ) THEN
                                WRITE( MESG, 94000 )
     &                          'Error', IERR, 'redefining P_BET in "'
     &                          // TRIM( INAME ) // '"'
                                CALL M3WARN( PNAME, 0, 0, MESG )
                            ELSE
                                P_BET3D = P_BET
                            END IF      !  if NF_PUT_ATT_DOUBLE() failed

                        END IF  !  if p_BET changed

                        P_GAM = GETDBLE( DBLE( BADVAL3 ),
     &                                  -DBLE( BADVAL3 ),
     &                                   P_GAM3D,
     &                  'Enter third  coordinate parameter P_GAM' )

                        IF ( P_GAM .NE. P_GAM3D ) THEN

                            IERR = NF_PUT_ATT_DOUBLE( FID, NCGLOBAL,
     &                             'P_GAM', NCDOUBLE, 1, P_GAM )

                            IF ( IERR .NE. 0 ) THEN
                                WRITE( MESG, 94000 )
     &                          'Error', IERR, 'redefining P_GAM in "'
     &                          // TRIM( INAME ) // '"'
                                CALL M3WARN( PNAME, 0, 0, MESG )
                            ELSE
                                P_GAM3D = P_GAM
                            END IF      !  if NF_PUT_ATT_DOUBLE() failed

                        END IF  !  if P_GAM changed

                    END IF      !  if not UTM.

                    XCENT = GETDBLE( DBLE( BADVAL3 ), -DBLE( BADVAL3 ),
     &                               XCENT3D,
     &                      'Enter X-center parameter XCENT' )

                    IF ( XCENT .NE. XCENT3D ) THEN

                        IERR = NF_PUT_ATT_DOUBLE( FID, NCGLOBAL,
     &                             'XCENT', NCDOUBLE, 1, XCENT )

                        IF ( IERR .NE. 0 ) THEN
                            WRITE( MESG, 94000 )
     &                      'Error', IERR, 'redefining XCENT in "' //
     &                      TRIM( INAME ) // '"'
                            CALL M3WARN( PNAME, 0, 0, MESG )
                        ELSE
                            XCENT3D = XCENT
                        END IF      !  if NF_PUT_ATT_DOUBLE() failed

                    END IF      !  if xcent changed

                    YCENT = GETDBLE( DBLE( BADVAL3 ), -DBLE( BADVAL3 ),
     &                               YCENT3D,
     &                      'Enter Y-center parameter YCENT' )

                    IF ( YCENT .NE. YCENT3D ) THEN

                        IERR = NF_PUT_ATT_DOUBLE( FID, NCGLOBAL,
     &                             'YCENT', NCDOUBLE, 1, YCENT )

                        IF ( IERR .NE. 0 ) THEN
                            WRITE( MESG, 94000 )
     &                      'Error', IERR, 'redefining YCENT in "' //
     &                      TRIM( INAME ) // '"'
                            CALL M3WARN( PNAME, 0, 0, MESG )
                        ELSE
                            YCENT3D = YCENT
                        END IF      !  if NF_PUT_ATT_DOUBLE() failed

                    END IF      !  if ycent changed

                END IF          !  if lat-lon, or not

            ELSE IF ( CHOICE .EQ. 2 ) THEN      !  horizontal grid structure

                XORIG = GETDBLE( DBLE( BADVAL3 ), -DBLE( BADVAL3 ),
     &                           XORIG3D,
     &                  'Enter grid X-origin XORIG' )

                IF ( XORIG .NE. XORIG3D ) THEN

                    IERR = NF_PUT_ATT_DOUBLE( FID, NCGLOBAL,
     &                             'XORIG', NCDOUBLE, 1, XORIG )

                    IF ( IERR .NE. 0 ) THEN
                        WRITE( MESG, 94000 )
     &                  'Error', IERR, 'redefining XORIG in "' //
     &                  TRIM( INAME ) // '"'
                        CALL M3WARN( PNAME, 0, 0, MESG )
                    ELSE
                        XORIG3D = XORIG
                    END IF      !  if NF_PUT_ATT_DOUBLE() failed

                END IF  !  if xorig changed

                YORIG = GETDBLE( DBLE( BADVAL3 ), -DBLE( BADVAL3 ),
     &                           YORIG3D,
     &                  'Enter grid Y-origin  YORIG' )

                IF ( YORIG .NE. YORIG3D ) THEN

                    IERR = NF_PUT_ATT_DOUBLE( FID, NCGLOBAL,
     &                             'YORIG', NCDOUBLE, 1, YORIG )

                    IF ( IERR .NE. 0 ) THEN
                        WRITE( MESG, 94000 )
     &                  'Error', IERR, 'redefining YORIG in "' //
     &                  TRIM( INAME ) // '"'
                        CALL M3WARN( PNAME, 0, 0, MESG )
                    ELSE
                        YORIG3D = YORIG
                    END IF      !  if NF_PUT_ATT_DOUBLE() failed

                END IF  !  if yorig changed

                XCELL = GETDBLE( DBLE( BADVAL3 ), -DBLE( BADVAL3 ),
     &                           XCELL3D,
     &                  'Enter grid X-cell size XCELL' )

                IF ( XCELL .NE. XCELL3D ) THEN

                    IERR = NF_PUT_ATT_DOUBLE( FID, NCGLOBAL,
     &                             'XCELL', NCDOUBLE, 1, XCELL )

                    IF ( IERR .NE. 0 ) THEN
                        WRITE( MESG, 94000 )
     &                  'Error', IERR, 'redefining XCELL in "' //
     &                  TRIM( INAME ) // '"'
                        CALL M3WARN( PNAME, 0, 0, MESG )
                    ELSE
                        XCELL3D = XCELL
                    END IF      !  if NF_PUT_ATT_DOUBLE() failed

                END IF  !  if xcell changed

                YCELL = GETDBLE( DBLE( BADVAL3 ), -DBLE( BADVAL3 ),
     &                           YCELL3D,
     &                  'Enter grid Y-cell size YCELL' )

                IF ( YCELL .NE. YCELL3D ) THEN

                    IERR = NF_PUT_ATT_DOUBLE( FID, NCGLOBAL,
     &                             'YCELL', NCDOUBLE, 1, YCELL )

                    IF ( IERR .NE. 0 ) THEN
                        WRITE( MESG, 94000 )
     &                  'Error', IERR, 'redefining YCELL in "' //
     &                  TRIM( INAME ) // '"'
                        CALL M3WARN( PNAME, 0, 0, MESG )
                    ELSE
                        YCELL3D = YCELL
                    END IF      !  if NF_PUT_ATT_DOUBLE() failed

                END IF  !  if ycell changed

            ELSE IF ( CHOICE .EQ. 3 ) THEN      !  vertical grid structure:

                CALL M3MESG( 'Supported vertical grid types are' )
                CALL M3MESG( '1:   VGSGPH3-- hydrostatic sigma-P' )
                CALL M3MESG( '2:   VGSGPN3 -- nonhydrostatic sigma-P' )
                CALL M3MESG( '3:   VGSIGZ3 -- sigma-Z' )
                CALL M3MESG( '4:   VGPRES3 -- pressure (mb)' )
                CALL M3MESG( '5:   VGZVAL3 -- Z (m above terrain)' )
                CALL M3MESG( '6:   VGHVAL3 -- H (m above sea level)' )
                CALL M3MESG( '-9999:   "missing" or "not applicable"' )

                VGTYP = GETNUM( -999999999, 999999999, VGTYP3D,
     &                  'Enter vertical coordinate type VGTYP' )

                IF ( VGTYP .NE. VGTYP3D ) THEN

                    IERR = NF_PUT_ATT_INT( FID, NCGLOBAL,
     &                             'VGTYP', NF_INT, 1, VGTYP )

                    IF ( IERR .NE. 0 ) THEN
                        WRITE( MESG, 94000 )
     &                  'Error', IERR, 'redefining VGTYP in "' //
     &                  TRIM( INAME ) // '"'
                        CALL M3WARN( PNAME, 0, 0, MESG )
                    ELSE
                        VGTYP3D = VGTYP
                    END IF      !  if NF_PUT_ATT_INT() failed

                END IF  !  if vgtyp changed

                IF ( VGTYP .EQ. VGSGPH3  .OR.
     &               VGTYP .EQ. VGSGPN3  .OR.
     &               VGTYP .EQ. VGSIGZ3 ) THEN

                    VGTOP = GETREAL( BADVAL3,  -BADVAL3, VGTOP3D,
     &              'Enter vertical grid sigma-top VGTOP' )

                    IF ( VGTOP .NE. VGTOP3D ) THEN

                        IERR = NF_PUT_ATT_REAL( FID, NCGLOBAL,
     &                              'VGTOP', NCFLOAT, 1, VGTOP )

                        IF ( IERR .NE. 0 ) THEN
                            WRITE( MESG, 94000 )
     &                      'Error', IERR, 'redefining VGTOP in "'
     &                      // TRIM( INAME ) // '"'
                            CALL M3WARN( PNAME, 0, 0, MESG )
                        ELSE
                            VGTOP3D = VGTOP
                        END IF      !  if NF_PUT_ATT_REAL() failed

                    END IF      !  if VGTOP changed

                END IF          !  if sigma coordinate:  vgtop meaningful

                CFLAG = .FALSE.

                DO  22  L = 1, NLAYS3D + 1

                    WRITE( MESG,94000 ) 'Enter full-level', L-1
                    VGLVS( L ) = GETREAL( BADVAL3,  -BADVAL3,
     &                                    VGLVS3D( L ), MESG )

                    IF ( VGLVS( L ) .NE. VGLVS3D( L ) ) CFLAG = .TRUE.

22              CONTINUE

                IF ( CFLAG ) THEN

                    IERR = NF_PUT_ATT_REAL( FID, NCGLOBAL,
     &                          'VGLVLS', NCFLOAT, NLAYS3D+1, VGLVS )

                    IF ( IERR .NE. 0 ) THEN
                        WRITE( MESG, 94000 )
     &                  'Error', IERR, 'redefining VGLVS in "'
     &                  // TRIM( INAME ) // '"'
                        CALL M3WARN( PNAME, 0, 0, MESG )
                    ELSE
                        DO  23  L = 1, NLAYS3D + 1
                            VGLVS3D( L ) = VGLVS( L )
23                      CONTINUE
                    END IF      !  if NF_PUT_ATT_REAL() failed

                END IF  !  if VGLVS changed

            ELSE IF ( CHOICE .EQ. 4 ) THEN      !  time structure

                IF ( TSTEP3D .EQ. 0 ) THEN      !  time independent

                    CALL M3MESG(
     &              'File is time-independent; no parameters to edit' )

                ELSE IF ( TSTEP3D .GT. 0 ) THEN !  "normal" timestepped

                    CALL M3MESG( '"normal" file:  TSTEP > 0' )

                    SDATE = GETNUM( 0, 9999999, SDATE3D,
     &                      'Enter starting date SDATE (yyyyddd)' )

                    IF ( SDATE .NE. SDATE3D ) THEN

                        IERR = NF_PUT_ATT_INT( FID, NCGLOBAL, 'SDATE',
     &                              NF_INT, 1, SDATE )

                        IF ( IERR .NE. 0 ) THEN
                            WRITE( MESG, 94000 )
     &                      'Error', IERR, 'redefining SDATE in "' //
     &                      TRIM( INAME ) // '"'
                            CALL M3WARN( PNAME, 0, 0, MESG )
                        ELSE
                            SDATE3D = SDATE
                        END IF  !  if NF_PUT_ATT_INT() failed

                    END IF      !  if sdate changed

                    STIME = GETNUM( 0, 999999999, STIME3D,
     &                      'Enter starting time STIME (hhmmss)' )

                    IF ( STIME .NE. STIME3D ) THEN

                        IERR = NF_PUT_ATT_INT( FID, NCGLOBAL, 'STIME',
     &                              NF_INT, 1, STIME )

                        IF ( IERR .NE. 0 ) THEN
                            WRITE( MESG, 94000 )
     &                      'Error', IERR, 'redefining STIME in "' //
     &                      TRIM( INAME ) // '"'
                            CALL M3WARN( PNAME, 0, 0, MESG )
                        ELSE
                            STIME3D = STIME
                        END IF  !  if NF_PUT_ATT_INT() failed

                    END IF      !  if stime changed

                    TSTEP = GETNUM( 0, 99999999, TSTEP3D,
     &                      'Enter time step TSTEP (hhmmss)' )

                    IF ( TSTEP .NE. TSTEP3D ) THEN

                        IERR = NF_PUT_ATT_INT( FID, NCGLOBAL, 'TSTEP',
     &                              NF_INT, 1, TSTEP )

                        IF ( IERR .NE. 0 ) THEN
                            WRITE( MESG, 94000 )
     &                      'Error', IERR, 'redefining TSTEP in "' //
     &                      TRIM( INAME ) // '"'
                            CALL M3WARN( PNAME, 0, 0, MESG )
                        ELSE
                            TSTEP3D = TSTEP
                        END IF  !  if NF_PUT_ATT_INT() failed

                    END IF      !  if tstep changed

                ELSE                            !  "restart"

                    CALL M3MESG( '"restart" file:  TSTEP < 0' )

                    SDATE = GETNUM( 0, 9999999, SDATE3D,
     &                      'Enter starting date SDATE (yyyyddd)' )

                    IF ( SDATE .NE. SDATE3D ) THEN

                        IERR = NF_PUT_ATT_INT( FID, NCGLOBAL, 'SDATE',
     &                              NF_INT, 1, SDATE )

                        IF ( IERR .NE. 0 ) THEN
                            WRITE( MESG, 94000 )
     &                      'Error', IERR, 'redefining SDATE in "' //
     &                      TRIM( INAME ) // '"'
                            CALL M3WARN( PNAME, 0, 0, MESG )
                        ELSE
                            SDATE3D = SDATE
                        END IF  !  if NF_PUT_ATT_INT() failed

                    END IF      !  if sdate changed

                    STIME = GETNUM( 0, 999999999, STIME3D,
     &                      'Enter starting time STIME (hhmmss)' )

                    IF ( STIME .NE. STIME3D ) THEN

                        IERR = NF_PUT_ATT_INT( FID, NCGLOBAL, 'STIME',
     &                              NF_INT, 1, STIME )

                        IF ( IERR .NE. 0 ) THEN
                            WRITE( MESG, 94000 )
     &                      'Error', IERR, 'redefining STIME in "' //
     &                      TRIM( INAME ) // '"'
                            CALL M3WARN( PNAME, 0, 0, MESG )
                        ELSE
                            STIME3D = STIME
                        END IF  !  if NF_PUT_ATT_INT() failed

                    END IF      !  if stime changed

                    TSTEP = GETNUM( -9999999, -1, TSTEP3D,
     &                      'Enter time step TSTEP (-hhmmss)' )

                    IF ( TSTEP .NE. TSTEP3D ) THEN

                        IERR = NF_PUT_ATT_INT( FID, NCGLOBAL, 'TSTEP',
     &                              NF_INT, 1, TSTEP )

                        IF ( IERR .NE. 0 ) THEN
                            WRITE( MESG, 94000 )
     &                      'Error', IERR, 'redefining TSTEP in "' //
     &                      TRIM( INAME ) // '"'
                            CALL M3WARN( PNAME, 0, 0, MESG )
                        END IF  !  if NF_PUT_ATT_INT() failed

                    END IF      !  if tstep changed

                END IF          !  if tstep 0, positive, or negative

            ELSE IF ( CHOICE .EQ. 5 ) THEN      !  variable names, etc.

                DO  33  V = 1, NVARS3D

                    WRITE( MESG,94000 )
     &                      'Variable', V,
     &                      ':  "' //
     &                      VNAME3D( V ) // '" (' //
     &                      UNITS3D( V ) // ') has description:'
                    CALL M3MESG( MESG )
                    CALL M3MESG( VDESC3D( V ) )

                    CALL GETSTR( 'Enter new name for this variable',
     &                           VNAME3D( V ), NAMBUF )

                    IF ( NAMBUF .NE. VNAME3D( V ) ) THEN

                        IERR = NF_RENAME_VAR( FID, VINDX3( V,FNUM ),
     &                               NAMBUF )

                        IF ( IERR .NE. 0 ) THEN
                            WRITE( MESG, 94000 )
     &                      'Error', IERR, 'renaming variable in "' //
     &                      TRIM( INAME ) // '"'
                            CALL M3WARN( PNAME, 0, 0, MESG )
                        ELSE
                            VNAME3D( V ) = NAMBUF
                        END IF  !  if NF_RENAME_VAR() failed

                        IERR = NF_PUT_ATT_TEXT( FID, VINDX3( V,FNUM ),
     &                           'long_name', NAMLEN3, NAMBUF )
                        IF ( IERR .NE. 0 ) THEN
                            SCRBUF = 'setting "long-name" to "' //
     &                               TRIM( NAMBUF ) // '" in "' //
     &                               TRIM( INAME ) // '"'
                            WRITE( MESG, 94000 ) 'Error', IERR, SCRBUF
                            CALL M3WARN( PNAME, 0, 0, MESG )
                        END IF              !  ierr nonzero:  NF_PUT_ATT_TEXT() failed
                    END IF      !  if vname(v) changed

                    CALL GETSTR( 'Enter new units for this variable',
     &                           UNITS3D( V ), NAMBUF )

                    IF ( NAMBUF .NE. UNITS3D( V ) ) THEN

                        IERR = NF_PUT_ATT_TEXT( FID, VINDX3( V,FNUM ),
     &                           'units', NAMLEN3, NAMBUF )

                        IF ( IERR .NE. 0 ) THEN
                            WRITE( MESG, 94000 )
     &                      'Error', IERR, 'redefining UNITS for "' //
     &                      TRIM( VNAME3D( V ) ) // '" in "' //
     &                      TRIM( INAME ) // '"'
                            CALL M3WARN( PNAME, 0, 0, MESG )
                        ELSE
                            UNITS3D( V ) = NAMBUF
                        END IF  !  if NF_PUT_ATT_TEXT() failed

                    END IF      !  if units(v) changed

                    CALL GETSTR(
     &              'Enter new description for this variable',
     &                           VDESC3D( V ), SCRBUF )

                    IF ( SCRBUF .NE. VDESC3D( V ) ) THEN

                        IERR = NF_PUT_ATT_TEXT( FID, VINDX3( V,FNUM ),
     &                           'var_desc', MXDLEN3, SCRBUF )

                        IF ( IERR .NE. 0 ) THEN
                            WRITE( MESG, 94000 )
     &                      'Error', IERR,
     &                      'redefining DESCRIPTION for "' //
     &                      TRIM( VNAME3D( V ) ) // '" in "' //
     &                      TRIM( INAME ) // '"'
                            CALL M3WARN( PNAME, 0, 0, MESG )
                        ELSE
                            VDESC3D( V ) = SCRBUF
                        END IF  !  if NF_PUT_ATT_TEXT() failed

                    END IF      !  if vdesc(v) changed

33              CONTINUE        !  end loop revising variables

                IERR = NF_PUT_ATT_TEXT( FID, NCGLOBAL, 'VAR-LIST',
     &                       NAMLEN3 * NVARS3D, VNAME3D )
                IF ( IERR .NE. 0 ) THEN

                    WRITE( MESG,94000 )
     &              'Error', IERR, 'updating variable-names in "' //
     &              TRIM( INAME ) // '"'
                    CALL M3WARN( PNAME, 0, 0, MESG )

                END IF              !  ierr nonzero:  NF_PUT_ATT_TEXT() failed

            ELSE IF ( CHOICE .EQ. 6 ) THEN      !  KM-based ~~~> M-based

                IF ( GDTYP3D .EQ. LATGRD3 ) THEN

                    CALL M3MESG(
     &              'File has a Lat-Lon grid; no parameters to edit' )

                ELSE    !  else not a lat-lon grid

                    IERR = NF_PUT_ATT_DOUBLE( FID, NCGLOBAL,
     &                             'XORIG', NCDOUBLE, 1, XORIG3D )

                    IF ( IERR .NE. 0 ) THEN
                        WRITE( MESG, 94000 )
     &                  'Error', IERR, 'redefining XORIG in "' //
     &                  TRIM( INAME ) // '"'
                        CALL M3WARN( PNAME, 0, 0, MESG )
                    ELSE
                        XORIG3D = 1.0D3 * XORIG3D
                    END IF      !  if NF_PUT_ATT_DOUBLE() failed

                    IERR = NF_PUT_ATT_DOUBLE( FID, NCGLOBAL,
     &                             'YORIG', NCDOUBLE, 1, YORIG3D )

                    IF ( IERR .NE. 0 ) THEN
                        WRITE( MESG, 94000 )
     &                  'Error', IERR, 'redefining YORIG in "' //
     &                  TRIM( INAME ) // '"'
                        CALL M3WARN( PNAME, 0, 0, MESG )
                    ELSE
                        YORIG3D = 1.0D3 * YORIG3D
                    END IF      !  if NF_PUT_ATT_DOUBLE() failed

                    IERR = NF_PUT_ATT_DOUBLE( FID, NCGLOBAL,
     &                     'XCELL', NCDOUBLE, 1, 1.0D3 * XCELL3D )

                    IF ( IERR .NE. 0 ) THEN
                        WRITE( MESG, 94000 )
     &                  'Error', IERR, 'redefining XCELL in "' //
     &                  TRIM( INAME ) // '"'
                        CALL M3WARN( PNAME, 0, 0, MESG )
                    ELSE
                        XCELL3D = 1.0D3 * XCELL3D
                    END IF      !  if NF_PUT_ATT_DOUBLE() failed

                    IERR = NF_PUT_ATT_DOUBLE( FID, NCGLOBAL,
     &                     'YCELL', NCDOUBLE, 1, 1.0D3 * YCELL3D )

                    IF ( IERR .NE. 0 ) THEN
                        WRITE( MESG, 94000 )
     &                  'Error', IERR, 'redefining YCELL in "' //
     &                  TRIM( INAME ) // '"'
                        CALL M3WARN( PNAME, 0, 0, MESG )
                    ELSE
                        YCELL3D = 1.0D3 * YCELL3D
                    END IF      !  if NF_PUT_ATT_DOUBLE() failed

                END IF          !  if lat-lon, or not

            ELSE IF ( CHOICE .EQ. 7 ) THEN      !  centers ~~~> corner based

                XORIG = XORIG - 0.5D0 * XCELL3D

                IERR = NF_PUT_ATT_DOUBLE( FID, NCGLOBAL,
     &                             'XORIG', NCDOUBLE, 1, XORIG )

                IF ( IERR .NE. 0 ) THEN
                    WRITE( MESG, 94000 )
     &              'Error', IERR, 'redefining XORIG in "' //
     &              TRIM( INAME ) // '"'
                    CALL M3WARN( PNAME, 0, 0, MESG )
                ELSE
                    XORIG3D = XORIG
                END IF      !  if NF_PUT_ATT_DOUBLE() failed

                YORIG = YORIG - 0.5D0 * YCELL3D

                IERR = NF_PUT_ATT_DOUBLE( FID, NCGLOBAL,
     &                             'YORIG', NCDOUBLE, 1, YORIG )

                IF ( IERR .NE. 0 ) THEN
                    WRITE( MESG, 94000 )
     &              'Error', IERR, 'redefining YORIG in "' //
     &              TRIM( INAME ) // '"'
                    CALL M3WARN( PNAME, 0, 0, MESG )
                ELSE
                    YORIG3D = YORIG
                END IF      !  if NF_PUT_ATT_DOUBLE() failed

            ELSE IF ( CHOICE .EQ. 8 ) THEN      !  change grid name

                CALL GETSTR( 'Enter new name for this variable',
     &                       VNAME3D( V ), NAMBUF )

                IF ( NAMBUF .NE. GDNAM3D ) THEN

                    IERR = NF_PUT_ATT_TEXT( FID, NCGLOBAL, 'GDNAM',
     &                           NAMLEN3, NAMBUF )

                    IF ( IERR .NE. 0 ) THEN
                        WRITE( MESG, 94000 )
     &                  'Error', IERR, 'renaming grid in "' //
     &                  TRIM( INAME ) // '"'
                        CALL M3WARN( PNAME, 0, 0, MESG )
                    ELSE
                        GDNAM3D = NAMBUF
                    END IF  !  if NF_PUT_ATT_TEXT() failed


                END IF  !  if nambuf != gdnam3d

            ELSE IF ( CHOICE .EQ. 9 ) THEN      !  get new file description

                MESG = 'Enter new file description one line at a time'
                CALL M3MESG( MESG )
                MESG = 'Use blank line (or RETURN) to finish'
                CALL M3MESG( MESG )
                CALL M3MESG( ' ' )

                DO L = 1, MXDESC3

                    K = MAX( 1, LEN_TRIM( FDESC3D( L ) ) )
                    WRITE( MESG,'( A, I3, 2X, 4 A )' )
     &                'Enter description line', L,
     &                '["', FDESC3D( L )( 1:K ), '"]', '>> '
                    CALL M3PROMPT( MESG, SCRBUF, IERR )

                    IF ( STATUS .GT. 0 ) THEN
                        WRITE( MESG, '( A, I10, 2X, A )' )
     &                    'Error', STATUS, 'reading response'
                        CALL M3MESG( MESG )
                    ELSE IF ( SCRBUF .NE. BLANK ) THEN
                        FDESC3D( L ) = SCRBUF
                    ELSE
                        GO TO 44
                    END IF

                END DO

44              CONTINUE        !  exit from loop

                DO K = L+1, MXDESC3
                    FDESC3D( K ) = BLANK
                END DO

                IERR = NF_PUT_ATT_TEXT( FID, NCGLOBAL, 'FILEDESC',
     &                       MXDLEN3 * MXDESC3, FDESC3D )

                    IF ( IERR .NE. 0 ) THEN
                        WRITE( MESG, 94000 )
     &                  'Error', IERR, 'changing FDESC3D in "' //
     &                  TRIM( INAME ) // '"'
                        CALL M3WARN( PNAME, 0, 0, MESG )
                    END IF  !  if NF_PUT_ATT_TEXT() failed

            END IF  !  end of choices to be processed

            GO TO  11   !  to head of edit-choices loop



C******************  FORMAT  STATEMENTS   ******************************

C...........   Internal buffering formats............ 94xxx

94000   FORMAT ( 100 ( A, :, I7, :, 2X ) )


        END  PROGRAM  M3EDHDR

