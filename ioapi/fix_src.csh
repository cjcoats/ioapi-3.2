#!/bin/csh -f
#!/usr/local/bin/tcsh
#.........................................................................
# Version "$Id: fix_src.csh 100 2015-01-16 16:52:16Z coats $"
# EDSS/Models-3 I/O API.  Copyright (C) 2002 MCNC
# Distributed under the GNU Lesser PUBLIC LICENSE version 2.1
# See file "LGPL.txt" for conditions of use.
#.........................................................................
#  Script to take I/O API-style Fortran source that is compatible with both 
#  standard F77/F90 fixed source form and F90 free source form, and produce
#  a file compatible with F77/F90 extended-column fixed source form
#.........................................................................
#  USAGE
#       fix_src.csh <input-file> <output-file>
#.........................................................................
#  IBM NOTE
#       IBM's "csh" misbehaves, so you may need to substitute "tcsh"
#       as indicated above
#.........................................................................

switch ( $#argv )
    case 2
        breaksw
    default:
        echo "Usage:  fix_src.csh <input-file> <output-file>" 
        exit( 2 ) 
endsw

sed -e 's/ *& *$//' < $1 > $2

set foo=$status
if ( ${foo} != 0 )  echo "ERROR ${foo} in script fix_src.csh \n\n"

exit( ${foo} )

