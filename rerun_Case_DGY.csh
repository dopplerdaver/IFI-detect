#!/bin/csh
#
# Script to completely rerun a case
#
# Starting time:
# $1 = year
# $2 = month
# $3 = day
# $4 = hour
# $5 = min
# $6 = sec
#
# Ending time:
# $7 = year
# $8 = month
# $9 = day
# $10 = hour
# $11 = min
# $12 = sec
#
# $13 = radar name  (KCLE)
# $14 = version name (default is realtime).
#                     Output data is written here, so unique is good.
#
# $15 = host for input radar data.  Typically either nws1 (for KCLE) or
#       radia for data that has been pulled in and is on the radia  host
#
# NOTE that the sounding ascii data must be copied over from jlab-nima1
#

setenv INSTANCE rerun
setenv I0 $1$2$3$4$5$6
setenv I1 $7$8$9${10}${11}${12}
setenv VSN ${14}
source $CONTROL_DIR/params/env.${13}
setenv RADAR_DATA_HOST ${15}

cd $SCRIPT_HOME/params

#
# Change this to WYOMING or CIP as needed
#
#setenv ASCII_FORMAT SIMPLE
setenv ASCII_FORMAT WYOMING
ModelAsciiToSpdb -params ModelAsciiToSpdb.replay

cd $RADIA_HOME/params
RadiaFilt -params RadiaFilt.FreezingLevel -interval ${I0}  ${I1}
RadiaFilt -params RadiaFilt.PidMask  -interval ${I0}  ${I1}
RadiaFilt -params RadiaFilt.Radia.DGY -interval ${I0}  ${I1}

exit 0


