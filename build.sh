#!/bin/bash

R=R
INCREASE_BUILDNUM=0
CHECKARG=""
while [ $# -gt 0 ] ; do
  case "$1" in 
    -dev)
       R=Rdev
       shift 1 ;;
    -increase-buildnum)
      INCREASE_BUILDNUM=1
      shift 1 ;;
    *)
       CHECKARG="$CHECKARG $1"
       shift 1 ;;
  esac
done

echo "######## Removing old build information..."
rm -rf output

if [ $INCREASE_BUILDNUM -eq 1 ] ; then
echo "######## Increasing build number..."

   R --slave << EOF 
      dcf <- read.dcf('pkg/DESCRIPTION')
      v <- strsplit(dcf[1,'Version'],'.',fixed=TRUE)[[1]]
      if (length(v[4])<4) v[4] <- 0
      v[4] <- sprintf('%04d',as.numeric(v[4])+1)
      dcf[,'Version'] <- paste(v,collapse='.')
      write.dcf(dcf,file='pkg/DESCRIPTION')
EOF
fi

echo "######## Generate documentation..."
$R -q -e "devtools::document('pkg')"


echo "######## Building package in output..."
mkdir output
cd output
$R CMD build ../pkg
echo "######## Testing package with $CHECKARG ..."
for x in *.tar.gz 
do 
    $R CMD check $CHECKARG $x
done

echo "**BUILT USING $R"
$R --version

