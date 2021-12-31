# Run R --no-save (has methods called)
function Rnosave {
  x="$1"
  mydir=`mktemp file.XXXX.sh`
  echo '#!/bin/bash' > $mydir
  echo "R --no-save < ${x}" >> $mydir
  shift
  qsub -cwd "$@" $mydir
  rm $mydir
}

# Run Rscript on an R Script
function Rbatch {
  x="$1"
  mydir=`mktemp file.XXXX.sh`
  echo '#!/bin/bash' > $mydir
  echo "Rscript ${x}" >> $mydir
  shift
  qsub -cwd "$@" $mydir
  rm $mydir
}
