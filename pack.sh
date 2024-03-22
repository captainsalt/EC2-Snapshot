SCRIPT=$(realpath "$0")
SCRIPTPATH=$(dirname "$SCRIPT")

(
    cd $SCRIPTPATH
    cd WorkScripts.Library
    dotnet pack -o '../NugetLocal'
)
