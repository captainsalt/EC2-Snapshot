SCRIPT=$(realpath "$0")
SCRIPTPATH=$(dirname "$SCRIPT")

(
    cd $SCRIPTPATH
    find . -name *.fsproj | xargs dotnet sln add
)