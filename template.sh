RED="0;31m"
GREEN="0;32m"
YELLOW="0;33m"
BLUE="0;34m"

function printf()
{
    local level=$1
    local msg=$2

    if [[ ${level} = "EMERGE" ]];then
        echo  -e "\033[${RED} EMERGE: '${msg}'\033[00m"
    elif [[ ${level} = "WARNING" ]];then
        echo  -e "\033[${YELLOW} WARNING: '${msg}'\033[00m"
    elif [[ ${level} = "MESSAGE" ]];then
        echo  -e "\033[${GREEN} MESSAGE: '${msg}'\033[00m"
    fi
}

printf EMERGE  "hello world"
printf WARNING "hello world"
printf MESSAGE "hello world"
