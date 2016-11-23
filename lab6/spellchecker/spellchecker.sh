#!/bin/bash

FILE_DOC_IN=$1
FILE_DICT_MAIN=$2
FILE_DICT_OUT=""
FILE_OUT="out.txt"
dict_ignored=""
word=""
file_out=""

process_document() {
    flag=0
    while IFS= read -r -N 1 -u9 character
    do
        if [[ $character = [a-zA-Z] ]]
        then
            word+=$character
            flag=1
        else
            if (($flag == 1))
            then
                is_know_dict=$(grep -ci $word $FILE_DICT_OUT)
                is_know_ignored=$(grep -ci $word $dict_ignored)
                let is_know=is_know_dict+is_know_ignored
                if (( $is_know == 0))
                then
                    echo "Palabra no reconocida: $word";
                    echo "Aceptar (a) - Ignorar (i) - Reemplazar (r): ";
                    while true;
                    do
                        read -p "Ingrese opcion: " op;
                        if [ "$op" == 'a' ]
                        then
                            echo $word >> $FILE_DICT_OUT
                            break
                        elif [ "$op" == 'i' ]
                        then
                            echo $word >> $dict_ignored
                            break
                        elif [ "$op" == 'r' ]
                        then
                            read -p "Ingrese una nueva palabra: " res;
                            word=$res
                            break
                        else
                            echo "ERROR: Opción Invalida.";
                        fi
                    done
                fi
                file_out+=$word
                word=""
                flag=0
            fi
            if [[ "$character" == $'\n' ]];
            then
                file_out+="\n"
            elif [[ "$character" == $' ' ]];
            then
                file_out+=" "
            else
                file_out+=$character
            fi
        fi
    done 9< "${FILE_DOC_IN}"
}

main() {
    if (( $# < 1 )); then
        echo "spellchecker.sh: nro de argumentos erroneo. Deben ser <documento> [<diccionario>]."
        exit
    elif (( $# == 1 )); then
        FILE_DICT_OUT="dict.txt"
        touch $FILE_DICT_OUT
    else
        FILE_DICT_OUT=$2
    fi
    dict_ignored="dict_ignored.txt"
    touch $dict_ignored
    FILE_DOC_IN=$1
    process_document
    touch $FILE_OUT
    rm $FILE_OUT
    # echo -e $file_out >> $FILE_OUT
    printf "$file_out" >> $FILE_OUT
    rm $dict_ignored
    echo "El documento '$FILE_DOC_IN' ha sido procesado. Resultados en out.txt"
}

main $FILE_DOC_IN $FILE_DICT_MAIN
