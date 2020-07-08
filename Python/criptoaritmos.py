#Christian Rodrigo Garcia Gomez
#Decidí usar Python por que se me facilita mucho más pensar el código en este lenguaje. 
#Especificamente para este problema es más sencillo usar Python porque resulta más claro a la hora de trabajar con listas. 

import random
import sys
import gc
sys.setrecursionlimit(30000000)

def criptoaritmos(string):
    return generate_valid_tuples(string)

def split_string(string):
    gc.collect()
    if(check_valid_string(string)):
        return merge_splits(split_in_math_symbol(string), split_in_eq_symbol(string))
    else:
        raise Exception("La cadena '{}' no es valida".format(string))

def get_math_symbol(string):
    if("+" in string):
        return "+"
    elif("*" in string):
        return "*"
    elif("-" in string):
        return "-"
    else:
        raise Exception("La cadena '{}' no es valida".format(string))

def split_in_math_symbol(string):
    gc.collect()
    return string.replace(" ", "").split(get_math_symbol(string))

def split_in_eq_symbol(string):
    gc.collect()
    return split_in_math_symbol(string)[1].split("=")

def merge_splits(list1, list2):
    gc.collect()
    return list1[0:len(list1) - 1] + list2

def get_inits(string):
    gc.collect()
    return get_inits_aux(split_string(string), list())

def get_inits_aux(splitted_string, list_aux):
    gc.collect()
    if(splitted_string == []):
        return list_aux
    else:
        list_aux.append(splitted_string[0][0])
        return get_inits_aux(splitted_string[1:len(splitted_string)], list_aux)

def to_int(splitted_string, inits):
    gc.collect()
    return to_int_aux("".join(set("".join(splitted_string))), inits, list())

def to_int_aux(string_without_duplicates, inits, list_aux):
    gc.collect()
    for char in string_without_duplicates:

        if(char in inits):
            list_aux.append((char, random.randint(1, 9)))
        else:
            list_aux.append((char, random.randint(0, 9)))

    return list_aux

def check_valid_string(string):
    gc.collect()
    return ("+" in string or "*" in string or "-" in string) and "=" in string

def build_packages(ints, string):
    gc.collect()
    return build_packages_aux(ints, split_string(string), list(), list())

def build_packages_aux(ints, splitted_string, list_aux1, list_aux2):
    gc.collect()
    for string in splitted_string:
        for char in string:
            list_aux2.append(str(get_int(char, ints)))
        list_aux1.append("".join(list_aux2.copy()))
        list_aux2.clear()

    return list_aux1

def get_int(char, ints):
    gc.collect()
    for tup in ints:
        if(char == tup[0]):
            return tup[1]

def is_valid(ints, string):
    gc.collect()
    if("+" in string):
        return int(build_packages(ints, string)[0]) + int(build_packages(ints, string)[1]) == int(build_packages(ints, string)[2])
    elif('*' in string):
        return int(build_packages(ints, string)[0]) * int(build_packages(ints, string)[1]) == int(build_packages(ints, string)[2])
    elif('-' in string):
        return int(build_packages(ints, string)[0]) - int(build_packages(ints, string)[1]) == int(build_packages(ints, string)[2])


def generate_valid_tuples(string):
    gc.collect()
    return generate_valid_tuples_aux(string, list())

def generate_valid_tuples_aux(string, list_aux):
    gc.collect()
    while(len(list_aux) != 1):
        list_aux.append(to_int(split_string(string), get_inits(string)).copy())
        print("\rLista analizada: %s" % (list_aux[-1]), end = '')
        if(not is_valid(list_aux[len(list_aux) - 1], string)):
            del list_aux[-1]
    sys.stdout.write("\033[K")
    print("\rLista final: %s         " % (list_aux[-1]), end = '\n')
    return list_aux

criptoaritmos(input("Escribe tu palabra: "))