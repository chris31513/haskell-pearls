#Christian Rodrigo Garcia Gomez
#Decidí usar Python para este ejercicio porque pensé el problema como una matrix y me resulta más sencillo manejarlas con Python

import sys
import gc
sys.setrecursionlimit(30000000)

def gen_zero(string):
    if(not "_" in string):
        raise Exception("La cadena {} no es válida".format(string))
    else:
        return build_matrix(lambda x: x.split("_"), string).copy()

def build_matrix(f, string):
    return build_matrix_aux(f(string), list(), list())

def build_matrix_aux(string_splitted, matrix, row):
    for string in string_splitted:

        for char in string:
            row.append(char)

        matrix.append(row.copy())
        row.clear()

    return matrix

def evolution(generation):
    return evolution_aux(generation, False)

def evolution_aux(previous_matrix, changes):
    if(not changes):
        for i in range(0, len(previous_matrix)):

            for j in range(0, len(previous_matrix[i])):

                if(((not count_good_ducks(i, j, previous_matrix) == 2 or not count_good_ducks(i, j, previous_matrix) == 3)) and previous_matrix[i][j] == "G"):
                    del previous_matrix[i][j]
                    previous_matrix[i].insert(j, "B")
                    return evolution_aux(previous_matrix, True)

                elif(count_good_ducks(i, j, previous_matrix) == 3 and previous_matrix[i][j] == "B"):
                    del previous_matrix[i][j]
                    previous_matrix[i].insert(j, "G")
                    return evolution_aux(previous_matrix, True)

                if(i == len(previous_matrix) - 1 and j == len(previous_matrix[i]) - 1):
                    return evolution_aux(previous_matrix.copy(), True)

        return evolution_aux(previous_matrix.copy(), False)
    else:
        return previous_matrix


def count_good_ducks(x, y, matrix):
    return count_good_ducks_aux(x, y, matrix, 0)

def count_good_ducks_aux(x, y, matrix, n):
    for i in range(0, len(matrix)):

        for j in range(0, len(matrix[i])):
            if(neighborhoods(x, y, i, j) and matrix[i][j] == "G"):
                n += 1

    return n

def neighborhoods(x, y, i, j):
    if(i == x and j == y):
        return False
    elif(i == (x - 1) and j == y):
        return True
    elif(i == (x + 1) and j == y):
        return True
    elif(i == x and j == (y - 1)):
        return True
    elif(i == x and j == (y + 1)):
        return True
    elif(i == (x - 1) and j == (y - 1)):
        return True
    elif(i == (x - 1) and j == (y + 1)):
        return True
    elif(i == (x + 1) and j == (y - 1)):
        return True
    elif(i == (x + 1) and j == (y + 1)):
        return True
    else:
        return False

def generations(previous_generation):
    return generations_aux(previous_generation, list(), 0)

def generations_aux(generation, generations, number):
    gc.collect()

    print("\rGeneracion %s: " % (number), end = "\n")
    for row in generation:
        print("".join(row))

    if(len(generations) > 2 and generations[-1] == generations[-2]):
        return generations
    else:
        generations.append(make_a_copy(generation, list()))
        return generations_aux(make_a_copy(evolution(generation), list()), generations, (number + 1))

def make_a_copy(matrix, l):

    for row in matrix:
        l.append(row.copy())

    return l

generations(gen_zero(input("Escribe el orden de tus patitos: ")))