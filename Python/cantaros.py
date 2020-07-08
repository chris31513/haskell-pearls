#Christian Rodrigo Garcia Gomez
#Decidí usar Python para este problema porque consideré que usar Haskell era, casi, matar una hormiga a cañonasos

import random
import sys
import gc
sys.setrecursionlimit(30000000)

actions = ["Llenar x", "Llenar y", "Llenar x con y", "Llenar y con x", "Vaciar x", "Vaciar y"]

def cantaros(x, y, z):
    if(z > x + y):

        raise Exception("Se solicitó una cantidad mayor")

    else:

        return choose_actions(x, y, z)

def choose_actions(x, y, z):
    gc.collect()

    if(z == x + y):

        return [actions[0], actions[1]]

    else:

        return choose_actions_aux(x, y, z, list(), [x,y, z])

def choose_actions_aux(x, y, z, act, inits):
    gc.collect()
    if(z == 0):

        print("\rAcciones tomadas: %s" % (act), end = '')
        print("")

        return act

    elif(z > inits[2]):

        return choose_actions_aux(inits[0], inits[1], inits[2], list(), inits)

    elif(z < 0):

        return choose_actions_aux(inits[0], inits[1], inits[2], list(), inits)

    elif(x < 0 or y < 0):

        return choose_actions_aux(inits[0], inits[1], inits[2], list(), inits)

    act.append(actions[random.randint(0, len(actions) - 1)])
    print("\rSeleccionando acciones: %s" % (act[-1]), end = '')

    if(act[-1] == "Llenar x"):

        return choose_actions_aux(0, y, (z - x), act, inits)

    elif(act[-1] == "Llenar y"):

        return choose_actions_aux(x, 0, (z - y), act, inits)

    elif(act[-1] == "Llenar x con y"):

        return choose_actions_aux((x - y), y, (z - (x - y)), act, inits)

    elif(act[-1] == "Llenar y con x"):

        return choose_actions_aux(x, (y - x), (z - (y - x)), act, inits)

    elif(act[-1] == "Vaciar x"):

        return choose_actions_aux(inits[0], y, (z + (inits[0] - x)), act, inits)

    elif(act[-1] == "Vaciar y"):

        return choose_actions_aux(x, inits[1], (z + (inits[1] - y)), act, inits)

cantaros(int(sys.argv[1]), int(sys.argv[2]), int(sys.argv[3]))