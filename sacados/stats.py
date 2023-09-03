import matplotlib.pyplot as plt
import numpy as np

def attack_stats(attack):
    f = 'attack'+str(attack)+".txt"
    if attack == 1:
        couleur='blue'
        text = 'attack_LO/n='
    elif attack == 2:
        couleur = 'orange'
        text = 'attack_MOS/n='
    else:
        couleur = 'green'
        text = 'attack_JS/n='
    densite = []
    precision = []
    ind = 0
    with open(f, 'r') as f:
        for line in f:
            if ind == 0:
                text += line
                ind+=1
                continue
            l = line.split("\t")
            densite.append(float(l[0]))
            precision.append(int(l[1]))
    plt.plot(densite, precision, color=couleur, label=text)

""" #attack_stats(1)
attack_stats(2)
#attack_stats(3)
plt.legend()
plt.show() """

def attack_stats_bis(attack):
    f = 'attack'+str(attack)+"_bis.txt"
    if attack == 1:
        couleur='blue'
        text = 'attack_LO/n='
    else:
        couleur = 'green'
        text = 'attack_JS/n='
    densite = []
    precision = []
    ind = 0
    with open(f, 'r') as f:
        for line in f:
            if ind == 0:
                text += line
                ind+=1
                continue
            l = line.split("\t")
            densite.append(float(l[0]))
            precision.append(int(l[1]))
    plt.plot(densite, precision, color=couleur, label=text)

attack_stats_bis(1)
attack_stats_bis(3)
X = np.linspace(-10,110)
plt.plot([0.64 for x in X],X , 'r--', label='d=0.64')
plt.plot([0.94 for x in X], X, 'r-', label='d=0.94')
plt.legend()
plt.show()