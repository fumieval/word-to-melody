# coding: utf-8
"""
隣接するアルファベットの対の出現頻度の分布から、よくある組み合わせは順次進行、あまりない組み合わせは跳躍分布になるようにアサインする
"""
import random
import string
from functools import partial
import operator
import math
from itertools import starmap, imap, chain, combinations

C = 0.7
N = 512
M = 0.005
B = 13

def choice(population):
    return int(len(population) * random.random() ** 2)
 
def crossover_uniform(a, b):
    c = dict(a.iteritems())
    d = dict(b.iteritems())
    for i in c:
        if random.random() < 0.5:
            c[i], d[i] = d[i], c[i]
    return c, d
 
def crossover_two(a, b):
    c = dict(a.iteritems())
    d = dict(b.iteritems())
    for i in random.sample(a.keys(), 2):
        c[i], d[i] = d[i], c[i]
 
    return c, d
    
def genetic(pick, crossing, mutation, fitness, initial, threshold=None, limit=None):
    generation = 0
    population = initial
    while True:
        fitnesses = zip(population, imap(fitness, population))
        fitnesses.sort(key=operator.itemgetter(1), reverse=True)
        if (threshold and fitnesses[0][1] >= threshold or
            limit and generation >= limit):
            return fitnesses[0]
        next_generation = []
        while len(next_generation) < len(population):
            v = random.random()
            if v < M:
                next_generation.append(mutation(fitnesses[pick(fitnesses)][0]))
            elif v < C + M:
                a = fitnesses[pick(fitnesses)][0]
                b = fitnesses[pick(fitnesses)][0]
                next_generation.extend(crossing(a, b))
            else:
                next_generation.append(fitnesses[pick(fitnesses)][0])
        population = next_generation
        generation += 1

def fitness(dist, indiv):
    result = 0
    table = [abs(indiv[a] - indiv[b]) for (a, b), v in dist]
    return sum(1 for i in starmap(operator.le, combinations(table, 2)) if i)
    
def mutate(f, indiv):
    m = dict(indiv.iteritems())
    for i in random.sample(m.keys(), B):
        m[i] == f()
    return m

def create_mapping():
    dom = string.lowercase
    cod_f = random.random
    dist = eval(raw_input())
    dist.sort(key=operator.itemgetter(1), reverse=True)
    population = [dict((i, cod_f()) for i in dom) for _ in xrange(N)]
    indiv = genetic(choice,
                    crossover_uniform,
                    partial(mutate, cod_f),
                    partial(fitness, dist),
                    population,
                    limit=50)
    print indiv

if __name__ == "__main__":
    create_mapping()