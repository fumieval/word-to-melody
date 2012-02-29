# coding: utf-8

import random
import string
from functools import partial
import operator
import math
from itertools import starmap, imap, chain

N = 256 #個体数
C = 0.5 #交叉率
M = 0.005 #突然変異率
B = 13 #突然変異で変化する点数

L = 6 # 1オクターブの音の数
R = 0.7 #望ましい順次進行の割合

def choice(population): #ランキング選択
    return int(len(population) * random.random() ** 2)
 
def crossover_uniform(a, b): #一様交叉
    c = dict(a.iteritems())
    d = dict(b.iteritems())
    for i in c:
        if random.random() < 0.5:
            c[i], d[i] = d[i], c[i]
    return c, d
 
def crossover_two(a, b): #二点交叉
    c = dict(a.iteritems())
    d = dict(b.iteritems())
    for i in random.sample(a.keys(), 2):
        c[i], d[i] = d[i], c[i]
 
    return c, d
    
def genetic(pick, crossing, mutation, fitness, initial, threshold=None, limit=None, stable=None):
    generation = 0
    population = initial
    while True:
        fitnesses = zip(population, imap(fitness, population))
        fitnesses.sort(key=operator.itemgetter(1), reverse=True)
        if (threshold and fitnesses[0][1] >= threshold or
            limit and generation >= limit):
            return fitnesses[0]
        
        print fitnesses[0][1]
        
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

def melody_value(rank, m): #旋律を評価する
    xs = list(m)
    it = iter(xs)
    j = next(it)
    c = 0
    d = 0
    r = 0
    for i in it:
        d = abs(i - j)
        if d == 0:
            if r != 0:
                return 0.0
            r += 1
        elif d == 1: #順次進行
            c += 1
        elif d <= L: #跳躍進行
            d += 1
        else: #1オクターブ以上の跳躍は禁則
            return 0.0
        j = i
    if xs[-1] % L == 0: #最後が主音
        if len(xs) >= 2 and xs[-2] == xs[-1] - 1: #最後から二番目が導音
            bonus = 2.5
        else:
            bonus = 2.0
    else:
        bonus = 0.8
    
    if c + d:
        return (1 - abs(float(c) / (c + d) - R)) * bonus * (1.5 - rank / 4000)
    else:
        return 0.0

def fitness(words, indiv): #適応度
    # 音をまんべんなく使うほど値が高くなる
    # 各単語における適応度の総和に比例
    return sum(melody_value(i, imap(indiv.__getitem__, word)) for i, word in enumerate(words)) * len(set(indiv.itervalues()))

def mutate(f, indiv):
    m = dict(indiv.iteritems())
    for i in random.sample(m.keys(), B):
        m[i] == f()
    return m

def create_mapping():
    import sys
    dom = string.lowercase
    cod_f = lambda: random.randint(-2, 7)
    
    words = map(str.lower, sys.stdin.read().splitlines())
    
    population = [dict((i, cod_f()) for i in dom) for _ in xrange(N)]
    indiv = genetic(choice,
                    crossover_uniform,
                    partial(mutate, cod_f),
                    partial(fitness, words),
                    population,
                    limit=25)
    result = indiv[0].items()
    result.sort(key=operator.itemgetter(1))
    print result

if __name__ == "__main__":
    # 標準入力から改行区切りの単語のリストを読み込み、音の割り当てを表示する
    create_mapping()