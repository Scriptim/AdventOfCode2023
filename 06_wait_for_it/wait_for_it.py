#!/usr/bin/env python3

from math import floor, sqrt

if __name__ == '__main__':
    with open('input.txt', 'r') as f:
        # parse input
        times = [int(time) for time in f.readline().split()[1:]]
        distances = [int(dist) for dist in f.readline().split()[1:]]
        races = zip(times, distances)

        # part 1
        result = 1
        for time, distance in races:
            min_holding_time = floor((time - sqrt(time ** 2 - 4 * distance)) / 2 + 1)
            num_beating_ways = time - 2 * min_holding_time + 1
            result *= num_beating_ways
        print(result)
