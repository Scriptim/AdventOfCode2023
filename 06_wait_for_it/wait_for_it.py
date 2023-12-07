#!/usr/bin/env python3

from math import floor, sqrt

def num_beating_ways(time: int, distance: int) -> int:
    min_holding_time = floor((time - sqrt(time ** 2 - 4 * distance)) / 2 + 1)
    return time - 2 * min_holding_time + 1

if __name__ == '__main__':
    with open('input.txt', 'r') as f:
        # parse input
        times = [int(time) for time in f.readline().split()[1:]]
        distances = [int(dist) for dist in f.readline().split()[1:]]
        races = zip(times, distances)

        # part 1
        result = 1
        for time, distance in races:
            result *= num_beating_ways(time, distance)
        print(result)

        # part 2
        time = int(''.join(str(time) for time in times))
        distance = int(''.join(str(dist) for dist in distances))
        result = num_beating_ways(time, distance)
        print(result)
