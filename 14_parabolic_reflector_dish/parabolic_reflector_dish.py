#!/usr/bin/env python3

def tilt_north(platform: list[list[str]]) -> list[list[str]]:
    stable = True
    for row in range(len(platform)):
        for col in range(len(platform[row])):
            if platform[row][col] == 'O':
                new_row = row
                while new_row > 0 and platform[new_row - 1][col] == '.':
                    new_row -= 1
                if new_row != row:
                    platform[row][col], platform[new_row][col] = '.', 'O'
                    stable = False
    return platform if stable else tilt_north(platform)

def tilt_all(platform: list[list[str]]) -> list[list[str]]:
    for _ in range(4):
        platform = tilt_north(platform)
        platform = list(map(list, zip(*platform[::-1])))
    return platform

def total_load(platform: list[list[str]]) -> int:
    load = 0
    for i, row in enumerate(platform):
        for col in row:
            if col == 'O':
                load += len(platform) - i
    return load

if __name__ == '__main__':
    with open('input.txt', 'r') as f:
        # parse input
        platform = list(map(lambda line: list(line.strip()), f.readlines()))

        # part 1
        result = total_load(tilt_north(platform))
        print(result)

        # part 2
        def hash_platform(platform): return ''.join(''.join(row) for row in platform)
        cache = {}
        cycle = 0
        while hash_platform(platform) not in cache:
            cache[hash_platform(platform)] = cycle
            platform = tilt_all(platform)
            cycle += 1
        cycle_start = cache[hash_platform(platform)]
        for _ in range((1000000000 - cycle_start) % (cycle - cycle_start)):
            platform = tilt_all(platform)
        result = total_load(platform)
        print(result)
