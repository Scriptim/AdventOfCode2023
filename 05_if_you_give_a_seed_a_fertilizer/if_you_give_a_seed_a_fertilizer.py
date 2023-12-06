#!/usr/bin/env python3

if __name__ == '__main__':
    with open('input.txt', 'r') as f:
        # parse input
        seeds = [int(seed) for seed in f.readline().split()[1:]]
        maps = []
        while line := f.readline():
            if line[0].isalpha():
                maps.append([])
            elif line[0].isdigit():
                dst_range_start, src_range_start, range_len = [int(num) for num in line.split()]
                maps[-1].append((dst_range_start, src_range_start, range_len))

        # part 1
        result = float('inf')
        for seed in seeds:
            for mapping in maps:
                for dst_range_start, src_range_start, range_len in mapping:
                    if seed in range(src_range_start, src_range_start + range_len):
                        seed = dst_range_start + seed - src_range_start
                        break
            result = min(result, seed)
        print(result)
