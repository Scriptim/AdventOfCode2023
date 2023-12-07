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

        # part 2
        seeds = [(seeds[i], seeds[i + 1]) for i in range(0, len(seeds), 2)]
        for mapping in maps:

            # divide seeds into intervals that are transformed uniformly
            breakpoints = set()
            for _, src_range_start, range_len in mapping:
                breakpoints.add(src_range_start)
                breakpoints.add(src_range_start + range_len)
            breakpoints = sorted(breakpoints)
            i = 0
            while i < len(seeds):
                seed_start, seed_len = seeds[i]
                for bp in breakpoints:
                    if bp in range(seed_start + 1, seed_start + seed_len):
                        seeds[i] = (seed_start, bp - seed_start)
                        seeds.append((bp, seed_start + seed_len - bp))
                        break
                i += 1

            # apply map
            new_seeds = []
            for seed_start, seed_len in seeds:
                transformed = False
                for dst_range_start, src_range_start, range_len in mapping:
                    if seed_start in range(src_range_start, src_range_start + range_len):
                        new_seeds.append((dst_range_start + seed_start - src_range_start, seed_len))
                        transformed = True
                        break
                if not transformed:
                    new_seeds.append((seed_start, seed_len))
            seeds = new_seeds

        result = min(seeds, key=lambda x: x[0])[0]
        print(result)
