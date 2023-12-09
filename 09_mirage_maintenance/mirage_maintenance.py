#!/usr/bin/env python3

def find_next(sequence: list[int]) -> int:
    if all(n == 0 for n in sequence):
        return 0
    deltas = [sequence[i] - sequence[i-1] for i in range(1, len(sequence))]
    return sequence[-1] + find_next(deltas)

if __name__ == '__main__':
    with open('input.txt', 'r') as f:
        # parse input
        sequences = [[int(n) for n in line.strip().split()] for line in f.readlines()]

        # part 1
        result = sum(find_next(sequence) for sequence in sequences)
        print(result)

        # part 2
        result = sum(find_next(list(reversed(sequence))) for sequence in sequences)
        print(result)
