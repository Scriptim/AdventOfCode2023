#!/usr/bin/env python3

from math import lcm

if __name__ == '__main__':
    with open('input.txt', 'r') as f:
        # parse input
        directions = f.readline().strip()
        f.readline()
        network = {}
        for line in f:
            [node, next_nodes] = line.strip().split(' = ')
            [left_node, right_node] = next_nodes[1:-1].split(', ')
            network[node] = (left_node, right_node)

        def direction(steps): return 0 if directions[steps % len(directions)] == 'L' else 1

        # part 1
        result = 0
        current_node = 'AAA'
        while current_node != 'ZZZ':
            current_node = network[current_node][direction(result)]
            result += 1
        print(result)

        # part 2
        z_distance = {}
        for starting_node in filter(lambda node: node[-1] == 'A', network.keys()):
            steps = 0
            current_node = starting_node
            while current_node[-1] != 'Z':
                current_node = network[current_node][direction(steps)]
                steps += 1
            z_distance[starting_node] = steps
        result = lcm(*z_distance.values())
        print(result)
