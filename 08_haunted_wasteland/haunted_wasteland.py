#!/usr/bin/env python3

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

        # part 1
        result = 0
        current_node = 'AAA'
        while current_node != 'ZZZ':
            direction = 0 if directions[result % len(directions)] == 'L' else 1
            current_node = network[current_node][direction]
            result += 1
        print(result)
