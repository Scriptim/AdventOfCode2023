#!/usr/bin/env python3

def find_start(maze: list[list[str]]) -> (int, int):
    for i, row in enumerate(maze):
        for j, tile in enumerate(row):
            if tile == 'S':
                return (i, j)

tile_directions = {
    '|': [(0, -1), (0, 1)],
    '-': [(-1, 0), (1, 0)],
    'L': [(0, -1), (1, 0)],
    'J': [(-1, 0), (0, -1)],
    '7': [(-1, 0), (0, 1)],
    'F': [(0, 1), (1, 0)],
}

def start_tile_type(maze: list[list[str]], start: (int, int)) -> str:
    x, y = start
    connect_directions = []
    for dx in [-1, 0, 1]:
        if dx + x < 0 or dx + x >= len(maze):
            continue
        for dy in [-1, 0, 1]:
            if dy + y < 0 or dy + y >= len(maze[dx + x]):
                continue
            if dx == dy == 0:
                continue

            if (-dx, -dy) in tile_directions[maze[x + dx][y + dy]]:
                connect_directions.append((dx, dy))

    for tile, directions in tile_directions.items():
        if directions == connect_directions:
            return tile

if __name__ == '__main__':
    with open('input.txt', 'r') as f:
        # parse input
        maze = [list(col) for col in zip(*f.read().splitlines())]
        start = find_start(maze)
        start_tile = start_tile_type(maze, start)
        maze[start[0]][start[1]] = start_tile

        # part 1
        x, y = start
        dir_x, dir_y = tile_directions[start_tile][0]
        loop_length = 0
        while True:
            x, y = x + dir_x, y + dir_y
            dir1, dir2 = tile_directions[maze[x][y]]
            dir_x, dir_y = dir1 if (-dir_x, -dir_y) == dir2 else dir2
            loop_length += 1

            if (x, y) == start:
                break
        result = loop_length // 2
        print(result)
