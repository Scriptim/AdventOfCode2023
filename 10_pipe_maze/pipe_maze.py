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
    '.': [],
}

tile_rights = {
    '|': ([(1, 0)], [(-1, 0)]),
    '-': ([(0, -1)], [(0, 1)]),
    'L': ([], [(-1, 0), (-1, 1), (0, 1)]),
    'J': ([], [(0, 1), (1, 1), (1, 0)]),
    '7': ([(1, 0), (1, -1), (0, -1)], []),
    'F': ([(0, -1), (-1, -1), (-1, 0)], []),
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

def loop_length(maze: list[list[str]], start: (int, int)) -> int:
    length = 0

    x, y = start
    dir_x, dir_y = tile_directions[maze[x][y]][0]
    while True:
        x, y = x + dir_x, y + dir_y
        dir1, dir2 = tile_directions[maze[x][y]]
        dir_x, dir_y = dir1 if (-dir_x, -dir_y) == dir2 else dir2

        length += 1

        if (x, y) == start:
            return length

def check_bounds(grid: list[list[str]], x: int, y: int) -> bool:
    return 0 <= x < len(grid) and 0 <= y < len(grid[x])

def flood_faces(faces: list[list[str]]) -> list[list[str]]:
    stable = False
    while not stable:
        stable = True

        for x, row in enumerate(faces):
            for y, face in enumerate(row):
                if face == 'X' or face is None:
                    continue

                for dx, dy in [(0, -1), (0, 1), (-1, 0), (1, 0)]:
                    if check_bounds(faces, x + dx, y + dy) and faces[x + dx][y + dy] is None:
                        faces[x + dx][y + dy] = face
                        stable = False

    return faces

def mark_faces(maze: list[list[str]], start: (int, int)) -> list[list[str]]:
    loop_faces = [[None for _ in row] for row in maze]
    right_turns = 0

    x, y = start
    dir_x, dir_y = tile_directions[maze[x][y]][0]
    while True:
        loop_faces[x][y] = 'X'  # part of loop

        # mark faces
        dir_index = tile_directions[maze[x][y]].index((dir_x, dir_y))
        right_tiles = tile_rights[maze[x][y]][dir_index]
        left_tiles = tile_rights[maze[x][y]][1 - dir_index]
        for dx, dy in [(0, -1), (0, 1), (-1, 0), (1, 0)]:
            if check_bounds(loop_faces, x + dx, y + dy) and loop_faces[x + dx][y + dy] is None:
                loop_faces[x + dx][y + dy] = 'O' if (dx, dy) in right_tiles else 'I'

        # keep track of loop orientation
        if not right_tiles:
            right_turns += 1
        elif not left_tiles:
            right_turns -= 1

        x, y = x + dir_x, y + dir_y
        dir1, dir2 = tile_directions[maze[x][y]]
        dir_x, dir_y = dir1 if (-dir_x, -dir_y) == dir2 else dir2

        if (x, y) == start:
            if right_turns > 0:
                for i, row in enumerate(loop_faces):
                    for j, tile in enumerate(row):
                        if tile == 'O':
                            loop_faces[i][j] = 'I'
                        elif tile == 'I':
                            loop_faces[i][j] = 'O'
            return flood_faces(loop_faces)

if __name__ == '__main__':
    with open('input.txt', 'r') as f:
        # parse input
        maze = [list(col) for col in zip(*f.read().splitlines())]
        start = find_start(maze)
        start_tile = start_tile_type(maze, start)
        maze[start[0]][start[1]] = start_tile

        # part 1
        result = loop_length(maze, start) // 2
        print(result)

        # part 2
        result = sum(1 for row in mark_faces(maze, start) for tile in row if tile == 'I')
        print(result)
