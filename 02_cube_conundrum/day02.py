#!/usr/bin/env python3

def parse_cube_set(cube_set: str) -> {str: int}:
    cube_set = cube_set.strip().split(', ')
    cube_set = (cubes.strip().split(' ') for cubes in cube_set)
    return { color: int(num) for [num, color] in cube_set}

def parse_game(line: str) -> (int, {str: int}):
    (game_id, cube_sets) = line.split(':', 1)

    game_id = int(game_id.split(' ')[-1])
    cube_sets = [parse_cube_set(cube_set) for cube_set in cube_sets.strip().split(';')]

    return (game_id, cube_sets)

if __name__ == '__main__':
    with open('input.txt', 'r') as f:
        lines = f.readlines()

        games = {}
        for line in lines:
            game_id, cube_sets = parse_game(line)
            games[game_id] = cube_sets

        # part 1
        max_cubes = {
            'red': 12,
            'green': 13,
            'blue': 14,
        }
        result = 0
        for game_id, cube_sets in games.items():
            if all(all(cubes[color] <= max_cubes[color] for color in cubes) for cubes in cube_sets):
                result += game_id
        print(result)

        # part 2
        result = 0
        for game_id, cube_sets in games.items():
            required_cubes = {color: 0 for color in ['red', 'green', 'blue']}
            for cube_set in cube_sets:
                for color, num in cube_set.items():
                    required_cubes[color] = max(required_cubes[color], num)
            power = 1
            for num in required_cubes.values():
                power *= num
            result += power
        print(result)
