#!/usr/bin/env python3

def hand_type(hand: list[int]) -> int:
    frequent_card = max((card for card in hand if card != 0), default=0, key=hand.count)
    hand = [frequent_card if card == 0 else card for card in hand]

    if hand.count(hand[0]) == 5:
        return 6  # five of a kind
    if any(hand.count(card) == 4 for card in hand):
        return 5  # four of a kind
    if any(hand.count(card) == 3 for card in hand) and any(hand.count(card) == 2 for card in hand):
        return 4  # full house
    if any(hand.count(card) == 3 for card in hand):
        return 3  # three of a kind
    if len(set(card for card in hand if hand.count(card) == 2)) == 2:
        return 2  # two pair
    if any(hand.count(card) == 2 for card in hand):
        return 1  # one pair
    return 0  # high card

def total_winnings(hands: list[tuple[list[int], int]]) -> int:
    result = 0
    for i, hand in enumerate(sorted(hands, key=lambda hand: (hand_type(hand[0]), hand[0]))):
        result += hand[1] * (i + 1)
    return result

if __name__ == '__main__':
    with open('input.txt', 'r') as f:
        # parse input
        hands = []
        for line in f:
            hand, bid = line.split()
            hand = ['23456789TJQKA'.index(card) + 2 for card in hand]
            bid = int(bid)
            hands.append((hand, bid))

        # part 1
        result = total_winnings(hands)
        assert result == 246795406
        print(result)

        # part 2
        result = total_winnings(([0 if card == 11 else card for card in hand], bid) for hand, bid in hands)
        assert result == 249356515
        print(result)
