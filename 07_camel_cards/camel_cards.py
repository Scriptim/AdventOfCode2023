#!/usr/bin/env python3

def hand_type(hand: list[int]) -> int:
    if hand.count(hand[0]) == 5:
        return 6  # five of a kind
    elif any(hand.count(card) == 4 for card in hand):
        return 5  # four of a kind
    elif any(hand.count(card) == 3 for card in hand) and any(hand.count(card) == 2 for card in hand):
        return 4  # full house
    elif any(hand.count(card) == 3 for card in hand):
        return 3  # three of a kind
    elif len(set(card for card in hand if hand.count(card) == 2)) == 2:
        return 2  # two pair
    elif any(hand.count(card) == 2 for card in hand):
        return 1  # one pair
    else:
        return 0  # high card

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
        result = 0
        for i, hand in enumerate(sorted(hands, key=lambda hand: (hand_type(hand[0]), hand[0]))):
            result += hand[1] * (i + 1)
        print(result)
