FEATURE: simple
  Simple QuickCheck properties

SCENARIO: Deleting an integer from a list should result in a list that does not
      contain that integer.
GIVEN I have the integer 0
AND I have the list []
THEN lists:member(0, lists:delete(0, [])) IS FALSE.

SCENARIO: Deleting an integer from a list should result in a list that does not
      contain that integer.
GIVEN I have the integer -1
AND I have the list []
THEN lists:member(-1, lists:delete(-1, [])) IS FALSE.

SCENARIO: Deleting an integer from a list should result in a list that does not
      contain that integer.
GIVEN I have the integer -3
AND I have the list [4]
THEN lists:member(-3, lists:delete(-3, [4])) IS FALSE.

SCENARIO: Deleting an integer from a list should result in a list that does not
      contain that integer.
GIVEN I have the integer 8
AND I have the list [2]
THEN lists:member(8, lists:delete(8, [2])) IS FALSE.

SCENARIO: Deleting an integer from a list should result in a list that does not
      contain that integer.
GIVEN I have the integer 8
AND I have the list [-2, -5, 5, 15]
THEN lists:member(8, lists:delete(8, [-2,-5,5,15])) IS FALSE.

SCENARIO: Deleting an integer from a list should result in a list that does not
      contain that integer.
GIVEN I have the integer 19
AND I have the list [7, -24, -18, 17, -8, -9, -8]
THEN lists:member(19, lists:delete(19, [7,-24,-18,17,-8,-9,-8])) IS FALSE.
