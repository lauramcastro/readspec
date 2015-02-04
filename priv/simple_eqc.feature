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
GIVEN I have the integer -2
AND I have the list []
THEN lists:member(-2, lists:delete(-2, [])) IS FALSE.

SCENARIO: Deleting an integer from a list should result in a list that does not 
      contain that integer.
GIVEN I have the integer -2
AND I have the list [7, -7, -1]
THEN lists:member(-2, lists:delete(-2, [7,-7,-1])) IS FALSE.

SCENARIO: Deleting an integer from a list should result in a list that does not 
      contain that integer.
GIVEN I have the integer -3
AND I have the list [4, -9, -14, -7]
THEN lists:member(-3, lists:delete(-3, [4,-9,-14,-7])) IS FALSE.

SCENARIO: Deleting an integer from a list should result in a list that does not 
      contain that integer.
GIVEN I have the integer 13
AND I have the list [6, 13, 11]
THEN lists:member(13, lists:delete(13, [6,13,11])) IS FALSE.
