-module(constants).

initial_coins() -> round(math:pow(2, 48)) - 1.
finality() -> 26.%/docs/security.py explains why.
validators_elected_per_block() -> 54.
minimum_validators_per_block() -> 36.
