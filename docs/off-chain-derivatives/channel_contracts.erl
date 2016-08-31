-module(channel_contracts).
-export([test/0]).



















-record(participate_oracle, {id, sign, oracle_id, choice_id, amount}). %this is a contract that probably stays unused. it is a threat.
-record(commit_decision, {id, sign, oracle_id, choice_id, salted_decision_hash}). %this contract gets unlocked 
-record(reveal, {id, sign, oracle_id, choice_id, decision}).%this gets unlocked.
-record(double_reveal, {id, sign, criminal_id, oracle_id, choice_id, decision}). %this contract probably isn't unlocked
-record(decide_honest, {id, sign, oracle_id, choice_id, decision}).%this gets unlocked. money from dishonest reports is redistributed to the honest. some dishonest money is deleted.
-record(double_decide_honest, {id, sign, criminal_id, oracle_id, choice_id, reveal1, reveal2}).%this is a threat, it probably stays unused.

%Betting involves a central manager, and all the gamblers.
%The central manager accepts multiple trades simultaniously at the same price, and charges a small fee for trading.
%We need a contract so that we can have betting happen in rounds with fixed prices.
%The central manager agrees to reveal his current price to trade at some point in time in an interval. If he fails to reveal, then he loses money on both sides.
%When he reveals his current price, all the open trades that have the current price as an acceptable price engage, the rest of the trades can either carry over to the next round, or be canceled, depending on the user's preference.

-record(reveal_price, {id, sign, market_id, round_id, price}).
-record(double_price, {id, sign, market_id, round_id, reveal_price1, reveal_price2}).
-record(failed_to_reveal, {id, sign, market_id, round_id}).
-record(new_trade, {id, market_id, amount, price_limit, time_limit}).
-record(engage_trade, {id, sign, market_id, round_id, reveal_price}).
-record(two_thirds_majority, {id, partner, sign, oracle_id, choice_id, revealss}).%this gets unlocked. 

