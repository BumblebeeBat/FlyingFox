"""This is to make magic numbers easier to deal with."""
import multiprocessing, os
peers={'192.241.212.114:7900':{'port':7900, 'blacklist':0, 'length':0, 'lag':40.0}}#, 
#'127.0.0.1:7900':{'port':7900, 'blacklist':0, 'length':0, 'lag':40.0}}
max_block_size=10#kilobytes
current_loc=os.path.dirname(os.path.abspath(__file__))
database_name = os.path.join(current_loc, 'DB')
log_file=os.path.join(current_loc, 'log')
port=7900
api_port=port-1
database_port=port-2
version = "0.0003"
all_money=21*10**15
creator='115nxUddLmxijWskiz5znHxk1KdMZpS'#the password to make this address is: "brainwallet"
max_key_length=6**4
block_fee = lambda x: 100000*(2**(x-1))
reward_blockmaker_vs_signers=lambda x: int(x/2)
signers=64
default_spend_fee=1000
# Lower limits on what the "time" tag in a block can say.
#get rid of decimal.
#for vitalik's slasher, 3000, 1000, 100
long_time=18#each needs to be at least 3x bigger 
medium_time=6
short_time=2
maximum_deposit=all_money/signers/long_time/2
minimum_deposit=maximum_deposit/100
jackpot_nonces=200
mmm = 100
download_many = 50  # Max number of blocks to request from a peer at the same time.
max_download = 58000
#buy_shares_target='0'*4+'1'+'9'*59
blocktime=60
DB = {
    'reward_peers_queue':multiprocessing.Queue(),
    'suggested_blocks': multiprocessing.Queue(),
    'suggested_txs': multiprocessing.Queue(),
    'heart_queue': multiprocessing.Queue(),
}


