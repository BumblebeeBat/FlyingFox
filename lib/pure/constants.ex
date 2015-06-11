defmodule Constants do
  #we need this instead of a config file to support more types of testing.
  def initial_coins do 2100_000_000_000_000 end
  def min_bond do 2_000_000_000 end
  def min_tx_fee do 5000 end
  def block_creation_fee do 31_000_000 end
  def max_bond do 1_000_000_000 end
  def signers_per_block do 54 end
  def epoch do 50 end
  def chances_per_address do 200 end
  def tcp_port do 6666 end
	def port_d do 1111 end
	def oracle_fee do 50_000_000 end
	def message_size do 10000 end #in bytes
	def max_nodes do 20 end
end
