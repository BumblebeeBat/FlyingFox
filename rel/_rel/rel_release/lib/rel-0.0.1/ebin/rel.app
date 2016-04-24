{application, rel, [
	{description, "New project"},
	{vsn, "0.0.1"},
	{modules, ['accounts','all_secrets','arbitrage','backup','block_dump','block_finality','block_pointers','block_tree','channel_block_tx','channel_close_tx','channel_funds_limit_tx','channel_manager','channel_manager_feeder','channel_partner','channel_slash_tx','channel_timeout_tx','channels','compiler','constants','create_account_tx','db','delete_account_tx','download_blocks','encryption','entropy','euler_problems','external_handler','flying_fox_app','flying_fox_sup','fork_slash_tx','fractions','free_constants','gas_price','handler','hash','inbox','internal_handler','keys','language','mail','main_handler','oracle','packer','peers','port','rel_app','rel_sup','repo_tx','reveal','secrets','serve','sign','sign_tx','slasher_tx','spend_tx','talker','test_fff','test_fff_keys','tester','timing_experiment','to_channel_tx','tx_pool','tx_pool_feeder','txs']},
	{registered, [rel_sup]},
	{applications, [kernel,stdlib]},
	{mod, {rel_app, []}}
]}.