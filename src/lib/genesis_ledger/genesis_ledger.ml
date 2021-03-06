[%%import
"../../config.mlh"]

[%%if
defined genesis_ledger]

[%%if
genesis_ledger = "release"]

include Release_ledger

[%%elif
genesis_ledger = "test"]

include Test_ledger

[%%elif
genesis_ledger = "test_split_two_stakers"]

include Test_split_two_stakes_ledger

[%%elif
genesis_ledger = "test_five_even_stakes"]

include Test_five_even_stakes

[%%elif
genesis_ledger = "testnet_posig"]

include Testnet_posig_ledger

[%%elif
genesis_ledger = "testnet_postake"]

include Testnet_postake_ledger

[%%else]

[%%error
"\"genesis_ledger\" is set to invalid value in config.mlh"]

[%%endif]

[%%else]

[%%error
"\"genesis_ledger\" not set in config.mlh"]

[%%endif]
