DATA_FILE=mainnet.yaml

all: yaml

.SECONDARY:

yaml: tx_rotate.yaml tx_sha256.yaml tx_btc-oracle.yaml tx_bro-lottery.yaml tx_bro-lottery-helpers.yaml tx_init.yaml

.pact:
	mkdir -p .pact

.pact/%.pact: templates/%.m4 .pact defs.m4
	m4 -I. $< > $@

tx_init.yaml: templates/init.tkpl ${DATA_FILE}
	kda gen -t $< -d ${DATA_FILE} -o $@

tx_rotate.yaml: templates/rotate.tkpl ${DATA_FILE}
	kda gen -t $< -d ${DATA_FILE} -o $@


tx_%.yaml: templates/%.tkpl .pact/%.pact ${DATA_FILE}
	kda gen -t $< -d ${DATA_FILE} -o $@

make clean:
	rm -rf .pact
	rm -f tx_*.yaml
