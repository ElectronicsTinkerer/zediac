
SRC_DIR = src
BUILD_DIR = build

SRC    = monitor.asm
BINARY = monitor.bin
LIST   = monitor.lst
SYMLST = monitor.sym

all: $(BUILD_DIR)/$(BINARY)
$(BUILD_DIR)/$(BINARY): $(SRC_DIR)/$(SRC)
	slime --asm $(SRC) --out $(BINARY) --base-dir $(SRC_DIR) --output-dir $(BUILD_DIR) --listing $(LIST) --sym $(SYMLST)

sim: all
	816ce --cmd-file sim/zediac.816ce

clean:
	rm -rf $(BUILD_DIR)


