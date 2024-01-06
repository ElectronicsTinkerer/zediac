
SRC_DIR = .
BUILD_DIR = build

SRC    = tracker.asm
BINARY = tracker.bin
LIST   = tracker.lst
SYMLST = tracker.sym

all: $(BUILD_DIR)/$(BINARY)
$(BUILD_DIR)/$(BINARY): $(SRC_DIR)/$(SRC)
	slime --asm $(SRC) --out $(BINARY) --base-dir $(SRC_DIR) --output-dir $(BUILD_DIR) --listing $(LIST) --sym $(SYMLST) --zedia-loader --hidden 

# sim: all
#	816ce --cmd-file sim/zediac.816ce

clean:
	rm -rf $(BUILD_DIR)


