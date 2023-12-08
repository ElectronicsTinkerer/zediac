
SRC_DIR = .
BUILD_DIR = build

SRC    = curses.asm
BINARY = curses.bin
LIST   = curses.lst
SYMLST = curses.sym

all: $(BUILD_DIR)/$(BINARY)
$(BUILD_DIR)/$(BINARY): $(SRC_DIR)/$(SRC)
	slime --asm $(SRC) --out $(BINARY) --base-dir $(SRC_DIR) --output-dir $(BUILD_DIR) --listing $(LIST) --sym $(SYMLST) --hidden

# sim: all
#	816ce --cmd-file sim/zediac.816ce

clean:
	rm -rf $(BUILD_DIR)


