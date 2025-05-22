PWD = $(shell pwd)
RLOX = $(PWD)/target/release/rlox
TEST_DIR = $(PWD)/tests
TEST_FILE = $(shell find $(TEST_DIR) -name "*.in" -exec basename {} \;) 

$(shell mkdir -p $(TEST_DIR)/res)
TEST_RES = $(addprefix $(TEST_DIR)/res/, $(TEST_FILE:%.in=%.pass))

all: build

build:
	@cargo build --release

#--------------------| TESTS |------------------#

test-clean:
	@rm -rf $(TEST_DIR)/res

test: build $(TEST_RES)
	@echo "Passed: `find $(TEST_DIR)/res/ -name *.pass | wc -l` , Failed: `find $(TEST_DIR)/res/ -name *.fail | wc -l`" 
	

$(TEST_DIR)/res/%.pass: $(TEST_DIR)/%.in build
	@echo "\033[33;1mTesting: $*\033[0m"
	@-{\
		$(RLOX) $< > $(TEST_DIR)/res/$*.res; \
		res=$$?; \
		if [ $$res -eq 101 ]; then \
			echo "\033[31;1mFail to run($$res)!\033[0m"; \
			touch $(TEST_DIR)/res/$*.fail; \
			exit 0; \
		fi; \
		diff -bBq $(TEST_DIR)/res/$*.res $(TEST_DIR)/$*.out > /dev/null; \
		if [ $$? -eq 0 ]; then \
			echo "\033[32;1mPass!\033[0m"; \
			touch $@; \
		else \
			echo "\033[31;1mFail! expect: $$(cat $(TEST_DIR)/$*.out), actual: $$(cat $(TEST_DIR)/res/$*.res ) \033[0m"; \
			touch $(TEST_DIR)/res/$*.fail; \
		fi; \
	}