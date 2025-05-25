PWD = $(shell pwd)
RLOX = $(PWD)/target/release/lox
TEST_DIR = $(PWD)/tests

ifeq ($(TEST), ) 
TEST = $(shell find $(TEST_DIR) -name "*.in" -exec basename {} \;) 
endif

$(shell mkdir -p $(TEST_DIR)/res)
TEST_RES = $(addprefix $(TEST_DIR)/res/, $(TEST:%.in=%.pass))

all: build

build:
	@cargo build --release

submit: build
	@-rm lox.zip
	@-rm -r target/debug target/release/.fingerprint target/release/deps target/release/build target/release/examples target/release/incremental 
	@{ \
		cd ..;\
		zip -r lox.zip ./lox;\
		mv lox.zip ./lox;\
	}

#--------------------| TESTS |------------------#

test-clean:
	@rm -rf $(TEST_DIR)/res

test: build $(TEST_RES)
	@echo "\033[32;1mPassed: `find $(TEST_DIR)/res/ -name *.pass | wc -l`(`find $(TEST_DIR)/res/ -name *.pass -exec basename {} \;`) , \033[31;1mFailed: `find $(TEST_DIR)/res/ -name *.fail | wc -l`(`find $(TEST_DIR)/res/ -name *.fail -exec basename {} \;`)\033[0m" 
	

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
			expect=$$(cat $(TEST_DIR)/$*.out); \
			actual=$$(cat $(TEST_DIR)/res/$*.res); \
			echo "\033[31;1mFail! Expect: \033[0m$$expect"; \
			echo "\033[31;1mActual: \033[0m$$actual"; \
			touch $(TEST_DIR)/res/$*.fail; \
		fi; \
	}