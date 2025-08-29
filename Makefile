# COBOL IPCrypt Implementation Makefile
# Updated for Fixed-Format COBOL Compilation

# Compiler settings
COBC = cobc
COBCFLAGS = -std=cobol2002 -O2 -W -Wall
COBCFLAGS_TEST = -x -std=cobol2002 -O2 -W

# Source files
SOURCES = IPCRYPT-TABLES.cob IPCRYPT-UTILS.cob IPCRYPT-AES.cob IPCRYPT-LIB.cob
TEST_SOURCE = TEST-IPCRYPT.cob

# Output files
MODULES = $(SOURCES:.cob=.so)
TEST_EXEC = test-ipcrypt

# Default target
all: modules test

# Compile individual modules (as dynamic libraries)
%.so: %.cob
	@echo "Compiling module: $<"
	$(COBC) $(COBCFLAGS) -m $< -o $@

# Compile all modules
modules: $(MODULES)
	@echo "All modules compiled successfully!"

# Test syntax of individual files
syntax-check:
	@echo "Performing syntax checks on all COBOL files..."
	@for file in $(SOURCES) $(TEST_SOURCE); do \
		echo "  Checking $$file..."; \
		$(COBC) $(COBCFLAGS) -fsyntax-only $$file || exit 1; \
		echo "    ✓ $$file syntax OK"; \
	done
	@echo "All syntax checks completed successfully!"

# Compile test executable
$(TEST_EXEC): $(TEST_SOURCE) $(MODULES)
	@echo "Compiling test program..."
	$(COBC) $(COBCFLAGS_TEST) $(TEST_SOURCE) $(SOURCES) -o $(TEST_EXEC)

# Run tests
test: $(TEST_EXEC)
	@echo "Running IPCrypt test suite..."
	./$(TEST_EXEC)

# Compile specific modules with explicit commands
IPCRYPT-TABLES.so: IPCRYPT-TABLES.cob
	@echo "Compiling lookup tables module..."
	$(COBC) $(COBCFLAGS) -m IPCRYPT-TABLES.cob -o IPCRYPT-TABLES.so

IPCRYPT-UTILS.so: IPCRYPT-UTILS.cob
	@echo "Compiling utility functions module..."
	$(COBC) $(COBCFLAGS) -m IPCRYPT-UTILS.cob -o IPCRYPT-UTILS.so

IPCRYPT-AES.so: IPCRYPT-AES.cob IPCRYPT-TABLES.so
	@echo "Compiling AES implementation module..."
	$(COBC) $(COBCFLAGS) -m IPCRYPT-AES.cob -o IPCRYPT-AES.so

IPCRYPT-LIB.so: IPCRYPT-LIB.cob $(MODULES)
	@echo "Compiling main library module..."
	$(COBC) $(COBCFLAGS) -m IPCRYPT-LIB.cob -o IPCRYPT-LIB.so

# Development build
dev-build: clean syntax-check modules test
	@echo "Development build completed successfully!"

# Debug build with additional flags
debug: COBCFLAGS += -g -fdebugging-line
debug: clean modules test
	@echo "Debug build completed!"

# Clean build artifacts
clean:
	@echo "Cleaning build artifacts..."
	rm -f *.so $(TEST_EXEC) *.c *.c.* *.h *.i *.lst
	@echo "Clean completed!"

# Help target
help:
	@echo "IPCrypt COBOL Implementation Makefile"
	@echo "======================================"
	@echo ""
	@echo "Available targets:"
	@echo "  all           - Build modules and run tests (default)"
	@echo "  modules       - Build all library modules"
	@echo "  test          - Build and run test suite"
	@echo "  syntax-check  - Perform syntax checks on all files"
	@echo "  dev-build     - Clean, check, build, and test"
	@echo "  debug         - Build with debug information"
	@echo "  clean         - Remove build artifacts"
	@echo "  help          - Show this help message"
	@echo ""
	@echo "Compiler: $(COBC)"
	@echo "Flags: $(COBCFLAGS)"
	@echo "Test Flags: $(COBCFLAGS_TEST)"
	@echo ""
	@echo "Source files:"
	@for file in $(SOURCES); do echo "  - $$file"; done
	@echo "  - $(TEST_SOURCE)"

# Information about compiler and system
info:
	@echo "IPCrypt COBOL Build Information"
	@echo "==============================="
	@echo "Compiler: $(COBC)"
	@echo "Flags: $(COBCFLAGS)"
	@echo "Test Flags: $(COBCFLAGS_TEST)"
	@echo "Build date: $(shell date)"
	@echo "System: $(shell uname -a)"
	@echo ""
	@echo "GNU COBOL version:"
	@$(COBC) --version 2>/dev/null || echo "GNU COBOL not found or not in PATH"
	@echo ""
	@echo "Source modules:"
	@for file in $(SOURCES); do echo "  - $$file"; done
	@echo "  - $(TEST_SOURCE)"

# Check GNU COBOL installation
check-cobol:
	@echo "Checking GNU COBOL installation..."
	@which $(COBC) > /dev/null 2>&1 || { \
		echo "ERROR: GNU COBOL compiler '$(COBC)' not found"; \
		echo "Please install GNU COBOL (GnuCOBOL) and ensure it's in your PATH"; \
		exit 1; \
	}
	@echo "GNU COBOL found: $(shell which $(COBC))"
	@echo "Version: $(shell $(COBC) --version 2>/dev/null | head -n 1 || echo 'Version unknown')"
	@echo "Fixed-format compilation ready!"

# Phony targets
.PHONY: all modules test syntax-check dev-build debug clean help info check-cobol

# Default goal
.DEFAULT_GOAL := all
