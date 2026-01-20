# Makefile for pure-tls
#
# SPDX-License-Identifier: MIT
# Copyright (C) 2026 Anthony Green <green@moxielogic.com>

SBCL := sbcl --noinform --non-interactive

# Common Lisp setup for ASDF (wrapped in progn for --eval)
# Push ocicl dirs first, then current dir so local project takes precedence
#SETUP := (progn \
#           (require :asdf) \
#           (dolist (dir (directory "ocicl/*/")) \
#             (push dir asdf:*central-registry*)) \
#           (push (truename ".") asdf:*central-registry*))
SETUP := 1

.PHONY: all test check all-tests unit-tests network-tests boringssl-tests boringssl-shim load clean help save-test-baseline check-regressions

all: help

help:
	@echo "pure-tls Makefile targets:"
	@echo "  test            - Run all tests (default)"
	@echo "  check           - Alias for test"
	@echo "  all-tests       - Run all tests (unit, network, verify, connect, boringssl)"
	@echo "  unit-tests      - Run unit tests (crypto, record, handshake, certificate)"
	@echo "  network-tests   - Run network integration tests (requires internet)"
	@echo "  boringssl-shim  - Build the BoringSSL test shim"
	@echo "  boringssl-tests - Run BoringSSL TLS 1.3 test suite"
	@echo "  save-test-baseline - Save current BoringSSL test results as baseline"
	@echo "  check-regressions  - Run BoringSSL tests and check for regressions"
	@echo "  load            - Load pure-tls and verify compilation"
	@echo "  connect         - Run connection test against example.com"
	@echo "  verify          - Run certificate verification tests"
	@echo "  clean           - Remove compiled files"

# Run all tests
test: all-tests

check: all-tests

all-tests: unit-tests network-tests verify connect boringssl-tests

unit-tests:
	@echo "=== Running pure-tls Unit Tests ==="
	$(SBCL) --eval '$(SETUP)' \
	        --eval '(asdf:load-system :pure-tls/test)' \
	        --eval '(if (pure-tls/test:run-tests) (sb-ext:exit :code 0) (sb-ext:exit :code 1))'

# Run network tests (requires internet connectivity)
network-tests:
	@echo "=== Running pure-tls Network Tests ==="
	$(SBCL) --eval '$(SETUP)' \
	        --eval '(asdf:load-system :pure-tls/test)' \
	        --eval '(if (pure-tls/test:run-network-tests) (sb-ext:exit :code 0) (sb-ext:exit :code 1))'

# Load test - verify everything compiles
load:
	@echo "=== Loading pure-tls ==="
	$(SBCL) --eval '$(SETUP)' \
	        --eval '(asdf:load-system :pure-tls)' \
	        --eval '(format t "~%pure-tls loaded successfully!~%")' \
	        --eval '(asdf:load-system :pure-tls/test)' \
	        --eval '(format t "pure-tls/test loaded successfully!~%")' \
	        --eval '(sb-ext:exit :code 0)'

# Integration test - connect to example.com
connect:
	@echo "=== Running Connection Test ==="
	$(SBCL) --script test-connect.lisp

# Certificate verification tests
verify:
	@echo "=== Running Verification Tests ==="
	$(SBCL) --script test-verify.lisp

# Build the BoringSSL test shim
boringssl-shim: pure-tls-shim

pure-tls-shim:
	@echo "=== Building BoringSSL Test Shim ==="
	$(SBCL) --eval '$(SETUP)' \
	        --eval '(asdf:load-system :pure-tls)' \
	        --eval '(require :usocket)' \
	        --eval '(require :babel)' \
	        --load test/boringssl-shim.lisp \
	        --eval '(pure-tls/boringssl-shim:build-shim)'

# Run BoringSSL TLS 1.3 tests
boringssl-tests: pure-tls-shim
	@echo "=== Running BoringSSL Tests ==="
	./test/run-boringssl-tests.sh

# Save BoringSSL test results as baseline for regression detection
save-test-baseline: pure-tls-shim
	@echo "=== Saving BoringSSL Test Baseline ==="
	./test/track-regressions.sh --save-baseline

# Run BoringSSL tests and check for regressions against baseline
check-regressions: pure-tls-shim
	@echo "=== Checking for Test Regressions ==="
	./test/track-regressions.sh --compare

# Clean compiled files
clean:
	find . -name "*.fasl" -delete
	find . -name "*.fas" -delete
	find . -name "*.lib" -delete
	find . -name "*.o" -delete
	rm -f pure-tls-shim
	@echo "Cleaned compiled files"
