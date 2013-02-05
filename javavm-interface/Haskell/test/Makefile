
TOP=$(realpath $(shell pwd)/../../..)
include $(TOP)/mk/common.mk

TESTDIRS=HelloWorld Exception

check:
	$(MAKE) -C HelloWorld check
	$(MAKE) -C Exception check

clean::
	$(MAKE) -C HelloWorld clean
	$(MAKE) -C Exception clean

