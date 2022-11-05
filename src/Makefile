# CFLAGS+=-I../lua

SRC= \
hksc.c \
hksclib.c \
lctype.c \
lcode.c \
ldebug.c \
ldump.c \
lerror.c \
lfunc.c \
lgc.c \
llex.c \
lmem.c \
lobject.c \
lopcodes.c \
lparser.c \
lstate.c \
lstring.c \
ltable.c \
lundump.c \
lzio.c

ALL_O:=$(SRC:%.c=%.o)
ALL_D:=$(SRC:%.c=%.d)

.PHONY: all clean depend echo test

all: hksc

hksc: $(ALL_O)
	$(CC) -o $@ $^

$(ALL_O): %.o: %.c
	$(CC) -MT $@ -MMD -MP -MF $*.d -c $< -o $@ $(CFLAGS)
# 	$(CC) $(CFLAGS) -c $< -o $@

test: hksc
	./hksc ../test.lua

clean:
	$(RM) $(ALL_O) $(ALL_D)

depend:
	@$(CC) $(CFLAGS) -MM *.c

echo:
	@echo "CC = $(CC)"
	@echo "CFLAGS = $(CFLAGS)"
	@echo "AR = $(AR)"
	@echo "RANLIB = $(RANLIB)"
	@echo "RM = $(RM)"
	@echo "MYCFLAGS = $(MYCFLAGS)"
	@echo "MYLDFLAGS = $(MYLDFLAGS)"
	@echo "MYLIBS = $(MYLIBS)"
	@echo "DL = $(DL)"

hksc.o: hksc.c

include $(wildcard $(ALL_D))
