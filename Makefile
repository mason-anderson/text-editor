bin_name = editor

CC=gcc
CFLAGS=-ggdb -std=c99 -Wall -Wextra -Werror -pedantic

SRCS=editor.c
OBJS=$(SRCS:%.c=%.o)

.PHONY: clean

all: $(bin_name)

%.o: %.c
	$(CC) -c -o $@ $< $(CFLAGS)

$(bin_name): $(OBJS)
	$(CC) -o $@ $(OBJS) $(CFLAGS)

clean:
	@echo "cleaning..."
	rm -f $(OBJS)
	rm -f $(bin_name)

