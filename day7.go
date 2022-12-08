package main

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"strconv"
	"strings"
)

func main() {
	var input io.Reader = os.Stdin

	if err := run(input); err != nil {
		fmt.Printf("Error: %v", err)
		os.Exit(1)
	}
}

func run(r io.Reader) error {
	ch := make(chan string)
	go func() {
		defer close(ch)

		scanner := bufio.NewScanner(r)
		for scanner.Scan() {
			ch <- scanner.Text()
		}
		if err := scanner.Err(); err != nil {
			panic(fmt.Errorf("scanning: %w", err))
		}
	}()

	_, sum, err := dirSize(ch)
	if err != nil {
		return fmt.Errorf("dir size: %w", err)
	}

	fmt.Println(sum)
	return nil

}

func dirSize(ch <-chan string) (int, int, error) {
	var mySize int
	var mySum int

Loop:
	for line := range ch {
		switch {
		case line == "$ cd ..":
			break Loop

		case strings.HasPrefix(line, "$ cd"):
			size, sum, err := dirSize(ch)
			if err != nil {
				return 0, 0, err
			}
			mySize += size
			mySum += sum

		case line == "$ ls", strings.HasPrefix(line, "dir"):
			continue

		default:
			sizeStr, _, ok := strings.Cut(line, " ")
			if !ok {
				return 0, 0, fmt.Errorf("unknown line %s", line)
			}

			size, err := strconv.Atoi(sizeStr)
			if err != nil {
				return 0, 0, fmt.Errorf("first part not an int in line %s", line)
			}
			mySize += size
		}
	}

	if mySize <= 100000 {
		mySum += mySize
	}
	return mySize, mySum, nil
}
