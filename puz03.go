package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
)

type tile struct {
	x, y int
}

type claim struct {
	tile
	id   int
	w, h int
}

func main() {
	grid := make(map[tile]int)
	idOverlaps := make(map[int]bool)
	s := bufio.NewScanner(os.Stdin)
	overlaps := 0
	for s.Scan() {
		var c claim
		if _, err := fmt.Sscanf(s.Text(), "#%d @ %d,%d: %dx%d", &c.id, &c.x, &c.y, &c.w, &c.h); err != nil {
			log.Fatalln("Unexpected input:", err, "; line:", s.Text())
		}
		if _, ok := idOverlaps[c.id]; ok {
			log.Fatalln("Duplicate input:", c.id)
		}
		idOverlaps[c.id] = false
		for i := 0; i < c.w; i++ {
			for j := 0; j < c.h; j++ {
				t := tile{x: c.x + i, y: c.y + j}
				n := grid[t]
				if n == 0 {
					grid[t] = c.id
				} else {
					idOverlaps[c.id] = true
					if n > 0 {
						overlaps++
						idOverlaps[n] = true
						grid[t] = -1
					}
				}
			}
		}
	}
	log.Println("Overlapping tiles:", overlaps)
	for k, v := range idOverlaps {
		if !v {
			log.Println("Non-overlapped claim ID:", k)
		}
	}
}
