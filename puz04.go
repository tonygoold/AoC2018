package main

import (
	"bufio"
	"errors"
	"fmt"
	"log"
	"os"
	"sort"
	"strings"
)

const (
	EventSentinel = 0
	EventStart    = 1
	EventWake     = 2
	EventSleep    = 3
)

type guard [60]int

func (g *guard) nap(from, to int) {
	for i := from; i < to; i++ {
		g[i]++
	}
}

func (g *guard) sum() int {
	s := 0
	for _, n := range *g {
		s += n
	}
	return s
}

func (g *guard) mostFreq() int {
	var maxm, maxn int
	for m, n := range *g {
		if n > maxn {
			maxm, maxn = m, n
		}
	}
	return maxm
}

type event struct {
	date   int
	minute int
	event  int
	guard  int
}

func newEvent(y, m, d, h, min int) event {
	return event{date: 1000000*y + 10000*m + 100*d + h, minute: min}
}

func main() {
	eq, err := readEvents()
	if err != nil {
		log.Fatalln("Error parsing input:", err)
	}

	sort.Slice(eq, func(i, j int) bool {
		ei, ej := eq[i], eq[j]
		if ei.date == ej.date {
			return ei.minute < ej.minute
		}
		return ei.date < ej.date
	})

	gs := mapEvents(eq)

	// Find the sleepiest guard and their sleepiest minute
	var maxgid, maxmins, minute int
	for gid, g := range gs {
		s := g.sum()
		if s > maxmins {
			maxmins = s
			maxgid = gid
			minute = g.mostFreq()
		}
	}

	log.Println(
		"Sleepiest guard ID:", maxgid,
		"Minute:", minute,
		"Solution:", maxgid*minute,
	)

	maxgid = 0
	maxmins = 0
	minute = 0
	for gid, g := range gs {
		m := g.mostFreq()
		if g[m] > maxmins {
			maxgid = gid
			maxmins = g[m]
			minute = m
		}
	}
	log.Println(
		"Most consistent guard ID:", maxgid,
		"Minute:", minute,
		"Solution:", maxgid*minute,
	)
}

func readEvents() ([]event, error) {
	var eq []event
	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		var y, m, d, h, mi int
		var evt string
		t := scanner.Text()
		if false {
			i := strings.Index(t, "]")
			if i < 0 {
				return nil, errors.New("Did not find closing bracket in input")
			}
			if _, err := fmt.Sscanf(t[:i+1], "[%d-%d-%d %d:%d]", &y, &m, &d, &h, &mi); err != nil {
				return nil, err
			}
			evt = t[i+2:]
		} else {
			if _, err := fmt.Sscanf(t, "[%d-%d-%d %d:%d]", &y, &m, &d, &h, &mi); err != nil {
				return nil, err
			}
			evt = t[19:]
		}
		e := newEvent(y, m, d, h, mi)
		if evt == "falls asleep" {
			e.event = EventSleep
		} else if evt == "wakes up" {
			e.event = EventWake
		} else {
			if _, err := fmt.Sscanf(evt, "Guard #%d begins shift", &e.guard); err != nil {
				return nil, err
			}
			e.event = EventStart
		}
		eq = append(eq, e)
	}
	if err := scanner.Err(); err != nil {
		return nil, err
	}
	return eq, nil
}

func mapEvents(eq []event) map[int]*guard {
	gid := -1
	gs := make(map[int]*guard)
	var prev event
	for _, e := range eq {
		switch e.event {
		case EventStart:
			if prev.event == EventSleep {
				log.Fatalln("The previous guard never woke up:", e)
			}
			gid = e.guard
			if gs[gid] == nil {
				gs[gid] = new(guard)
			}
		case EventSleep:
			if prev.event != EventStart && prev.event != EventWake {
				log.Fatalln("Unexpected sleep:", e)
			}
		case EventWake:
			if prev.event != EventSleep {
				log.Fatalln("Unexpected wake:", e)
			}
			gs[gid].nap(prev.minute, e.minute)
		default:
			log.Fatalln("Unexpected event:", e)
		}
		prev = e
	}
	return gs
}
