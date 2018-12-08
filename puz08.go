package main

import (
	"bufio"
	"log"
	"os"
	"strconv"
)

type node struct {
	children []*node
	metadata []int
}

func (n *node) visit(f func(n *node)) {
	f(n)
	for _, c := range n.children {
		c.visit(f)
	}
}

func (n *node) visitIndexed(f func(n *node)) {
	f(n)
	l := len(n.children)
	if l == 0 {
		return
	}
	for _, i := range n.metadata {
		if i > 0 && i < l+1 {
			n.children[i-1].visitIndexed(f)
		}
	}
}

func parseNode(ns []int) (*node, int) {
	nc := ns[0]
	nm := ns[1]
	n := &node{children: make([]*node, nc)}
	offset := 2
	for i := 0; i < nc; i++ {
		var l int
		n.children[i], l = parseNode(ns[offset:])
		offset += l
	}
	n.metadata = ns[offset : offset+nm]
	offset += nm
	return n, offset
}

func main() {
	var ns []int
	s := bufio.NewScanner(os.Stdin)
	s.Split(bufio.ScanWords)
	for s.Scan() {
		i, err := strconv.Atoi(s.Text())
		if err != nil {
			log.Fatalln("Parsing int failed:", err)
		}
		ns = append(ns, i)
	}

	root, _ := parseNode(ns)
	sum := 0
	root.visit(func(n *node) {
		for _, v := range n.metadata {
			sum += v
		}
	})
	log.Println("Sum of metadata:", sum)

	sum = 0
	root.visitIndexed(func(n *node) {
		if len(n.children) > 0 {
			return
		}
		for _, v := range n.metadata {
			sum += v
		}
	})
	log.Println("Sum of indexed metadata:", sum)
}
