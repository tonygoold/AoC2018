package main

import (
	"bytes"
	"io"
	"log"
	"os"
)

func cancels(x, y byte) bool {
	if x > y {
		x, y = y, x
	}
	return y-x == 32 && x >= 65 && x <= 90
}

func polyLength(r io.ByteReader, lc, uc byte) int {
	var bs []byte
	var b byte
	var err error
	for b, err = r.ReadByte(); err == nil; b, err = r.ReadByte() {
		if b == lc || b == uc || b < 65 || (b > 90 && b < 97) || b > 122 {
			continue
		}
		bs = append(bs, b)
		l := len(bs)
		if l > 1 && cancels(bs[l-2], bs[l-1]) {
			bs = bs[:l-2]
		}
	}
	return len(bs)
}

func main() {
	buf := &bytes.Buffer{}
	if _, err := buf.ReadFrom(os.Stdin); err != nil {
		log.Fatalln("Unable to read input:", err)
	}
	r := bytes.NewReader(buf.Bytes())
	l := polyLength(r, 0, 0)
	log.Println("Polymer length:", l)

	for b := byte(65); b <= 90; b++ {
		r.Seek(0, io.SeekStart)
		limp := polyLength(r, b, b+32)
		if limp < l {
			l = limp
		}
	}
	log.Println("Improved polymer length:", l)
}
