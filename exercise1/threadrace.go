
package main

import(
	. "fmt"
	"runtime"
	"time"
)

var i = 0;

func thread_1(){
	for j := 0; j < 1000000; j++{
		i++;
	}
}
func thread_2(){
	for k := 0; k < 1000000; k++{
		i--;
	}
}

func main(){
	runtime.GOMAXPROCS(2)
	go thread_1()
	go thread_2()
	
	time.Sleep(100*time.Millisecond)
	Println(i)
}






