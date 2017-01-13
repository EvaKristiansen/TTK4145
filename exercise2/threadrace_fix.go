
package main

import(
	. "fmt"
	"runtime"
	"time"
)

var counterbuff = make(chan int, 1);
var busybuff = make(chan bool, 2);

func thread_1(){
	for j := 0; j < 1000000; j++{
		check := true
		for (check == true){
			check = <- busybuff
			busybuff <- true
		}
		Println("plusser")

		i := <- counterbuff
		
		i++;
		counterbuff <- i

		<- busybuff
		busybuff <- false
		
	}
}
func thread_2(){
	for j := 0; j < 1000000; j++{
		check := true
		for (check == true){
			check = <- busybuff
			busybuff <- true
		}

		i := <- counterbuff
		Println("hei")

		i--;
		counterbuff <- i

		<- busybuff
		busybuff <- false
	}
}

func main(){
	i := 0
	counterbuff <- i
	busybuff <- false 
	runtime.GOMAXPROCS(runtime.NumCPU())
	go thread_1()
	go thread_2()
	
	time.Sleep(100*time.Millisecond)
	i = <- counterbuff	
	Println(i)
}






