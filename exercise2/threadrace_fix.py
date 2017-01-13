import threading

i = 0
lock = threading.Lock()

def thread_1():
	global i
	for x in range (0,1000000):
		lock.acquire()
		i+=1
		lock.release()
def thread_2():
	global i
	for x in range (0,1000001):
		lock.acquire()
		i-=1
		lock.release()

def main():
	Thread1 = threading.Thread(target=thread_1,args=(),)
	Thread1.start()
	Thread2 = threading.Thread(target=thread_2,args=(),)
	Thread2.start()
	
	Thread1.join()
	Thread2.join()
	print('Hei')
	print(i)
	
main()
