from threading import Thread
i = 0

def thread_1():
	global i
	for x in range (0,1000000):
		Lock.aquire()
		i+=1
		Lock.release()
def thread_2():
	global i
	for x in range (0,1000000):
		Lock.aquire
		i-=1
		Lock.release()

def main():
	Thread1 = Thread(target=thread_1,args=(),)
	Thread1.start()
	Thread2 = Thread(target=thread_2,args=(),)
	Thread2.start()
	
	Thread1.join()
	Thread2.join()
	print('Hei')
	print(i)
	
main()
