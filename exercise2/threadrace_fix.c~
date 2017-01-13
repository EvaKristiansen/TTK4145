#include <pthread.h>
#include <stdio.h>

int i = 0;

void thread1(){
	for (int x = 0; x < 1000000; x++){
	i++;
	}
}

void thread2(){
	for (int x = 0; x < 1000000; x++){
	i--;
	}
}

int main(){
	pthread_t t1;
	pthread_create(&t1, NULL,thread1,NULL);
	
	pthread_t t2;
	pthread_create(&t2, NULL,thread2,NULL);
	pthread_join(t1,NULL);
	pthread_join(t2,NULL);
	printf("%d",i);
	return 0;
}
