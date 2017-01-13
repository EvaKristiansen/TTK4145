#include <pthread.h>
#include <stdio.h>

int i = 0;
pthread_mutex_t lock;

void thread1(){
	for (int x = 0; x < 1000000; x++){
	pthread_mutex_lock(&lock);
	i++;
	pthread_mutex_unlock(&lock);
	}
}

void thread2(){
	for (int x = 0; x < 1000000; x++){
	pthread_mutex_lock(&lock);
	i--;
	pthread_mutex_unlock(&lock);
	}
}

int main(){
	phtread_mutex_init(&lock,NULL);
	
	pthread_t t1;
	pthread_create(&t1, NULL,thread1,NULL);
	
	pthread_t t2;
	pthread_create(&t2, NULL,thread2,NULL);
	pthread_join(t1,NULL);
	pthread_join(t2,NULL);

	pthread_mutex_destroy(&lock);

	printf("%d",i);
	return 0;
}
