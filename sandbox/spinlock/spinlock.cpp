#include <iostream>
#include <atomic>
#include <thread>

int data;
std::atomic<bool> ready(false);

void f()
{
    while(!ready.load(std::memory_order_acquire)) {
    }

    std::cout << data << std::endl;
}

int main()
{
    std::thread t(f);

    data = 1;
    ready.store(true, std::memory_order_release);

    t.join();
}
