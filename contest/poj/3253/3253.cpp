#include <iostream>
#include <queue>

using namespace std;

int main(int argc, const char *argv[])
{
    long long n;
    cin >> n;

    priority_queue<long long> queue;
    for(int i = 0;i < n; i++)
    {
        int v;
        cin >> v;
        queue.push(-v);
    }

    long long ret = 0LL;
    while(1 < queue.size())
    {
        long long a = queue.top(); queue.pop();
        long long b = queue.top(); queue.pop();
        long long c = a + b;
        ret += c;
        queue.push(c);
    }
    
    cout << -ret << endl;
    return 0;
}
