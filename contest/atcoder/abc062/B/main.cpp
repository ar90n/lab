#include <bits/stdc++.h>
using namespace std;



int main(){
    int  h, w;
    cin >> h >> w;
    stringstream ss;
    for(int i = 0; i < (w+2); ++i){
        ss << "#";
    }
    ss << endl;

    for(int i = 0; i < h; ++i){
        string s;
        cin >> s;
        ss << "#" << s << "#" << endl;
    }

    for(int i = 0; i < (w+2); ++i){
        ss << "#";
    }
    ss << endl;

    // Failed to predict input format
    cout << ss.str();
    return 0;
}
