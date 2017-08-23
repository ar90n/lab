// from http://algoogle.hadrori.jp/algorithm/rbtree.html
#include <iostream>
#include <random>

using namespace std;

template<class T> class rbtree {
    public:
        struct node {
            T key;
            int red;
            node *par, *ch[2]; //{left, right}
            node(T key): key(key) {
                par = ch[0] = ch[1] = NULL;
            }
        };

        unsigned sz;
        node *root, *nil;

        rbtree():sz(0) {
            nil = new node(-1);
            nil->red = 0;
            root = nil->par = nil->ch[0] = nil->ch[1] = nil;
        }

        unsigned size() { return sz; }

        void print(){ print(root, 0, 0); }
        void print(node *v, int dep, int lr) {
            if(v == nil) return;
            print(v->ch[1],dep+1,1);
            for(int i = 0; i < dep; i++) cerr << "  ";
            if(!lr) cerr << "--";
            else if(lr == 1) cerr << "「";
            else cerr << "Ｌ";
            if(v->red) cerr << "\x1b[31m";
            cerr << v->key << endl;
            cerr << "\x1b[0m";
            print(v->ch[0],dep+1,2);
        }

        void rotate(node *x, int d) {
            int e = d^1;
            node *y = x->ch[e];
            x->ch[e] = y->ch[d];
            if(y->ch[d] != nil) y->ch[d]->par = x;
            y->par = x->par;
            if(x->par == nil) root = y;
            else if(x == x->par->ch[d]) x->par->ch[d] = y;
            else x->par->ch[e] = y;
            y->ch[d] = x;
            x->par = y;
        }

        node *find(T key) {
            node *x = root;
            while(x != nil) {
                if(key != x->key) x = x->ch[key > x->key];
                else return x;
            }

            return x;
        }

        node *insert(T key) {
            node *x = root, *y = nil;
            while(x != nil) {
                y = x;
                if(key != x->key) x = x->ch[key > x->key];
                else return x;
            }
            sz++;
            node *z = new node(key);
            z->par = y;
            if(y == nil) root = z;
            else y->ch[z->key >= y->key] = z;
            z->ch[0] = z->ch[1] = nil;
            insert_update(z);
            nil->par = nil->ch[0] = nil->ch[1] = nil;
            return z;
        }

        void insert_update(node *z) {
            node *y;
            while(z->par->red) {
                int d = z->par == z->par->par->ch[0] ? 0: 1, e = d^1;
                y = z->par->par->ch[e];
                if(y->red) {
                    z->par->red = 0;
                    y->red = 0;
                    z->par->par->red =1;
                    z = z->par->par;
                }
                else if(z == z->par->ch[e]) {
                    z = z->par;
                    rotate(z,d);
                }
                else {
                    z->par->red = 0;
                    z->par->par->red = 1;
                    rotate(z->par->par,e);
                }
            }

            root->red = 0;
        }

        void erase(T key) { erase(find(key)); }

        void erase(node *z) {
            if(z == nil) return;
            sz--;
            node *y = z, *x;
            int prevy = y->red;
            if(z->ch[0] == nil) {
                x = z->ch[0];
                transplant(z, z->ch[1]);
            }
            else if(z->ch[1] == nil) {
                x = z->ch[1];
                transplant(z, z->ch[0]);
            }
            else {
                y = minimum(z->ch[1]);
                prevy = y->red;
                x = y->ch[1];
                if(y->par == z) x->par = y;
                else {
                    transplant(y, y->ch[1]);
                    y->ch[1] = z->ch[1];
                    y->ch[1]->par = y;
                }
                transplant(z,y);
                y->ch[0] = z->ch[0];
                y->ch[0]->par = y;
                y->red = z->red;
            }
            nil->par = nil->ch[0] = nil->ch[1] = nil;
            if(!prevy) erase_update(x);
            delete z;
            nil->par = nil->ch[0] = nil->ch[1] = nil;
        }

        void transplant(node *u, node *v) {
            if(u->par == nil) root = v;
            else if(u == u->par->ch[0]) u->par->ch[0] = v;
            else u->par->ch[1] = v;
            v->par = u->par;
        }

        node *minimum(node* x) {
            while(x->ch[0] != nil) x = x->ch[0];
            return x;
        }

        void erase_update(node *x) {
            node *w = nil;
            while(x != root && !x->red) {
                print();
                int d = x == x->par->ch[0] ? 0 : 1, e = d^1;
                w = x->par->ch[e];
                if(w->red) {
                    w->red = 0;
                    x->par->red = 1;
                    rotate(x->par,d);
                    w = x->par->ch[e];
                }
                else if(!w->ch[d]->red && !w->ch[e]->red) {
                    w->red = 1;
                    x = x->par;
                }
                else if(!w->ch[e]->red) {
                    w->ch[d]->red = 0;
                    w->red = 1;
                    rotate(w,e);
                    w = x->par->ch[e];
                }
                else {
                    w->red = x->par->red;
                    x->par->red = 0;
                    w->ch[e]->red = 0;
                    rotate(x->par,d);
                    x = root;
                }
            }
            x->red = 0;
            print();
        }
};

int main(int argc, char *argv[])
{
    rbtree<int> tree;

    random_device rnd;
    for(int i = 0; i < 32; ++i )
    {
        tree.insert(rnd());
    }
    tree.print();

    return 0;
}
