"""Compares the results of the Rust solver and of the Rocq model: compare.py cases rust rocq [examples]."""
import sys,re,collections
cases=open(sys.argv[1]).read().splitlines()
def parse(p):
    r={}
    for l in open(p):
        k,_,v=l.rstrip('\n').partition('|')
        o=[]
        for opt in (v.split(' || ') if v else []):
            d={}
            for e in opt.split(';'):
                if e:
                    s,_,x=e.rpartition('=');d[s]=float(x)
            o.append(d)
        r[int(k)]=o
    return r
a=parse(sys.argv[2]);b=parse(sys.argv[3])
TOL=2e-7
def same(x,y):
    ks=set(x)|set(y)
    return all(abs(x.get(k,0)-y.get(k,0))<=TOL for k in ks)
bad=[];ok=0;empty=0;byb=collections.Counter()
for i,c in enumerate(cases):
    x,y=a.get(i),b.get(i)
    if x is None or y is None: bad.append((i,'missing'));continue
    if not x and not y: empty+=1;continue
    yy=list(y);m=True
    for o in x:
        f=next((j for j,p in enumerate(yy) if same(o,p)),None)
        if f is None: m=False;break
        yy.pop(f)
    if m and not yy: ok+=1;byb[c.split('|')[0]]+=1
    else: bad.append((i,None))
print('cases',len(cases),'agree',ok,'both empty',empty,'differ',len(bad),dict(byb))
chs=collections.Counter()
for i,_ in bad: 
    p=cases[i].split('|');chs[(p[0],p[4])]+=1
print('differing chapters',len(chs),chs.most_common(25))
for i,_ in bad[:int(sys.argv[4]) if len(sys.argv)>4 else 3]:
    print('CASE',cases[i]);x,y=a.get(i),b.get(i)
    only=[o for o in (x or []) if not any(same(o,p) for p in (y or []))]
    oy=[o for o in (y or []) if not any(same(o,p) for p in (x or []))]
    print(' rust-only',len(only),str(only[:2])[:600]);print(' rocq-only',len(oy),str(oy[:2])[:600]);print(' counts',len(x or []),len(y or []))
